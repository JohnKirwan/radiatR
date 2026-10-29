# Runtime behind radiatR_tools(): a per-session object store, a runner that
# evaluates and records calls, argument checks, and the JSON result envelope.
# Nothing here depends on ellmer.

# ---- argument errors ----------------------------------------------------------
.agent_missing <- function(arg, why = NULL) {
  rlang::abort(
    sprintf("`%s` is required%s; it is never guessed or defaulted.", arg,
            if (is.null(why)) "" else paste0(" ", why)),
    class = "radiatR_missing_argument", arg = arg)
}

.agent_invalid <- function(arg, what) {
  rlang::abort(sprintf("`%s` must be %s.", arg, what),
               class = "radiatR_invalid_argument", arg = arg)
}

# ---- store -------------------------------------------------------------------
# `objects` holds session objects under their handle names (tracks_1, ...).
# Its parent is baseenv() so emitted, namespace-qualified code evaluates the
# same way in a fresh session. `log` has one entry of code per successful tool
# call and `results` the value that code returned; `counts` is the last number
# used per handle prefix.
.agent_store <- function() {
  store <- new.env(parent = emptyenv())
  store$objects <- new.env(parent = baseenv())
  store$log <- character(0)
  store$results <- list()
  store$counts <- list()
  store
}

.agent_next_handle <- function(store, prefix) {
  paste0(prefix, "_", (store$counts[[prefix]] %||% 0L) + 1L)
}

.agent_get <- function(store, handle, class = NULL) {
  if (is.null(handle)) .agent_missing("handle")
  have <- ls(store$objects)
  if (!is.character(handle) || length(handle) != 1L || !handle %in% have) {
    rlang::abort(
      sprintf("Unknown handle '%s'. Existing handles: %s.",
              paste(format(handle), collapse = " "),
              if (length(have)) paste(have, collapse = ", ") else "none"),
      class = "radiatR_unknown_handle", handle = handle, handles = have)
  }
  obj <- get(handle, envir = store$objects, inherits = FALSE)
  if (!is.null(class) && !inherits(obj, class)) {
    rlang::abort(
      sprintf("Handle '%s' is a %s; this tool needs a %s handle.",
              handle, class(obj)[1L], paste(class, collapse = " or ")),
      class = "radiatR_wrong_handle_type", handle = handle, expected = class)
  }
  obj
}

# ---- runner ------------------------------------------------------------------
# One step = one call, optionally assigned to a handle. `check(value)` runs
# straight after the call, inside the same all-or-nothing scope, so a check
# that aborts leaves no trace of the step.
.agent_step <- function(fn, args = list(), ns = "radiatR", creates = NULL,
                        check = NULL) {
  call <- rlang::call2(fn, !!!args, .ns = ns)
  if (!is.null(creates)) call <- rlang::call2("<-", rlang::sym(creates), call)
  list(stmt = call, creates = creates, check = check)
}

.agent_doubles <- function(x) {
  if (is.call(x) || is.list(x)) unlist(lapply(as.list(x), .agent_doubles))
  else if (is.double(x)) x
}

# Emitted code must reproduce numeric arguments exactly. R's default 15
# significant digits do that for most values (0.2 stays "0.2"); 17 digits are
# used only when some argument would otherwise change (0.1 + 0.2).
.agent_deparse <- function(call) {
  exact <- vapply(.agent_doubles(call), function(v)
    !is.finite(v) || identical(as.numeric(deparse(v)), v), logical(1))
  deparse1(call, collapse = "\n",
           control = c("keepNA", "keepInteger", "niceNames", "showAttributes",
                       if (!all(exact)) "digits17"))
}

# Evaluate `thunk`, returning its value together with every warning and
# message it raised, everything it printed, and the error if it failed. A
# stdio MCP server's stderr never reaches the model and its stdout is the
# protocol stream, so all of these are rerouted into the result. The caller
# must re-raise `error`: this function never decides that a failure is fine.
.agent_capture <- function(thunk) {
  # rlang signals some warnings once per session; a server process is one long
  # session, so without this the second identical call would lose them.
  rlang::local_options(rlib_warning_verbosity = "verbose",
                       rlib_message_verbosity = "verbose")
  conds <- list()
  record <- function(cnd) {
    # With the verbosity forced, rlang's "displayed once per session" footer
    # would be false; the rest of the message is kept as raised.
    msg <- sub("\n[^\n]*This (warning|message) is displayed once per session\\.[^\n]*",
               "", conditionMessage(cnd))
    conds[[length(conds) + 1L]] <<- list(class = class(cnd), message = msg)
  }
  value <- NULL
  error <- NULL
  output <- utils::capture.output(
    value <- withCallingHandlers(
      tryCatch(thunk(), error = function(e) { error <<- e; NULL }),
      # tryInvokeRestart(): a condition signalled with signalCondition() has
      # no muffle restart, and must not turn into an error here.
      warning = function(w) { record(w); tryInvokeRestart("muffleWarning") },
      message = function(m) { record(m); tryInvokeRestart("muffleMessage") }
    )
  )
  list(value = value, warnings = conds,
       output = paste(output, collapse = "\n"), error = error)
}

# Only message text crosses to the model, so the classes, the call, and any
# warnings or output that preceded the failure all go into the message. Only
# radiatR's own classes are copied onto the new condition: a foreign class can
# carry a conditionMessage() method that would replace this message. The
# original condition travels as `original` (not `parent`, which would make
# rlang repeat its message).
.agent_rethrow <- function(run, tool, code) {
  e <- run$error
  cls <- setdiff(class(e), c("rlang_error", "simpleError", "error", "condition"))
  before <- vapply(run$warnings, function(w)
    sprintf("- [%s] %s", paste(setdiff(w$class, "condition"), collapse = ", "),
            trimws(w$message)), character(1))
  rlang::abort(
    paste0(
      tool, " failed",
      if (length(cls)) sprintf(" [%s]", paste(cls, collapse = ", ")),
      ": ", conditionMessage(e),
      "\nCall: ", code,
      if (length(before))
        paste0("\nRaised before the error:\n", paste(before, collapse = "\n")),
      if (nzchar(run$output)) paste0("\nPrinted before the error:\n", run$output)),
    class = c(grep("^radiatR_", cls, value = TRUE), "radiatR_tool_error"),
    tool = tool, code = code, original = e, call = NULL)
}

# Evaluate `steps` in order in the store. All-or-nothing: on error every handle
# the steps created is removed, the log and counters are left untouched, and
# the error is re-raised with context.
.agent_run <- function(store, tool, steps) {
  code <- paste(vapply(steps, function(s) .agent_deparse(s$stmt), character(1)),
                collapse = "\n")
  created <- unlist(lapply(steps, `[[`, "creates"))
  run <- .agent_capture(function() {
    value <- NULL
    for (step in steps) {
      value <- eval(step$stmt, store$objects)
      if (!is.null(step$check)) step$check(value)
    }
    value
  })
  if (!is.null(run$error)) {
    rm(list = intersect(created, ls(store$objects)), envir = store$objects)
    .agent_rethrow(run, tool, code)
  }
  for (h in created)
    store$counts[[sub("_[0-9]+$", "", h)]] <- as.integer(sub("^.*_", "", h))
  store$log <- c(store$log, code)
  store$results <- c(store$results, list(run$value))
  run$code <- code
  run
}

.agent_script <- function(store) {
  paste(c(sprintf("# radiatR %s session script (radiatR_tools)",
                  utils::packageVersion("radiatR")),
          store$log), collapse = "\n")
}

# ---- argument checks ---------------------------------------------------------
# Tool arguments arrive as parsed JSON. An MCP server calls the tool function
# directly, without applying the schema, so "required", types and enums are
# enforced here: a JSON null must never turn into a package default.
.agent_string <- function(x, arg, choices = NULL, required = TRUE, why = NULL) {
  if (is.null(x)) {
    if (required) .agent_missing(arg, why)
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x))
    .agent_invalid(arg, "a single string")
  if (!is.null(choices) && !x %in% choices)
    .agent_invalid(arg, sprintf("one of %s (got '%s')",
                                paste0("'", choices, "'", collapse = ", "), x))
  x
}

.agent_strings <- function(x, arg, required = FALSE) {
  if (is.null(x)) {
    if (required) .agent_missing(arg)
    return(NULL)
  }
  if (!is.character(x) || !length(x) || anyNA(x))
    .agent_invalid(arg, "one or more strings")
  x
}

.agent_number <- function(x, arg, required = FALSE, whole = FALSE, min = NULL) {
  if (is.null(x)) {
    if (required) .agent_missing(arg)
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x))
    .agent_invalid(arg, "a single finite number")
  if (!is.null(min) && x < min)
    .agent_invalid(arg, sprintf("at least %s (got %s)", format(min), format(x)))
  if (!whole) return(x)
  if (x != round(x) || abs(x) > .Machine$integer.max)
    .agent_invalid(arg, sprintf("a whole number (got %s)", format(x)))
  as.integer(x)
}

.agent_flag <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    .agent_invalid(arg, "true or false")
  x
}

.agent_compact <- function(x) Filter(Negate(is.null), x)

# An argument the call would ignore is refused, so the model is never told it
# got something it did not.
.agent_unused <- function(x, arg, unless) {
  if (!is.null(x))
    .agent_invalid(arg, sprintf("omitted unless %s (it would be ignored)", unless))
  invisible(NULL)
}

# A JSON object: absent means empty; every field must be named, known and
# given once. Field names are checked before null fields are dropped.
.agent_object <- function(x, arg, allowed) {
  x <- as.list(x)
  if (!length(x)) return(list())
  nms <- names(x)
  if (is.null(nms) || !all(nzchar(nms)))
    .agent_invalid(arg, "an object whose fields are all named")
  unknown <- setdiff(nms, allowed)
  if (length(unknown))
    .agent_invalid(arg, sprintf("an object with fields from: %s (unknown: %s)",
                                paste(allowed, collapse = ", "),
                                paste(unknown, collapse = ", ")))
  if (anyDuplicated(nms))
    .agent_invalid(arg, sprintf("an object naming each field once (repeated: %s)",
                                paste(unique(nms[duplicated(nms)]), collapse = ", ")))
  .agent_compact(x)
}

.agent_columns <- function(cols, available, arg) {
  absent <- setdiff(cols, available)
  if (length(absent))
    rlang::abort(
      sprintf("`%s` names column(s) that are not in the data: %s. Available: %s.",
              arg, paste(absent, collapse = ", "),
              paste(available, collapse = ", ")),
      class = "radiatR_invalid_argument", arg = arg, columns = absent)
  cols
}

.agent_paths <- function(paths, arg = "paths") {
  paths <- .agent_strings(paths, arg, required = TRUE)
  absent <- paths[!file.exists(paths)]
  if (length(absent)) {
    rlang::abort(
      sprintf("Path(s) not found: %s. Server working directory: '%s'.",
              paste0("'", absent, "'", collapse = ", "), getwd()),
      class = "radiatR_path_not_found", paths = absent, wd = getwd())
  }
  normalizePath(paths, winslash = "/", mustWork = TRUE)
}

# Drop NULLs so emitted calls carry only what the agent chose and package
# defaults apply to the rest. Every value passed here has already been checked.
.agent_args <- function(...) .agent_compact(list(...))

# ---- result envelope ---------------------------------------------------------
# Numbers go out at 15 significant digits (jsonlite's maximum for digits = NA);
# exact values are reproduced by the script. Missing and non-finite values are
# encoded as the strings "NA", "NaN", "Inf", "-Inf" rather than dropped, and
# date-times as ISO 8601 in UTC.
.agent_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, na = "string",
                   null = "null", dataframe = "rows", POSIXt = "ISO8601",
                   UTC = TRUE)
}

.agent_envelope <- function(run = NULL, handle = NULL, result = NULL,
                            units = NULL, code = run$code) {
  env <- list()
  if (!is.null(handle)) env$handle <- handle
  if (!is.null(result)) env$result <- result
  if (!is.null(units)) env$units <- units
  env$warnings <- lapply(run$warnings, function(w)
    list(class = I(w$class), message = w$message))
  if (!is.null(run) && nzchar(run$output)) env$output <- run$output
  if (!is.null(code)) env$code <- code
  .agent_json(env)
}

.agent_angle_units <- "radians, unit-circle convention (0 = East, counter-clockwise)"

.agent_units <- function(display = NULL) {
  units <- list(angles = .agent_angle_units)
  if (!is.null(display)) {
    units$mean_dir_deg <- sprintf(
      "degrees in the display convention: 0 at %s degrees counter-clockwise from East, increasing %s",
      format(display$zero * 180 / pi),
      if (isTRUE(display$clockwise)) "clockwise" else "counter-clockwise")
  }
  units
}

.agent_describe <- function(obj) {
  if (inherits(obj, "Tracks")) {
    d <- obj@data
    ids <- unique(d[[obj@cols$id]])
    return(list(
      class = "Tracks", n_rows = nrow(d), n_tracks = length(ids),
      first_ids = I(as.character(utils::head(ids, 10L))),
      mapped_columns = .agent_compact(obj@cols),
      data_columns = I(names(d)),
      meta = obj@meta[setdiff(names(obj@meta), "transform_history")],
      head = utils::head(as.data.frame(d), 5L)))
  }
  out <- list(class = class(obj)[1L], n_rows = nrow(obj),
              columns = I(names(obj)))
  if (inherits(obj, "headings_frame")) {
    col <- attr(obj, "heading_col", exact = TRUE)
    out$heading_col <- col
    out$coords <- attr(obj, "coords", exact = TRUE)
    out$n_missing_headings <- sum(is.na(obj[[col]]))
    if ("id" %in% names(obj)) out$n_tracks <- length(unique(obj$id))
    missing_ids <- attr(obj, "missing_ids", exact = TRUE)
    if (!is.null(missing_ids)) out$missing_ids <- I(as.character(missing_ids))
  }
  out$head <- utils::head(as.data.frame(obj), 5L)
  out
}
