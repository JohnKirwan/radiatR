# Tool definitions for radiatR_tools(). Each tool checks its JSON arguments,
# maps them to one or more .agent_step()s, and wraps the result; evaluation,
# logging and the result envelope live in agent_runtime.R.

# Parameters each built-in heading rule reads from `...`. derive_headings()
# ignores unknown `...` silently, so the tool rejects a parameter the chosen
# rule does not use.
.agent_rule_params <- list(
  crossing        = c("circ0", "circ1", "direction", "first_only", "return_coords"),
  distal          = c("max_radius", "return_coords"),
  straight        = c("tol", "min_len", "return_coords"),
  origin_mean     = "r_power",
  net             = "return_coords",
  velocity_mean   = "weight_by",
  velocity_axis   = "weight_by",
  window_net      = c("window_n", "stride"),
  goal_bias       = character(0),
  pca_axis        = c("source", "return_coords"),
  ransac_straight = c("eps", "min_inliers", "n_iter"),
  maxspeed_window = "window_n",
  vm_fit          = "weight_by",
  exit            = "r0",
  entry           = "r0",
  ring_tangent    = c("r_star", "smooth_n", "deriv_span", "direction")
)

.agent_rule_param_types <- c(
  circ0 = "number", circ1 = "number", direction = "string",
  first_only = "boolean", return_coords = "boolean", max_radius = "number",
  tol = "number", min_len = "integer", r_power = "number",
  weight_by = "string", window_n = "integer", stride = "integer",
  source = "string", eps = "number", min_inliers = "integer",
  n_iter = "integer", r0 = "number", r_star = "number",
  smooth_n = "integer", deriv_span = "integer"
)

.agent_roles <- c("id", "time", "x", "y", "angle", "weight")
.agent_delim_readers <- c(comma = "read.csv", semicolon = "read.csv2", tab = "read.delim")
.agent_delim_seps <- c(comma = ",", semicolon = ";", tab = "\t")
.agent_compare_fns <- c(mean_direction = "test_mean_directions",
                        concentration = "test_concentration",
                        distribution = "test_distributions")
# Uniformity tests whose tabled p-value p_method = "monte_carlo" replaces, and
# those whose p-value is always simulated (see ?test_uniformity).
.agent_mc_tests <- c("kuiper", "rao", "watson")
.agent_sim_tests <- c("hermans_rasson", "pycke")

.agent_guide_ref <- "Conventions and pitfalls: vignette('agent-guide', package = 'radiatR')."

.agent_tool <- function(name, title, description, arguments, fun) {
  ellmer::tool(
    fun,
    name = paste0("radiatR_", name),
    description = paste(description, .agent_guide_ref),
    arguments = arguments,
    annotations = ellmer::tool_annotations(
      title = title, read_only_hint = TRUE, open_world_hint = FALSE)
  )
}

.agent_choices <- function(fn, arg, drop = character(0)) {
  setdiff(eval(formals(fn)[[arg]]), drop)
}

.agent_arg_seed <- function() ellmer::type_integer(
  "RNG seed (whole number). Always required so every result can be reproduced; it is recorded in `code`.")

.agent_seed_step <- function(seed) {
  .agent_step("set.seed", list(.agent_number(seed, "seed", required = TRUE,
                                             whole = TRUE)), ns = "base")
}

# ---- discovery / session -----------------------------------------------------
.tool_list_options <- function(store) {
  .agent_tool(
    "list_options", "List radiatR options",
    "List the loader dialects, heading rules (with the parameters each accepts) and statistical tests available. Call this before loading data or deriving headings.",
    list(),
    function() {
      .agent_envelope(result = list(
        loader_dialects = I(list_loader_dialects()),
        heading_rules = lapply(.agent_rule_params, I),
        uniformity_tests = I(.agent_choices(test_uniformity, "test")),
        compare_groups = list(
          mean_direction = I(.agent_choices(test_mean_directions, "method")),
          concentration = I(c("parametric = true", "parametric = false")),
          distribution = I(.agent_choices(test_distributions, "method")))
      ))
    })
}

.tool_describe <- function(store) {
  .agent_tool(
    "describe", "Describe a session object",
    "Describe one object held in this session (class, size, columns, first rows). With no handle, list every handle in the session.",
    list(handle = ellmer::type_string(
      "Handle returned by a load or derive tool, e.g. 'tracks_1'. Omit to list all handles.",
      required = FALSE)),
    function(handle = NULL) {
      if (is.null(handle)) {
        handles <- ls(store$objects)
        return(.agent_envelope(result = list(handles = lapply(
          stats::setNames(handles, handles),
          function(h) class(get(h, envir = store$objects))[1L]))))
      }
      obj <- .agent_get(store, handle)
      # A plain table is still in the file's own units; say nothing about them.
      .agent_envelope(
        result = .agent_describe(obj),
        units = if (inherits(obj, c("Tracks", "headings_frame"))) .agent_units())
    })
}

.tool_export_script <- function(store) {
  .agent_tool(
    "export_script", "Export the session as an R script",
    "Return one R script that reproduces every object and result produced in this session, in order. Save it next to the analysis so the work can be rerun without the agent.",
    list(),
    function() .agent_envelope(code = .agent_script(store)))
}

# ---- loading -----------------------------------------------------------------
# read_tracks() fills an unmapped angle or weight role from any column with a
# matching name (theta, angle, bearing, w, weight, ...), in the default unit.
# The tool never lets that pass: a weight role nobody mapped, or an angle that
# is not simply the direction of the stored position, means a column was
# guessed.
.agent_check_roles <- function(ts, mapping) {
  if (is.null(mapping$weight) && !is.null(ts@cols$weight))
    rlang::abort(
      sprintf("read_tracks() picked up column '%s' as the weight, but no weight column was mapped. If it holds weights, name it in `mapping.weight`; otherwise rename or remove it before loading (read_tracks() cannot be told to ignore it).",
              ts@cols$weight),
      class = "radiatR_guessed_column", role = "weight", column = ts@cols$weight)
  if (is.null(mapping$angle)) {
    d <- ts@data
    from_xy <- cartesian_to_polar(d[[ts@cols$x]], d[[ts@cols$y]],
                                  normalize = FALSE)$theta
    if (!identical(d[[ts@cols$angle]], from_xy))
      rlang::abort(
        sprintf("read_tracks() picked up column '%s' as the angle, but no angle column was mapped, so its unit was never stated. If it holds the heading angle, name it in `mapping.angle` and give `angle_unit`; otherwise rename or remove it before loading (read_tracks() cannot be told to ignore it).",
                ts@cols$angle),
        class = "radiatR_guessed_column", role = "angle", column = ts@cols$angle)
  }
}

.tool_load_tracks <- function(store) {
  time_types <- .agent_choices(read_tracks, "time_type", drop = "auto")
  angle_units <- .agent_choices(read_tracks, "angle_unit", drop = "auto")
  collisions <- .agent_choices(read_tracks, "id_collision")
  invalids <- .agent_choices(read_tracks, "on_invalid")
  .agent_tool(
    "load_tracks", "Load trajectory files",
    "Load one or more tracking files into a Tracks object with radiatR::read_tracks() and return its handle. Give either a registered `dialect` or a `mapping` naming at least the id, time, x and y columns. Units are never guessed: `time_type` is always required, and `angle_unit` is required when an angle column is mapped. Without a dialect, columns that are not mapped are dropped unless named in `keep` (list every grouping column you will need later).",
    list(
      paths = ellmer::type_array(ellmer::type_string(), "File paths to load."),
      time_type = ellmer::type_enum(time_types,
        "How the time column is expressed. 'frames' also needs `fps`."),
      mapping = ellmer::type_object(
        "Column names in the file for each role. Omit when using a dialect.",
        id = ellmer::type_string("Trajectory id column.", required = FALSE),
        time = ellmer::type_string("Time or frame column.", required = FALSE),
        x = ellmer::type_string("x coordinate column.", required = FALSE),
        y = ellmer::type_string("y coordinate column.", required = FALSE),
        angle = ellmer::type_string("Angle column, if the file has one. It must already be in the unit-circle convention (0 = East, counter-clockwise): read_tracks() has no convention argument, so a compass bearing would be misread.", required = FALSE),
        weight = ellmer::type_string("Weight column, if any.", required = FALSE),
        .required = FALSE),
      dialect = ellmer::type_enum(list_loader_dialects(),
        "Registered tracker dialect that reads the file itself.", required = FALSE),
      angle_unit = ellmer::type_enum(angle_units,
        "Unit of the mapped angle column.", required = FALSE),
      fps = ellmer::type_number(
        "Capture frame rate in frames per second. Only with time_type 'frames'.",
        required = FALSE),
      normalize_xy = ellmer::type_boolean(
        "TRUE rescales each trajectory to the unit circle on its own bounding box. This preserves shape only and is NOT calibration. Default FALSE.",
        required = FALSE),
      origin_x = ellmer::type_number("Arena centre x in file units (calibration; give with origin_y and radius).", required = FALSE),
      origin_y = ellmer::type_number("Arena centre y in file units.", required = FALSE),
      radius = ellmer::type_number("Arena radius in file units.", required = FALSE),
      keep = ellmer::type_array(ellmer::type_string(),
        "Extra, unmapped columns to retain (e.g. treatment or condition columns).", required = FALSE),
      id_collision = ellmer::type_enum(collisions,
        "What to do when the same id appears in more than one file. Default 'error'.", required = FALSE),
      on_invalid = ellmer::type_enum(invalids,
        "What to do with rows whose x/y are missing or non-finite. Default 'error'; 'drop' removes them and reports it under `warnings`.", required = FALSE)
    ),
    function(paths = NULL, time_type = NULL, mapping = NULL, dialect = NULL,
             angle_unit = NULL, fps = NULL, normalize_xy = NULL,
             origin_x = NULL, origin_y = NULL, radius = NULL, keep = NULL,
             id_collision = NULL, on_invalid = NULL) {
      paths <- .agent_paths(paths)
      time_type <- .agent_string(time_type, "time_type", time_types)
      dialect <- .agent_string(dialect, "dialect", list_loader_dialects(),
                               required = FALSE)
      mapping <- .agent_object(mapping, "mapping", .agent_roles)
      for (role in names(mapping))
        .agent_string(mapping[[role]], paste0("mapping.", role))
      if (is.null(dialect)) {
        unmapped <- setdiff(c("id", "time", "x", "y"), names(mapping))
        if (length(unmapped))
          .agent_missing(
            sprintf("mapping.%s", paste(unmapped, collapse = ", mapping.")),
            "when no dialect is given (read_tracks() would otherwise guess the column)")
      }
      if (is.null(mapping$angle))
        .agent_unused(angle_unit, "angle_unit", "an angle column is mapped")
      angle_unit <- .agent_string(angle_unit, "angle_unit", angle_units,
                                  required = !is.null(mapping$angle),
                                  why = "when an angle column is mapped")
      fps <- .agent_number(fps, "fps")
      if (time_type != "frames")
        .agent_unused(fps, "fps", "time_type is 'frames'")
      origin_x <- .agent_number(origin_x, "origin_x")
      origin_y <- .agent_number(origin_y, "origin_y")
      if (is.null(origin_x) != is.null(origin_y))
        .agent_invalid("origin_x, origin_y", "given together or not at all")
      keep <- .agent_strings(keep, "keep")

      handle <- .agent_next_handle(store, "tracks")
      run <- .agent_run(store, "radiatR_load_tracks", list(.agent_step(
        "read_tracks",
        .agent_args(paths,
                    mapping = if (length(mapping)) mapping,
                    dialect = dialect, angle_unit = angle_unit,
                    time_type = time_type, fps = fps,
                    normalize_xy = .agent_flag(normalize_xy, "normalize_xy"),
                    origin = if (!is.null(origin_x)) c(origin_x, origin_y),
                    radius = .agent_number(radius, "radius"),
                    keep = keep,
                    id_collision = .agent_string(id_collision, "id_collision",
                                                 collisions, required = FALSE),
                    on_invalid = .agent_string(on_invalid, "on_invalid",
                                               invalids, required = FALSE)),
        creates = handle,
        check = function(ts) {
          .agent_columns(keep, names(ts@data), "keep")
          .agent_check_roles(ts, mapping)
        })))
      .agent_envelope(run, handle = handle,
                      result = .agent_describe(run$value),
                      units = .agent_units())
    })
}

# read.csv() reshapes a file whose records differ in length without a word: a
# longer data row shifts the columns (the first becomes row names) or starts a
# new row, and a shorter one is padded. Refuse such a file instead.
.agent_check_rectangular <- function(path, sep) {
  n <- utils::count.fields(path, sep = sep, quote = "\"", comment.char = "",
                           blank.lines.skip = TRUE)
  n <- n[!is.na(n)]   # NA marks the inner lines of a quoted multi-line field
  odd <- which(n != n[1L])
  if (length(odd))
    rlang::abort(
      sprintf("'%s' is not a rectangular table: the header has %d field(s) but record(s) %s have %s (records are non-blank lines; the header is record 1). Nothing was loaded.",
              path, n[1L], paste(utils::head(odd, 10L), collapse = ", "),
              paste(unique(n[odd]), collapse = ", ")),
      class = "radiatR_ragged_file", path = path, records = odd)
}

.tool_load_headings <- function(store) {
  .agent_tool(
    "load_headings", "Load a table of headings",
    "Load a delimited text file that already holds one heading angle per row and return a headings handle. The delimiter, the angle column, its unit and its convention must all be stated; nothing is guessed. All other columns are kept, under their names as written in the file, and can be used for grouping.",
    list(
      path = ellmer::type_string("Path to a .csv, .tsv or .txt file."),
      delim = ellmer::type_enum(names(.agent_delim_readers),
        "Field delimiter. 'semicolon' also means decimal commas (read.csv2)."),
      col = ellmer::type_string("Name of the angle column."),
      units = ellmer::type_enum(c("radians", "degrees"), "Unit of the angle column."),
      angle_convention = ellmer::type_enum(c("unit_circle", "clock"),
        "'unit_circle': 0 = East, counter-clockwise. 'clock': 0 = North, clockwise (compass bearings).")
    ),
    function(path = NULL, delim = NULL, col = NULL, units = NULL,
             angle_convention = NULL) {
      path <- .agent_paths(.agent_string(path, "path"), "path")
      ext <- tolower(tools::file_ext(path))
      if (!ext %in% c("csv", "tsv", "txt"))
        rlang::abort(
          sprintf("Unsupported file type '.%s' for '%s'. Headings tables must be delimited text (.csv, .tsv, .txt).", ext, path),
          class = "radiatR_unsupported_file", path = path)
      delim <- .agent_string(delim, "delim", names(.agent_delim_readers))
      col <- .agent_string(col, "col")
      units <- .agent_string(units, "units", c("radians", "degrees"))
      angle_convention <- .agent_string(angle_convention, "angle_convention",
                                        c("unit_circle", "clock"))
      df <- .agent_next_handle(store, "df")
      handle <- .agent_next_handle(store, "headings")
      run <- .agent_run(store, "radiatR_load_headings", list(
        # check.names = FALSE: column names stay exactly as written in the file.
        .agent_step(.agent_delim_readers[[delim]],
                    list(path, check.names = FALSE), ns = "utils", creates = df,
                    check = function(table) {
                      .agent_check_rectangular(path, .agent_delim_seps[[delim]])
                      .agent_columns(col, names(table), "col")
                      if (!is.numeric(table[[col]]))
                        .agent_invalid("col", sprintf(
                          "a numeric column ('%s' was read as %s)", col,
                          class(table[[col]])[1L]))
                    }),
        .agent_step("headings_frame",
                    list(rlang::sym(df), col = col, units = units,
                         angle_convention = angle_convention),
                    creates = handle)))
      .agent_envelope(run, handle = handle,
                      result = .agent_describe(run$value),
                      units = .agent_units())
    })
}

# ---- public ------------------------------------------------------------------
#' Tools for driving radiatR from an AI agent
#'
#' Returns a set of [ellmer::tool()] definitions that let a language model run
#' a radiatR analysis: load tracks or a table of headings, derive headings,
#' summarise, test and plot. Register them with an ellmer chat, or serve them
#' to an MCP client such as Claude Code or Claude Desktop with the
#' \pkg{mcptools} package.
#'
#' Each call to `radiatR_tools()` creates its own private session. Objects are
#' referred to by handles (`"tracks_1"`, `"headings_1"`); every result includes
#' the R code that produced it, and the `radiatR_export_script` tool returns
#' the whole session as one script that reproduces the data objects and result
#' tables exactly.
#'
#' The tools do not fill in what was left unsaid: the time type, the unit of
#' any angle column, the convention of a table of angles, and a seed for
#' heading derivation and for every test must be stated. A missing or invalid
#' argument is an error rather than a default, and so is an argument the call
#' would ignore. One thing is still detected rather than stated: `load_tracks`
#' reads a file as [read_tracks()] does, sniffing its delimiter and decimal
#' mark. Warnings and printed
#' output are returned to the model in the result instead of being written to
#' the console, and an error message carries the condition class, the failing
#' call, and anything raised before the failure.
#'
#' @section Serving over MCP:
#' ```
#' claude mcp add radiatR -- Rscript -e \
#'   'mcptools::mcp_server(tools = radiatR::radiatR_tools(), session_tools = FALSE)'
#' ```
#' `session_tools = FALSE` keeps every call in the server process, where the
#' handles live. Handles therefore cannot be inspected from another R console;
#' the exported script is the bridge. Anything R prints at startup corrupts the
#' MCP stream: in a project that uses renv, set
#' `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` for the server.
#'
#' @return A named list of `ellmer` tool definitions.
#' @examplesIf rlang::is_installed("ellmer", version = "0.5.0") && rlang::is_installed("jsonlite")
#' tools <- radiatR_tools()
#' names(tools)
#' @md
#' @export
radiatR_tools <- function() {
  rlang::check_installed(c("ellmer (>= 0.5.0)", "jsonlite"),
                         reason = "to build agent tools.")
  store <- .agent_store()
  builders <- list(
    list_options = .tool_list_options, load_tracks = .tool_load_tracks,
    load_headings = .tool_load_headings, describe = .tool_describe,
    export_script = .tool_export_script)
  lapply(builders, function(build) build(store))
}
