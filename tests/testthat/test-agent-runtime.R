test_that(".agent_run assigns the handle, logs the code it ran and keeps the value", {
  store <- .agent_store()
  handle <- .agent_next_handle(store, "df")
  run <- .agent_run(store, "t", list(.agent_step(
    "data.frame", list(a = 0.2), ns = "base", creates = handle)))

  expect_identical(handle, "df_1")
  expect_identical(store$objects$df_1, data.frame(a = 0.2))
  expect_identical(run$code, "df_1 <- base::data.frame(a = 0.2)")
  expect_identical(store$log, run$code)
  expect_identical(store$results, list(data.frame(a = 0.2)))
  expect_identical(.agent_next_handle(store, "df"), "df_2")
})

test_that("emitted code reproduces a double exactly, using 17 digits only when needed", {
  store <- .agent_store()
  run <- .agent_run(store, "t", list(.agent_step(
    "data.frame", list(a = 0.1 + 0.2, b = 1 / 3), ns = "base", creates = "df_1")))
  expect_match(run$code, "a = 0.30000000000000004", fixed = TRUE)

  env <- new.env(parent = baseenv())
  eval(parse(text = .agent_script(store)), env)
  expect_identical(env$df_1, store$objects$df_1)
})

test_that(".agent_run is all-or-nothing when a later step fails", {
  store <- .agent_store()
  steps <- list(
    .agent_step("data.frame", list(a = 1), ns = "base", creates = "df_1"),
    .agent_step("stop", list("boom"), ns = "base", creates = "df_2"))
  err <- expect_error(.agent_run(store, "my_tool", steps),
                      class = "radiatR_tool_error")

  expect_identical(ls(store$objects), character(0))
  expect_identical(store$log, character(0))
  expect_identical(store$results, list())
  expect_identical(.agent_next_handle(store, "df"), "df_1")
  expect_match(conditionMessage(err), "my_tool failed: boom", fixed = TRUE)
  expect_match(conditionMessage(err), "Call: df_1 <- base::data.frame(a = 1)",
               fixed = TRUE)
  expect_s3_class(err$original, "simpleError")
})

test_that("a step check that aborts undoes the step", {
  store <- .agent_store()
  err <- expect_error(
    .agent_run(store, "my_tool", list(.agent_step(
      "data.frame", list(a = 1), ns = "base", creates = "df_1",
      check = function(value)
        rlang::abort("not allowed", class = "radiatR_invalid_argument")))),
    class = "radiatR_invalid_argument")
  expect_s3_class(err, "radiatR_tool_error")
  expect_identical(ls(store$objects), character(0))
  expect_identical(store$log, character(0))
})

test_that("a re-raised error keeps radiatR classes and names every class", {
  store <- .agent_store()
  err <- expect_error(
    .agent_run(store, "my_tool", list(.agent_step(
      "abort", list("too few", class = "radiatR_nonestimable"), ns = "rlang"))),
    class = "radiatR_nonestimable")
  expect_s3_class(err, "radiatR_tool_error")
  expect_match(conditionMessage(err),
               "my_tool failed [radiatR_nonestimable]: too few", fixed = TRUE)
})

test_that("a foreign condition class cannot replace the re-raised message", {
  store <- .agent_store()
  err <- expect_error(
    .agent_run(store, "my_tool", list(.agent_step(
      "check_installed", list("notapkgxyz"), ns = "rlang"))),
    class = "radiatR_tool_error")
  expect_false(inherits(err, "rlib_error_package_not_found"))
  expect_match(conditionMessage(err), "my_tool failed [rlib_error_package_not_found]",
               fixed = TRUE)
  expect_match(conditionMessage(err), "Call: rlang::check_installed(\"notapkgxyz\")",
               fixed = TRUE)
})

test_that(".agent_capture reroutes warnings, messages and printed output", {
  expect_silent(
    run <- .agent_capture(function() {
      cat("printed\n"); message("note"); warning("careful"); 42
    }))
  expect_identical(run$value, 42)
  expect_null(run$error)
  expect_identical(run$output, "printed")
  expect_identical(vapply(run$warnings, `[[`, "", "message"),
                   c("note\n", "careful"))
  expect_true("message" %in% run$warnings[[1]]$class)
  expect_true("warning" %in% run$warnings[[2]]$class)
})

test_that("warnings and output raised before an error reach the error message", {
  expect_silent(
    run <- .agent_capture(function() {
      cat("printed\n"); warning("careful"); stop("boom")
    }))
  expect_s3_class(run$error, "simpleError")
  err <- expect_error(.agent_rethrow(run, "my_tool", "f()"),
                      class = "radiatR_tool_error")
  msg <- conditionMessage(err)
  expect_match(msg, "my_tool failed: boom\nCall: f()", fixed = TRUE)
  expect_match(msg, "Raised before the error:\n- [simpleWarning, warning] careful",
               fixed = TRUE)
  expect_match(msg, "Printed before the error:\nprinted", fixed = TRUE)
})

test_that("a once-per-session warning is captured every time", {
  once <- function() {
    rlang::warn("only once", .frequency = "once",
                .frequency_id = "radiatR-agent-capture-test")
    1
  }
  first <- .agent_capture(once)
  second <- .agent_capture(once)
  expect_length(first$warnings, 1L)
  expect_length(second$warnings, 1L)
  # rlang's "displayed once per session" footer would now be false.
  expect_identical(second$warnings[[1]]$message, "only once")
})

test_that("a condition signalled without a muffle restart is recorded, not an error", {
  # signalCondition() offers no muffle restart. A message is used here because
  # such a condition also travels on to outer handlers, and testthat reports a
  # warning that reaches it; warnings take the same path in .agent_capture().
  bare <- function() {
    signalCondition(simpleMessage("bare message"))
    1
  }
  run <- .agent_capture(bare)
  expect_null(run$error)
  expect_identical(run$value, 1)
  expect_identical(run$warnings[[1]]$message, "bare message")
})

test_that("a seed step makes a stochastic step reproducible and is logged", {
  store <- .agent_store()
  steps <- function() list(
    .agent_step("set.seed", list(7L), ns = "base"),
    .agent_step("runif", list(3L), ns = "stats"))
  first <- .agent_run(store, "t", steps())
  second <- .agent_run(store, "t", steps())
  expect_identical(first$value, second$value)
  expect_identical(first$code, "base::set.seed(7L)\nstats::runif(3L)")
})

test_that("unknown and wrongly typed handles are classed errors", {
  store <- .agent_store()
  .agent_run(store, "t", list(.agent_step(
    "data.frame", list(a = 1), ns = "base", creates = "df_1")))

  err <- expect_error(.agent_get(store, "tracks_9"),
                      class = "radiatR_unknown_handle")
  expect_match(conditionMessage(err), "Existing handles: df_1", fixed = TRUE)
  expect_error(.agent_get(store, NULL), class = "radiatR_missing_argument")
  expect_error(.agent_get(store, c("df_1", "df_1")), class = "radiatR_unknown_handle")
  expect_error(.agent_get(store, "df_1", "Tracks"),
               class = "radiatR_wrong_handle_type")
  expect_identical(.agent_get(store, "df_1", "data.frame"), data.frame(a = 1))
})

test_that("stores are independent", {
  a <- .agent_store(); b <- .agent_store()
  .agent_run(a, "t", list(.agent_step(
    "data.frame", list(a = 1), ns = "base", creates = "df_1")))
  expect_identical(ls(b$objects), character(0))
  expect_identical(b$log, character(0))
})

test_that("a missing required argument is an error, never a default", {
  expect_identical(.agent_string("degrees", "units", c("radians", "degrees")),
                   "degrees")
  err <- expect_error(.agent_string(NULL, "units"),
                      class = "radiatR_missing_argument")
  expect_match(conditionMessage(err), "`units` is required", fixed = TRUE)
  expect_null(.agent_string(NULL, "units", required = FALSE))
  expect_error(.agent_string("deg", "units", c("radians", "degrees")),
               class = "radiatR_invalid_argument")
  expect_error(.agent_string(c("a", "b"), "units"),
               class = "radiatR_invalid_argument")
  expect_error(.agent_string(1, "units"), class = "radiatR_invalid_argument")
  expect_error(.agent_strings(NULL, "paths", required = TRUE),
               class = "radiatR_missing_argument")
  expect_error(.agent_strings(list("a"), "keep"),
               class = "radiatR_invalid_argument")
})

test_that("numbers are checked, and a non-whole value is never truncated", {
  expect_identical(.agent_number(30, "fps"), 30)
  expect_identical(.agent_number(7, "seed", whole = TRUE), 7L)
  expect_null(.agent_number(NULL, "fps"))
  expect_error(.agent_number(NULL, "seed", required = TRUE),
               class = "radiatR_missing_argument")
  expect_error(.agent_number(5.9, "seed", whole = TRUE),
               class = "radiatR_invalid_argument")
  expect_error(.agent_number("7", "seed"), class = "radiatR_invalid_argument")
  expect_error(.agent_number(Inf, "fps"), class = "radiatR_invalid_argument")
  expect_identical(.agent_number(1, "n_sim", whole = TRUE, min = 1), 1L)
  expect_error(.agent_number(0, "n_sim", whole = TRUE, min = 1),
               class = "radiatR_invalid_argument")
  expect_true(.agent_flag(TRUE, "axial"))
  expect_null(.agent_flag(NULL, "axial"))
  expect_error(.agent_flag("yes", "axial"), class = "radiatR_invalid_argument")
})

test_that("objects must be fully named and hold only known fields", {
  expect_identical(.agent_object(NULL, "mapping", c("id", "x")), list())
  expect_identical(.agent_object(list(id = "a", x = NULL), "mapping", c("id", "x")),
                   list(id = "a"))
  expect_error(.agent_object(list("a"), "mapping", c("id", "x")),
               class = "radiatR_invalid_argument")
  err <- expect_error(.agent_object(list(id = "a", z = 1), "mapping", c("id", "x")),
                      class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "unknown: z", fixed = TRUE)
  # An unknown field is refused even when its value is null, and a field given
  # twice is refused rather than the first one winning.
  expect_error(.agent_object(list(id = "a", z = NULL), "mapping", c("id", "x")),
               class = "radiatR_invalid_argument")
  err <- expect_error(.agent_object(list(x = "a", x = "b"), "mapping", c("id", "x")),
                      class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "repeated: x", fixed = TRUE)
  expect_null(.agent_unused(NULL, "fps", "time_type is 'frames'"))
  err <- expect_error(.agent_unused(30, "fps", "time_type is 'frames'"),
                      class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "it would be ignored", fixed = TRUE)
  expect_identical(.agent_columns(c("a", "b"), c("a", "b", "c"), "keep"), c("a", "b"))
  expect_null(.agent_columns(NULL, "a", "keep"))
  err <- expect_error(.agent_columns(c("a", "typo"), c("a", "b"), "keep"),
                      class = "radiatR_invalid_argument")
  expect_match(conditionMessage(err), "not in the data: typo", fixed = TRUE)
})

test_that(".agent_paths makes paths absolute and rejects missing ones", {
  dir <- withr::local_tempdir()
  writeLines("a", file.path(dir, "x.csv"))
  withr::local_dir(dir)

  expect_identical(.agent_paths("x.csv"),
                   normalizePath(file.path(dir, "x.csv"), winslash = "/"))
  err <- expect_error(.agent_paths(c("x.csv", "nope.csv")),
                      class = "radiatR_path_not_found")
  expect_match(conditionMessage(err), "'nope.csv'", fixed = TRUE)
  expect_match(conditionMessage(err), "Server working directory", fixed = TRUE)
  expect_error(.agent_paths(NULL), class = "radiatR_missing_argument")
})

test_that(".agent_json states missing and non-finite values instead of dropping them", {
  skip_if_not_installed("jsonlite")
  json <- .agent_json(list(x = c(1, NA, Inf, 2 * pi), s = NA_character_))
  expect_identical(
    as.character(json),
    '{"x":[1,"NA","Inf",6.28318530717959],"s":"NA"}')
  when <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  expect_identical(as.character(.agent_json(list(t = when))),
                   '{"t":"2026-01-02T03:04:05Z"}')
})

test_that(".agent_envelope carries handle, result, units, conditions, output and code", {
  skip_if_not_installed("jsonlite")
  run <- list(value = 1, code = "f()", output = "printed",
              warnings = list(list(class = "radiatR_dropped_rows", message = "m")))
  env <- jsonlite::fromJSON(
    .agent_envelope(run, handle = "tracks_1", result = list(n = 2L),
                    units = .agent_units()),
    simplifyVector = FALSE)

  expect_identical(names(env),
                   c("handle", "result", "units", "warnings", "output", "code"))
  expect_identical(env$warnings[[1]]$class, list("radiatR_dropped_rows"))
  expect_identical(env$units, list(angles = .agent_angle_units))
  expect_identical(env$code, "f()")

  quiet <- jsonlite::fromJSON(
    .agent_envelope(list(code = "f()", output = "", warnings = list())),
    simplifyVector = FALSE)
  expect_identical(names(quiet), c("warnings", "code"))
  expect_identical(quiet$warnings, list())
})

test_that(".agent_units describes the display convention of degree columns", {
  units <- .agent_units(circ_display())
  expect_match(units$mean_dir_deg, "0 at 90 degrees counter-clockwise from East",
               fixed = TRUE)
  expect_match(units$mean_dir_deg, "increasing clockwise", fixed = TRUE)
})

test_that(".agent_describe summarises Tracks, headings and plain tables", {
  sim <- as.data.frame(simulate_tracks(seed = 1, n_points = 50))
  ts <- tracks(sim, id = "trial_id", time = "frame", x = "abs_x", y = "abs_y")
  d <- .agent_describe(ts)
  expect_identical(d$class, "Tracks")
  expect_identical(d$n_tracks, 60L)
  expect_identical(d$mapped_columns$id, "trial_id")
  expect_false(d$meta$normalize_xy)
  expect_false("transform_history" %in% names(d$meta))
  expect_identical(nrow(d$head), 5L)

  hd <- headings_frame(data.frame(b = c(10, NA, 350), g = c("a", "a", "b")),
                       col = "b", units = "degrees", angle_convention = "clock")
  h <- .agent_describe(hd)
  expect_identical(h$class, "headings_frame")
  expect_identical(h$heading_col, "b")
  expect_identical(h$n_missing_headings, 1L)

  expect_identical(.agent_describe(data.frame(a = 1:2))$n_rows, 2L)
})
