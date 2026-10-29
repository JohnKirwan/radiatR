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
