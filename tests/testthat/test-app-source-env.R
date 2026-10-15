# CRAN policy: the app must not write to the user's global environment.
# app.R sources its helper files with local = TRUE, so they land in the
# environment app.R is evaluated in (shiny's app environment, or `e` here).

test_that("sourcing app.R leaves the global environment untouched", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")

  before <- ls(globalenv(), all.names = TRUE)
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  expect_identical(ls(globalenv(), all.names = TRUE), before)
  # Helper-file objects are in the app environment.
  expect_true(is.function(e$build_plot_spec))
  expect_true(is.numeric(e$APP_SEED))
})
