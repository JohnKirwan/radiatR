# group_compare_table is defined at top level in inst/app/app.R; source it
# inside each test the way other app-fn tests in this repo do.

test_that("a non-estimable test yields a reason dash row, not a blank dash", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  hd <- data.frame(
    heading = c(0.1, 0.2, 0.3, 1.0),   # group b: n=1
    grp     = c("a", "a", "a", "b"),
    stringsAsFactors = FALSE
  )
  tbl <- e$group_compare_table(hd, "grp")
  mean_row <- tbl[tbl$Test == "Mean direction", ]
  expect_match(mean_row$Method, "n =|observ|estimable", ignore.case = TRUE)
})

test_that("an unexpected error is re-raised, not silently dashed", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  hd <- data.frame(heading = c(0.1, 0.2), grp = c("a", "b"))
  expect_error(
    e$group_compare_table(hd, "not_a_column"),
    "not found"
  )
})
