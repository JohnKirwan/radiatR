# group_compare_table is defined at top level in inst/app/app.R; source it
# the way other app-fn tests in this repo do (see test-app-example.R).
local({
  app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  if (!file.exists(app_file)) return(invisible())
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  group_compare_table <<- e$group_compare_table
})

test_that("a non-estimable test yields a reason dash row, not a blank dash", {
  skip_if_not_installed("shiny")
  hd <- data.frame(
    heading = c(0.1, 0.2, 0.3, 1.0),   # group b: n=1
    grp     = c("a", "a", "a", "b"),
    stringsAsFactors = FALSE
  )
  tbl <- group_compare_table(hd, "grp")
  mean_row <- tbl[tbl$Test == "Mean direction", ]
  expect_match(mean_row$Method, "n =|observ|estimable", ignore.case = TRUE)
})

test_that("an unexpected error is re-raised, not silently dashed", {
  skip_if_not_installed("shiny")
  hd <- data.frame(heading = c(0.1, 0.2), grp = c("a", "b"))
  expect_error(
    group_compare_table(hd, "not_a_column"),
    "not found"
  )
})
