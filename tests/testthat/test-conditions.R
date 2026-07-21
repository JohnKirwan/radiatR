test_that("radiatR_abort_invalid_rows carries a structured payload", {
  e <- tryCatch(
    radiatR_abort_invalid_rows(rows = c(2L, 5L), ids = c("a", "b"),
                               columns = c("x", "y"), values = c("NA/1", "3/NA")),
    radiatR_invalid_rows = function(e) e
  )
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_identical(e$rows, c(2L, 5L))
  expect_identical(e$ids, c("a", "b"))
  expect_match(conditionMessage(e), "2 row")
})

test_that("radiatR_warn_dropped_rows is a classed warning with payload", {
  w <- tryCatch(
    radiatR_warn_dropped_rows(rows = 3L, ids = "z", columns = c("x", "y"),
                              values = "NA/NA"),
    radiatR_dropped_rows = function(w) w
  )
  expect_s3_class(w, c("radiatR_dropped_rows", "warning"))
  expect_identical(w$rows, 3L)
})

test_that("radiatR_abort_nonestimable carries method/group/n/reason", {
  e <- tryCatch(
    radiatR_abort_nonestimable(method = "Watson-Williams", group = "ctrl",
                               n = 1L, reason = "fewer than 2 observations"),
    radiatR_nonestimable = function(e) e
  )
  expect_s3_class(e, "radiatR_nonestimable")
  expect_identical(e$method, "Watson-Williams")
  expect_identical(e$group, "ctrl")
  expect_identical(e$n, 1L)
  expect_match(e$reason, "2 observations")
})
