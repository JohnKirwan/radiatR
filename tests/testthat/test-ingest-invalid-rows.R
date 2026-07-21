make_df <- function() {
  data.frame(
    id   = c("t1", "t1", "t1"),
    time = c(0, 1, 2),
    x    = c(0.1, NA, 0.3),
    y    = c(0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )
}

test_that("read_tracks errors by default on non-finite coordinates", {
  e <- tryCatch(
    read_tracks(make_df(), mapping = list(id = "id", time = "time", x = "x", y = "y")),
    radiatR_invalid_rows = function(e) e
  )
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_identical(e$rows, 2L)
  expect_identical(e$ids, "t1")
})

test_that("read_tracks(on_invalid='drop') warns and drops the row", {
  w <- NULL
  tr <- withCallingHandlers(
    read_tracks(make_df(),
                mapping = list(id = "id", time = "time", x = "x", y = "y"),
                on_invalid = "drop"),
    radiatR_dropped_rows = function(cond) { w <<- cond; rlang::cnd_muffle(cond) }
  )
  expect_s3_class(w, "radiatR_dropped_rows")
  expect_identical(w$rows, 2L)
  expect_equal(nrow(as.data.frame(tr)), 2L)
})
