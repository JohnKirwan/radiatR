valid_df <- function() {
  data.frame(id = rep(c("a", "b"), each = 3), time = rep(1:3, 2),
             x = c(.1, .2, .3, .4, .5, .6), y = c(.3, .2, .1, .1, .2, .3),
             w = 1)
}
mk <- function(d = valid_df(), ...) {
  tracks(d, id = "id", time = "time", x = "x", y = "y", ...)
}
# Rows of `p` as "rule/column/row" strings, for order-free comparison.
keys <- function(p) sort(paste(p$rule, p$column, p$row, sep = "/"))

# ---- .tracks_problems() -------------------------------------------------------

test_that(".tracks_problems returns zero rows for valid data", {
  ts <- mk(weight = "w")
  p <- .tracks_problems(ts@data, ts@cols)
  expect_identical(names(p), c("rule", "column", "row"))
  expect_identical(nrow(p), 0L)
  expect_identical(nrow(.tracks_problems(ts@data[0, ], ts@cols)), 0L)
})

test_that(".tracks_problems names each rule, column and row", {
  ts <- mk(weight = "w")
  cl <- ts@cols
  broken <- function(col, rows, value) {
    d <- ts@data
    d[[col]][rows] <- value
    keys(.tracks_problems(d, cl))
  }
  expect_identical(broken("id", 2, NA), "id_missing/id/2")
  expect_identical(broken("time", 2, NA), "time_nonfinite/time/2")
  expect_identical(broken("time", 3, 2), "time_duplicate/time/3")
  expect_identical(broken("time", 2, 5), "time_unsorted/time/3")
  expect_identical(broken("angle", 2, NA), "angle_nonfinite/angle/2")
  expect_identical(broken("angle", 2, 2 * pi), "angle_range/angle/2")
  expect_identical(broken("angle", 2, -0.1), "angle_range/angle/2")
  expect_identical(broken("y", 4, NaN), "coord_nonfinite/y/4")
  expect_identical(broken("w", c(1, 6), c(-1, NA)),
                   c("weight_invalid/w/1", "weight_invalid/w/6"))
  expect_identical(broken("w", 2, 0), character())
})

test_that("a non-finite time mid-trajectory is reported once, as time_nonfinite", {
  ts <- mk()
  d <- ts@data
  d$time[2] <- Inf                     # steps +Inf then -Inf: not "unsorted"
  expect_identical(keys(.tracks_problems(d, ts@cols)), "time_nonfinite/time/2")
})

test_that("POSIXct time is checked like numeric time", {
  ts <- mk()
  d <- ts@data
  d$time <- as.POSIXct(d$time, origin = "1970-01-01", tz = "UTC")
  expect_identical(nrow(.tracks_problems(d, ts@cols)), 0L)
  d$time[3] <- d$time[2]
  expect_identical(keys(.tracks_problems(d, ts@cols)), "time_duplicate/time/3")
})

test_that("factor ids are compared by value", {
  d <- valid_df()
  d$id <- factor(d$id, levels = c("b", "a"))   # level order is not alphabetical
  ts <- mk(d)
  expect_identical(nrow(.tracks_problems(ts@data, ts@cols)), 0L)
  dd <- ts@data
  dd$time[6] <- 2L
  expect_identical(keys(.tracks_problems(dd, ts@cols)), "time_duplicate/time/6")
  dd <- ts@data
  dd$id <- addNA(dd$id)
  dd$id[2] <- NA                               # NA as a level: is.na() is FALSE
  expect_identical(keys(.tracks_problems(dd, ts@cols)), "id_missing/id/2")
})

test_that("skip, absent rel columns and shared columns are handled", {
  ts <- mk()
  d <- ts@data
  d$x[1] <- NA
  expect_identical(nrow(.tracks_problems(d, ts@cols, skip = "x")), 0L)
  cl <- ts@cols
  cl$rel_x <- "rel_x"; cl$rel_y <- "rel_y"       # named, not stored
  expect_identical(keys(.tracks_problems(d, cl)), "coord_nonfinite/x/1")
  cl$rel_x <- "x"; cl$rel_y <- "y"               # one column, two roles
  expect_identical(keys(.tracks_problems(d, cl)), "coord_nonfinite/x/1")
})

test_that(".format_tracks_problems gives one line per rule, truncated at 10", {
  p <- data.frame(rule = c(rep("time_duplicate", 12), "id_missing"),
                  column = c(rep("time", 12), "id"), row = c(12:1, 20L),
                  stringsAsFactors = FALSE)
  out <- .format_tracks_problems(p, ids = c(rep(c("a", "b"), 6), NA))
  expect_length(out, 2L)
  expect_match(out[1], "time repeated within an id (column(s) time): 12 row(s): 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...; id(s): a, b",
               fixed = TRUE)
  expect_match(out[2], "missing id (column(s) id): 1 row(s): 20; id(s): NA",
               fixed = TRUE)
})
