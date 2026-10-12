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

# ---- float noise at the 0 / 2*pi seam -----------------------------------------

test_that("angles that wrap to exactly 2*pi are stored as 0", {
  expect_identical(wrap_to_2pi(-1e-17), 0)
  expect_identical(wrap_to_2pi(c(NA, -1)), c(NA, 2 * pi - 1))
  d <- data.frame(id = "a", time = 1:2, ang = c(-1e-17, 1))
  ts <- tracks(d, id = "id", time = "time", angle = "ang", angle_unit = "radians")
  expect_identical(ts@data$ang, c(0, 1))
  d <- data.frame(id = "a", time = 1:2, x = 1, y = c(0.3 - 0.1 - 0.2, 0.5))
  expect_s4_class(tracks(d, id = "id", time = "time", x = "x", y = "y"), "Tracks")
})

# ---- validator (direct slot writes) -------------------------------------------

test_that("the validator rejects each broken rule after a slot write", {
  rejects <- function(col, rows, value, msg, ...) {
    ts <- mk(...)
    ts@data[[col]][rows] <- value
    expect_error(methods::validObject(ts), msg, fixed = TRUE)
  }
  rejects("id", 3, NA, "missing id (column(s) id): 1 row(s): 3")
  rejects("time", 2, NA, "missing or non-finite time (column(s) time): 1 row(s): 2")
  rejects("time", 3, 2, "time repeated within an id (column(s) time): 1 row(s): 3")
  rejects("time", 2, 5, "time decreasing within an id (column(s) time): 1 row(s): 3")
  rejects("angle", 2, NA, "missing or non-finite angle (column(s) angle): 1 row(s): 2")
  rejects("angle", 2, 7, "angle outside [0, 2*pi) (column(s) angle): 1 row(s): 2")
  rejects("x", 2, NA, "missing or non-finite coordinate (column(s) x): 1 row(s): 2")
  rejects("w", 2, -1, "weight missing, non-finite or negative (column(s) w): 1 row(s): 2",
          weight = "w")
})

test_that("raw and rel coordinate columns are covered by the validator", {
  ts <- mk(normalize_xy = TRUE)
  ts@data$x_raw[2] <- NA
  expect_error(methods::validObject(ts), "coordinate (column(s) x_raw)", fixed = TRUE)
  d <- valid_df(); d$rx <- d$x; d$ry <- d$y
  ts <- mk(d, rel_x = "rx", rel_y = "ry")
  ts@data$ry[5] <- Inf
  expect_error(methods::validObject(ts), "coordinate (column(s) ry)", fixed = TRUE)
})

test_that("NA time fails validation with a message, not a crash", {
  ts <- mk()
  ts@data$time[2] <- NA
  err <- tryCatch(methods::validObject(ts), error = function(e) conditionMessage(e))
  expect_no_match(err, "TRUE/FALSE needed")
  expect_match(err, "missing or non-finite time")
})

test_that("non-contiguous NA ids report both the grouping and the id rule", {
  ts <- mk()
  ts@data$id[c(1, 4)] <- NA
  err <- tryCatch(methods::validObject(ts), error = function(e) conditionMessage(e))
  expect_match(err, "rows must be grouped by id")
  expect_match(err, "missing id (column(s) id): 2 row(s): 1, 4", fixed = TRUE)
})

test_that("interleaved ids fail on grouping alone", {
  ts <- mk()
  ts@data <- ts@data[c(1, 4, 2, 5, 3, 6), ]
  err <- tryCatch(methods::validObject(ts), error = function(e) conditionMessage(e))
  expect_match(err, "rows must be grouped by id")
  expect_no_match(err, "time")
})

test_that("several broken rules are all reported", {
  ts <- mk(weight = "w")
  ts@data$x[1] <- NA
  ts@data$time[6] <- 2
  ts@data$w[4] <- Inf
  err <- tryCatch(methods::validObject(ts), error = function(e) conditionMessage(e))
  expect_match(err, "non-finite coordinate")
  expect_match(err, "time repeated within an id")
  expect_match(err, "weight missing, non-finite or negative")
})

test_that("the weight role is checked structurally", {
  ts <- mk()
  ts@cols$weight <- "nope"
  expect_error(methods::validObject(ts), "weight column not present in data")
  ts <- mk()
  ts@data$w <- "heavy"
  ts@cols$weight <- "w"
  expect_error(methods::validObject(ts), "weight must be numeric")
})

test_that("valid, zero-row and shipped objects pass", {
  expect_true(methods::validObject(mk(weight = "w")))
  expect_true(methods::validObject(mk(valid_df()[0, ])))
  expect_true(methods::validObject(cpunctatus))
})

# ---- tracks() constructor ------------------------------------------------------

invalid <- function(expr) tryCatch(expr, radiatR_invalid_tracks = function(e) e)

test_that("tracks() reports problems against the rows it was given", {
  d <- valid_df()[c(6, 1, 5, 2, 4, 3), ]   # unsorted input
  d$x[1] <- NA                             # input row 1 (id b)
  d$time[2] <- NA                          # input row 2 (id a)
  e <- invalid(mk(d))
  expect_s3_class(e, "radiatR_invalid_tracks")
  expect_identical(names(e$problems), c("rule", "column", "row", "id"))
  expect_identical(keys(e$problems),
                   c("coord_nonfinite/x/1", "time_nonfinite/time/2"))
  expect_identical(e$problems$id[e$problems$rule == "coord_nonfinite"], "b")
  expect_identical(e$rows, 1:2)
  expect_identical(sort(e$ids), c("a", "b"))
  expect_match(conditionMessage(e), "Invalid track data:")
})

test_that("tracks() raises each rule as the classed condition", {
  rule_of <- function(col, rows, value, ...) {
    d <- valid_df()
    d[[col]][rows] <- value
    e <- invalid(mk(d, ...))
    expect_s3_class(e, "radiatR_invalid_tracks")
    keys(e$problems)
  }
  expect_identical(rule_of("id", 2, NA), "id_missing/id/2")
  expect_identical(rule_of("time", 2, Inf), "time_nonfinite/time/2")
  expect_identical(rule_of("time", 3, 2), "time_duplicate/time/3")
  expect_identical(rule_of("w", 5, -2, weight = "w"), "weight_invalid/w/5")
})

test_that("a missing coordinate is reported once, under the supplied column", {
  d <- valid_df(); d$x[5] <- NA
  expect_identical(keys(invalid(mk(d))$problems), "coord_nonfinite/x/5")
  expect_identical(keys(invalid(mk(d, normalize_xy = TRUE))$problems),
                   "coord_nonfinite/x/5")
  expect_identical(keys(invalid(mk(d, origin = c(0, 0), radius = 2))$problems),
                   "coord_nonfinite/x/5")
})

test_that("a missing angle is reported once when x/y are derived from it", {
  d <- valid_df()[c("id", "time")]
  d$ang <- c(0.1, NA, 0.3, 0.4, 0.5, 0.6)
  e <- invalid(tracks(d, id = "id", time = "time", angle = "ang",
                      angle_unit = "radians"))
  expect_identical(keys(e$problems), "angle_nonfinite/ang/2")
})

test_that("a non-numeric time column keeps its structural message", {
  d <- valid_df(); d$time <- as.character(d$time)
  err <- tryCatch(mk(d), error = function(e) e)
  expect_false(inherits(err, "radiatR_invalid_tracks"))
  expect_match(conditionMessage(err), "time must be POSIXct or numeric")
})

test_that("read_tracks() surfaces a bad time value as the classed condition", {
  d <- valid_df(); d$time[2] <- NA
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(d, path, row.names = FALSE)
  e <- invalid(suppressMessages(
    read_tracks(path, mapping = list(id = "id", time = "time", x = "x", y = "y"))))
  expect_s3_class(e, "radiatR_invalid_tracks")
  expect_identical(unique(e$problems$rule), "time_nonfinite")
})

test_that("a partly empty SLEAP track column is an id_missing error", {
  sleap <- data.frame(track = c("track_0", NA, "track_0"), frame_idx = 0:2,
                      A.x = c(1, 2, 3), A.y = c(1, 1, 2), check.names = FALSE)
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sleap, path, row.names = FALSE, na = "")
  e <- invalid(read_tracks(path, dialect = "sleap"))
  expect_s3_class(e, "radiatR_invalid_tracks")
  expect_identical(keys(e$problems), "id_missing/id/2")
})

# ---- radiate() on a data frame -------------------------------------------------

test_that("radiate() rejects a data frame whose group_col is not a trajectory id", {
  sim <- simulate_tracks(seed = 1)
  e <- invalid(radiate(sim, group_col = "condition",
                       x_col = "rel_x", y_col = "rel_y"))
  expect_s3_class(e, "radiatR_invalid_tracks")
  expect_identical(unique(e$problems$rule), "time_duplicate")
  expect_s3_class(radiate(sim, group_col = "trial_id",
                          x_col = "rel_x", y_col = "rel_y"), "ggplot")
})

test_that("non-finite relative coordinates are rejected at construction", {
  d <- valid_df(); d$rx <- d$x; d$ry <- d$y
  d$rx[2] <- NA
  e <- invalid(mk(d, rel_x = "rx", rel_y = "ry"))
  expect_s3_class(e, "radiatR_invalid_tracks")
  expect_identical(keys(e$problems), "coord_nonfinite/rx/2")
})
