# Numeric time is a frame index unless the Tracks says it is seconds. Seconds
# are used as they are: no frame rate is needed and none is applied.

secs_df <- function(id = "A", t = c(0, 0.5, 1, 1.5)) {
  data.frame(id = id, t = t, x = c(0, 1, 2, 3), y = 0)
}
load_secs <- function(...)
  read_tracks(secs_df(), mapping = list(id = "id", time = "t", x = "x", y = "y"),
              time_type = "seconds", ...)

test_that("time read as seconds needs no frame rate", {
  ts <- load_secs()
  expect_null(frame_rate(ts))
  expect_equal(elapsed_seconds(ts), c(0, 0.5, 1, 1.5))
  expect_equal(elapsed_seconds(ts, "milliseconds"), c(0, 500, 1000, 1500))
  expect_equal(track_duration(ts)$duration, 1.5)
  expect_equal(track_speed(ts)$speed[1], 2)        # 3 units in 1.5 s
})

test_that("a frame rate set on seconds time is not applied to it", {
  ts <- set_frame_rate(load_secs(), 2)
  expect_equal(frame_rate(ts), 2)
  expect_equal(elapsed_seconds(ts), c(0, 0.5, 1, 1.5))
  expect_equal(elapsed_seconds(load_secs(fps = 2)), c(0, 0.5, 1, 1.5))
})

test_that("a duration column is seconds, whatever its units", {
  df <- secs_df()
  df$t <- as.difftime(c(0, 1, 2, 3), units = "mins")
  ts <- read_tracks(df, mapping = list(id = "id", time = "t", x = "x", y = "y"))
  expect_equal(elapsed_seconds(ts), c(0, 60, 120, 180))
})

test_that("numeric time of unknown unit is still a frame index", {
  map <- list(id = "id", time = "t", x = "x", y = "y")
  ts <- read_tracks(secs_df(t = 0:3), mapping = map)
  expect_error(elapsed_seconds(ts), "frame rate")
  expect_error(elapsed_seconds(ts), "time_type = \"seconds\"", fixed = TRUE)
  expect_equal(elapsed_seconds(set_frame_rate(ts, 2)), (0:3) / 2)
  expect_equal(elapsed_seconds(read_tracks(secs_df(t = 0:3), mapping = map, fps = 2)),
               (0:3) / 2)
})

test_that("the time unit survives subsetting and must agree in c()", {
  a <- load_secs()
  b <- read_tracks(secs_df(id = "B"), mapping = list(id = "id", time = "t", x = "x", y = "y"),
                   time_type = "seconds")
  expect_equal(elapsed_seconds(a["A"]), c(0, 0.5, 1, 1.5))
  expect_equal(elapsed_seconds(c(a, b)), rep(c(0, 0.5, 1, 1.5), 2))
  frames <- read_tracks(secs_df(id = "C", t = 0:3),
                        mapping = list(id = "id", time = "t", x = "x", y = "y"))
  expect_error(c(a, frames), "conflicting 'time_unit'")
})

test_that("an unknown time unit in the metadata is an error, not frames", {
  ts <- load_secs()
  ts@meta$time_unit <- "minutes"
  expect_error(elapsed_seconds(ts), "time_unit")
})

test_that("radiate() colours seconds-time tracks by elapsed time without a frame rate", {
  expect_s3_class(radiate(load_secs(normalize_xy = TRUE), track_colour = "time"), "ggplot")
})
