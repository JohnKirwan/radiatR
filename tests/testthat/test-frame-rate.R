mk_ts <- function(time, id = rep("a", length(time))) {
  # minimal Tracks with id/time/angle
  df <- data.frame(id = id, time = time, angle = rep(0, length(time)))
  methods::new("Tracks", data = df,
               cols = list(id = "id", time = "time", angle = "angle"),
               angle_unit = "radians", meta = list())
}

test_that("set_frame_rate / frame_rate round-trip and validate", {
  ts <- mk_ts(1:5)
  expect_null(frame_rate(ts))
  ts2 <- set_frame_rate(ts, 30)
  expect_equal(frame_rate(ts2), 30)
  expect_error(set_frame_rate(ts, 0), "positive")
  expect_error(set_frame_rate(ts, -5), "positive")
  expect_error(set_frame_rate(ts, c(30, 60)), "single")
  expect_error(set_frame_rate(ts, Inf), "positive")
})

test_that(".elapsed_time: numeric frames need a frame rate and convert per track", {
  # two tracks, frames; first point of each -> 0
  el <- radiatR:::.elapsed_time(c(0, 30, 60, 10, 40), id = rep(c("a", "b"), c(3, 2)), fps = 30)
  expect_equal(el, c(0, 1, 2, 0, 1))            # (frame - min)/30 per id
  expect_error(radiatR:::.elapsed_time(c(0, 30), id = c("a", "a"), fps = NULL),
               "frame rate")
})

test_that(".elapsed_time: POSIXct uses difftime seconds with no frame rate; units scale", {
  t0 <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  tt <- t0 + c(0, 2, 5)
  expect_equal(radiatR:::.elapsed_time(tt, id = rep("a", 3)), c(0, 2, 5))
  expect_equal(radiatR:::.elapsed_time(tt, id = rep("a", 3), units = "milliseconds"),
               c(0, 2000, 5000))
  expect_equal(radiatR:::.elapsed_time(tt, id = rep("a", 3), units = "minutes"),
               c(0, 2/60, 5/60))
})

test_that("elapsed_seconds and track_duration on a Tracks", {
  ts <- set_frame_rate(mk_ts(c(0, 30, 60, 0, 30), id = rep(c("a", "b"), c(3, 2))), 30)
  expect_equal(elapsed_seconds(ts), c(0, 1, 2, 0, 1))
  d <- track_duration(ts)
  expect_named(d, c("id", "duration"))
  expect_equal(d$duration[d$id == "a"], 2)      # 0..2 s
  expect_equal(d$duration[d$id == "b"], 1)
})
