mk_cal_ts <- function() {
  d <- data.frame(id = "a", frame = 0:2, x = c(0, 1, 2), y = 0, angle = 0)
  methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
}

test_that("set_distance_scale / distance_scale / distance_unit round-trip and validate", {
  ts <- mk_cal_ts()
  expect_null(distance_scale(ts))
  expect_null(distance_unit(ts))
  ts2 <- set_distance_scale(ts, 50, unit = "mm")
  expect_equal(distance_scale(ts2), 50)
  expect_equal(distance_unit(ts2), "mm")
  expect_error(set_distance_scale(ts, 0), "positive")
  expect_error(set_distance_scale(ts, -1), "positive")
  expect_error(set_distance_scale(ts, c(1, 2)), "single")
  expect_error(set_distance_scale(ts, 5, unit = c("mm", "cm")), "single string")
})

test_that("calibrate_distance computes scale = real / coord", {
  ts <- calibrate_distance(mk_cal_ts(), coord_distance = 0.8, real_distance = 40, unit = "mm")
  expect_equal(distance_scale(ts), 50)
  expect_equal(distance_unit(ts), "mm")
  expect_error(calibrate_distance(mk_cal_ts(), 0, 40), "positive")
  expect_error(calibrate_distance(mk_cal_ts(), 1, -3), "positive")
})
