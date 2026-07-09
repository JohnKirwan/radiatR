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

mk_len_ts <- function() {
  d <- rbind(
    data.frame(id = "a", frame = 0:3, x = (0:3) * 2, y = 0, angle = 0),  # length 6 coord-units
    data.frame(id = "b", frame = 0:2, x = 0:2,       y = 0, angle = 0)   # length 2
  )
  methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
}

test_that("track_length: coord units without a scale; physical units with one", {
  ts <- mk_len_ts()
  L <- track_length(ts)
  expect_named(L, c("id", "length"))
  expect_equal(L$length[L$id == "a"], 6)
  expect_equal(L$length[L$id == "b"], 2)
  ts2 <- set_distance_scale(ts, 50, "mm")
  expect_equal(track_length(ts2)$length[L$id == "a"], 6 * 50)   # 300 mm
})

test_that("track_speed / instantaneous_speed scale to physical units", {
  ts <- set_frame_rate(mk_len_ts(), 30)
  expect_equal(track_speed(ts)$speed[1], 60)                    # a: 2 units/frame * 30
  ts2 <- set_distance_scale(ts, 50, "mm")
  expect_equal(track_speed(ts2)$speed[1], 60 * 50)             # mm/s
  expect_equal(max(instantaneous_speed(ts2), na.rm = TRUE), 60 * 50)
})
