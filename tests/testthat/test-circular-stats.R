test_that("circ_summary matches circular package statistics", {
  df <- data.frame(
    id = rep(c("a", "b"), each = 4),
    time = rep(seq_len(4), times = 2),
    angle = c(0, pi / 6, pi / 3, pi / 2, pi, 5 * pi / 4, 3 * pi / 2, 11 * pi / 6)
  )

  ts <- TrajSet(df, id = "id", time = "time", angle = "angle", angle_unit = "radians")
  summary_by_id <- circ_summary(ts, by = "id")

  wrap_to_2pi_local <- function(theta) {
    out <- theta %% (2 * pi)
    out[out < 0] <- out[out < 0] + 2 * pi
    out
  }

  expected <- lapply(split(df$angle, df$id), function(theta) {
    tc <- circular::circular(theta, units = "radians", modulo = "2pi")
    list(
      mean = wrap_to_2pi_local(as.numeric(circular::mean.circular(tc))),
      R = as.numeric(circular::rho.circular(tc))
    )
  })

  for (grp in names(expected)) {
    row <- summary_by_id[summary_by_id$id == grp, ]
    expect_equal(row$mean_dir, expected[[grp]]$mean, tolerance = 1e-8)
    expect_equal(row$resultant_R, expected[[grp]]$R, tolerance = 1e-8)
  }

  global <- circ_summary(ts, by = "global")
  tc_global <- circular::circular(df$angle, units = "radians", modulo = "2pi")
  expect_equal(global$mean_dir, wrap_to_2pi_local(as.numeric(circular::mean.circular(tc_global))), tolerance = 1e-8)
  expect_equal(global$resultant_R, as.numeric(circular::rho.circular(tc_global)), tolerance = 1e-8)
})

test_that("clockwise conversions preserve angles", {
  angles <- seq(-2 * pi, 2 * pi, length.out = 11)
  clock <- rad2clock(angles)
  back_to_unit <- rad_unclock(clock)
  expect_true(all(back_to_unit <= pi & back_to_unit > -pi + 1e-12))
  expect_equal(back_to_unit, ((angles + pi) %% (2 * pi)) - pi, tolerance = 1e-8)
})

test_that("shepherding functions wrap angles into valid ranges", {
  angles <- c(-4*pi, -pi, -pi/2, 0, pi, 3*pi)
  wrapped_unit <- rad_shepherd(angles)
  expect_true(all(wrapped_unit <= pi & wrapped_unit > -pi - 1e-10))

  clock_angles <- c(-2*pi, -0.1, 0, pi, 3*pi, 5*pi)
  wrapped_clock <- rad_shepherd_clock(clock_angles)
  expect_true(all(wrapped_clock >= 0 & wrapped_clock < 2*pi + 1e-10))
})

test_that("circ_summary angle_convention='clock' converts mean_dir output", {
  # x=0, y>0 gives atan2(y,x) = pi/2 for all points (North in unit-circle)
  df <- data.frame(id = "A", time = 1:4,
                   x = c(0, 0, 0, 0), y = c(0.2, 0.4, 0.6, 0.8))
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                normalize_xy = FALSE)
  summ_clock <- circ_summary(ts, angle_convention = "clock")
  # Absolute clock: (pi/2 - pi/2) %% 2pi = 0
  expect_equal(summ_clock$mean_dir, 0, tolerance = 1e-6)
  # Default (unit_circle) unchanged
  summ_uc <- circ_summary(ts)
  expect_equal(summ_uc$mean_dir, pi / 2, tolerance = 1e-6)
})
