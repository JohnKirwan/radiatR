test_that("mean resultant length and circular mean behave as expected", {
  angles <- c(0, pi / 2, pi / 4)
  expected_mean <- atan2(mean(sin(angles)), mean(cos(angles)))
  expect_equal(circular_mean(angles), expected_mean, tolerance = 1e-8)

  expected_R <- sqrt(mean(cos(angles))^2 + mean(sin(angles))^2)
  expect_equal(mean_resultant_length(angles), expected_R, tolerance = 1e-8)
})

test_that("circular standard deviation validates inputs", {
  expect_equal(circular_sd_from_R(1), 0)
  expect_error(circular_sd_from_R(0), "`R` must lie in the interval")
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
