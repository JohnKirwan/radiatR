# as_angle(): map periodic time/date/numeric data onto unit-circle radians,
# aligned with the clock/calendar circumference labels (top, clockwise).

.tc <- function(...) as.POSIXct(c(...), tz = "UTC")

test_that("as_angle('day') maps time-of-day to the clock convention", {
  t  <- .tc("2020-03-10 00:00:00", "2020-03-10 06:00:00",
            "2020-03-10 12:00:00", "2020-03-10 18:00:00")
  expect_equal(as_angle(t, "day"),
               (pi / 2 - 2 * pi * c(0, .25, .5, .75)) %% (2 * pi),
               tolerance = 1e-9)
})

test_that("as_angle('day') data coincides with the hours labels", {
  # scale_clock(24) labels hours 0/6/12/18 at .scale_positions(24, c(0,6,12,18));
  # the same hours via as_angle must land at the same raw unit-circle angle.
  t  <- .tc("2020-03-10 00:00:00", "2020-03-10 06:00:00",
            "2020-03-10 12:00:00", "2020-03-10 18:00:00")
  expect_equal(as_angle(t, "day"), scale_clock(24)$at, tolerance = 1e-9)
})

test_that("as_angle('year') is day-accurate, Jan 1 at the top, leap-safe", {
  jan1 <- .tc("2021-01-01 00:00:00")
  expect_equal(as_angle(jan1, "year"), pi / 2, tolerance = 1e-9)
  # the 1st of each month lands near its scale_months position
  firsts <- as.Date(sprintf("2021-%02d-01", 1:12))
  expect_equal(as_angle(firsts, "year"), scale_months()$at, tolerance = 0.02)
  # leap year: Dec 31 is still inside the cycle (f < 1, angle finite)
  dec31 <- as.Date("2020-12-31")
  a <- as_angle(dec31, "year")
  expect_true(is.finite(a) && a >= 0 && a < 2 * pi)
})

test_that("as_angle(numeric period) wraps and uses the clock convention", {
  expect_equal(as_angle(c(0, 2.5, 5, 7.5), period = 10),
               (pi / 2 - 2 * pi * c(0, .25, .5, .75)) %% (2 * pi),
               tolerance = 1e-9)
  expect_equal(as_angle(10, period = 10), as_angle(0, period = 10))   # wraps
})

test_that("as_angle propagates NA", {
  t <- .tc("2020-03-10 06:00:00", NA)
  out <- as_angle(t, "day")
  expect_false(is.na(out[1]))
  expect_true(is.na(out[2]))
})

test_that("as_angle validates period and input type", {
  expect_error(as_angle(as.Date("2020-01-01"), "day"), "POSIXct")     # day needs time
  expect_error(as_angle(1:3, "day"), "POSIXct|numeric")
  expect_error(as_angle(.tc("2020-01-01 00:00:00"), period = 10), "numeric")
  expect_error(as_angle(1:3, period = 0), "period")
  expect_error(as_angle(1:3, period = -1), "period")
  expect_error(as_angle(1:3, "week"), "period")
})
