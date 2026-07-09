# bin_angles() snaps angles (radians) to fixed-width circular bin centres.
# phase = 0 centres bins on the reference direction (0, width, 2*width, ...);
# phase = width/2 reproduces circular::plot.circular's edge-aligned bins.

deg <- function(d) d * pi / 180

test_that("phase = 0 centres bins on zero (reference on a column, not a boundary)", {
  w <- deg(5)
  # a 1 deg point sits in the bin centred on 0
  expect_equal(bin_angles(deg(1), w, phase = 0), 0)
  # 3 deg rounds up to the 5 deg centre
  expect_equal(bin_angles(deg(3), w, phase = 0), deg(5))
  # exact centre is unchanged
  expect_equal(bin_angles(deg(10), w, phase = 0), deg(10))
})

test_that("phase = width/2 reproduces circular's edge-aligned bins", {
  w <- deg(5)
  # circular: a 1 deg point falls in the [0,5) bin -> centre 2.5 deg
  expect_equal(bin_angles(deg(1), w, phase = w / 2), deg(2.5))
  expect_equal(bin_angles(deg(4), w, phase = w / 2), deg(2.5))
  expect_equal(bin_angles(deg(6), w, phase = w / 2), deg(7.5))
})

test_that("results are wrapped to [0, 2*pi)", {
  w <- deg(5)
  # 359 deg snaps to the 0 bin (centre 0), not 360
  expect_equal(bin_angles(deg(359), w, phase = 0), 0)
  # a negative angle wraps into range
  expect_equal(bin_angles(deg(-1), w, phase = 0), 0)
  expect_true(all(bin_angles(seq(-2 * pi, 4 * pi, length.out = 50), w) < 2 * pi))
  expect_true(all(bin_angles(seq(-2 * pi, 4 * pi, length.out = 50), w) >= 0))
})

test_that("an arbitrary phase aligns bins to quadrant boundaries", {
  # width = 90 deg, phase = 45 deg -> bin centres at 45/135/225/315 (quadrant
  # centres), so boundaries fall on the axes (0/90/180/270).
  w <- deg(90)
  expect_equal(bin_angles(deg(10), w, phase = deg(45)), deg(45))
  expect_equal(bin_angles(deg(100), w, phase = deg(45)), deg(135))
})

test_that("the snapped values are multiples of width plus phase", {
  w <- deg(5)
  set.seed(1)
  a <- runif(200, 0, 2 * pi)
  out <- bin_angles(a, w, phase = 0)
  # every value is k*w (mod 2pi)
  expect_true(all(abs(((out / w) - round(out / w))) < 1e-8))
})

test_that("NA in -> NA out, length preserved, vectorised", {
  w <- deg(5)
  # >= 2 NAs: a logical-index assignment in wrap_to_2pi errors on multiple NAs,
  # so this guards the NA-safe path specifically.
  out <- bin_angles(c(deg(1), NA, deg(91), NA, deg(2)), w)
  expect_length(out, 5L)
  expect_true(all(is.na(out[c(2, 4)])))
  expect_equal(out[1], 0)
  expect_equal(out[3], deg(90))
  expect_equal(out[5], 0)
})

test_that("width must be a single positive number", {
  expect_error(bin_angles(1, 0), "positive")
  expect_error(bin_angles(1, -1), "positive")
  expect_error(bin_angles(1, c(1, 2)), "single")
  expect_error(bin_angles(1, NA_real_), "positive")
})
