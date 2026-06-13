test_that("derive_coords reproduces the loader mapping's derived fields", {
  set.seed(1)
  origin <- c(100, 100); reference <- c(160, 100)   # radius 60, ref at East
  m  <- build_unit_circle_mapping(origin, reference, flip_y = TRUE)
  x  <- runif(20, 40, 160); y <- runif(20, 40, 160)
  mp <- m$map(x, y)

  dc <- derive_coords(mp$trans_x, mp$trans_y, reference = m$ref_theta_unit)
  expect_equal(dc$trans_rho,       mp$trans_rho)
  expect_equal(dc$abs_theta_clock, mp$abs_theta_clock)
  expect_equal(dc$abs_theta_unit,  mp$abs_theta_unit)
  expect_equal(dc$rel_theta_unit,  mp$rel_theta_unit)
  expect_equal(dc$rel_x,           mp$rel_x)
  expect_equal(dc$rel_y,           mp$rel_y)
})

test_that("derive_coords with reference 0 gives relative == absolute", {
  dc <- derive_coords(c(0.5, -0.3), c(0.2, 0.4), reference = 0)
  expect_equal(dc$rel_theta_unit, dc$abs_theta_unit)
  expect_equal(dc$rel_x, c(0.5, -0.3))
  expect_equal(dc$rel_y, c(0.2, 0.4))
})
