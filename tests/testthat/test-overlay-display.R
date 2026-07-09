disp_id    <- circ_display()
disp_clock <- circ_display(zero = 0)

# Each overlay returns one geom_polygon whose data has x/y columns. Under a
# non-identity display the coordinates must be the identity coordinates mapped
# through .uc_to_display_coords (rotation applied once, consistently).
expect_rotates <- function(l0, l1) {
  exp <- .uc_to_display_coords(l0$data$x, l0$data$y, disp_clock)
  expect_equal(l1$data$x, exp$x, tolerance = 1e-9)
  expect_equal(l1$data$y, exp$y, tolerance = 1e-9)
}

test_that("add_angle_rose honors the display rotation", {
  hd <- data.frame(heading = c(0, 0.1, -0.1, 0.2, 0.05, 1.0, 3.0))
  expect_rotates(add_angle_rose(hd, display = disp_id),
                 add_angle_rose(hd, display = disp_clock))
})

test_that("add_circular_kde honors the display rotation", {
  set.seed(1)
  hd <- data.frame(heading = c(rnorm(20, 0, 0.4), rnorm(20, 2, 0.4)))
  expect_rotates(add_circular_kde(hd, display = disp_id),
                 add_circular_kde(hd, display = disp_clock))
})

test_that("add_vonmises_density honors the display rotation", {
  fit <- data.frame(mu = 0.3, kappa = 4)
  expect_rotates(add_vonmises_density(fit, display = disp_id),
                 add_vonmises_density(fit, display = disp_clock))
})

test_that("add_wrappedcauchy_density honors the display rotation", {
  fit <- data.frame(mu = 0.3, rho = 0.6)
  expect_rotates(add_wrappedcauchy_density(fit, display = disp_id),
                 add_wrappedcauchy_density(fit, display = disp_clock))
})

test_that("default display is the identity (backward compatible)", {
  hd <- data.frame(heading = c(0, 0.1, 0.2, 1.0))
  expect_equal(add_angle_rose(hd)$data$x,
               add_angle_rose(hd, display = disp_id)$data$x)
})

test_that("the display attribute is honored when no display argument is given", {
  hd <- data.frame(heading = c(0, 0.1, 0.2, 1.0))
  attr(hd, "display") <- disp_clock
  expect_equal(add_angle_rose(hd)$data$x,
               add_angle_rose(hd, display = disp_clock)$data$x)
})

test_that("a rotated overlay aligns with the heading-point direction", {
  fit <- data.frame(mu = 0, kappa = 6)
  for (disp in list(disp_id, disp_clock)) {
    vm <- add_vonmises_density(fit, display = disp)
    rr <- sqrt(vm$data$x^2 + vm$data$y^2)
    pk <- which.max(rr)
    hd <- data.frame(heading = 0); attr(hd, "display") <- disp
    hp <- add_heading_points(hd)
    expect_equal(atan2(vm$data$y[pk], vm$data$x[pk]),
                 atan2(hp$data$.y_head[1], hp$data$.x_head[1]),
                 tolerance = 1e-3)
  }
})
