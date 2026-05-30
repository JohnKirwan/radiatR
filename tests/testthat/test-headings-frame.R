test_that("headings_frame returns correct classes and attributes", {
  df <- data.frame(bearing = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "bearing", units = "radians")
  expect_s3_class(hf, "headings_frame")
  expect_s3_class(hf, "data.frame")
  expect_equal(attr(hf, "heading_col"),      "bearing")
  expect_equal(attr(hf, "angle_convention"), "unit_circle")
  expect_equal(attr(hf, "coords"),           "absolute")
})

test_that("headings_frame converts degrees to radians in place", {
  df <- data.frame(deg = c(0, 90, 180, 270))
  hf <- headings_frame(df, col = "deg", units = "degrees")
  expect_equal(hf$deg, c(0, pi/2, pi, 3*pi/2), tolerance = 1e-10)
})

test_that("headings_frame accepts non-default angle_convention and coords", {
  df <- data.frame(h = c(0, pi/4))
  hf <- headings_frame(df, col = "h", units = "radians",
                       angle_convention = "clock", coords = "relative")
  expect_equal(attr(hf, "angle_convention"), "clock")
  expect_equal(attr(hf, "coords"),           "relative")
})

test_that("headings_frame errors informatively when col not found", {
  df <- data.frame(x = 1)
  expect_error(headings_frame(df, col = "bearing", units = "radians"),
               "column 'bearing' not found")
})

test_that("headings_frame errors informatively when units missing", {
  df <- data.frame(heading = 0)
  expect_error(headings_frame(df, col = "heading"),
               "'units' must be specified")
})

test_that("headings_frame preserves all original columns", {
  df <- data.frame(id = "A", trial = 1L, heading = pi/4)
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_true(all(c("id", "trial", "heading") %in% names(hf)))
})

test_that("headings_frame accepts unquoted col name", {
  df <- data.frame(bearing = c(0, pi/4))
  hf <- headings_frame(df, col = bearing, units = "radians")
  expect_equal(attr(hf, "heading_col"), "bearing")
})

# ---- stack_headings -----------------------------------------------------------

test_that("stack_headings always adds stack_r and stack_n columns", {
  df <- data.frame(heading = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf)
  expect_true(all(c("stack_r", "stack_n") %in% names(out)))
  expect_equal(nrow(out), 3L)  # row count unchanged
})

test_that("stack_headings inward: outermost rank=1 at base_r, inner ranks decrease", {
  df <- data.frame(heading = c(0, 0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, step = 0.025, direction = "inward", base_r = 1)
  expect_equal(max(out$stack_r), 1, tolerance = 1e-10)
  expect_equal(sort(out$stack_r, decreasing = TRUE),
               c(1, 0.975, 0.950), tolerance = 1e-10)
})

test_that("stack_headings outward: outermost rank=1 at base_r, inner ranks increase", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, step = 0.025, direction = "outward", base_r = 1)
  expect_equal(min(out$stack_r), 1, tolerance = 1e-10)
  expect_equal(sort(out$stack_r), c(1, 1.025), tolerance = 1e-10)
})

test_that("stack_headings unique angles: every observation gets stack_r == base_r", {
  df <- data.frame(heading = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, base_r = 1)
  expect_true(all(out$stack_r == 1))
  expect_true(all(out$stack_n == 1L))
})

test_that("stack_headings stack_n counts coincident angles correctly", {
  df <- data.frame(heading = c(0, 0, 0, pi/2, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf)
  expect_equal(out$stack_n[out$heading == 0],    rep(3L, 3))
  expect_equal(out$stack_n[out$heading == pi/2], rep(2L, 2))
})

test_that("stack_headings shade=TRUE adds shade_n column equal to stack_n", {
  df <- data.frame(heading = c(0, 0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shade = TRUE)
  expect_true("shade_n" %in% names(out))
  expect_equal(out$shade_n, out$stack_n)
})

test_that("stack_headings shade=FALSE does not add shade_n", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shade = FALSE)
  expect_false("shade_n" %in% names(out))
})

test_that("stack_headings shape=TRUE: n=1 group gets shape_code=1", {
  df <- data.frame(heading = c(0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shape = TRUE)
  expect_true("shape_code" %in% names(out))
  expect_true(all(out$shape_code == 1L))
})

test_that("stack_headings shape=TRUE: n=3 group gets codes 1, 2, 3", {
  df <- data.frame(heading = c(0, 0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shape = TRUE)
  expect_equal(sort(out$shape_code), c(1L, 2L, 3L))
})

test_that("stack_headings shape=FALSE does not add shape_code", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shape = FALSE)
  expect_false("shape_code" %in% names(out))
})

test_that("stack_headings tol groups near-coincident angles", {
  # 0 and 0.01 are within tol=0.05; pi/2 is separate
  df <- data.frame(heading = c(0, 0.01, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, tol = 0.05)
  expect_equal(out$stack_n[out$heading == 0],    2L)
  expect_equal(out$stack_n[out$heading == 0.01], 2L)
  expect_equal(out$stack_n[out$heading == pi/2], 1L)
})

test_that("stack_headings col defaults to heading_col attribute", {
  df <- data.frame(bearing = c(0, 0))
  hf <- headings_frame(df, col = "bearing", units = "radians")
  out <- stack_headings(hf)   # no col arg — should find "bearing" via attribute
  expect_equal(max(out$stack_n), 2L)
})

test_that("stack_headings errors when step <= 0", {
  df <- data.frame(heading = 0)
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_error(stack_headings(hf, step = 0),  "'step' must be positive")
  expect_error(stack_headings(hf, step = -1), "'step' must be positive")
})

test_that("stack_headings errors when tol < 0", {
  df <- data.frame(heading = 0)
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_error(stack_headings(hf, tol = -0.1), "'tol' must be NULL or non-negative")
})

test_that("stack_headings shape=TRUE: depth=2 group gets codes 1 and 2", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, shape = TRUE)
  expect_equal(sort(out$shape_code), c(1L, 2L))
})

test_that("stack_headings NA angles produce NA stack_r and stack_n", {
  df <- data.frame(heading = c(0, NA_real_, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf)
  expect_true(is.na(out$stack_r[2L]))
  expect_true(is.na(out$stack_n[2L]))
})

# ---- add_stacked_headings ----------------------------------------------------

test_that("add_stacked_headings returns a ggplot2 layer", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  layer <- add_stacked_headings(hf)
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_stacked_headings auto-computes stack_r when absent", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, 0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_no_error(add_stacked_headings(hf))
})

test_that("add_stacked_headings uses pre-computed stack_r without recomputing", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  pre <- stack_headings(hf)
  pre$stack_r <- c(0.99, 0.99)  # custom values
  layer <- add_stacked_headings(pre)
  # The layer data must retain our custom stack_r
  expect_equal(layer$data$stack_r, c(0.99, 0.99))
})

test_that("add_stacked_headings col defaults to heading_col attribute", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(bearing = c(0, pi/4))
  hf <- headings_frame(df, col = "bearing", units = "radians")
  expect_no_error(add_stacked_headings(hf))  # finds "bearing" via attribute
})

test_that("add_stacked_headings errors when col not found", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1)
  expect_error(add_stacked_headings(df, col = "bearing"),
               "column 'bearing' not found")})
