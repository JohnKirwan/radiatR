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
