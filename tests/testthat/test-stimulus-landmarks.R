test_that(".display_to_uc_angle inverts .uc_angle_to_display", {
  displays <- list(
    circ_display(),                                  # default: zero=pi/2, CW, degrees
    circ_display(zero = 0),                          # CW, degrees
    circ_display(clockwise = FALSE),                 # CCW, degrees
    circ_display(zero = 0, clockwise = FALSE),       # CCW, degrees
    circ_display(units = "radians")                  # default rotation, radians
  )
  theta <- seq(0, 2 * pi, length.out = 24)[-24]      # UC angles to round-trip
  for (d in displays) {
    disp <- radiatR:::.uc_angle_to_display(theta, d) # UC -> display
    back <- radiatR:::.display_to_uc_angle(disp, d)  # display -> UC
    expect_equal(back %% (2 * pi), theta %% (2 * pi), tolerance = 1e-9)
  }
})

test_that("add_stimulus_arc returns a list holding one ggplot layer", {
  out <- add_stimulus_arc(bearing = 0, width = 20)
  expect_type(out, "list")
  expect_length(out, 1L)
  expect_s3_class(out[[1]], "ggproto")   # annotate() returns a LayerInstance/ggproto
})

test_that("add_stimulus_arc places the arc at the expected canvas position (default display)", {
  # Default display: 0 = top, 90 = right, 180 = bottom, 270 = left (clockwise).
  # The ribbon is curved, so its vertex-centroid sits fractionally inside the
  # rim (chord effect); the arc's *direction* (normalised centroid) is the exact
  # display-mapped bearing, which is the property this maps to.
  library(ggplot2)
  direction <- function(bearing) {
    p  <- ggplot() + coord_fixed() +
      add_stimulus_arc(bearing = bearing, width = 10, thickness = 0.05)
    ld <- layer_data(p, 1)
    v  <- c(x = mean(ld$x), y = mean(ld$y))
    expect_equal(sqrt(sum(v^2)), 1, tolerance = 5e-3)  # centred on the rim
    v / sqrt(sum(v^2))
  }
  expect_equal(unname(direction(0)),   c(0,  1), tolerance = 1e-6)  # top
  expect_equal(unname(direction(90)),  c(1,  0), tolerance = 1e-6)  # right
  expect_equal(unname(direction(180)), c(0, -1), tolerance = 1e-6)  # bottom
  expect_equal(unname(direction(270)), c(-1, 0), tolerance = 1e-6)  # left
})

test_that("add_stimulus_arc honours a rotated display", {
  # circ_display(zero = 0): a stimulus at display 0 (East data) is drawn at top.
  library(ggplot2)
  p  <- ggplot() + coord_fixed() +
    add_stimulus_arc(bearing = 0, width = 10, thickness = 0.05,
                     display = circ_display(zero = 0))
  ld <- layer_data(p, 1)
  v  <- c(mean(ld$x), mean(ld$y))
  expect_equal(sqrt(sum(v^2)), 1, tolerance = 5e-3)      # centred on the rim
  expect_equal(v / sqrt(sum(v^2)), c(0, 1), tolerance = 1e-6)
})

test_that("add_stimulus_arc width and thickness scale the ribbon", {
  library(ggplot2)
  span <- function(width, thickness) {
    ld <- layer_data(
      ggplot() + coord_fixed() +
        add_stimulus_arc(bearing = 90, width = width, thickness = thickness), 1)
    c(radial = diff(range(sqrt(ld$x^2 + ld$y^2))),
      angular = diff(range(atan2(ld$y, ld$x))))
  }
  wide   <- span(60, 0.05)
  narrow <- span(10, 0.05)
  thick  <- span(60, 0.20)
  expect_gt(wide[["angular"]], narrow[["angular"]])   # wider width -> larger arc
  expect_gt(thick[["radial"]], wide[["radial"]])      # larger thickness -> wider band
})

test_that("add_landmark returns a point layer, plus a text layer when labelled", {
  bare <- add_landmark(bearing = 0)
  expect_type(bare, "list")
  expect_length(bare, 1L)

  labelled <- add_landmark(bearing = 0, label = "sun")
  expect_length(labelled, 2L)
})

test_that("add_landmark places the point at the expected canvas position", {
  library(ggplot2)
  # Default display, r = 1.12: bearing 90 -> right, at radius 1.12.
  ld <- layer_data(
    ggplot() + coord_fixed() + add_landmark(bearing = 90, r = 1.12), 1)
  expect_equal(c(ld$x[1], ld$y[1]), c(1.12, 0), tolerance = 1e-6)

  # Rotated display: bearing 0 under zero = 0 -> top.
  ld2 <- layer_data(
    ggplot() + coord_fixed() +
      add_landmark(bearing = 0, r = 1.12, display = circ_display(zero = 0)), 1)
  expect_equal(c(ld2$x[1], ld2$y[1]), c(0, 1.12), tolerance = 1e-6)
})
