test_that("add_heading_points returns a geom_point layer", {
  hd <- data.frame(id = "A", time = 1, heading = pi / 4)
  layer <- add_heading_points(hd)
  expect_s3_class(layer, "LayerInstance")
  # colour_col mapping
  hd$grp <- "X"
  layer_col <- add_heading_points(hd, colour_col = "grp")
  expect_s3_class(layer_col, "LayerInstance")
  # missing heading column should error
  expect_error(add_heading_points(data.frame(x = 1)), "heading")
})

test_that("add_heading_vectors returns a geom_segment layer", {
  hd <- data.frame(id = "A", time = 1, heading = pi / 4,
                   x_inner = 0.14, y_inner = 0.14)
  layer <- add_heading_vectors(hd)
  expect_s3_class(layer, "LayerInstance")
  # missing required columns should error
  expect_error(add_heading_vectors(data.frame(heading = 1)), "x_inner")
})

test_that("add_heading_points and add_heading_vectors can be added to ggplot", {
  library(ggplot2)
  hd <- data.frame(id = c("A", "B"), time = c(1, 1),
                   heading   = c(pi / 4, pi),
                   x_inner   = c(0.14,  -0.14),
                   y_inner   = c(0.14,   0.00))
  p <- ggplot() + coord_fixed() +
    add_heading_points(hd) +
    add_heading_vectors(hd)
  expect_s3_class(p, "ggplot")
  # should build without error
  expect_silent(ggplot_build(p))
})

# ---- circ_mean_segments: arrow endpoint arithmetic --------------------------

test_that("circ_mean_segments tip is at R*cos(mean_dir), R*sin(mean_dir)", {
  stats_df <- data.frame(mean_dir = pi / 3, resultant_R = 0.7)
  seg <- circ_mean_segments(stats_df)

  expect_equal(seg$x,    0,                          tolerance = 1e-10)
  expect_equal(seg$y,    0,                          tolerance = 1e-10)
  expect_equal(seg$xend, 0.7 * cos(pi / 3),          tolerance = 1e-10)
  expect_equal(seg$yend, 0.7 * sin(pi / 3),          tolerance = 1e-10)
})

test_that("circ_mean_segments scale parameter multiplies arrow length", {
  stats_df <- data.frame(mean_dir = pi / 4, resultant_R = 0.5)
  seg2 <- circ_mean_segments(stats_df, scale = 2)

  expect_equal(seg2$xend, 2 * 0.5 * cos(pi / 4), tolerance = 1e-10)
  expect_equal(seg2$yend, 2 * 0.5 * sin(pi / 4), tolerance = 1e-10)
})

test_that("circ_mean_segments custom origin shifts arrow base", {
  stats_df <- data.frame(mean_dir = 0, resultant_R = 1)
  seg <- circ_mean_segments(stats_df, x0 = 0.5, y0 = 0.5)

  expect_equal(seg$x,    0.5, tolerance = 1e-10)
  expect_equal(seg$y,    0.5, tolerance = 1e-10)
  expect_equal(seg$xend, 1.5, tolerance = 1e-10)  # 0.5 + 1*cos(0)
  expect_equal(seg$yend, 0.5, tolerance = 1e-10)  # 0.5 + 1*sin(0)
})

# ---- directedness_arrow: tip position ---------------------------------------

test_that("directedness_arrow tip is at rho*(cos(mean), sin(mean))", {
  # All angles at pi/4 => circular mean = pi/4, rho = 1
  layer <- suppressWarnings(
    directedness_arrow(data.frame(theta = rep(pi / 4, 5)), theta)
  )
  tip <- layer$data  # geom_segment stores its data frame in $data

  expect_equal(tip$x,    0,              tolerance = 1e-6)
  expect_equal(tip$y,    0,              tolerance = 1e-6)
  expect_equal(tip$xend, cos(pi / 4),   tolerance = 1e-6)
  expect_equal(tip$yend, sin(pi / 4),   tolerance = 1e-6)
})

test_that("directedness_arrow tip for known spread of angles", {
  # Angles: 0, pi/2, pi, 3*pi/2 => rho = 0 => tip at (0, 0)
  layer <- suppressWarnings(
    directedness_arrow(data.frame(theta = c(0, pi / 2, pi, 3 * pi / 2)), theta)
  )
  tip <- layer$data

  expect_equal(tip$xend, 0, tolerance = 1e-6)
  expect_equal(tip$yend, 0, tolerance = 1e-6)
})

test_that("directedness_arrow tip for three asymmetric angles matches analytic result", {
  # Angles: pi/6, pi/2, 5*pi/6 => mean = pi/2, rho = 2/3  (see heading tests)
  layer <- suppressWarnings(
    directedness_arrow(
      data.frame(theta = c(pi / 6, pi / 2, 5 * pi / 6)),
      theta
    )
  )
  tip <- layer$data

  expect_equal(tip$xend, (2 / 3) * cos(pi / 2), tolerance = 1e-6)
  expect_equal(tip$yend, (2 / 3) * sin(pi / 2), tolerance = 1e-6)
})

# ---- add_heading_points / vectors endpoint arithmetic -----------------------

test_that("add_heading_points places markers at (cos(h), sin(h))", {
  library(ggplot2)
  hd <- data.frame(heading = c(0, pi / 2, pi))
  p  <- ggplot() + add_heading_points(hd)
  built <- ggplot_build(p)
  pts   <- built$data[[1]]
  expect_equal(pts$x, cos(hd$heading), tolerance = 1e-6)
  expect_equal(pts$y, sin(hd$heading), tolerance = 1e-6)
})

test_that("add_heading_vectors segments run from (x_inner,y_inner) to (cos(h),sin(h))", {
  library(ggplot2)
  hd <- data.frame(heading = pi / 4, x_inner = 0.14, y_inner = 0.14)
  p  <- ggplot() + add_heading_vectors(hd)
  built <- ggplot_build(p)
  seg   <- built$data[[1]]
  expect_equal(seg$x,    0.14,          tolerance = 1e-6)
  expect_equal(seg$y,    0.14,          tolerance = 1e-6)
  expect_equal(seg$xend, cos(pi / 4),   tolerance = 1e-6)
  expect_equal(seg$yend, sin(pi / 4),   tolerance = 1e-6)
})

test_that("plotting helpers return ggplot layers", {
  ticks <- add_ticks()
  expect_s3_class(ticks, "LayerInstance")

  circ <- add_circ(radius = 0.5)
  expect_type(circ, "list")
  expect_s3_class(circ[[1]], "LayerInstance")

  labs <- degree_labs()
  expect_length(labs, 4)

  arrow <- suppressWarnings(directedness_arrow(data.frame(theta = c(0, pi/2, pi)), theta))
  expect_s3_class(arrow, "LayerInstance")
})

test_that("trajectory helpers create ggplot objects", {
  df <- data.frame(
    id = rep("A", 4),
    time = 1:4,
    x = c(0, 0.5, 1, 1.5),
    y = c(0, 0.1, 0.2, 0.3)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "time", angle_unit = "radians")
  gp <- gg_traj(ts, geom = "path", coord = "cartesian")
  expect_s3_class(gp, "ggplot")

  layer_list <- draw_tracks(df, "x", "y")
  expect_type(layer_list, "list")
  expect_s3_class(layer_list[[0 + 1]], "LayerInstance")
})

test_that("line-circle intercept utilities behave as expected", {
  ints <- line_circle_intercept(0, 0, 2, 0)
  expect_equal(unname(unlist(ints)), c(1, 0), tolerance = 1e-8)

  df <- data.frame(x = c(0, 2), y = c(0, 0))
  ints_df <- line_circle_intercept_df(df, 1, 2)
  expect_equal(ints_df$x_int, 1, tolerance = 1e-8)

  traj_df <- data.frame(
    id = c("A", "A"),
    time = c(0, 1),
    x = c(0, 2),
    y = c(0, 0),
    angle = c(0, 0)
  )
  ts <- TrajSet(traj_df, id = "id", time = "time", x = "x", y = "y", angle = "angle")
  ints_traj <- line_circle_intercept_traj(ts, "A", 1:2)
  expect_equal(ints_traj$y_int, 0, tolerance = 1e-8)
})
