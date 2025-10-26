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
