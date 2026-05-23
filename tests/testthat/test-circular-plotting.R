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

# ---- assign_cycle_colours ---------------------------------------------------

test_that("assign_cycle_colours cycles indices 1:n across trajectories", {
  df <- data.frame(id = paste0("T", 1:12), val = 1:12)
  out <- assign_cycle_colours(df, id_col = "id", n = 4L)
  expect_true("cycle_colour" %in% names(out))
  expect_s3_class(out$cycle_colour, "factor")
  expect_equal(levels(out$cycle_colour), as.character(1:4))
  # 12 ids with n=4 should give indices 1,2,3,4,1,2,3,4,1,2,3,4
  expect_equal(as.integer(out$cycle_colour), rep(1:4, 3))
})

test_that("assign_cycle_colours resets cycle within each panel", {
  df <- data.frame(id = paste0("T", 1:12),
                   panel = rep(c("A", "B"), each = 6))
  out <- assign_cycle_colours(df, id_col = "id", n = 4L, panel_col = "panel")
  # within panel A: ids T1-T6 get indices 1,2,3,4,1,2
  idx_A <- as.integer(out$cycle_colour[out$panel == "A"])
  expect_equal(idx_A, c(1, 2, 3, 4, 1, 2))
  # within panel B: ids T7-T12 get indices 1,2,3,4,1,2
  idx_B <- as.integer(out$cycle_colour[out$panel == "B"])
  expect_equal(idx_B, c(1, 2, 3, 4, 1, 2))
})

test_that("assign_cycle_colours accepts character vector for n", {
  df <- data.frame(id = paste0("T", 1:6), val = 1:6)
  cols <- c("red", "blue", "green")
  out <- assign_cycle_colours(df, id_col = "id", n = cols)
  expect_equal(levels(out$cycle_colour), as.character(1:3))
  expect_equal(as.integer(out$cycle_colour), c(1, 2, 3, 1, 2, 3))
})

test_that("assign_cycle_colours errors on missing id_col", {
  df <- data.frame(x = 1:3)
  expect_error(assign_cycle_colours(df, id_col = "id", n = 3), "id_col")
})

test_that("assign_cycle_colours errors on missing panel_col", {
  df <- data.frame(id = c("A", "B"))
  expect_error(assign_cycle_colours(df, id_col = "id", n = 2, panel_col = "grp"), "panel_col")
})

test_that("assign_cycle_colours custom out_col name is honoured", {
  df <- data.frame(id = c("A", "B", "C"))
  out <- assign_cycle_colours(df, id_col = "id", n = 2L, out_col = ".cyc")
  expect_true(".cyc" %in% names(out))
  expect_false("cycle_colour" %in% names(out))
})

# ---- radiate() colour_cycle parameter ---------------------------------------

test_that("radiate colour_cycle integer produces a ggplot with colour scale", {
  library(ggplot2)
  sim <- simulate_tracks(conditions = data.frame(n_trials = 6L), n_points = 20, seed = 1)
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id", colour_cycle = 3L,
               show_arrow = FALSE, show_labels = FALSE)
  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  # At least one built layer (the paths) should carry >= 2 distinct colours
  # (3-cycle over 6 trajectories → 2 repetitions of 3 colours)
  any_varied <- any(vapply(built$data, function(d) {
    "colour" %in% names(d) && length(unique(d$colour)) >= 2L
  }, logical(1)))
  expect_true(any_varied)
})

test_that("radiate colour_cycle character vector applies a discrete colour scale", {
  library(ggplot2)
  sim <- simulate_tracks(conditions = data.frame(n_trials = 3L), n_points = 20, seed = 2)
  cols <- c("red", "blue", "green")
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id", colour_cycle = cols,
               show_arrow = FALSE, show_labels = FALSE)
  expect_s3_class(p, "ggplot")
  # A ScaleDiscrete should be present for the colour aesthetic
  has_discrete_colour <- any(vapply(p$scales$scales, function(s)
    inherits(s, "ScaleDiscrete"), logical(1)))
  expect_true(has_discrete_colour)
  # The built paths should use only the supplied colours
  built <- ggplot_build(p)
  path_colours <- unlist(lapply(built$data, function(d) {
    if ("colour" %in% names(d) && length(unique(d$colour)) >= 2L) unique(d$colour)
  }))
  expect_true(all(path_colours %in% cols))
})

test_that("radiate errors when colour_col and colour_cycle both set", {
  sim <- simulate_tracks(conditions = data.frame(n_trials = 2L), n_points = 10, seed = 3)
  expect_error(
    radiate(sim, x_col = "rel_x", y_col = "rel_y",
            group_col = "trial_id", colour_col = "trial_id", colour_cycle = 3L),
    "colour_col.*colour_cycle|colour_cycle.*colour_col"
  )
})

# ---- strip_labels: panel annotations ----------------------------------------

test_that("strip_labels defaults to shown when panel_by is set", {
  library(ggplot2)
  sim <- suppressWarnings(
    simulate_tracks(conditions = data.frame(n_trials = 3L, condition = c("A","B","C")),
                    n_points = 5, seed = 9)
  )
  sim$grp <- rep(c("X","Y","Z"), each = nrow(sim) / 3)
  # Default (strip_labels = NULL) with panel_by set should show strip text
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id", panel_by = "condition",
               show_arrow = FALSE, show_labels = FALSE)
  # strip.text should NOT be element_blank after radiate override
  strip_el <- ggplot2::calc_element("strip.text", p$theme)
  expect_false(inherits(strip_el, "element_blank"))
})

test_that("strip_labels = FALSE hides strip text", {
  library(ggplot2)
  sim <- suppressWarnings(
    simulate_tracks(conditions = data.frame(n_trials = 2L),
                    n_points = 5, seed = 10)
  )
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id", panel_by = "condition",
               strip_labels = FALSE,
               show_arrow = FALSE, show_labels = FALSE)
  strip_el <- ggplot2::calc_element("strip.text", p$theme)
  expect_true(inherits(strip_el, "element_blank"))
})

test_that("strip_position inside adds a geom_text layer inside the plot", {
  library(ggplot2)
  sim <- suppressWarnings(
    simulate_tracks(conditions = data.frame(n_trials = 2L),
                    n_points = 5, seed = 11)
  )
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id", panel_by = "condition",
               strip_position = "inside",
               show_arrow = FALSE, show_labels = FALSE)
  built <- ggplot_build(p)
  # One geom_text layer should carry y = -1.25 annotation
  text_layers <- Filter(function(d) "label" %in% names(d), built$data)
  expect_true(length(text_layers) >= 1L)
  y_vals <- unlist(lapply(text_layers, function(d) d$y))
  expect_true(any(abs(y_vals - (-1.25)) < 1e-6))
})

test_that("strip_position bottom passes through to facet_wrap", {
  library(ggplot2)
  sim <- suppressWarnings(
    simulate_tracks(conditions = data.frame(n_trials = 2L),
                    n_points = 5, seed = 12)
  )
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id", panel_by = "condition",
               strip_position = "bottom",
               show_arrow = FALSE, show_labels = FALSE)
  # facet_wrap stores strip.position in its params
  facet_params <- p$facet$params
  expect_equal(facet_params$strip.position, "bottom")
})

test_that("add_heading_points and add_heading_vectors accept fixed colour parameter", {
  library(ggplot2)
  hd <- data.frame(heading = pi / 3, x_inner = 0.1, y_inner = 0.1)
  p_pts <- ggplot() + add_heading_points(hd, colour = "steelblue")
  p_vec <- ggplot() + add_heading_vectors(hd, colour = "tomato")
  expect_s3_class(p_pts, "ggplot")
  expect_s3_class(p_vec, "ggplot")
  built_pts <- ggplot_build(p_pts)
  expect_true(all(built_pts$data[[1]]$colour == "steelblue"))
})
