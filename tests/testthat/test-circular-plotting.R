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

# ---- compute_circular_density -----------------------------------------------

test_that("compute_circular_density vonmises returns theta and density columns", {
  set.seed(1)
  hd <- data.frame(heading = rnorm(40, 0.3, 0.5))
  d  <- compute_circular_density(hd, method = "vonmises")
  expect_true(all(c("theta", "density") %in% names(d)))
  expect_true(all(is.finite(d$theta)))
  expect_true(all(d$density >= 0))
})

test_that("compute_circular_density kernel returns theta and density columns", {
  set.seed(2)
  hd <- data.frame(heading = rnorm(40, 0.3, 0.5))
  d  <- compute_circular_density(hd, method = "kernel")
  expect_true(all(c("theta", "density") %in% names(d)))
  expect_true(all(d$density >= 0))
})

test_that("compute_circular_density histogram returns bin-count densities", {
  set.seed(3)
  hd <- data.frame(heading = rnorm(40, 0.3, 0.5))
  d  <- compute_circular_density(hd, method = "histogram", bins = 24)
  expect_equal(nrow(d), 24L)
  expect_true(all(d$density >= 0))
})

test_that("compute_circular_density with colour_col returns per-group rows", {
  set.seed(4)
  hd <- data.frame(
    heading = c(rnorm(30, 0.3, 0.4), rnorm(30, -1.2, 0.4)),
    grp     = rep(c("A", "B"), each = 30)
  )
  d <- compute_circular_density(hd, colour_col = "grp", n_theta = 100L)
  expect_true("grp" %in% names(d))
  expect_equal(sort(unique(d$grp)), c("A", "B"))
  expect_equal(nrow(d), 200L)
})

test_that("compute_circular_density errors on missing heading_col", {
  expect_error(compute_circular_density(data.frame(x = 1:5)), "heading_col")
})

# ---- add_circular_density (rendering only) ----------------------------------

test_that("add_circular_density renders a pre-computed density data frame", {
  library(ggplot2)
  theta_grid <- seq(-pi, pi, length.out = 200)
  dens_df <- data.frame(theta = theta_grid,
                        density = exp(-2 * (1 - cos(theta_grid - 0.5))))
  layers <- add_circular_density(dens_df)
  expect_type(layers, "list")
  expect_s3_class(layers[[length(layers)]], "LayerInstance")
})

test_that("add_circular_density fill adds a polygon layer", {
  library(ggplot2)
  dens_df <- compute_circular_density(
    data.frame(heading = rnorm(30, 1, 0.4)), method = "vonmises"
  )
  layers_no_fill <- add_circular_density(dens_df)
  layers_fill    <- add_circular_density(dens_df, fill = "steelblue")
  expect_equal(length(layers_fill), length(layers_no_fill) + 1L)
})

test_that("add_circular_density path stays within [1, 1+scale]", {
  library(ggplot2)
  set.seed(5)
  dens_df   <- compute_circular_density(data.frame(heading = rnorm(50, 0, 0.6)))
  scale_val <- 0.5
  p <- ggplot() + coord_fixed() + add_circular_density(dens_df, scale = scale_val)
  built  <- ggplot_build(p)
  path_d <- built$data[[1]]
  r_vals <- sqrt(path_d$x^2 + path_d$y^2)
  expect_true(all(r_vals >= 1 - 1e-6))
  expect_true(all(r_vals <= 1 + scale_val + 1e-6))
})

test_that("add_circular_density with colour_col draws per-group paths", {
  library(ggplot2)
  set.seed(6)
  hd <- data.frame(
    heading = c(rnorm(30, 0.3, 0.4), rnorm(30, -1.2, 0.4)),
    grp     = rep(c("A", "B"), each = 30)
  )
  dens_df <- compute_circular_density(hd, colour_col = "grp")
  layers  <- add_circular_density(dens_df, colour_col = "grp")
  p       <- ggplot() + coord_fixed() + layers
  built   <- ggplot_build(p)
  expect_true(length(unique(built$data[[1]]$group)) >= 2L)
})

test_that("add_circular_density errors on missing theta or density column", {
  expect_error(add_circular_density(data.frame(theta = 1:5)), "density")
  expect_error(add_circular_density(data.frame(density = 1:5)), "theta")
})

test_that("add_circular_density accepts external (Bayesian-style) density", {
  library(ggplot2)
  theta_grid  <- seq(-pi, pi, length.out = 360)
  # Simulate a Bayesian posterior predictive density: mixture of two von Mises
  external_d  <- 0.6 * exp(3 * cos(theta_grid - 0.3)) +
                 0.4 * exp(2 * cos(theta_grid + 1.2))
  ext_df <- data.frame(theta = theta_grid, density = external_d)
  layers <- add_circular_density(ext_df, fill = "tomato", alpha = 0.3)
  p <- ggplot() + coord_fixed() + layers
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot_build(p))
})

# ---- add_heading_density (convenience wrapper) ------------------------------

test_that("add_heading_density vonmises returns ggplot layers", {
  library(ggplot2)
  set.seed(8)
  hd <- data.frame(heading = rnorm(40, 0.3, 0.5))
  layers <- add_heading_density(hd, method = "vonmises")
  expect_type(layers, "list")
  expect_s3_class(layers[[length(layers)]], "LayerInstance")
})

test_that("add_heading_density kernel returns ggplot layers", {
  library(ggplot2)
  set.seed(9)
  hd <- data.frame(heading = rnorm(40, 0.3, 0.5))
  layers <- add_heading_density(hd, method = "kernel")
  expect_type(layers, "list")
  expect_s3_class(layers[[length(layers)]], "LayerInstance")
})

test_that("add_heading_density histogram returns ggplot layers", {
  library(ggplot2)
  set.seed(10)
  hd <- data.frame(heading = rnorm(40, 0.3, 0.5))
  layers <- add_heading_density(hd, method = "histogram", bins = 24)
  expect_type(layers, "list")
  expect_s3_class(layers[[length(layers)]], "LayerInstance")
})

test_that("add_heading_density result matches compute + add in sequence", {
  library(ggplot2)
  set.seed(11)
  hd <- data.frame(heading = rnorm(50, -0.5, 0.6))
  # Convenience wrapper
  p_conv <- ggplot() + coord_fixed() +
    add_heading_density(hd, method = "vonmises", scale = 0.3)
  # Two-step
  dens_df <- compute_circular_density(hd, method = "vonmises")
  p_step  <- ggplot() + coord_fixed() +
    add_circular_density(dens_df, scale = 0.3)
  # Both should build without error
  expect_silent(ggplot_build(p_conv))
  expect_silent(ggplot_build(p_step))
})

test_that("add_heading_density works inside radiate context", {
  library(ggplot2)
  set.seed(12)
  sim <- suppressWarnings(
    simulate_tracks(conditions = data.frame(n_trials = 10L), n_points = 30, seed = 12)
  )
  hd <- suppressWarnings(
    derive_headings(
      TrajSet(sim, id = "trial_id", time = "frame",
              angle = "rel_theta", x = "rel_x", y = "rel_y",
              angle_unit = "radians", normalize_xy = FALSE),
      rule = "crossing", circ0 = 0.2, circ1 = 0.4
    )
  )
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id", show_arrow = FALSE, show_labels = FALSE) +
    add_heading_density(hd, method = "vonmises")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot_build(p))
})

# ---- bootstrap CI band -------------------------------------------------------

test_that("compute_circular_density boot_reps adds density_lower/upper columns", {
  set.seed(42)
  hd <- data.frame(heading = rnorm(50, 0.5, 0.4))
  d  <- compute_circular_density(hd, method = "vonmises", boot_reps = 99L,
                                 boot_alpha = 0.05, n_theta = 100L)
  expect_true(all(c("density_lower", "density_upper") %in% names(d)))
  expect_true(all(d$density_lower <= d$density + 1e-8))
  expect_true(all(d$density_upper >= d$density - 1e-8))
})

test_that("compute_circular_density without boot_reps has no CI columns", {
  set.seed(1)
  hd <- data.frame(heading = rnorm(30, 0, 0.5))
  d  <- compute_circular_density(hd, method = "vonmises")
  expect_false("density_lower" %in% names(d))
  expect_false("density_upper" %in% names(d))
})

test_that("add_circular_density CI band adds an extra polygon layer", {
  library(ggplot2)
  set.seed(43)
  hd <- data.frame(heading = rnorm(50, 0.3, 0.5))
  dens_no_ci <- compute_circular_density(hd, n_theta = 100L)
  dens_ci    <- compute_circular_density(hd, boot_reps = 99L, n_theta = 100L)
  layers_no_ci <- add_circular_density(dens_no_ci)
  layers_ci    <- add_circular_density(dens_ci)
  expect_equal(length(layers_ci), length(layers_no_ci) + 1L)
})

test_that("add_circular_density CI band polygon stays outside unit circle", {
  library(ggplot2)
  set.seed(44)
  hd      <- data.frame(heading = rnorm(60, 0, 0.6))
  dens_ci <- compute_circular_density(hd, boot_reps = 99L, n_theta = 100L)
  layers  <- add_circular_density(dens_ci, scale = 0.4)
  p       <- ggplot() + coord_fixed() + layers
  built   <- ggplot_build(p)
  # CI band is first layer; all its radii should be >= 1
  ci_d  <- built$data[[1]]
  r_vals <- sqrt(ci_d$x^2 + ci_d$y^2)
  expect_true(all(r_vals >= 1 - 1e-6))
})

test_that("add_heading_density boot_reps convenience path renders without error", {
  library(ggplot2)
  set.seed(45)
  hd <- data.frame(heading = rnorm(50, 0.5, 0.4))
  p  <- ggplot() + coord_fixed() +
    add_heading_density(hd, boot_reps = 99L, fill = "grey80", ci_fill = "grey60")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot_build(p))
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

# ---- circ_mean_segments clock convention auto-detect -------------------------

test_that("circ_mean_segments with clock attr draws arrow pointing North for mean_dir=0", {
  # mean_dir = 0 in clock convention = North = (0, 1) in Cartesian
  stats_df <- data.frame(mean_dir = 0, resultant_R = 1)
  attr(stats_df, "angle_convention") <- "clock"
  seg <- circ_mean_segments(stats_df)
  expect_equal(seg$xend, 0, tolerance = 1e-10)
  expect_equal(seg$yend, 1, tolerance = 1e-10)
})

test_that("circ_mean_segments with clock attr draws arrow pointing East for mean_dir=pi/2", {
  # mean_dir = pi/2 in absolute clock = East = (1, 0) in Cartesian
  stats_df <- data.frame(mean_dir = pi / 2, resultant_R = 1)
  attr(stats_df, "angle_convention") <- "clock"
  seg <- circ_mean_segments(stats_df)
  expect_equal(seg$xend, 1, tolerance = 1e-10)
  expect_equal(seg$yend, 0, tolerance = 1e-10)
})

test_that("circ_mean_segments without clock attr is unchanged", {
  # Without clock attr, mean_dir is treated as unit-circle
  stats_df <- data.frame(mean_dir = pi / 3, resultant_R = 0.7)
  seg <- circ_mean_segments(stats_df)
  expect_equal(seg$xend, 0.7 * cos(pi / 3), tolerance = 1e-10)
  expect_equal(seg$yend, 0.7 * sin(pi / 3), tolerance = 1e-10)
})

# ---- compute_circ_interval ---------------------------------------------------

test_that("compute_circ_interval stat='sd' arc width equals 2 * circular SD", {
  hd <- data.frame(heading = c(-0.1, 0.0, 0.1, -0.05, 0.05))
  iv <- compute_circ_interval(hd, stat = "sd")
  expect_true(all(c("mean_dir", "lower", "upper", "wraps") %in% names(iv)))
  expect_equal(nrow(iv), 1L)
  circ   <- circular::circular(hd$heading, units = "radians", modulo = "2pi")
  sd_val <- as.numeric(circular::sd.circular(circ))
  expect_equal(iv$upper - iv$lower, 2 * sd_val, tolerance = 1e-8)
  expect_false(iv$wraps)
})

test_that("compute_circ_interval sd mean_dir matches circular::mean.circular", {
  hd   <- data.frame(heading = c(0.2, 0.3, 0.25, 0.1, 0.4))
  iv   <- compute_circ_interval(hd, stat = "sd")
  circ <- circular::circular(hd$heading, units = "radians", modulo = "2pi")
  expected <- atan2(sin(as.numeric(circular::mean.circular(circ))),
                    cos(as.numeric(circular::mean.circular(circ))))
  expect_equal(iv$mean_dir, expected, tolerance = 1e-8)
})

test_that("compute_circ_interval wider spread gives wider sd arc", {
  tight <- data.frame(heading = c(-0.05, 0.0, 0.05))
  wide  <- data.frame(heading = c(-1.0, 0.0, 1.0))
  iv_tight <- compute_circ_interval(tight, stat = "sd")
  iv_wide  <- compute_circ_interval(wide,  stat = "sd")
  expect_true((iv_wide$upper - iv_wide$lower) > (iv_tight$upper - iv_tight$lower))
})

test_that("compute_circ_interval stat='bootstrap_ci' returns finite bounds", {
  set.seed(42)
  hd <- data.frame(heading = c(0.2, 0.3, 0.1, 0.4, 0.25, 0.15))
  iv <- compute_circ_interval(hd, stat = "bootstrap_ci", boot_reps = 99L)
  expect_equal(nrow(iv), 1L)
  expect_true(all(c("mean_dir", "lower", "upper", "wraps") %in% names(iv)))
  expect_true(is.finite(iv$lower))
  expect_true(is.finite(iv$upper))
  expect_true(is.finite(iv$mean_dir))
})

test_that("compute_circ_interval returns NA bounds for n < 3", {
  iv2 <- compute_circ_interval(data.frame(heading = c(0.1, 0.2)), stat = "sd")
  expect_true(is.na(iv2$lower))
  expect_true(is.na(iv2$upper))
  iv0 <- compute_circ_interval(data.frame(heading = numeric(0)), stat = "sd")
  expect_true(is.na(iv0$lower))
  expect_true(is.na(iv0$upper))
})

test_that("compute_circ_interval colour_col returns one row per group", {
  hd <- data.frame(
    heading = c(0.1, 0.2, 0.15, 1.0, 1.1, 1.05),
    grp     = rep(c("A", "B"), each = 3)
  )
  iv <- compute_circ_interval(hd, colour_col = "grp", stat = "sd")
  expect_equal(nrow(iv), 2L)
  expect_true("grp" %in% names(iv))
  expect_equal(sort(iv$grp), c("A", "B"))
})
