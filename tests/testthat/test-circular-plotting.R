# ---- circ_display and transform helpers -------------------------------------

test_that("circ_display() returns a list with correct defaults", {
  d <- circ_display()
  expect_equal(d$zero,      pi / 2)
  expect_true(d$clockwise)
  expect_equal(d$units, "degrees")
  expect_s3_class(d, "circ_display")
})

test_that("circ_display() accepts custom args", {
  d <- circ_display(zero = 0, clockwise = FALSE, units = "radians")
  expect_equal(d$zero, 0)
  expect_false(d$clockwise)
  expect_equal(d$units, "radians")
})

test_that(".uc_to_display_coords: default (zero=pi/2, CW) is identity", {
  r <- .uc_to_display_coords(1, 0, circ_display())
  expect_equal(r$x, 1, tolerance = 1e-9)
  expect_equal(r$y, 0, tolerance = 1e-9)
  r2 <- .uc_to_display_coords(0, 1, circ_display())
  expect_equal(r2$x, 0, tolerance = 1e-9)
  expect_equal(r2$y, 1, tolerance = 1e-9)
})

test_that(".uc_to_display_coords: zero=0 rotates East to top (90 CCW)", {
  d <- circ_display(zero = 0)
  r <- .uc_to_display_coords(1, 0, d)
  expect_equal(r$x, 0, tolerance = 1e-9)
  expect_equal(r$y, 1, tolerance = 1e-9)
  r2 <- .uc_to_display_coords(0, -1, d)
  expect_equal(r2$x, 1, tolerance = 1e-9)
  expect_equal(r2$y, 0, tolerance = 1e-9)
})

test_that(".uc_angle_to_display: default clock degrees", {
  d <- circ_display()
  expect_equal(.uc_angle_to_display(pi / 2, d), 0,   tolerance = 1e-9)
  expect_equal(.uc_angle_to_display(0,      d), 90,  tolerance = 1e-9)
  expect_equal(.uc_angle_to_display(3 * pi / 2, d), 180, tolerance = 1e-9)
})

test_that(".uc_angle_to_display: zero=0 clockwise degrees", {
  d <- circ_display(zero = 0)
  expect_equal(.uc_angle_to_display(0, d), 0, tolerance = 1e-9)
  expect_equal(.uc_angle_to_display(3 * pi / 2, d), 90, tolerance = 1e-9)
})

test_that(".uc_angle_to_display: radians units", {
  d <- circ_display(units = "radians")
  expect_equal(.uc_angle_to_display(pi / 2, d), 0, tolerance = 1e-9)
  expect_equal(.uc_angle_to_display(0, d), pi / 2, tolerance = 1e-9)
})

# ---- heading layers ----------------------------------------------------------

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

test_that("add_heading_vectors maps colour when colour_col is supplied", {
  hd <- data.frame(id = c("A", "B"), time = 1, heading = c(pi / 4, pi / 2),
                   x_inner = c(0.14, 0.0), y_inner = c(0.14, 0.2),
                   grp = c("a", "b"))
  expect_true("colour" %in% names(add_heading_vectors(hd, colour_col = "grp")$mapping))
  # no colour_col -> fixed colour, no colour aesthetic
  expect_false("colour" %in% names(add_heading_vectors(hd)$mapping))
})

test_that("radiate(arrow_colour_col=) draws one coloured arrow per group", {
  ts <- simulate_tracks(n_points = 30, seed = 1, output = "trajset")
  n_cond <- length(unique(as.data.frame(ts)$condition))

  p <- radiate(ts, group_col = "trial_id", show_arrow = TRUE,
               arrow_colour_col = "condition")
  seg <- Find(function(l) inherits(l$geom, "GeomSegment") &&
                !is.null(l$geom_params$arrow), p$layers)
  expect_false(is.null(seg))
  expect_true("colour" %in% names(seg$mapping))      # arrow colour mapped to group
  expect_equal(nrow(seg$data), n_cond)               # one arrow per condition
  expect_true("condition" %in% names(seg$data))      # group value carried for the scale
})

test_that("radiate without arrow_colour_col keeps a single fixed-colour arrow", {
  ts <- simulate_tracks(n_points = 30, seed = 1, output = "trajset")
  p <- radiate(ts, group_col = "trial_id", show_arrow = TRUE)
  seg <- Find(function(l) inherits(l$geom, "GeomSegment") &&
                !is.null(l$geom_params$arrow), p$layers)
  expect_false(is.null(seg))
  expect_false("colour" %in% names(seg$mapping))     # no colour aesthetic
  expect_equal(nrow(seg$data), 1L)                   # single summary arrow
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

test_that("radiate arrow length equals rho of per-trial headings via arrow_angle_col", {
  # The Shiny app drives the directedness arrow off the chosen heading method by
  # broadcasting each trial's heading onto every frame and passing the column as
  # `arrow_angle_col`. radiate takes the per-trial mean (a no-op for a constant
  # column) then the resultant, so the arrow length must equal rho.circular of
  # those per-trial headings -- matching the summary table's R. Guards the arrow
  # against silently diverging from the headings it is supposed to summarise.
  library(ggplot2)
  sim <- simulate_tracks(conditions = data.frame(n_trials = 8L),
                         n_points = 15, seed = 11, output = "trajset")
  idc <- sim@cols$id
  d   <- sim@data
  ids <- unique(d[[idc]])
  set.seed(1)
  headings <- stats::runif(length(ids), -pi, pi)   # known per-trial headings
  names(headings) <- as.character(ids)
  d[[".h"]] <- headings[as.character(d[[idc]])]
  sim@data  <- d

  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = idc, show_arrow = TRUE,
               arrow_angle_col = ".h", show_labels = FALSE)
  built <- ggplot_build(p)

  # The arrow is the only geom_segment whose base sits at the origin.
  seg <- NULL
  for (dd in built$data) {
    if (all(c("x", "y", "xend", "yend") %in% names(dd))) {
      base0 <- dd[abs(dd$x) < 1e-9 & abs(dd$y) < 1e-9, , drop = FALSE]
      if (nrow(base0) == 1L) { seg <- base0; break }
    }
  }
  expect_false(is.null(seg))

  arrow_len <- sqrt(seg$xend^2 + seg$yend^2)
  R <- as.numeric(circular::rho.circular(
    circular::circular(headings, units = "radians", type = "angles")))
  expect_equal(arrow_len, R, tolerance = 1e-6)
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

test_that("add_heading_points: default display puts UC North at top", {
  library(ggplot2)
  hd  <- data.frame(heading = pi / 2)   # UC North
  pts <- ggplot_build(ggplot() + add_heading_points(hd))$data[[1]]
  # default circ_display(zero=pi/2): identity transform -> UC North (0,1) stays (0,1)
  expect_equal(pts$x, 0, tolerance = 1e-6)
  expect_equal(pts$y, 1, tolerance = 1e-6)
})

test_that("add_heading_points: display zero=0 puts UC East at top", {
  library(ggplot2)
  hd <- data.frame(heading = 0)  # UC East = toward stimulus
  attr(hd, "display") <- circ_display(zero = 0)
  pts <- ggplot_build(ggplot() + add_heading_points(hd))$data[[1]]
  expect_equal(pts$x, 0, tolerance = 1e-6)
  expect_equal(pts$y, 1, tolerance = 1e-6)
})

test_that("add_heading_vectors: default display, endpoint at (cos(h), sin(h))", {
  library(ggplot2)
  hd  <- data.frame(heading = pi / 4, x_inner = 0, y_inner = 0)
  seg <- ggplot_build(ggplot() + add_heading_vectors(hd))$data[[1]]
  # zero=pi/2 -> identity -> endpoint = (cos(pi/4), sin(pi/4))
  expect_equal(seg$xend, cos(pi / 4), tolerance = 1e-6)
  expect_equal(seg$yend, sin(pi / 4), tolerance = 1e-6)
})

test_that("add_heading_vectors: display zero=0 rotates endpoint", {
  library(ggplot2)
  hd  <- data.frame(heading = 0, x_inner = 0, y_inner = 0)
  attr(hd, "display") <- circ_display(zero = 0)
  seg <- ggplot_build(ggplot() + add_heading_vectors(hd))$data[[1]]
  # UC East (1,0) after 90 CCW -> (0,1)
  expect_equal(seg$xend, 0, tolerance = 1e-6)
  expect_equal(seg$yend, 1, tolerance = 1e-6)
})

test_that("add_ticks: n controls count, length the reach, colour/linewidth applied", {
  library(ggplot2)
  d <- ggplot_build(ggplot() + add_ticks(n = 12, length = 0.2,
                                          colour = "red", linewidth = 2))$data[[1]]
  expect_equal(nrow(d), 12L)
  r_in  <- sqrt(d$x^2 + d$y^2)
  r_out <- sqrt(d$xend^2 + d$yend^2)
  expect_equal(unique(round(r_out - r_in, 6)), 0.2)   # each tick spans `length`
  expect_equal(d$colour[1], "red")
  expect_equal(d$linewidth[1], 2)
})

test_that("add_ticks default reproduces 8 ticks", {
  library(ggplot2)
  d <- ggplot_build(ggplot() + add_ticks())$data[[1]]
  expect_equal(nrow(d), 8L)
})

test_that("plotting helpers return ggplot layers", {
  ticks <- add_ticks()
  expect_s3_class(ticks, "LayerInstance")

  circ <- add_circ(radius = 0.5)
  expect_type(circ, "list")
  expect_s3_class(circ[[1]], "LayerInstance")

  labs <- degree_labs()
  expect_length(labs, 4)
  expect_true(any(grepl("45", sapply(labs, function(l) l$aes_params$label))))

  labs_rad <- degree_labs(display = circ_display(units = "radians"))
  labels_r <- sapply(labs_rad, function(l) l$aes_params$label)
  expect_true(any(grepl("π", labels_r)))   # pi fractions, e.g. "pi/4"

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
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y", angle = "time", angle_unit = "radians")
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
  ts <- tracks(traj_df, id = "id", time = "time", x = "x", y = "y", angle = "angle", angle_unit = "radians",
                normalize_xy = FALSE)
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

test_that("assign_cycle_colours handles NA panel values without error", {
  df <- data.frame(id = paste0("T", 1:6),
                   panel = c("a", "a", NA, NA, "b", "b"))
  out <- assign_cycle_colours(df, id_col = "id", n = 10L, panel_col = "panel")
  expect_s3_class(out$cycle_colour, "factor")
  expect_equal(nrow(out), 6L)
  # NA-panel rows form their own group: T3, T4 -> indices 1, 2
  na_rows <- as.integer(out$cycle_colour[is.na(df$panel)])
  expect_equal(na_rows, c(1L, 2L))
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
      tracks(sim, id = "trial_id", time = "frame",
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
  expect_false(iv_tight$wraps)
  expect_false(iv_wide$wraps)
  expect_true((iv_wide$upper - iv_wide$lower) > (iv_tight$upper - iv_tight$lower))
})

test_that("compute_circ_interval sd wraps=TRUE for distribution centred near pi", {
  # Angles tightly clustered around pi/-pi boundary
  hd <- data.frame(heading = c(pi - 0.1, pi, pi + 0.1, -(pi - 0.1), -pi))
  iv <- compute_circ_interval(hd, stat = "sd")
  # mean_dir is near pi/-pi; after atan2 normalisation lower > upper
  expect_true(iv$wraps)
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

# ---- add_circ_interval -------------------------------------------------------

test_that("add_circ_interval returns a geom_path LayerInstance", {
  iv    <- data.frame(mean_dir = 0.3, lower = 0.1, upper = 0.5, wraps = FALSE)
  layer <- add_circ_interval(iv)
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_circ_interval arc points lie at the specified radius", {
  library(ggplot2)
  iv     <- data.frame(mean_dir = 0.0, lower = -0.4, upper = 0.4, wraps = FALSE)
  p      <- ggplot() + coord_fixed() + add_circ_interval(iv, radius = 1.1, n_theta = 200L)
  built  <- ggplot_build(p)
  r_vals <- sqrt(built$data[[1]]$x^2 + built$data[[1]]$y^2)
  expect_true(all(abs(r_vals - 1.1) < 1e-4))
})

test_that("add_circ_interval silently skips NA rows without error", {
  library(ggplot2)
  iv <- data.frame(mean_dir = 0.3, lower = NA_real_, upper = NA_real_, wraps = FALSE)
  expect_silent(layer <- add_circ_interval(iv))
  expect_silent(ggplot_build(ggplot() + coord_fixed() + layer))
})

test_that("add_circ_interval wrapping arc passes through angle pi", {
  library(ggplot2)
  # lower=2.8, upper=-2.8: arc wraps through +/-pi; seq(2.8, -2.8+2pi)=[2.8,3.48]
  iv    <- data.frame(mean_dir = pi, lower = 2.8, upper = -2.8, wraps = TRUE)
  p     <- ggplot() + coord_fixed() +
    add_circ_interval(iv, radius = 1.05, n_theta = 500L)
  built  <- ggplot_build(p)
  pts    <- built$data[[1]]
  r_vals <- sqrt(pts$x^2 + pts$y^2)
  expect_true(all(abs(r_vals - 1.05) < 1e-4))
  # Arc passes through angle pi => x near -1.05
  expect_true(any(pts$x < -1.0))
})

test_that("add_circ_interval with colour_col draws per-group arcs", {
  library(ggplot2)
  iv <- data.frame(
    mean_dir = c(0.3, 1.5),
    lower    = c(0.1, 1.3),
    upper    = c(0.5, 1.7),
    wraps    = c(FALSE, FALSE),
    grp      = c("A", "B")
  )
  layer <- add_circ_interval(iv, colour_col = "grp")
  built <- ggplot_build(ggplot() + coord_fixed() + layer)
  expect_true(length(unique(built$data[[1]]$group)) >= 2L)
})

test_that("add_circ_interval wraps correctly when wraps column is absent", {
  library(ggplot2)
  # No wraps column — function should detect lower > upper and take the long arc
  iv <- data.frame(mean_dir = pi, lower = 2.8, upper = -2.8)
  p  <- ggplot() + coord_fixed() +
    add_circ_interval(iv, radius = 1.05, n_theta = 500L)
  built  <- ggplot_build(p)
  pts    <- built$data[[1]]
  # Arc passes through angle pi => x near -1.05
  expect_true(any(pts$x < -1.0))
})


# ---- add_heading_interval ----------------------------------------------------

test_that("add_heading_interval returns a LayerInstance", {
  hd    <- data.frame(heading = c(0.1, 0.2, 0.15, 0.05, 0.12))
  layer <- add_heading_interval(hd, stat = "sd")
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_heading_interval sd matches compute_circ_interval + add_circ_interval", {
  library(ggplot2)
  hd <- data.frame(heading = c(0.1, 0.2, 0.15, 0.05, 0.25, 0.3))
  p_conv <- ggplot() + coord_fixed() + add_heading_interval(hd, stat = "sd")
  iv     <- compute_circ_interval(hd, stat = "sd")
  p_step <- ggplot() + coord_fixed() + add_circ_interval(iv)
  b_conv <- ggplot_build(p_conv)
  b_step <- ggplot_build(p_step)
  expect_equal(b_conv$data[[1]]$x, b_step$data[[1]]$x, tolerance = 1e-8)
  expect_equal(b_conv$data[[1]]$y, b_step$data[[1]]$y, tolerance = 1e-8)
})

test_that("add_heading_interval can be added to a radiate plot without error", {
  library(ggplot2)
  sim <- simulate_tracks(conditions = data.frame(n_trials = 5L), n_points = 20, seed = 1)
  hd  <- suppressWarnings(
    derive_headings(
      tracks(sim, id = "trial_id", time = "frame",
              angle = "rel_theta", x = "rel_x", y = "rel_y",
              angle_unit = "radians", normalize_xy = FALSE),
      rule = "crossing", circ0 = 0.2, circ1 = 0.4
    )
  )
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id",
               show_arrow = FALSE, show_labels = FALSE) +
    add_heading_interval(hd, stat = "sd", radius = 1.05)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot_build(p))
})

# ---- compute_circ_mean -------------------------------------------------------

test_that("compute_circ_mean returns correct mean_dir and R for UC angles", {
  hd <- data.frame(heading = c(0, 0, 0, 0, 0))
  result <- compute_circ_mean(hd)
  expect_equal(result$mean_dir,    0, tolerance = 1e-10)
  expect_equal(result$resultant_R, 1, tolerance = 1e-10)
})

test_that("compute_circ_mean grouped output has one row per group", {
  hd <- data.frame(heading = c(0, 0, pi, pi),
                   grp     = c("A", "A", "B", "B"))
  result <- compute_circ_mean(hd, colour_col = "grp")
  expect_equal(nrow(result), 2L)
  expect_true("grp" %in% names(result))
})

test_that("compute_circ_mean returns NA for fewer than 2 finite angles", {
  hd <- data.frame(heading = c(0.5))
  result <- compute_circ_mean(hd)
  expect_true(is.na(result$mean_dir))
  expect_true(is.na(result$resultant_R))
})

test_that("compute_circ_mean handles all-NA input without error", {
  hd <- data.frame(heading = c(NA_real_, NA_real_, NA_real_))
  result <- compute_circ_mean(hd)
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$mean_dir))
})

test_that("compute_circ_mean preserves factor levels on colour_col", {
  hd <- data.frame(heading = c(0.1, 0.2, 0.3, 0.4),
                   grp     = factor(c("B", "A", "B", "A"), levels = c("A", "B", "C")))
  result <- compute_circ_mean(hd, colour_col = "grp")
  expect_equal(levels(result$grp), c("A", "B", "C"))
})

test_that("compute_circ_mean carries the input's display attribute onto its output", {
  # add_circ_mean() orients the arrow from attr(summary_df, "display"); if
  # compute_circ_mean() dropped the input display, the arrow would fall back to
  # the default orientation and disagree with the rest of the figure.
  disp <- circ_display(zero = 0)
  hd <- data.frame(heading = c(0, 0, 0, 0))
  attr(hd, "display") <- disp
  result <- compute_circ_mean(hd)
  expect_equal(attr(result, "display", exact = TRUE), disp)
})

# ---- add_circ_mean -----------------------------------------------------------

test_that("add_circ_mean: default display points UC North to top", {
  library(ggplot2)
  sm <- data.frame(mean_dir = pi / 2, resultant_R = 1)
  seg <- ggplot_build(ggplot() + add_circ_mean(sm))$data[[1]]
  # UC North (0,1) with zero=pi/2: identity -> (0,1)
  expect_equal(seg$xend, 0, tolerance = 1e-6)
  expect_equal(seg$yend, 1, tolerance = 1e-6)
})

test_that("add_circ_mean: display zero=0 puts UC East at top", {
  library(ggplot2)
  sm <- data.frame(mean_dir = 0, resultant_R = 1)
  attr(sm, "display") <- circ_display(zero = 0)
  seg <- ggplot_build(ggplot() + add_circ_mean(sm))$data[[1]]
  expect_equal(seg$xend, 0, tolerance = 1e-6)
  expect_equal(seg$yend, 1, tolerance = 1e-6)
})

test_that("add_circ_mean returns a LayerInstance", {
  sm <- data.frame(mean_dir = 0, resultant_R = 0.8)
  expect_s3_class(add_circ_mean(sm), "LayerInstance")
})

test_that("add_circ_mean returns empty layer for all-NA summary", {
  library(ggplot2)
  sm    <- data.frame(mean_dir = NA_real_, resultant_R = NA_real_)
  layer <- add_circ_mean(sm)
  expect_s3_class(layer, "LayerInstance")
  p <- ggplot() + coord_fixed() + layer
  expect_silent(ggplot_build(p))
})

test_that("add_circ_mean maps colour_col as aesthetic when present", {
  library(ggplot2)
  sm <- data.frame(mean_dir = c(0, pi / 2), resultant_R = c(0.8, 0.6),
                   grp = c("A", "B"))
  layer <- add_circ_mean(sm, colour_col = "grp")
  p     <- ggplot() + coord_fixed() + layer
  built <- ggplot_build(p)
  expect_equal(length(unique(built$data[[1]]$colour)), 2L)
})

test_that("add_circ_mean drops NA rows when some but not all rows are NA", {
  library(ggplot2)
  sm <- data.frame(mean_dir = c(0, NA), resultant_R = c(0.8, NA),
                   grp = c("A", "B"))
  layer <- add_circ_mean(sm, colour_col = "grp")
  p     <- ggplot() + coord_fixed() + layer
  built <- ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 1L)
  expect_false(any(is.na(built$data[[1]]$xend)))
})


# ---- add_heading_arrow -------------------------------------------------------

test_that("add_heading_arrow returns a LayerInstance", {
  hd <- data.frame(heading = c(0.1, 0.2, 0.3, 0.4, 0.5))
  expect_s3_class(add_heading_arrow(hd), "LayerInstance")
})

test_that("add_heading_arrow matches two-step compute_circ_mean + add_circ_mean", {
  library(ggplot2)
  hd     <- data.frame(heading = c(0.1, 0.2, 0.15, 0.12, 0.18))
  p_wrap <- ggplot() + coord_fixed() + add_heading_arrow(hd)
  sm     <- compute_circ_mean(hd)
  p_step <- ggplot() + coord_fixed() + add_circ_mean(sm)
  bw <- ggplot_build(p_wrap)
  bs <- ggplot_build(p_step)
  expect_equal(bw$data[[1]]$x,    bs$data[[1]]$x,    tolerance = 1e-8)
  expect_equal(bw$data[[1]]$xend, bs$data[[1]]$xend, tolerance = 1e-8)
  expect_equal(bw$data[[1]]$y,    bs$data[[1]]$y,    tolerance = 1e-8)
  expect_equal(bw$data[[1]]$yend, bs$data[[1]]$yend, tolerance = 1e-8)
})

test_that("add_heading_arrow integrates with radiate() without error", {
  library(ggplot2)
  sim <- simulate_tracks(conditions = data.frame(n_trials = 5L), n_points = 20, seed = 1)
  hd  <- suppressWarnings(
    derive_headings(
      tracks(sim, id = "trial_id", time = "frame",
              angle = "rel_theta", x = "rel_x", y = "rel_y",
              angle_unit = "radians", normalize_xy = FALSE),
      rule = "crossing", circ0 = 0.2, circ1 = 0.4
    )
  )
  p <- radiate(sim, x_col = "rel_x", y_col = "rel_y",
               group_col = "trial_id",
               show_arrow = FALSE, show_labels = FALSE) +
    add_heading_arrow(hd)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot_build(p))
})

# ---- clock display convention: .to_clock_display helper ---------------------

test_that(".to_clock_display maps (1,0) to (0,1)", {
  r <- .to_clock_display(1, 0)
  expect_equal(r$x, 0)
  expect_equal(r$y, 1)
})

test_that(".to_clock_display maps (0,1) to (-1,0)", {
  r <- .to_clock_display(0, 1)
  expect_equal(r$x, -1)
  expect_equal(r$y,  0)
})

test_that(".to_clock_display maps (-1,0) to (0,-1)", {
  r <- .to_clock_display(-1, 0)
  expect_equal(r$x,  0)
  expect_equal(r$y, -1)
})

test_that(".to_clock_display maps (0,-1) to (1,0)", {
  r <- .to_clock_display(0, -1)
  expect_equal(r$x, 1)
  expect_equal(r$y, 0)
})


# ---- radiate() display param ------------------------------------------------

test_that("radiate applies display transform to track coordinates", {
  library(ggplot2)
  # A single track heading East (x increases) in UC.
  # With default circ_display (zero=pi/2, identity), the track stays East.
  df <- data.frame(trial_id = "A", frame = 1:3,
                   x = c(0, 0.5, 1), y = c(0, 0, 0))
  ts <- tracks(df, id = "trial_id", time = "frame", x = "x", y = "y",
                normalize_xy = FALSE)
  p     <- radiate(ts, group_col = "trial_id", show_labels = FALSE,
                   show_arrow = FALSE, grid = "cartesian")
  built <- ggplot_build(p)
  path_layer <- Filter(function(d) "x" %in% names(d) && "y" %in% names(d),
                       built$data)[[1]]
  expect_equal(path_layer$x[path_layer$x > 0.4][1], 0.5, tolerance = 1e-3)
})

test_that("radiate with display zero=0 rotates East track to North on canvas", {
  library(ggplot2)
  df <- data.frame(trial_id = "A", frame = 1:2,
                   x = c(0, 1), y = c(0, 0))  # eastward track
  ts <- tracks(df, id = "trial_id", time = "frame", x = "x", y = "y",
                normalize_xy = FALSE)
  d  <- circ_display(zero = 0)
  p  <- radiate(ts, group_col = "trial_id", show_labels = FALSE,
                show_arrow = FALSE, display = d, grid = "cartesian")
  built <- ggplot_build(p)
  path_layer <- Filter(function(g) "x" %in% names(g) && "y" %in% names(g),
                       built$data)[[1]]
  # East (1,0) rotated 90 CCW -> should appear at x~0, y~1
  last <- path_layer[nrow(path_layer), ]
  expect_equal(last$x, 0, tolerance = 1e-3)
  expect_equal(last$y, 1, tolerance = 1e-3)
})

# ---- add_vonmises_density ----------------------------------------------------

test_that("add_vonmises_density returns a ggplot2 layer", {
  fit <- data.frame(mu = 0, kappa = 3, n = 30L)
  layer <- add_vonmises_density(fit)
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_vonmises_density polygon data has x, y, .vm_grp columns", {
  fit <- data.frame(mu = pi / 4, kappa = 3, n = 30L)
  layer <- add_vonmises_density(fit, n_pts = 36L)
  expect_true(all(c("x", "y", ".vm_grp") %in% names(layer$data)))
  expect_equal(nrow(layer$data), 36L)
})

test_that("add_vonmises_density peak radius equals scale", {
  # Use a synthetic fit with known kappa to avoid degenerate identical angles
  fit   <- data.frame(mu = 0, kappa = 3, n = 50L)
  layer <- add_vonmises_density(fit, scale = 0.3, n_pts = 360L)
  r     <- sqrt(layer$data$x^2 + layer$data$y^2)
  expect_equal(max(r), 0.3, tolerance = 1e-3)
})

test_that("add_vonmises_density peak is near mu", {
  fit   <- data.frame(mu = pi / 2, kappa = 4, n = 50L)
  layer <- add_vonmises_density(fit, n_pts = 360L)
  idx   <- which.max(sqrt(layer$data$x^2 + layer$data$y^2))
  peak_angle <- atan2(layer$data$y[idx], layer$data$x[idx])
  expect_equal(peak_angle, pi / 2, tolerance = 0.05)
})

test_that("add_vonmises_density returns NULL for NA fit", {
  fit <- data.frame(mu = NA_real_, kappa = NA_real_, n = 1L)
  expect_null(add_vonmises_density(fit))
})

test_that("add_vonmises_density handles group_col for faceting", {
  fit <- data.frame(id = c("A", "B"),
                    mu = c(0, pi / 2), kappa = c(3, 3), n = c(30L, 30L))
  layer <- add_vonmises_density(fit, group_col = "id", n_pts = 36L)
  expect_true("id" %in% names(layer$data))
  expect_equal(length(unique(layer$data$id)), 2L)
})

test_that("add_vonmises_density(axial = TRUE) draws a point-symmetric two-peak curve", {
  fit <- data.frame(mu = 0.3, kappa = 4)
  lyr <- add_vonmises_density(fit, axial = TRUE)
  d   <- lyr$data
  key  <- function(m) apply(round(m, 4), 1L, paste, collapse = ",")
  expect_true(all(key(cbind(-d$x, -d$y)) %in% key(cbind(d$x, d$y))))
})

test_that("add_vonmises_density(axial = FALSE) is unchanged (single peak)", {
  fit <- data.frame(mu = 0.3, kappa = 4)
  d_def <- add_vonmises_density(fit)$data
  d_dir <- add_vonmises_density(fit, axial = FALSE)$data
  expect_equal(d_def, d_dir)
  key <- function(m) apply(round(m, 4), 1L, paste, collapse = ",")
  expect_false(all(key(cbind(-d_dir$x, -d_dir$y)) %in% key(cbind(d_dir$x, d_dir$y))))
})

# ---- add_circular_kde --------------------------------------------------------

test_that("add_circular_kde returns a ggplot2 layer", {
  set.seed(1)
  hd <- data.frame(heading = rnorm(40, 0, 0.4))
  layer <- add_circular_kde(hd)
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_circular_kde polygon has x, y, .kde_grp columns", {
  set.seed(1)
  hd    <- data.frame(heading = rnorm(40, 0, 0.4))
  layer <- add_circular_kde(hd, n_pts = 64L)
  expect_true(all(c("x", "y", ".kde_grp") %in% names(layer$data)))
  expect_equal(nrow(layer$data), 64L)
})

test_that("add_circular_kde peak radius equals scale", {
  set.seed(1)
  hd    <- data.frame(heading = rnorm(60, pi / 3, 0.3))
  layer <- add_circular_kde(hd, scale = 0.35, n_pts = 512L)
  r     <- sqrt(layer$data$x^2 + layer$data$y^2)
  expect_equal(max(r), 0.35, tolerance = 1e-3)
})

test_that("add_circular_kde peak is in the correct direction", {
  set.seed(1)
  hd    <- data.frame(heading = rnorm(80, 0, 0.3))
  layer <- add_circular_kde(hd, n_pts = 512L)
  idx   <- which.max(sqrt(layer$data$x^2 + layer$data$y^2))
  peak  <- atan2(layer$data$y[idx], layer$data$x[idx])
  expect_equal(peak, 0, tolerance = 0.1)
})

test_that("add_circular_kde handles group_col for faceting", {
  set.seed(1)
  hd <- data.frame(
    grp     = rep(c("A","B"), each = 40),
    heading = c(rnorm(40, 0, 0.3), rnorm(40, pi, 0.3))
  )
  layer <- add_circular_kde(hd, group_col = "grp", n_pts = 64L)
  expect_true("grp" %in% names(layer$data))
  expect_equal(length(unique(layer$data$grp)), 2L)
})

test_that("add_circular_kde returns NULL for fewer than 2 observations", {
  hd <- data.frame(heading = 0.5)
  expect_null(add_circular_kde(hd))
})

# ---- add_wrappedcauchy_density -----------------------------------------------

test_that("add_wrappedcauchy_density returns a ggplot2 layer", {
  fit <- data.frame(mu = 0, rho = 0.6, convergence = 0L, n = 50L)
  layer <- add_wrappedcauchy_density(fit)
  expect_s3_class(layer, "LayerInstance")
})

test_that("add_wrappedcauchy_density peak radius equals scale", {
  fit   <- data.frame(mu = 0, rho = 0.6, convergence = 0L, n = 50L)
  layer <- add_wrappedcauchy_density(fit, scale = 0.35, n_pts = 360L)
  r     <- sqrt(layer$data$x^2 + layer$data$y^2)
  expect_equal(max(r), 0.35, tolerance = 1e-3)
})

test_that("add_wrappedcauchy_density peak is near mu", {
  fit   <- data.frame(mu = pi / 3, rho = 0.7, convergence = 0L, n = 50L)
  layer <- add_wrappedcauchy_density(fit, n_pts = 360L)
  idx   <- which.max(sqrt(layer$data$x^2 + layer$data$y^2))
  peak  <- atan2(layer$data$y[idx], layer$data$x[idx])
  expect_equal(peak, pi / 3, tolerance = 0.05)
})

test_that("add_wrappedcauchy_density returns NULL for NA fit", {
  fit <- data.frame(mu = NA_real_, rho = NA_real_, n = 1L)
  expect_null(add_wrappedcauchy_density(fit))
})

test_that("add_wrappedcauchy_density handles group_col", {
  fit <- data.frame(grp = c("A", "B"),
                    mu  = c(0, pi/2),
                    rho = c(0.6, 0.5),
                    n   = c(40L, 40L))
  layer <- add_wrappedcauchy_density(fit, group_col = "grp", n_pts = 36L)
  expect_true("grp" %in% names(layer$data))
  expect_equal(length(unique(layer$data$grp)), 2L)
})

# ---- add_critical_r ----------------------------------------------------------

test_that("add_critical_r pooled radius matches Rayleigh formula", {
  set.seed(1)
  hd <- data.frame(heading = rnorm(40, 0, 0.5))
  layer <- add_critical_r(hd)
  r <- sqrt(layer$data$x[1]^2 + layer$data$y[1]^2)
  expect_equal(r, sqrt(-log(0.05) / 40), tolerance = 1e-6)
})

test_that("add_critical_r vtest radius matches formula", {
  hd <- data.frame(heading = rnorm(30, 0, 0.5))
  layer <- add_critical_r(hd, test = "vtest")
  r <- sqrt(layer$data$x[1]^2 + layer$data$y[1]^2)
  expect_equal(r, stats::qnorm(0.95) / sqrt(2 * 30), tolerance = 1e-6)
})

test_that("add_critical_r conservative uses smallest n", {
  hd <- data.frame(
    grp     = rep(c("A", "B"), times = c(10, 40)),
    heading = rnorm(50, 0, 0.5)
  )
  layer <- add_critical_r(hd, group_col = "grp", per_group = FALSE)
  r <- sqrt(layer$data$x[1]^2 + layer$data$y[1]^2)
  expect_equal(r, sqrt(-log(0.05) / 10), tolerance = 1e-6)  # n = 10, smaller
})

test_that("add_critical_r per_group draws one circle per group", {
  hd <- data.frame(
    grp     = rep(c("A", "B", "C"), times = c(15, 30, 50)),
    heading = rnorm(95, 0, 0.5)
  )
  layer <- add_critical_r(hd, group_col = "grp", per_group = TRUE)
  expect_true("grp" %in% names(layer$data))
  expect_equal(length(unique(layer$data$grp)), 3L)
  # smaller n -> larger radius
  radii <- tapply(sqrt(layer$data$x^2 + layer$data$y^2),
                  layer$data$grp, function(v) v[1])
  expect_gt(radii[["A"]], radii[["C"]])
})

test_that("add_critical_r colour_by_group = FALSE keeps per-panel circles a fixed colour", {
  hd <- data.frame(
    grp     = rep(c("A", "B", "C"), times = c(15, 30, 50)),
    heading = rnorm(95, 0, 0.5)
  )
  layer <- add_critical_r(hd, group_col = "grp", per_group = TRUE,
                          colour_by_group = FALSE, colour = "firebrick")
  # group column survives so the circles still facet alongside the parent plot
  expect_true("grp" %in% names(layer$data))
  expect_equal(length(unique(layer$data$grp)), 3L)
  # but colour is NOT mapped (fixed), so it cannot collide with the parent scale
  expect_false("colour" %in% names(layer$mapping))
  expect_identical(layer$aes_params$colour, "firebrick")
  # per-panel radii are still distinct (per-group n, not pooled)
  radii <- tapply(sqrt(layer$data$x^2 + layer$data$y^2),
                  layer$data$grp, function(v) v[1])
  expect_gt(radii[["A"]], radii[["C"]])
})

test_that("add_critical_r higher alpha gives smaller circle", {
  hd <- data.frame(heading = rnorm(30, 0, 0.5))
  r05 <- sqrt(add_critical_r(hd, alpha = 0.05)$data$x[1]^2 +
              add_critical_r(hd, alpha = 0.05)$data$y[1]^2)
  r10 <- sqrt(add_critical_r(hd, alpha = 0.10)$data$x[1]^2 +
              add_critical_r(hd, alpha = 0.10)$data$y[1]^2)
  expect_lt(r10, r05)
})

test_that("add_critical_r returns NULL when n < 2", {
  expect_null(add_critical_r(data.frame(heading = 0.5)))
})
# ---- add_critical_v_line -----------------------------------------------------

test_that("add_critical_v_line chord foot is at distance c along mu0", {
  hd <- data.frame(heading = rnorm(50, 0, 0.5))
  layer <- add_critical_v_line(hd, mu0 = 0)
  ch <- layer[[1]]$data
  mid_x <- (ch$x + ch$xend) / 2
  mid_y <- (ch$y + ch$yend) / 2
  c_exp <- stats::qnorm(0.95) / sqrt(2 * 50)
  expect_equal(mid_x, c_exp, tolerance = 1e-6)
  expect_equal(mid_y, 0,     tolerance = 1e-6)
})

test_that("add_critical_v_line endpoints lie on the unit circle", {
  hd <- data.frame(heading = rnorm(40, 0, 0.5))
  ch <- add_critical_v_line(hd, mu0 = pi / 3)[[1]]$data
  expect_equal(sqrt(ch$x^2 + ch$y^2),       1, tolerance = 1e-6)
  expect_equal(sqrt(ch$xend^2 + ch$yend^2), 1, tolerance = 1e-6)
})

test_that("add_critical_v_line mu0 rotates the chord", {
  hd <- data.frame(heading = rnorm(50, 0, 0.5))
  ch <- add_critical_v_line(hd, mu0 = pi / 2)[[1]]$data
  c_exp <- stats::qnorm(0.95) / sqrt(2 * 50)
  expect_equal((ch$x + ch$xend) / 2, 0,     tolerance = 1e-6)
  expect_equal((ch$y + ch$yend) / 2, c_exp, tolerance = 1e-6)
})

test_that("add_critical_v_line show_region adds a polygon layer", {
  hd <- data.frame(heading = rnorm(40, 0, 0.5))
  no_region <- add_critical_v_line(hd, mu0 = 0, show_region = FALSE)
  with_region <- add_critical_v_line(hd, mu0 = 0, show_region = TRUE)
  expect_length(no_region, 1L)
  expect_length(with_region, 2L)
})

test_that("add_critical_v_line per_group: smaller n is further from centre", {
  hd <- data.frame(
    grp     = rep(c("A", "B"), times = c(15, 60)),
    heading = rnorm(75, 0, 0.5)
  )
  ch <- add_critical_v_line(hd, mu0 = 0, group_col = "grp",
                            per_group = TRUE)[[1]]$data
  foot <- tapply(seq_len(nrow(ch)), ch$grp,
                 function(i) (ch$x[i] + ch$xend[i]) / 2)
  expect_gt(foot[["A"]], foot[["B"]])   # n=15 line further out than n=60
})

test_that("add_critical_v_line requires mu0", {
  hd <- data.frame(heading = rnorm(30, 0, 0.5))
  expect_error(add_critical_v_line(hd), "mu0")
})

test_that("add_critical_v_line returns NULL when n < 2", {
  hd <- data.frame(heading = 0.5)  # n = 1, no boundary
  expect_null(add_critical_v_line(hd, mu0 = 0))
})

test_that("add_critical_v_line returns NULL when boundary exceeds unit circle", {
  # tiny alpha pushes c = z / sqrt(2n) beyond 1 for small n
  hd <- data.frame(heading = rnorm(3, 0, 0.5))  # n = 3
  expect_null(add_critical_v_line(hd, mu0 = 0, alpha = 1e-6))
})

# ---- directedness arrow respects the clock display convention ----------------
# Regression: in clock-display mode the tracks are rotated 90 degrees (East ->
# North) but the mean-direction arrow was left in unit-circle coordinates, so
# it pointed ~90 degrees off the trajectories it was meant to summarise.

.arrow_seg <- function(g) {
  for (ly in g$layers) {
    d <- ly$data
    if (is.data.frame(d) && all(c("x", "y", "xend", "yend") %in% names(d)) &&
        nrow(d) == 1L && d$x[1] == 0 && d$y[1] == 0) {
      return(d)
    }
  }
  NULL
}

.arrow_fixture <- function(clock) {
  df <- data.frame(
    id      = rep(c("A", "B"), each = 3),
    time    = rep(1:3, 2),
    trans_x = c(0, 0.5, 1, 0, 0.5, 1),
    trans_y = c(0, 0, 0, 0, 0, 0),
    rel_x   = c(0, 0.5, 1, 0, 0.5, 1),
    rel_y   = c(0, 0, 0, 0, 0, 0),
    angle   = rep(0, 6)            # heading due East (unit circle 0)
  )
  ts <- tracks(df, id = "id", time = "time", x = "trans_x", y = "trans_y",
                rel_x = "rel_x", rel_y = "rel_y", angle = "angle",
                angle_unit = "radians", normalize_xy = FALSE)
  if (clock) {
    ts@meta$plot_x_col <- "rel_x"
    ts@meta$plot_y_col <- "rel_y"
  }
  ts
}

test_that("mean arrow stays in unit-circle frame with default display", {
  g <- radiate(.arrow_fixture(clock = FALSE), group_col = "id",
               show_arrow = TRUE, show_labels = FALSE)
  a <- .arrow_seg(g)
  expect_false(is.null(a))
  # East heading -> default display (zero=pi/2, identity) -> arrow points East (+x)
  expect_gt(a$xend, 0.9)
  expect_lt(abs(a$yend), 1e-6)
})

test_that("mean arrow rotates with display zero=0 (stimulus at top)", {
  g <- radiate(.arrow_fixture(clock = TRUE), group_col = "id",
               show_arrow = TRUE, show_labels = FALSE,
               display = circ_display(zero = 0))
  a <- .arrow_seg(g)
  expect_false(is.null(a))
  # East heading (angle=0) with zero=0 -> 90 CCW rotation -> arrow points North (+y)
  expect_gt(a$yend, 0.9)
  expect_lt(abs(a$xend), 1e-6)
})

test_that(".theme_grid_style exposes major/minor styles, panel fill, and has_grid", {
  s <- radiatR:::.theme_grid_style("grey")
  expect_true(s$has_grid)
  expect_equal(tolower(s$major$colour), "white")   # theme_grey gridlines are white
  expect_true(is.numeric(s$major$linewidth))
  expect_false(is.na(s$fill))                       # grey panel has a fill
  expect_equal(s$colour, s$major$colour)            # back-compat flat alias

  v <- radiatR:::.theme_grid_style("void")
  expect_false(v$has_grid)
  expect_true(is.na(v$fill))
  expect_equal(v$colour, "grey60")                  # fallback preserved
})

test_that("add_origin_point draws one point at the origin with given style", {
  library(ggplot2)
  d <- ggplot_build(ggplot() + add_origin_point(colour = "red", size = 3))$data[[1]]
  expect_equal(nrow(d), 1L)
  expect_equal(d$x, 0); expect_equal(d$y, 0)
  expect_equal(d$colour, "red")
})

test_that(".radial_spokes places n radii at evenly spaced angles", {
  sp <- radiatR:::.radial_spokes(4, phase = 0)
  d  <- sp$data
  ang <- sort(round((atan2(d$yend, d$xend) %% (2 * pi)) * 180 / pi))
  expect_equal(ang, c(0, 90, 180, 270))
})

test_that("add_radial_grid composes disc, rings, spokes and origin", {
  ls <- add_radial_grid(rings_major = 0.5, rings_minor = c(0.25, 0.75),
                        spokes_major = 4, spokes_minor = 4,
                        colour = "white", disc_fill = "grey92", origin = TRUE)
  geoms <- vapply(ls, function(l) class(l$geom)[1], character(1))
  expect_equal(sum(geoms == "GeomPolygon"), 1L)   # disc
  expect_equal(sum(geoms == "GeomPath"),    3L)   # rings at .25/.5/.75
  expect_equal(sum(geoms == "GeomSegment"), 2L)   # major + minor spoke layers
  expect_equal(sum(geoms == "GeomPoint"),   1L)   # origin
})

test_that("add_radial_grid omits the disc when disc_fill is NA and origin when FALSE", {
  ls <- add_radial_grid(disc_fill = NA, origin = FALSE)
  geoms <- vapply(ls, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPolygon" %in% geoms)
  expect_false("GeomPoint" %in% geoms)
})

test_that("add_radial_grid treats disc_fill = NULL as no disc", {
  ls <- add_radial_grid(disc_fill = NULL)
  geoms <- vapply(ls, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPolygon" %in% geoms)
})

test_that("radiate(): radial grid is the default for grid themes", {
  data(cpunctatus, package = "radiatR")
  p <- radiate(cpunctatus, theme = "grey")
  expect_true(inherits(p$theme$panel.grid, "element_blank"))      # cartesian grid removed
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% geoms)                           # disc
  expect_true(any(geoms == "GeomSegment"))                        # crosshairs/diagonals
  expect_false("GeomPoint" %in% geoms)                            # no origin dot on grid themes
})

test_that("radiate(grid='cartesian') keeps the native grid, no radial guides", {
  data(cpunctatus, package = "radiatR")
  p <- radiate(cpunctatus, theme = "grey", grid = "cartesian")
  expect_false(inherits(p$theme$panel.grid, "element_blank"))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPolygon" %in% geoms)
})

test_that("radiate(grid='none') blanks the grid with no radial guides", {
  data(cpunctatus, package = "radiatR")
  p <- radiate(cpunctatus, theme = "grey", grid = "none")
  expect_true(inherits(p$theme$panel.grid, "element_blank"))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPolygon" %in% geoms)
})

test_that("radiate(): gridless theme draws no origin dot by default; origin=TRUE adds it", {
  data(cpunctatus, package = "radiatR")
  geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPoint" %in% geoms(radiate(cpunctatus, theme = "void")))
  expect_false("GeomPolygon" %in% geoms(radiate(cpunctatus, theme = "void")))
  expect_true("GeomPoint" %in% geoms(radiate(cpunctatus, theme = "void", origin = TRUE)))
})

test_that("radiate(grid_colour=) overrides the derived guide colour", {
  data(cpunctatus, package = "radiatR")
  p <- radiate(cpunctatus, theme = "grey", grid_colour = "red")
  seg <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))[1]]]
  expect_equal(seg$aes_params$colour, "red")
})

test_that(".theme_axis_style resolves axis elements, ink, and legibility", {
  cl <- radiatR:::.theme_axis_style("classic")
  expect_true(cl$line$present)                      # classic draws axis.line
  expect_equal(tolower(cl$line$colour), "black")

  gr <- radiatR:::.theme_axis_style("grey")
  expect_false(gr$line$present)                     # grey: axis.line blank
  expect_true(gr$ticks$present)                     # grey: ticks drawn
  expect_true(gr$text$present)                      # grey: axis.text drawn
  expect_equal(gr$ink, gr$ticks$colour)             # ink falls through to a present axis colour (light disc)

  dk <- radiatR:::.theme_axis_style("dark")
  expect_true(radiatR:::.is_dark(dk$fill))          # dark theme has a dark panel
  expect_true(radiatR:::.is_dark("black"))
  expect_false(radiatR:::.is_dark("white"))
  expect_equal(dk$ink, "grey85")                    # ink flips light on the dark disc
})

test_that("degree_labs: size and family are applied to the label layers", {
  ls <- degree_labs(size = 6, family = "mono")
  expect_equal(ls[[1]]$aes_params$size, 6)
  expect_equal(ls[[1]]$aes_params$family, "mono")
})

test_that("add_circ: colour/linewidth aliases and linetype apply", {
  library(ggplot2)
  lyr <- add_circ(colour = "red", linewidth = 3, linetype = "dashed")[[1]]
  expect_equal(lyr$aes_params$colour, "red")
  expect_equal(lyr$aes_params$linewidth, 3)
  expect_equal(lyr$aes_params$linetype, "dashed")
})

test_that("radiate(): origin is opt-in everywhere", {
  data(cpunctatus, package = "radiatR")
  expect_false("GeomPoint" %in%
    vapply(radiate(cpunctatus, theme = "void")$layers,
           function(l) class(l$geom)[1], character(1)))
  expect_true("GeomPoint" %in%
    vapply(radiate(cpunctatus, theme = "void", origin = TRUE)$layers,
           function(l) class(l$geom)[1], character(1)))
})

test_that("radiate(): circumference is a fallback (drawn on void, removable)", {
  data(cpunctatus, package = "radiatR")
  np <- function(p) sum(vapply(p$layers, function(l) inherits(l$geom, "GeomPath"), logical(1)))
  expect_equal(np(radiate(cpunctatus, theme = "void")) -
               np(radiate(cpunctatus, theme = "void", circumference = FALSE)), 1L)
})

test_that("radiate(): grid themes mark the boundary with a 1.0 ring", {
  data(cpunctatus, package = "radiatR")
  g <- radiate(cpunctatus, theme = "grey")
  has_rim <- any(vapply(g$layers, function(l) {
    if (!inherits(l$geom, "GeomPath")) return(FALSE)
    d <- l$data
    !is.null(d) && all(c("x", "y") %in% names(d)) &&
      isTRUE(abs(max(sqrt(d$x^2 + d$y^2)) - 1) < 1e-6)
  }, logical(1)))
  expect_true(has_rim)
})

test_that("radiate(): tick/label presence is argument-controlled, not theme-gated", {
  data(cpunctatus, package = "radiatR")
  n_seg <- function(p) sum(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))
  n_txt <- function(p) sum(vapply(p$layers, function(l) inherits(l$geom, "GeomText"),    logical(1)))
  base <- radiate(cpunctatus, theme = "void")
  expect_gt(n_seg(base), n_seg(radiate(cpunctatus, theme = "void", ticks = FALSE)))
  expect_gt(n_txt(base), 0L)                                                         # void keeps labels
  expect_equal(
    n_txt(radiate(cpunctatus, theme = "void", angle_labels = "none", show_labels = FALSE)),
    0L)
})

test_that("radiate(): chrome styling comes from the named theme", {
  data(cpunctatus, package = "radiatR")
  cl   <- radiate(cpunctatus, theme = "classic")
  circ_layers <- Filter(function(l) inherits(l$geom, "GeomPath") &&
                           !is.null(l$aes_params$colour), cl$layers)
  circ <- circ_layers[[1]]
  expect_equal(tolower(circ$aes_params$colour), "black")   # classic axis.line is black

  dk  <- radiate(cpunctatus, theme = "dark")
  lab <- Filter(function(l) inherits(l$geom, "GeomText"), dk$layers)[[1]]
  expect_equal(lab$aes_params$colour, "grey85")            # legibility guard on dark disc
})

test_that(".mirror_axial doubles rows and adds antipodal angles, preserving other columns", {
  df <- data.frame(heading = c(0, pi / 2, 3), grp = c("a", "b", "a"),
                   x_inner = c(0.1, 0.2, 0.3), stringsAsFactors = FALSE)
  out <- radiatR:::.mirror_axial(df, "heading")

  expect_equal(nrow(out), 2L * nrow(df))
  expect_equal(out[seq_len(nrow(df)), , drop = FALSE], df)
  mirrored <- out[(nrow(df) + 1L):nrow(out), , drop = FALSE]
  expect_equal(mirrored$heading, (df$heading + pi) %% (2 * pi))
  expect_equal(mirrored$grp, df$grp)
  expect_equal(mirrored$x_inner, df$x_inner)
})

test_that(".mirror_axial negates requested coordinate columns on the mirrored copy", {
  df  <- data.frame(heading = c(0, 1), x_inner = c(0.1, -0.2), y_inner = c(0.3, 0.4))
  out <- radiatR:::.mirror_axial(df, "heading", negate = c("x_inner", "y_inner"))
  mirrored <- out[3:4, , drop = FALSE]
  expect_equal(mirrored$x_inner, -df$x_inner)
  expect_equal(mirrored$y_inner, -df$y_inner)
  expect_equal(out[1:2, "x_inner"], df$x_inner)
})

test_that(".mirror_axial on an empty frame returns an empty frame", {
  df  <- data.frame(heading = numeric(0), grp = character(0))
  out <- radiatR:::.mirror_axial(df, "heading")
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), names(df))
})

test_that("add_heading_points(axial = TRUE) draws each datum at both poles", {
  hd  <- data.frame(heading = c(0, pi / 2))
  lyr <- add_heading_points(hd, axial = TRUE)
  d   <- lyr$data
  expect_equal(nrow(d), 4L)
  expect_equal(d[[".x_head"]][3:4], -d[[".x_head"]][1:2], tolerance = 1e-12)
  expect_equal(d[[".y_head"]][3:4], -d[[".y_head"]][1:2], tolerance = 1e-12)
})

test_that("add_heading_points(axial = FALSE) is unchanged", {
  hd <- data.frame(heading = c(0, pi / 2))
  expect_equal(nrow(add_heading_points(hd)$data), 2L)
  expect_equal(nrow(add_heading_points(hd, axial = FALSE)$data), 2L)
})

test_that("add_heading_vectors(axial = TRUE) mirrors endpoints and inner start coords", {
  hd  <- data.frame(heading = c(0, pi / 2),
                    x_inner = c(0.1, 0.0), y_inner = c(0.0, 0.2))
  lyr <- add_heading_vectors(hd, axial = TRUE)
  d   <- lyr$data
  expect_equal(nrow(d), 4L)
  expect_equal(d[[".x_head"]][3:4],  -d[[".x_head"]][1:2],  tolerance = 1e-12)
  expect_equal(d[[".y_head"]][3:4],  -d[[".y_head"]][1:2],  tolerance = 1e-12)
  expect_equal(d[[".x_inner"]][3:4], -d[[".x_inner"]][1:2], tolerance = 1e-12)
  expect_equal(d[[".y_inner"]][3:4], -d[[".y_inner"]][1:2], tolerance = 1e-12)
})

test_that("add_stacked_headings(axial = TRUE) stacks each antipodal cluster within itself", {
  hd  <- data.frame(heading = c(0, 0))
  lyr <- add_stacked_headings(hd, axial = TRUE)
  d   <- lyr$data
  expect_equal(nrow(d), 4L)
  expect_equal(length(unique(round(d$stack_r, 6))), 2L)
})

test_that("add_stacked_headings(axial = FALSE) is unchanged", {
  hd <- data.frame(heading = c(0, 0))
  expect_equal(nrow(add_stacked_headings(hd)$data), 2L)
})

test_that("add_angle_rose(axial = TRUE) produces a point-symmetric rose", {
  hd  <- data.frame(heading = c(0.1, 0.2, 0.3, 0.15))
  lyr <- add_angle_rose(hd, bins = 12L, axial = TRUE)
  d   <- lyr$data
  pts  <- unique(round(cbind(d$x, d$y), 6))
  refl <- unique(round(cbind(-d$x, -d$y), 6))
  expect_setequal(
    apply(pts,  1L, paste, collapse = ","),
    apply(refl, 1L, paste, collapse = ",")
  )
})

test_that("add_circular_kde(axial = TRUE) produces a point-symmetric ring", {
  set.seed(1)
  hd  <- data.frame(heading = rnorm(40, 0.3, 0.2))
  # Use an odd n_pts so the density grid seq(0, 2*pi, length.out = n) has a
  # vertex exactly at theta + pi for every vertex at theta; this lets axial
  # symmetry be asserted at the vertex level. The even default grid samples
  # the (genuinely symmetric) curve at pi-misaligned points, so its vertices
  # interleave rather than reflect onto each other.
  lyr <- add_circular_kde(hd, axial = TRUE, n_pts = 513L)
  d   <- lyr$data
  key  <- function(m) apply(round(m, 4), 1L, paste, collapse = ",")
  pts  <- cbind(d$x, d$y)
  refl <- cbind(-d$x, -d$y)
  expect_true(all(key(refl) %in% key(pts)))
})

test_that("compute_circular_density(axial = TRUE) yields a period-pi symmetric density", {
  set.seed(2)
  hd   <- data.frame(heading = rnorm(60, 0.4, 0.3))
  # Fixed bw keeps the kernel density non-degenerate (the auto bandwidth on a
  # bimodal mirrored sample collapses to a flat density, which is trivially
  # symmetric and would not actually test anything).
  dens <- compute_circular_density(hd, method = "kernel", bw = 20, axial = TRUE)
  th   <- dens$theta
  d    <- dens$density
  # The (genuinely symmetric) density is sampled on an even grid whose origin
  # is not pi-aligned, so theta + pi lands half a grid step off the nearest
  # vertex. Asserting symmetry by nearest-neighbour snapping is therefore
  # brittle (~5e-3 error). Compare instead against a wrap-aware linear
  # interpolation of the density at theta + pi, which recovers symmetry to the
  # interpolation residual of a smooth curve.
  o   <- order(th); th <- th[o]; d <- d[o]
  wrap   <- function(a) ((a + pi) %% (2 * pi)) - pi
  thx    <- c(th - 2 * pi, th, th + 2 * pi)
  dx     <- c(d, d, d)
  keep   <- !duplicated(thx)
  anti   <- stats::approx(thx[keep], dx[keep], xout = wrap(th + pi))$y
  expect_equal(d, anti, tolerance = 1e-3)
})

test_that("add_heading_density(axial = TRUE) forwards to the estimator", {
  set.seed(3)
  hd       <- data.frame(heading = rnorm(60, 0.4, 0.3))
  # Forwarding is verified by equivalence: estimating with axial = TRUE must be
  # identical to pre-mirroring the raw sample and estimating without it. (A
  # geometric reflection assertion on the rendered polygon is unreliable here
  # because the kernel's even grid interleaves antipodal vertices.)
  mirrored <- rbind(hd, data.frame(heading = (hd$heading + pi) %% (2 * pi)))
  lyr_ax   <- add_heading_density(hd,       method = "kernel", bw = 20, axial = TRUE)
  lyr_man  <- add_heading_density(mirrored, method = "kernel", bw = 20)
  expect_equal(lyr_ax[[length(lyr_ax)]]$data,
               lyr_man[[length(lyr_man)]]$data)
})

test_that("add_wrappedcauchy_density(axial = TRUE) draws a point-symmetric two-peak curve", {
  fit <- data.frame(mu = 0.3, rho = 0.6)
  lyr <- add_wrappedcauchy_density(fit, axial = TRUE)
  d   <- lyr$data
  key <- function(m) apply(round(m, 4), 1L, paste, collapse = ",")
  expect_true(all(key(cbind(-d$x, -d$y)) %in% key(cbind(d$x, d$y))))
})

test_that("add_wrappedcauchy_density(axial = FALSE) is unchanged (single peak)", {
  fit <- data.frame(mu = 0.3, rho = 0.6)
  d_def <- add_wrappedcauchy_density(fit)$data
  d_dir <- add_wrappedcauchy_density(fit, axial = FALSE)$data
  expect_equal(d_def, d_dir)
  key <- function(m) apply(round(m, 4), 1L, paste, collapse = ",")
  expect_false(all(key(cbind(-d_dir$x, -d_dir$y)) %in% key(cbind(d_dir$x, d_dir$y))))
})

test_that(".seq_position gives 0 at the first and 1 at the last point per track", {
  d <- data.frame(id = rep(c("a", "b"), c(4, 1)), t = c(1, 2, 3, 4, 9))
  out <- radiatR:::.seq_position(d, id_col = "id", time_col = "t")
  expect_equal(out$.seq[out$id == "a"], c(0, 1/3, 2/3, 1))
  expect_equal(out$.seq[out$id == "b"], 0)            # n == 1 -> 0
})

test_that(".seq_position falls back to row order with a message when no time column", {
  d <- data.frame(id = rep("a", 3))
  expect_message(out <- radiatR:::.seq_position(d, id_col = "id", time_col = NULL),
                 "row order")
  expect_equal(out$.seq, c(0, 0.5, 1))
})

test_that("radiate track_colour = 'sequence' uses a continuous colour scale on .seq", {
  data(cpunctatus, package = "radiatR", envir = environment())
  p <- radiate(cpunctatus, show_tracks = TRUE, track_colour = "sequence")
  b <- ggplot2::ggplot_build(p)
  # there is a continuous colour scale (not a discrete/manual one)
  col_scale <- p$scales$get_scales("colour")
  expect_true(inherits(col_scale, "ScaleContinuous"))
  # the track path layer carries a numeric colour (the gradient), not a constant
  path_data <- b$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomPath"), logical(1)))[1]]]
  expect_true("colour" %in% names(path_data))
  expect_gt(length(unique(path_data$colour)), 1L)     # a gradient, not one colour
})

test_that("radiate track_colour = 'trajectory' is unchanged (no .seq, discrete colours)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  p <- radiate(cpunctatus, show_tracks = TRUE)         # default trajectory mode
  expect_false(".seq" %in% names(p$layers[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomPath"), logical(1)))[1]]]$data))
})

test_that("track_colour = 'sequence' cannot combine with colour_col/colour_cycle", {
  data(cpunctatus, package = "radiatR", envir = environment())
  expect_error(radiate(cpunctatus, show_tracks = TRUE, track_colour = "sequence",
                       colour_cycle = 4), "cannot be combined")
})

test_that("a faceted sequence plot builds", {
  data(cpunctatus, package = "radiatR", envir = environment())
  df <- as.data.frame(cpunctatus)
  facet <- intersect(c("arc", "type", "obstacle"), names(df))[1]
  skip_if(is.na(facet), "no facet column in cpunctatus")
  p <- radiate(cpunctatus, show_tracks = TRUE, track_colour = "sequence", panel_by = facet)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("radiate track_colour = 'time' colours by elapsed seconds with a frame rate", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  p <- radiate(ts, show_tracks = TRUE, track_colour = "time")
  cs <- p$scales$get_scales("colour")
  expect_true(inherits(cs, "ScaleContinuous"))
  b <- ggplot2::ggplot_build(p)
  path_data <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomPath"), logical(1)))[1]]]
  expect_gt(length(unique(path_data$colour)), 1L)
})

test_that("radiate track_colour = 'time' errors on numeric frames without a frame rate", {
  bare <- methods::new("Tracks",
    data = data.frame(id = "a", frame = 0:3, x = (0:3) / 3, y = 0, angle = 0),
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
  expect_error(radiate(bare, show_tracks = TRUE, track_colour = "time"),
               "frame rate")
})

test_that("radiate track_colour = 'speed' colours by instantaneous speed with a frame rate", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  p  <- radiate(ts, show_tracks = TRUE, track_colour = "speed")
  cs <- p$scales$get_scales("colour")
  expect_true(inherits(cs, "ScaleContinuous"))
  expect_equal(sum(vapply(p$scales$scales,
    function(s) "colour" %in% s$aesthetics, logical(1))), 1L)   # exactly one colour scale
  b  <- ggplot2::ggplot_build(p)
  pd <- b$data[[which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomPath"), logical(1)))[1]]]
  expect_gt(length(unique(pd$colour)), 1L)                      # a gradient
})

test_that("radiate track_colour = 'speed' errors on numeric frames without a frame rate", {
  bare <- methods::new("Tracks",
    data = data.frame(id = "a", frame = 0:3, x = (0:3) / 3, y = 0, angle = 0),
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
  expect_error(radiate(bare, show_tracks = TRUE, track_colour = "speed"), "frame rate")
})

test_that("radiate track_colour = 'speed' cannot combine with colour_cycle", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_frame_rate(cpunctatus, 30)
  expect_error(radiate(ts, show_tracks = TRUE, track_colour = "speed", colour_cycle = 4),
               "cannot be combined")
})

test_that("radiate track_colour='speed' colourbar shows the distance unit when calibrated", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- set_distance_scale(set_frame_rate(cpunctatus, 30), 50, "mm")
  p  <- radiate(ts, show_tracks = TRUE, track_colour = "speed")
  cs <- p$scales$get_scales("colour")
  # ggplot2 4.x stores the guide title under `$params$title`; older versions
  # exposed it directly at `$title`.
  guide_title <- if (!is.null(cs$guide$params$title)) cs$guide$params$title else cs$guide$title
  expect_equal(guide_title, "speed (mm/s)")
})
