test_that("headings_frame returns correct classes and attributes", {
  df <- data.frame(bearing = c(0, pi/4, pi/2))
  hf <- headings_frame(df, col = "bearing", units = "radians")
  expect_s3_class(hf, "headings_frame")
  expect_s3_class(hf, "data.frame")
  expect_equal(hf_heading_col(hf), "bearing")
  expect_s3_class(hf_display(hf),  "circ_display")
  expect_equal(hf_coords(hf),      "absolute")
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
  # orientation is consolidated into a single display attribute now
  expect_s3_class(hf_display(hf), "circ_display")
  expect_equal(hf_coords(hf),     "relative")
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

test_that("headings_frame converts clock angles to unit-circle internally", {
  # Clock 0 (North) should become unit-circle pi/2 (North)
  df <- data.frame(h = c(0, 90, 180))  # degrees, clock convention
  hf <- headings_frame(df, col = "h", units = "degrees",
                       angle_convention = "clock")
  # After conversion: clock 0° → UC pi/2, clock 90° → UC 0, clock 180° → UC 3pi/2
  expect_equal(hf$h, c(pi/2, 0, 3*pi/2), tolerance = 1e-10)
  # data is normalised to unit-circle radians; orientation carried as display
  expect_s3_class(hf_display(hf), "circ_display")
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

test_that("stack_headings start_sep offsets the whole stack off base_r (inward)", {
  df <- data.frame(heading = c(0, 0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, step = 0.045, start_sep = 0.035,
                        direction = "inward", base_r = 1)
  # outermost dot is start_sep inside base_r; successive dots step by `step`
  expect_equal(sort(out$stack_r, decreasing = TRUE),
               c(0.965, 0.920, 0.875), tolerance = 1e-10)
})

test_that("stack_headings start_sep offsets outward too", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, step = 0.045, start_sep = 0.035,
                        direction = "outward", base_r = 1)
  expect_equal(sort(out$stack_r), c(1.035, 1.080), tolerance = 1e-10)
})

test_that("stack_headings start_sep defaults to 0 (back-compatible)", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, step = 0.025, direction = "inward", base_r = 1)
  expect_equal(max(out$stack_r), 1, tolerance = 1e-10)  # first dot still on base_r
})

test_that("stack_headings rejects negative start_sep", {
  df <- data.frame(heading = c(0, 0))
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_error(stack_headings(hf, start_sep = -0.1), "non-negative")
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

# ---- radiate() backward compatibility ----------------------------------------

test_that("radiate() still works on a Tracks after S3 refactor", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    id      = rep("A", 4L),
    time    = 1:4,
    x       = c(0.5, 0.7, 0.6, 0.8),
    y       = c(0.5, 0.6, 0.7, 0.5)
  )
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y")
  p  <- radiate(ts)
  expect_s3_class(p, "ggplot")
})

test_that("radiate() still works on a plain data frame after S3 refactor", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    trial_id = rep("T1", 3L),
    frame    = 1:3,
    rel_x    = c(0.5, 0.6, 0.7),
    rel_y    = c(0.5, 0.4, 0.3)
  )
  p <- radiate(df, x_col = "rel_x", y_col = "rel_y", group_col = "trial_id")
  expect_s3_class(p, "ggplot")
})

# ---- radiate.headings_frame --------------------------------------------------

test_that("radiate(headings_frame) returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4, pi/2, pi, 3*pi/2))
  hf <- headings_frame(df, col = "heading", units = "radians")
  p  <- radiate(hf)
  expect_s3_class(p, "ggplot")
})

test_that("radiate(headings_frame) with panel_by produces facets", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4, pi/2, pi),
                   grp     = c("a", "a", "b", "b"))
  hf <- headings_frame(df, col = "heading", units = "radians")
  p  <- radiate(hf, panel_by = "grp")
  expect_false(inherits(p$facet, "FacetNull"))
})

test_that("radiate(headings_frame) forwards stacking params", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, 0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_no_error(radiate(hf, direction = "outward"))
})

test_that("radiate(headings_frame) panel_by errors on missing column", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(heading = c(0, pi/4))
  hf <- headings_frame(df, col = "heading", units = "radians")
  expect_error(radiate(hf, panel_by = "nonexistent"),
               "panel_by column")
})

test_that("stack_headings(group=) stacks within each group independently", {
  df <- data.frame(heading = c(0, 0, 0, 0), g = c("a", "a", "b", "b"))
  hf <- headings_frame(df, col = "heading", units = "radians")
  out <- stack_headings(hf, group = "g", step = 0.025, base_r = 1)
  # each group restarts the stack: ranks 1,2 within "a" and within "b"
  expect_equal(sort(out$stack_r[out$g == "a"], decreasing = TRUE), c(1, 0.975), tolerance = 1e-9)
  expect_equal(sort(out$stack_r[out$g == "b"], decreasing = TRUE), c(1, 0.975), tolerance = 1e-9)
})

test_that("radiate.headings_frame draws frame-only when show_markers = FALSE", {
  hf <- headings_frame(
    data.frame(angle = c(10, 20, 30, 200, 210)),
    col = angle, units = "degrees"
  )
  g_frame   <- radiate(hf, show_markers = FALSE, theme = "void")
  g_markers <- radiate(hf, show_markers = TRUE,  theme = "void")
  expect_s3_class(g_frame, "ggplot")
  expect_silent(ggplot2::ggplot_build(g_frame))
  # The marker layer is the GeomPoint added by add_stacked_headings; absent in
  # frame-only mode, present otherwise.
  has_points <- function(g) any(vapply(g$layers,
    function(l) inherits(l$geom, "GeomPoint"), logical(1)))
  expect_false(has_points(g_frame))
  expect_true(has_points(g_markers))
})

test_that("radiate.headings_frame uses the modern themed chrome (grid theme draws a grid)", {
  hf <- headings_frame(data.frame(angle = c(0, 90, 180, 270)),
                       col = angle, units = "degrees")
  g <- radiate(hf, show_markers = FALSE, theme = "bw")
  expect_silent(ggplot2::ggplot_build(g))
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPath" %in% geoms)
})

test_that("radiate.headings_frame markers honour a non-default display", {
  hf <- headings_frame(data.frame(angle = c(0, 45, 90)),
                       col = angle, units = "degrees")
  pts <- function(g) {
    b <- ggplot2::ggplot_build(g)
    pt_layers <- which(vapply(g$layers,
      function(l) inherits(l$geom, "GeomPoint"), logical(1)))
    d <- b$data[[pt_layers[1]]]
    as.matrix(d[order(d$x, d$y), c("x", "y")])
  }
  g_default <- radiate(hf, show_markers = TRUE, display = circ_display(zero = 0))
  g_rot     <- radiate(hf, show_markers = TRUE,
                       display = circ_display(zero = pi / 2))
  # A different zero direction must move the marker coordinates.
  expect_false(isTRUE(all.equal(pts(g_default), pts(g_rot))))
})

# ---- new_headings_frame durable tibble subclass ------------------------------

test_that("new_headings_frame is a tibble subclass carrying the canonical attributes", {
  hd <- new_headings_frame(tibble::tibble(heading = c(0.1, 0.2, 0.3)),
                           display = circ_display(zero = 0, clockwise = FALSE),
                           heading_col = "heading", colour_col = NULL, coords = "absolute")
  expect_s3_class(hd, "headings_frame")
  expect_s3_class(hd, "tbl_df")
  expect_s3_class(hf_display(hd), "circ_display")
  expect_equal(hf_display(hd)$zero, 0)
  expect_equal(hf_heading_col(hd), "heading")
  expect_equal(hf_coords(hd), "absolute")
  expect_null(hf_colour_col(hd))
})

test_that("the class and display attribute survive dplyr verbs and base subsetting", {
  hd <- new_headings_frame(tibble::tibble(heading = seq(0, 1, length.out = 6), g = rep(c("a","b"), 3)),
                           display = circ_display(zero = 0, clockwise = FALSE))
  # class (incl. tbl_df, so it stays a real tibble) AND display must both survive
  keeps <- function(x) expect_true(inherits(x, "headings_frame") &&
                                   inherits(x, "tbl_df") &&
                                   identical(hf_display(x)$zero, 0))
  keeps(dplyr::mutate(hd, h2 = heading * 2))
  keeps(dplyr::filter(hd, heading > 0.1))
  keeps(dplyr::select(hd, heading))
  keeps(dplyr::arrange(hd, dplyr::desc(heading)))
  keeps(dplyr::slice(hd, 1:3))
  keeps(dplyr::bind_rows(hd, hd))
  keeps(hd[1:3, ])
  # group_by is dplyr's special case: it returns a grouped_df (the headings_frame
  # subclass reverts) but the display attribute still survives, so re-plotting a
  # grouped/summarised result keeps its orientation.
  g <- dplyr::group_by(hd, g)
  expect_true(inherits(g, "grouped_df") && identical(hf_display(g)$zero, 0))
})

test_that("hf_* accessors fall back sensibly on a plain data frame", {
  df <- data.frame(heading = c(0.1, 0.2))
  expect_s3_class(hf_display(df), "circ_display")     # circ_display() default
  expect_equal(hf_heading_col(df), "heading")
  expect_null(hf_colour_col(df))
  expect_equal(hf_coords(df), "absolute")
})

test_that("headings_frame() constructor consolidates orientation to a single display attribute", {
  d  <- data.frame(theta = c(10, 20, 350))
  hf <- headings_frame(d, theta, units = "degrees", angle_convention = "clock")
  expect_s3_class(hf, "headings_frame")
  expect_s3_class(hf_display(hf), "circ_display")
  # the vestigial string attributes are gone
  expect_null(attr(hf, "display_convention"))
  expect_null(attr(hf, "angle_convention"))
  # data still normalised to unit-circle radians (clock 10deg -> (pi/2 - 10deg) wrapped)
  expect_equal(hf$theta[1], wrap_to_2pi(pi/2 - 10 * pi/180), tolerance = 1e-9)
})
