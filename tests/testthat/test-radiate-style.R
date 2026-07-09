test_that("radiate generates a ggplot for every named theme", {
  ts <- simulate_tracks(n_points = 30, seed = 123, output = "trajset")
  themes <- c("void", "minimal", "classic", "bw", "grey", "light",
              "dark", "linedraw")
  for (th in themes) {
    p <- radiate(
      ts,
      group_col  = "trial_id",
      colour_col = "trial_id",
      show_arrow = FALSE,
      theme      = th
    )
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("radial_theme returns the matching ggplot2 base theme", {
  expect_identical(radial_theme("bw"),      ggplot2::theme_bw(base_size = 11))
  expect_identical(radial_theme("minimal"), ggplot2::theme_minimal(base_size = 11))
  expect_identical(radial_theme("grey"),    ggplot2::theme_grey(base_size = 11))
  expect_identical(radial_theme("gray"),    radial_theme("grey"))   # alias
  expect_error(radial_theme("nope"))
})

test_that("dark theme uses light ink for axis chrome so it stays visible", {
  ts <- simulate_tracks(n_points = 20, seed = 7, output = "trajset")
  # Axis ticks are always drawn (ticks = TRUE by default); their colour is
  # derived from the theme's axis.ticks, legibility-guarded against the panel
  # fill. theme_dark()'s axis.ticks colour is dark-on-dark, so it gets bumped
  # to the light "grey85" ink. theme_minimal() has no axis.ticks; it falls
  # back to its own (light-panel) ink, which is not "grey85".
  tick_colour <- function(p) {
    for (l in p$layers) {
      d <- l$data
      if (inherits(l$geom, "GeomSegment") && !is.null(l$aes_params$colour) &&
          is.data.frame(d) && all(c("x", "y", "xend", "yend") %in% names(d))) {
        r <- sqrt(d$x^2 + d$y^2)
        # ticks straddle r = 1 (radial marks just inside/outside the rim);
        # spokes run from the centre (r ~ 0) to the rim (r = 1).
        if (all(abs(r - 1) < 0.5) && !any(r < 0.5)) return(l$aes_params$colour)
      }
    }
    NA_character_
  }
  p_dark  <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                     show_labels = FALSE, theme = "dark")
  p_light <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                     show_labels = FALSE, theme = "minimal")
  expect_equal(tick_colour(p_dark),  "grey85")
  expect_false(identical(tick_colour(p_light), "grey85"))
})

test_that("show_tracks toggles the trajectory layer", {
  ts <- simulate_tracks(n_points = 30, seed = 123, output = "trajset")

  # The track layer is the geom_path whose data has one row per track point;
  # the unit circle is also a geom_path but with its own small generated data.
  n_track_layers <- function(g) {
    n_pts <- nrow(ts@data)
    sum(vapply(g$layers, function(l) {
      inherits(l$geom, "GeomPath") &&
        is.data.frame(l$data) && nrow(l$data) == n_pts
    }, logical(1)))
  }

  g_on  <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, show_tracks = TRUE)
  g_off <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, show_tracks = FALSE)

  expect_equal(n_track_layers(g_on), 1L)
  expect_equal(n_track_layers(g_off), 0L)
  # the circle still renders without tracks
  expect_s3_class(g_off, "ggplot")
  expect_silent(ggplot2::ggplot_build(g_off))
})

test_that("quadrant lines are off by default and added by quadrants = TRUE", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 3L),
                        n_points = 20, seed = 5, output = "trajset")
  # The quadrant lines are the only geom_segment layer with exactly two rows
  # (the horizontal and vertical diameters). Ticks have eight; the arrow is
  # disabled here.
  has_quadrants <- function(g) {
    any(vapply(g$layers, function(l) {
      d <- l$data
      inherits(l$geom, "GeomSegment") && is.data.frame(d) && nrow(d) == 2L &&
        all(c("x", "y", "xend", "yend") %in% names(d))
    }, logical(1)))
  }
  g_off <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE)
  g_on  <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, quadrants = TRUE)
  expect_false(has_quadrants(g_off))
  expect_true(has_quadrants(g_on))
})

test_that("quadrant lines inherit the theme's grid colour, with a fallback", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 3L),
                        n_points = 20, seed = 6, output = "trajset")
  quad_colour <- function(g) {
    for (l in g$layers) {
      d <- l$data
      if (inherits(l$geom, "GeomSegment") && is.data.frame(d) && nrow(d) == 2L)
        return(l$aes_params$colour)
    }
    NA_character_
  }
  grid_colour <- function(name)
    ggplot2::calc_element("panel.grid", radial_theme(name))$colour

  g_min <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, quadrants = TRUE, theme = "minimal",
                   grid = "cartesian")
  expect_equal(quad_colour(g_min), grid_colour("minimal"))

  # void draws no grid -> fall back to a subtle grey
  g_void <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                    show_labels = FALSE, quadrants = TRUE, theme = "void")
  expect_equal(quad_colour(g_void), "grey60")
})

test_that("guide rings are off by default and added by rings = TRUE", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 3L),
                        n_points = 20, seed = 7, output = "trajset")
  # Guide rings are annotate('path') circles (1000 points each) strictly inside
  # the unit boundary (radius < 1); the boundary circle has radius 1, tracks
  # have far fewer points.
  n_inner_rings <- function(g) {
    sum(vapply(g$layers, function(l) {
      d <- l$data
      inherits(l$geom, "GeomPath") && is.data.frame(d) &&
        all(c("x", "y") %in% names(d)) && nrow(d) >= 999L &&
        max(sqrt(d$x^2 + d$y^2)) < 0.99
    }, logical(1)))
  }
  g_off <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, show_tracks = FALSE)
  g_on  <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, show_tracks = FALSE, rings = TRUE)
  expect_equal(n_inner_rings(g_off), 0L)
  expect_equal(n_inner_rings(g_on), 3L)   # radii 0.25, 0.5, 0.75
})

test_that("degree_labs formats degrees with a symbol and radians as pi fractions", {
  dlabs <- vapply(degree_labs(units = "degrees"),
                  function(l) l$aes_params$label, character(1))
  rlabs <- vapply(degree_labs(units = "radians"),
                  function(l) l$aes_params$label, character(1))
  expect_setequal(dlabs, c("45°", "135°", "225°", "315°"))
  expect_setequal(rlabs, c("π/4", "3π/4", "5π/4", "7π/4"))
})

test_that("radiate.default chrome layer signature is stable across the refactor", {
  ts <- simulate_tracks(n_points = 20, seed = 11, output = "trajset")

  # void: no grid -> circumference fallback + 8-tick segment + labels, no tracks
  g <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
               show_labels = FALSE, show_tracks = FALSE, theme = "void")
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPath"    %in% geoms)   # circumference
  expect_true("GeomSegment" %in% geoms)   # ticks
  expect_silent(ggplot2::ggplot_build(g))

  # bw: grid theme -> radial grid present (GeomPath rings), no separate bold circ
  gb <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                show_labels = FALSE, show_tracks = FALSE, theme = "bw")
  expect_s3_class(gb, "ggplot")
  expect_silent(ggplot2::ggplot_build(gb))
})

test_that("radiate angle_labels switches between degrees, none and radians", {
  ts <- simulate_tracks(conditions = data.frame(n_trials = 3L),
                        n_points = 20, seed = 8, output = "trajset")
  # Collect the text of the angle-label layers (GeomText, no faceting/title so
  # the only text layers are the degree labels).
  label_texts <- function(g) {
    unlist(lapply(g$layers, function(l) {
      if (inherits(l$geom, "GeomText")) l$aes_params$label else NULL
    }))
  }
  base <- function(...) radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                                show_labels = FALSE, show_tracks = FALSE, ...)

  expect_true(any(grepl("°", label_texts(base(angle_labels = "degrees")))))
  expect_true(any(grepl("π", label_texts(base(angle_labels = "radians")))))
  expect_length(label_texts(base(angle_labels = "none")), 0L)
  # Back-compat: degrees = FALSE hides the labels.
  expect_length(label_texts(base(degrees = FALSE)), 0L)
})

test_that("radiate(rows=, cols=) builds a facet_grid over the right variables", {
  data(cpunctatus, package = "radiatR", envir = environment())
  p   <- radiate(cpunctatus, group_col = "trial_id", show_arrow = FALSE,
                 show_labels = FALSE, rows = "type", cols = "arc")
  expect_s3_class(p$facet, "FacetGrid")
  lay <- ggplot2::ggplot_build(p)$layout$layout
  expect_true(all(c("ROW", "COL", "type", "arc") %in% names(lay)))
  expect_equal(length(unique(lay$type)), 2L)        # rows = the 2 type levels
})

test_that("radiate multi-variable grid side nests like facet_grid", {
  data(cpunctatus, package = "radiatR", envir = environment())
  p <- radiate(cpunctatus, group_col = "trial_id", show_arrow = FALSE,
               show_labels = FALSE, rows = c("type", "arc"))
  expect_s3_class(p$facet, "FacetGrid")
  lay <- ggplot2::ggplot_build(p)$layout$layout
  expect_true(all(c("type", "arc") %in% names(lay)))
  expect_equal(length(unique(lay$COL)), 1L)         # cols = . -> single column
})

test_that("radiate errors when facets and rows/cols are both set", {
  data(cpunctatus, package = "radiatR", envir = environment())
  expect_error(
    radiate(cpunctatus, group_col = "trial_id", facets = "type", cols = "arc"),
    "facet_grid|both", ignore.case = TRUE)
})

test_that("radiate errors on a missing rows/cols column", {
  data(cpunctatus, package = "radiatR", envir = environment())
  expect_error(
    radiate(cpunctatus, group_col = "trial_id", rows = "nope"),
    "not found", ignore.case = TRUE)
})

test_that("radiate facet_grid routes one mean arrow per occupied cell", {
  data(cpunctatus, package = "radiatR", envir = environment())
  p <- radiate(cpunctatus, group_col = "trial_id", rows = "type",
               show_labels = FALSE, show_arrow = TRUE)
  b <- ggplot2::ggplot_build(p)
  # the mean arrow's geom_segment data starts at the origin (0,0)
  arrow_d <- NULL
  for (i in seq_along(p$layers)) {
    d <- b$data[[i]]
    if (inherits(p$layers[[i]]$geom, "GeomSegment") &&
        all(abs(d$x) < 1e-9) && all(abs(d$y) < 1e-9)) { arrow_d <- d; break }
  }
  expect_false(is.null(arrow_d))
  expect_equal(length(unique(arrow_d$PANEL)), 2L)   # one arrow per type panel
})
