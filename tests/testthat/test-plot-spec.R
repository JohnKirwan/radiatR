# plot_spec.R is a shiny-free helper sourced by the app; source it directly.
.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

test_that("build_headings_input maps a degrees/compass column to a headings frame", {
  df  <- data.frame(dir = c(0, 90, 180, 270), cond = c("a","a","b","b"))
  hf  <- build_headings_input(df, col = "dir", units = "degrees",
                              convention = "clock", group = "cond")
  expect_s3_class(hf, "headings_frame")
  expect_true("heading" %in% names(hf))          # angle column renamed to "heading"
  expect_true(all(hf$heading >= 0 & hf$heading < 2 * pi))  # radians, wrapped
  expect_true("cond" %in% names(hf))             # group column carried through
})

test_that("build_headings_input coerces a plain derive_headings()-style frame", {
  df  <- data.frame(id = c("t1","t2"), time = c(5, 9),
                    heading = c(0.5, 2.1))        # already radians/unit-circle
  hf  <- build_headings_input(df, col = "heading", units = "radians",
                              convention = "unit_circle", group = NULL)
  expect_s3_class(hf, "headings_frame")
  expect_equal(hf$heading, c(0.5, 2.1))
})

test_that("build_plot_spec captures the figure choices", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "condition", colour_by = "__trajectory__",
                  plot_theme = "bw", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE)
  )
  expect_equal(spec$group_col, ts@cols$id)
  expect_equal(spec$facet_by, "condition")
  expect_equal(spec$colour$by, "trajectory")
  expect_false(spec$colour$legend)          # trajectory cycles -> no legend
  expect_equal(spec$theme, "bw")
  expect_equal(spec$heading_display, "points")
  expect_true(spec$show$arrow)
})

test_that("build_plot_spec: low-cardinality colour -> distinct + legend", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", colour_by = "condition",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "stacked",
                  show_tracks = TRUE, show_arrow = FALSE, show_vectors = FALSE)
  )
  expect_equal(spec$colour$by, "condition")
  expect_true(spec$colour$legend)           # 3 conditions <= cap -> legend
  expect_null(spec$facet_by)
})

test_that("build_plot_spec: rule 'none' -> no headings", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  spec <- build_plot_spec(
    ts = ts, hd = NULL, method = "none",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", colour_by = "__trajectory__",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE)
  )
  expect_equal(spec$headings$rule, "none")
})

# helper: does the plot have a layer with the given geom class?
.has_geom <- function(p, cls) any(vapply(p$layers,
  function(l) inherits(l$geom, cls), logical(1)))

test_that("spec_to_plot renders tracks + stacked markers + arrow for an example spec", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "condition", colour_by = "__trajectory__",
                  plot_theme = "bw", angle_labels = "degrees",
                  heading_display = "stacked",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE))
  p <- spec_to_plot(spec, ts, hd)
  expect_s3_class(p, "ggplot")
  expect_true(.has_geom(p, "GeomPoint"))     # stacked markers
  expect_true(.has_geom(p, "GeomSegment"))   # arrow
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("spec_to_plot: rule 'none' draws tracks only (no markers)", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  spec <- build_plot_spec(
    ts = ts, hd = NULL, method = "none",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", colour_by = "__trajectory__",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE))
  p <- spec_to_plot(spec, ts, NULL)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("build_plot_spec: headings mode records mode and headings data block", {
  df <- data.frame(dir = c(0, 90, 180), cond = c("a","a","b"))
  hf <- build_headings_input(df, col = "dir", units = "degrees",
                             convention = "clock", group = "cond")
  spec <- build_plot_spec(
    ts = NULL, hd = hf, method = NULL,
    data = list(source = "file", mode = "headings", path = "angles.csv",
                col = "dir", units = "degrees", convention = "clock",
                group = "cond"),
    inputs = list(colour_by = "cond", cond_col = "cond",
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees")
  )
  expect_identical(spec$mode, "headings")
  expect_identical(spec$facet_by, "cond")
  expect_false(isTRUE(spec$show$tracks))     # no tracks in headings mode
})
