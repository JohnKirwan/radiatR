# plot_spec.R is a shiny-free helper sourced by the app; source it directly.
.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

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
