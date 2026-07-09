# Freezes today's default (group unset) emitted figure + stats code so the
# Group-by feature cannot silently perturb the backward-compatible default.
.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

char_spec <- function(facet = NULL) {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus
  hd <- derive_headings(ts, rule = "distal")
  build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = facet %||% "", colour_by = "__trajectory__",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points", omnibus_test = "rao",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE,
                  show_rayleigh = TRUE, show_ci = TRUE, show_vtest = TRUE))
}

test_that("default (group unset) figure code is frozen — plain", {
  expect_snapshot(cat(spec_to_code(char_spec())))
})
test_that("default (group unset) stats code is frozen — plain", {
  expect_snapshot(cat(spec_to_stats_code(char_spec())))
})
test_that("default (group unset) figure code is frozen — faceted", {
  expect_snapshot(cat(spec_to_code(char_spec("type"))))
})
test_that("default (group unset) stats code is frozen — faceted", {
  expect_snapshot(cat(spec_to_stats_code(char_spec("type"))))
})
