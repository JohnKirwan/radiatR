# results_overlays.R is a shiny-free helper sourced by the app; source it directly
# (installed location first, then the source tree), as for preview.R.
for (.f in c("results_overlays.R")) {
  .p <- system.file("app", .f, package = "radiatR")
  if (!nzchar(.p) || !file.exists(.p)) {
    .p <- testthat::test_path("..", "..", "inst", "app", .f)
  }
  source(.p, local = TRUE)
}

make_hd <- function() {
  data.frame(id = c("a", "b", "c", "d"), time = 1:4,
             heading = c(0.1, 0.2, 1.0, 1.0))
}

# heading_marker_layer returns a single ggplot2 layer (or NULL), so read its data
# directly.
has_cols <- function(layer, cols) {
  is.data.frame(layer$data) && all(cols %in% names(layer$data))
}

test_that("points style returns a heading-points layer", {
  l <- heading_marker_layer(make_hd(), "points", NULL, circ_display())
  expect_false(is.null(l))
  expect_true(has_cols(l, c(".x_head", ".y_head")))
})

test_that("stacked style returns a stacked-headings layer", {
  l <- heading_marker_layer(make_hd(), "stacked", NULL, circ_display())
  expect_false(is.null(l))
  expect_true(has_cols(l, c(".x_stk", ".y_stk")))
})

test_that("none style returns NULL", {
  expect_null(heading_marker_layer(make_hd(), "none", NULL, circ_display()))
})

test_that("an unknown style falls back to points", {
  l <- heading_marker_layer(make_hd(), "wat", NULL, circ_display())
  expect_true(has_cols(l, c(".x_head", ".y_head")))
})

test_that("stacked style stacks within groups when a condition column is present", {
  hd <- make_hd(); hd$arc <- c("L", "L", "R", "R")
  l <- heading_marker_layer(hd, "stacked", "arc", circ_display())
  expect_true(has_cols(l, c(".x_stk", ".y_stk")))
  # the grouping column survives so the layer can facet alongside the tracks
  expect_true("arc" %in% names(l$data))
})
