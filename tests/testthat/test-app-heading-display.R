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

test_that("stacked style actually stacks continuous headings into bins", {
  # Many continuous headings concentrated in a narrow arc: with the old
  # exact-equality grouping every dot sat at base_r (stack depth 1); after
  # binning they must form inward columns (stack depth > 1).
  set.seed(1)
  hd <- data.frame(id = seq_len(60), time = seq_len(60),
                   heading = rnorm(60, mean = 0, sd = 0.15))  # tight cluster
  l <- heading_marker_layer(hd, "stacked", NULL, circ_display())
  expect_true("stack_r" %in% names(l$data))
  expect_gt(max(l$data$stack_n), 1L)          # at least one real column
  expect_gt(length(unique(round(l$data$stack_r, 6))), 1L)  # not all at base_r
})

test_that("stacked style tolerates NA headings (undefined-heading trials)", {
  # Some heading rules return NA for trials with no defined heading. Two or more
  # NAs previously tripped a logical-index assignment in wrap_to_2pi.
  hd <- data.frame(id = 1:8, time = 1:8,
                   heading = c(0.01, 0.02, 0.03, NA, 1.0, 1.0, NA, 2.0))
  expect_no_error(
    l <- heading_marker_layer(hd, "stacked", NULL, circ_display())
  )
  expect_true("stack_r" %in% names(l$data))
})

test_that("stacked headings snap to 5-degree bin centres (centred on zero)", {
  hd <- data.frame(id = 1:3, time = 1:3,
                   heading = c(0.01, 0.02, 0.03))  # all within the 0 bin (<2.5 deg)
  l <- heading_marker_layer(hd, "stacked", NULL, circ_display())
  # snapped to the centre-on-zero bin -> all three share heading 0 and stack
  expect_equal(unique(l$data$heading), 0)
  expect_equal(max(l$data$stack_n), 3L)
})

test_that("stacked style stacks within groups when a condition column is present", {
  hd <- make_hd(); hd$arc <- c("L", "L", "R", "R")
  l <- heading_marker_layer(hd, "stacked", "arc", circ_display())
  expect_true(has_cols(l, c(".x_stk", ".y_stk")))
  # the grouping column survives so the layer can facet alongside the tracks
  expect_true("arc" %in% names(l$data))
})
