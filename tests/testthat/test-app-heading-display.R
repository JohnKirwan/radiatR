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

test_that("attach_cycle_colour reproduces radiate's per-trajectory cycle index", {
  ordered <- c("t3", "t1", "t2", "t4")          # track-data trajectory order
  hd <- data.frame(id = c("t1", "t2", "t3", "t4", "t1"), heading = 0)
  out <- attach_cycle_colour(hd, ordered_ids = ordered, n = 20L)
  # index = position of each id in `ordered`, cycled mod n
  expect_equal(as.integer(out$.cycle_colour), c(2L, 3L, 1L, 4L, 2L))
  expect_identical(levels(out$.cycle_colour), as.character(1:20))
})

test_that("attach_cycle_colour wraps the cycle past n trajectories", {
  ordered <- paste0("t", 1:25)
  hd <- data.frame(id = c("t1", "t21", "t25"), heading = 0)
  out <- attach_cycle_colour(hd, ordered_ids = ordered, n = 20L)
  # t21 -> index 21 wraps to 1; t25 -> 5
  expect_equal(as.integer(out$.cycle_colour), c(1L, 1L, 5L))
})

test_that("colour_col maps dot colour for both points and stacked styles", {
  hd <- make_hd(); hd$grp <- c("a", "b", "a", "b")
  lp <- heading_marker_layer(hd, "points",  NULL, circ_display(), colour_col = "grp")
  ls <- heading_marker_layer(hd, "stacked", NULL, circ_display(), colour_col = "grp")
  expect_true("colour" %in% names(lp$mapping))
  expect_true("colour" %in% names(ls$mapping))
})

test_that("no colour_col leaves dots a fixed colour (no colour aesthetic)", {
  l <- heading_marker_layer(make_hd(), "points", NULL, circ_display())
  expect_false("colour" %in% names(l$mapping))
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

test_that("default stacked dots sit inside the rim with the wider step", {
  set.seed(1)
  hd <- data.frame(id = seq_len(40), time = seq_len(40),
                   heading = rnorm(40, 0, 0.1))   # one tight column
  l <- heading_marker_layer(hd, "stacked", NULL, circ_display())
  r <- sort(unique(round(l$data$stack_r, 6)), decreasing = TRUE)
  # outermost dot is start_sep (0.035) inside the unit circle -> abuts, not on it
  expect_equal(r[1], 1 - STACK_START_SEP, tolerance = 1e-9)
  expect_lt(r[1], 1)
  # successive dots are STACK_STEP apart (wider than circular's 0.025)
  expect_equal(r[1] - r[2], STACK_STEP, tolerance = 1e-9)
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
