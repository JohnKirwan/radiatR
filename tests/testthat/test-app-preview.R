# preview.R is a shiny-free helper sourced by the app; source it directly.
source(testthat::test_path("..", "..", "inst", "app", "preview.R"),
       local = TRUE)

test_that("demo_tracks returns a cached TrajSet subset of cpunctatus", {
  d <- demo_tracks()
  expect_s4_class(d, "TrajSet")

  e <- new.env()
  utils::data("cpunctatus", package = "radiatR", envir = e)
  full <- e$cpunctatus
  idc  <- full@cols$id

  demo_ids <- unique(d@data[[idc]])
  full_ids <- unique(full@data[[idc]])
  expect_gte(length(demo_ids), 2L)
  expect_true(all(demo_ids %in% full_ids))
  expect_lt(length(demo_ids), length(full_ids))   # a strict subset

  # cached: identical object on second call
  expect_identical(demo_tracks(), d)
})

test_that("build_method_preview builds a ggplot for every exposed method", {
  d <- demo_tracks()
  methods <- c("none", "distal", "net", "crossing", "straight", "window_net",
               "origin_mean", "velocity_mean", "maxspeed_window", "vm_fit",
               "pca_axis", "ransac_straight", "goal_bias")
  for (m in methods) {
    p <- build_method_preview(d, m, circ0 = 0.3, circ1 = 0.6)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("crossing draws two rings that other methods do not", {
  d <- demo_tracks()
  # Inner rings are annotate('path') circles of ~1000 points with radius < 1.
  n_inner_rings <- function(p) {
    sum(vapply(p$layers, function(l) {
      dd <- l$data
      inherits(l$geom, "GeomPath") && is.data.frame(dd) &&
        all(c("x", "y") %in% names(dd)) && nrow(dd) >= 999L &&
        max(sqrt(dd$x^2 + dd$y^2)) < 0.99
    }, logical(1)))
  }
  expect_equal(n_inner_rings(build_method_preview(d, "crossing", 0.3, 0.6)), 2L)
  expect_equal(n_inner_rings(build_method_preview(d, "distal",   0.3, 0.6)), 0L)
})

test_that("only crossing draws heading vectors (segments), never an origin ray", {
  d <- demo_tracks()
  n_segments <- function(p) {
    sum(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"),
               logical(1)))
  }
  # The base plot already carries a tick segment layer, so compare relative
  # counts. Non-crossing methods add no segment (the origin ray was removed) and
  # so match each other; crossing adds exactly one extra layer for its dashed
  # inner->boundary heading vectors.
  base_segs <- n_segments(build_method_preview(d, "distal", 0.3, 0.6))
  expect_equal(n_segments(build_method_preview(d, "net",  0.3, 0.6)), base_segs)
  expect_equal(n_segments(build_method_preview(d, "none", 0.3, 0.6)), base_segs)
  expect_equal(n_segments(build_method_preview(d, "crossing", 0.3, 0.6)),
               base_segs + 1L)
})

test_that("crossing heading vector spans the two rings, not the rim", {
  d <- demo_tracks()
  circ0 <- 0.3; circ1 <- 0.6
  p <- build_method_preview(d, "crossing", circ0, circ1)

  seg <- NULL
  for (l in p$layers) {
    dd <- l$data
    if (inherits(l$geom, "GeomSegment") && is.data.frame(dd) &&
        all(c("x_in", "y_in", "x_out", "y_out") %in% names(dd))) {
      seg <- dd
      break
    }
  }
  expect_false(is.null(seg))

  r_in  <- sqrt(seg$x_in^2  + seg$y_in^2)
  r_out <- sqrt(seg$x_out^2 + seg$y_out^2)
  # Outer endpoint sits exactly on the outer ring (placed there by construction),
  # never extrapolated to the unit boundary as the old origin-to-rim vector was.
  expect_true(all(abs(r_out - circ1) < 1e-6))
  # Inner endpoint sits at the inner ring crossing, near circ0 and far from rim.
  expect_true(all(r_in < circ0 + 0.1))
})

test_that("none mode draws no heading points", {
  d <- demo_tracks()
  has_points <- function(p)
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"),
               logical(1)))
  expect_false(has_points(build_method_preview(d, "none", 0.3, 0.6)))
  expect_true(has_points(build_method_preview(d, "distal", 0.3, 0.6)))
})
