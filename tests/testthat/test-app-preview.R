# preview.R and preview_constructions.R are shiny-free helpers sourced by the app;
# source them directly. Under R CMD check the package is installed, so the files
# land at system.file("app", ...); under devtools::test() the source tree is used
# and the relative path resolves. Try the installed location first, then source.
for (.f in c("preview.R", "preview_constructions.R")) {
  .p <- system.file("app", .f, package = "radiatR")
  if (!nzchar(.p) || !file.exists(.p)) {
    .p <- testthat::test_path("..", "..", "inst", "app", .f)
  }
  source(.p, local = TRUE)
}

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
  # inner-crossing->rim heading vector.
  base_segs <- n_segments(build_method_preview(d, "distal", 0.3, 0.6))
  expect_equal(n_segments(build_method_preview(d, "net",  0.3, 0.6)), base_segs)
  expect_equal(n_segments(build_method_preview(d, "none", 0.3, 0.6)), base_segs)
  expect_equal(n_segments(build_method_preview(d, "crossing", 0.3, 0.6)),
               base_segs + 1L)
})

test_that("crossing vector runs from inner crossing to the unit-circle rim", {
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
  # Vector extends through the crossings out to the unit-circle periphery.
  expect_true(all(abs(r_out - 1) < 1e-6))
  # Inner endpoint sits at the inner ring crossing, near circ0 and far from rim.
  expect_true(all(r_in < circ0 + 0.1))
})

test_that("crossing marks dots on both detection rings", {
  d <- demo_tracks()
  circ0 <- 0.3; circ1 <- 0.6
  p <- build_method_preview(d, "crossing", circ0, circ1)

  dots <- NULL
  for (l in p$layers) {
    dd <- l$data
    if (inherits(l$geom, "GeomPoint") && is.data.frame(dd) &&
        all(c("px", "py") %in% names(dd))) {
      dots <- dd
      break
    }
  }
  expect_false(is.null(dots))

  r <- sqrt(dots$px^2 + dots$py^2)
  expect_true(any(abs(r - circ1) < 1e-6))  # a dot exactly on the outer ring
  expect_true(any(r < circ0 + 0.1))        # a dot at the inner ring crossing
})

test_that("heading markers and crossing vector are coloured per trajectory", {
  d <- demo_tracks()
  has_colour <- function(l) "colour" %in% names(l$mapping)

  # A non-crossing method colours its heading point per track.
  pd <- build_method_preview(d, "distal", 0.3, 0.6)
  pt <- Find(function(l) inherits(l$geom, "GeomPoint"), pd$layers)
  expect_false(is.null(pt))
  expect_true(has_colour(pt))

  # Crossing colours its dashed chord per track.
  pc <- build_method_preview(d, "crossing", 0.3, 0.6)
  segl <- Find(function(l) inherits(l$geom, "GeomSegment") &&
                 is.data.frame(l$data) && "x_in" %in% names(l$data), pc$layers)
  expect_false(is.null(segl))
  expect_true(has_colour(segl))
})

test_that("none mode draws no heading points", {
  d <- demo_tracks()
  has_points <- function(p)
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"),
               logical(1)))
  expect_false(has_points(build_method_preview(d, "none", 0.3, 0.6)))
  expect_true(has_points(build_method_preview(d, "distal", 0.3, 0.6)))
})
