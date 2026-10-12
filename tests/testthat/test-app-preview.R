# preview.R and preview_constructions.R are shiny-free helpers sourced by the app;
# source them directly. Under R CMD check the package is installed, so the files
# land at system.file("app", ...); under devtools::test() the source tree is used
# and the relative path resolves. Try the installed location first, then source.
for (.f in c("plot_spec.R", "preview.R", "preview_constructions.R")) {
  .p <- system.file("app", .f, package = "radiatR")
  if (!nzchar(.p) || !file.exists(.p)) {
    .p <- testthat::test_path("..", "..", "inst", "app", .f)
  }
  source(.p, local = TRUE)
}

test_that("demo_tracks returns a cached Tracks subset of cpunctatus", {
  d <- demo_tracks()
  expect_s4_class(d, "Tracks")

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

# Identify a method's own construction layers (distinct from base ticks, the
# crossing geometry, and the heading-point layer).
constr_point <- function(p) {
  Find(function(l) inherits(l$geom, "GeomPoint") && is.data.frame(l$data) &&
         all(c("px", "py") %in% names(l$data)), p$layers)
}
constr_segment <- function(p) {
  Find(function(l) inherits(l$geom, "GeomSegment") && is.data.frame(l$data) &&
         all(c("x0", "y0", "x1", "y1") %in% names(l$data)), p$layers)
}

test_that("distal draws a furthest-point marker", {
  d <- demo_tracks()
  expect_false(is.null(constr_point(build_method_preview(d, "distal", 0.3, 0.6))))
})

test_that("net, straight and pca_axis each draw a construction segment", {
  d <- demo_tracks()
  for (m in c("net", "straight", "pca_axis")) {
    expect_false(is.null(constr_segment(build_method_preview(d, m, 0.3, 0.6))),
                 info = m)
  }
})

test_that("a construction-less in-dropdown method shows only its heading point", {
  d <- demo_tracks()
  p <- build_method_preview(d, "vm_fit", 0.3, 0.6)
  expect_null(constr_point(p))
  expect_null(constr_segment(p))
  # but the common heading point is still drawn
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"),
                         logical(1))))
})

test_that("pca_axis construction segment is clipped to the unit circle", {
  d <- demo_tracks()
  seg <- constr_segment(build_method_preview(d, "pca_axis", 0.3, 0.6))
  expect_false(is.null(seg))
  r0 <- sqrt(seg$x0^2 + seg$y0^2)
  r1 <- sqrt(seg$x1^2 + seg$y1^2)
  expect_true(all(abs(r0 - 1) < 1e-6))
  expect_true(all(abs(r1 - 1) < 1e-6))
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
  # The outer dot is the interpolated outer crossing, so it sits near (not
  # exactly on) circ1; the inner dot sits near the inner ring crossing.
  expect_true(any(abs(r - circ1) < 0.05))
  expect_true(any(r < circ0 + 0.1))
})

test_that("crossing heading marker coincides with the vector's rim endpoint", {
  d <- demo_tracks()
  p <- build_method_preview(d, "crossing", 0.3, 0.6)

  seg <- Find(function(l) inherits(l$geom, "GeomSegment") && is.data.frame(l$data) &&
                all(c("x_out", "y_out") %in% names(l$data)), p$layers)
  hp  <- Find(function(l) inherits(l$geom, "GeomPoint") && is.data.frame(l$data) &&
                all(c(".x_head", ".y_head") %in% names(l$data)), p$layers)
  expect_false(is.null(seg))
  expect_false(is.null(hp))

  # the dashed vector's rim endpoint is exactly the heading marker for each track
  expect_equal(sort(seg$x_out), sort(hp$.x_head), tolerance = 1e-9)
  expect_equal(sort(seg$y_out), sort(hp$.y_head), tolerance = 1e-9)
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

test_that("the RANSAC preview is drawn under the app seed, leaving the caller's RNG alone", {
  d <- demo_tracks()
  set.seed(1); expected <- runif(1)
  set.seed(1); p1 <- build_method_preview(d, "ransac_straight"); got <- runif(1)
  expect_identical(got, expected)
  set.seed(2); p2 <- build_method_preview(d, "ransac_straight")
  expect_equal(ggplot2::ggplot_build(p2)$data, ggplot2::ggplot_build(p1)$data)
})

# --- Heading frame ----------------------------------------------------------

# Interior points of the drawn tracks (clipping only adds points on the rim, and
# NA rows where it breaks a path).
drawn_track_xy <- function(p) {
  trk <- ggplot2::layer_data(p, 1L)
  trk[which(sqrt(trk$x^2 + trk$y^2) < 0.99), c("x", "y")]
}
xy_key <- function(x, y) paste(signif(x, 10), signif(y, 10))

test_that("the preview draws tracks and headings in the requested frame", {
  d <- demo_tracks()
  for (frame in c("relative", "absolute")) {
    p  <- build_method_preview(d, "distal", coords = frame)
    xc <- if (frame == "relative") d@cols$rel_x else d@cols$x
    yc <- if (frame == "relative") d@cols$rel_y else d@cols$y
    trk <- drawn_track_xy(p)
    expect_true(all(xy_key(trk$x, trk$y) %in% xy_key(d@data[[xc]], d@data[[yc]])),
                info = frame)

    hd <- derive_headings(d, rule = "distal", coords = frame, return_coords = TRUE)
    hd <- hd[is.finite(hd$heading), , drop = FALSE]
    pt <- constr_point(p)$data
    expect_equal(pt$px, hd$x_distal, info = frame)
    expect_equal(pt$py, hd$y_distal, info = frame)
  }
})

test_that("the frames give different previews on the demo tracks", {
  d <- demo_tracks()
  rel <- constr_point(build_method_preview(d, "distal", coords = "relative"))$data
  abs <- constr_point(build_method_preview(d, "distal", coords = "absolute"))$data
  expect_false(isTRUE(all.equal(rel[c("px", "py")], abs[c("px", "py")])))
})

test_that("crossing previews in the absolute frame", {
  d  <- demo_tracks()
  p  <- build_method_preview(d, "crossing", 0.3, 0.6, coords = "absolute")
  hd <- derive_headings(d, rule = "crossing", coords = "absolute",
                        circ0 = 0.3, circ1 = 0.6, return_coords = TRUE)
  hd <- hd[is.finite(hd$heading), , drop = FALSE]
  seg <- Find(function(l) inherits(l$geom, "GeomSegment") && is.data.frame(l$data) &&
                "x_in" %in% names(l$data), p$layers)$data
  expect_equal(seg$x_in, hd$x_inner)
  expect_equal(seg$y_in, hd$y_inner)
})

test_that("a relative request floors to absolute without a landmark frame", {
  d <- demo_tracks()
  d@cols$rel_x <- NULL
  d@cols$rel_y <- NULL
  expect_equal(
    ggplot2::ggplot_build(build_method_preview(d, "distal", coords = "relative"))$data,
    ggplot2::ggplot_build(build_method_preview(d, "distal", coords = "absolute"))$data)
})

test_that("the app previews in the frame the Results will derive in", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    expect_equal(preview_coords(), "relative")
    session$setInputs(frame = "absolute")
    expect_equal(preview_coords(), "absolute")

    # No landmark frame: the control stays on "relative" but Results are absolute.
    ts <- rv$ts
    ts@cols$rel_x <- NULL
    ts@cols$rel_y <- NULL
    rv$ts <- ts
    session$setInputs(frame = "relative")
    expect_equal(preview_coords(), "absolute")
  })
})
