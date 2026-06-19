.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# fingerprint of a built ggplot: per-layer geom class + the rounded position
# columns of the built data. Comparing coordinates (not just geom + row count)
# means a layer drawn at the wrong orientation -- e.g. an overlay that fell back
# to the default display convention -- is caught.
.fingerprint <- function(p) {
  b <- ggplot2::ggplot_build(p)
  pos <- c("x", "y", "xend", "yend")
  coords <- lapply(b$data, function(d) {
    cols <- intersect(pos, names(d))
    m <- as.matrix(d[, cols, drop = FALSE])
    round(m[order(m[, 1], m[, 2]), , drop = FALSE], 6)
  })
  list(
    geom   = vapply(p$layers, function(l) class(l$geom)[1], character(1)),
    rows   = vapply(b$data, nrow, integer(1)),
    coords = coords
  )
}

roundtrip_spec <- function(heading_display, by, facet, arrow, vectors,
                           rayleigh = FALSE, ci = FALSE, vtest = FALSE,
                           subtitle = NULL, caption = NULL,
                           quadrants = FALSE, rings = FALSE, axial = FALSE,
                           boxplot = FALSE) {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus
  hd <- derive_headings(ts, rule = "distal")
  if (!is.null(facet)) {
    df <- as.data.frame(ts)
    hd[[facet]] <- df[[facet]][match(hd$id, df[[ts@cols$id]])]
  }
  spec <- list(
    data = list(source = "example", path = NULL, dialect = NULL),
    headings = list(rule = "distal"),
    group_col = ts@cols$id, facet_by = facet,
    colour = list(by = by, cap = 20,
                  legend = !identical(by, "trajectory") &&
                           length(unique(as.data.frame(ts)[[by]])) <= 20),
    theme = "void", angle_labels = "degrees", display = list(zero = 0),
    axial = axial,
    heading_display = heading_display,
    subtitle = subtitle, caption = caption,
    show = list(tracks = TRUE, arrow = arrow, vectors = vectors,
                rayleigh = rayleigh, ci = ci, vtest = vtest,
                quadrants = quadrants, rings = rings, boxplot = boxplot))
  list(spec = spec, ts = ts, hd = hd)
}

# Build the live figure and the eval'd emitted figure under the same RNG state,
# then compare fingerprints. The seed matters only for the bootstrap CI overlay
# (otherwise the two builds resample independently); deterministic layers are
# unaffected.
expect_roundtrip <- function(rt, seed = 1L) {
  set.seed(seed); live  <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  env <- new.env(parent = globalenv())
  set.seed(seed); evald <- eval(parse(text = spec_to_code(rt$spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
}

test_that("emitted code reproduces spec_to_plot (stacked, trajectory, faceted, arrow)", {
  rt <- roundtrip_spec("stacked", "trajectory", "type", TRUE, FALSE)
  live  <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  env   <- new.env(parent = globalenv())
  evald <- eval(parse(text = spec_to_code(rt$spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

test_that("emitted code reproduces spec_to_plot (points, distinct colour, no facet)", {
  rt <- roundtrip_spec("points", "type", NULL, FALSE, FALSE)
  live  <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  env   <- new.env(parent = globalenv())
  evald <- eval(parse(text = spec_to_code(rt$spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

test_that("emitted code reproduces spec_to_plot (Rayleigh circle, faceted)", {
  rt <- roundtrip_spec("points", "trajectory", "type", FALSE, FALSE,
                       rayleigh = TRUE)
  live  <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  env   <- new.env(parent = globalenv())
  evald <- eval(parse(text = spec_to_code(rt$spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

test_that("emitted code reproduces spec_to_plot (Rayleigh circle, no facet)", {
  rt <- roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE,
                       rayleigh = TRUE)
  live  <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  env   <- new.env(parent = globalenv())
  evald <- eval(parse(text = spec_to_code(rt$spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

# the overlay flag must actually add layer(s) to the figure (otherwise the
# round-trip below would pass vacuously, both sides simply omitting the overlay).
.n_layers <- function(rt) {
  set.seed(1); length(spec_to_plot(rt$spec, rt$ts, rt$hd)$layers)
}
expect_overlay_adds_layers <- function(field, ...) {
  off <- roundtrip_spec("points", "trajectory", ..., FALSE, FALSE)
  on  <- off
  on$spec$show[[field]] <- TRUE
  expect_gt(.n_layers(on), .n_layers(off))
}

test_that("emitted code reproduces spec_to_plot (bootstrap CI, no facet)", {
  expect_overlay_adds_layers("ci", NULL)
  expect_roundtrip(
    roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE, ci = TRUE))
})

test_that("emitted code reproduces spec_to_plot (bootstrap CI, faceted)", {
  expect_overlay_adds_layers("ci", "type")
  expect_roundtrip(
    roundtrip_spec("points", "trajectory", "type", FALSE, FALSE, ci = TRUE))
})

test_that("emitted code reproduces spec_to_plot (V-test line, no facet)", {
  expect_overlay_adds_layers("vtest", NULL)
  expect_roundtrip(
    roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE, vtest = TRUE))
})

test_that("emitted code reproduces spec_to_plot (V-test line, faceted)", {
  expect_overlay_adds_layers("vtest", "type")
  expect_roundtrip(
    roundtrip_spec("points", "trajectory", "type", FALSE, FALSE, vtest = TRUE))
})

test_that("emitted code reproduces spec_to_plot (quadrant lines + guide rings)", {
  expect_overlay_adds_layers("quadrants", NULL)
  expect_overlay_adds_layers("rings", NULL)
  expect_roundtrip(
    roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE,
                   quadrants = TRUE, rings = TRUE))
})

test_that("emitted code reproduces spec_to_plot (quadrants + rings, faceted)", {
  expect_roundtrip(
    roundtrip_spec("points", "trajectory", "type", FALSE, FALSE,
                   quadrants = TRUE, rings = TRUE))
})

test_that("emitted code reproduces spec_to_plot (radial grid on a grid theme)", {
  # On a grid-bearing theme radiate() draws the radial grid by default; verify it
  # is present and that the spec->plot / spec->code paths reproduce it identically.
  rt <- roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE)
  rt$spec$theme <- "grey"
  live  <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  geoms <- vapply(live$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% geoms)   # the radial-grid disc
  expect_roundtrip(rt)
})

test_that("emitted code reproduces spec_to_plot (crossing rule + heading vectors)", {
  # the vectors layer needs the crossing construction coords, so the emitted
  # derive_headings() must request return_coords = TRUE.
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.3, circ1 = 0.6,
                        return_coords = TRUE, on_missing = "quiet")
  spec <- list(
    data = list(source = "example", path = NULL, dialect = NULL),
    headings = list(rule = "crossing", circ0 = 0.3, circ1 = 0.6),
    group_col = ts@cols$id, facet_by = NULL,
    colour = list(by = "trajectory", cap = 20, legend = FALSE),
    theme = "bw", angle_labels = "degrees", display = list(zero = 0),
    heading_display = "points",
    show = list(tracks = TRUE, arrow = FALSE, vectors = TRUE))
  live  <- spec_to_plot(spec, ts, hd)
  env   <- new.env(parent = globalenv())
  # The emitted script keeps derive_headings()'s default attrition warning
  # (a standalone user should see it); suppress it here to keep the test clean.
  evald <- suppressWarnings(eval(parse(text = spec_to_code(spec)), envir = env))   # must not error
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

# Subtitle/caption are plot labels, not layers, so the coordinate fingerprint
# above does not see them -- assert them directly.

test_that("spec_to_plot applies the spec subtitle and caption", {
  rt <- roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE,
                       subtitle = "Heading method: distal-most point",
                       caption  = "Mean straightness: 0.62")
  p <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  expect_equal(p$labels$subtitle, "Heading method: distal-most point")
  expect_equal(p$labels$caption,  "Mean straightness: 0.62")
})

test_that("spec_to_plot omits subtitle/caption labels when the spec has none", {
  rt <- roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE)
  p  <- spec_to_plot(rt$spec, rt$ts, rt$hd)
  expect_null(p$labels$subtitle)
  expect_null(p$labels$caption)
})

test_that("spec_to_code emits the spec subtitle and caption as literal labs()", {
  rt <- roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE,
                       subtitle = "Heading method: distal-most point",
                       caption  = "Mean straightness: 0.62")
  code <- spec_to_code(rt$spec)
  expect_match(code, 'subtitle = "Heading method: distal-most point"', fixed = TRUE)
  expect_match(code, 'caption = "Mean straightness: 0.62"', fixed = TRUE)
  # the emitted code must run and carry the labels through
  env <- new.env(parent = globalenv())
  p   <- eval(parse(text = code), envir = env)
  expect_equal(p$labels$subtitle, "Heading method: distal-most point")
  expect_equal(p$labels$caption,  "Mean straightness: 0.62")
})

test_that("spec_to_code quotes special characters in subtitle/caption", {
  rt <- roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE,
                       subtitle = 'has "quotes" and \\ backslash')
  code <- spec_to_code(rt$spec)
  env  <- new.env(parent = globalenv())
  p    <- eval(parse(text = code), envir = env)   # must not error on the quoting
  expect_equal(p$labels$subtitle, 'has "quotes" and \\ backslash')
})

test_that("round-trip: headings example reproduces the rendered figure", {
  data(cpunctatus, package = "radiatR")
  hd0 <- derive_headings(cpunctatus, rule = "distal", coords = "relative")
  cond <- unique(as.data.frame(cpunctatus)[, c("trial_id", "type")])
  hd0  <- merge(hd0, cond, by.x = "id", by.y = "trial_id", all.x = TRUE)
  hd   <- build_headings_input(hd0, col = "heading", units = "radians",
                               convention = "unit_circle", group = "type")

  spec <- build_plot_spec(
    ts = NULL, hd = hd, method = NULL,
    data = list(source = "example", mode = "headings", path = NULL,
                col = "heading", units = "radians", convention = "unit_circle",
                group = "type"),
    inputs = list(colour_by = "type", cond_col = "type",
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees",
                  show_arrow = TRUE, show_rayleigh = TRUE)
  )

  set.seed(1L); live  <- spec_to_plot(spec, NULL, hd)
  env <- new.env(parent = globalenv())
  set.seed(1L); evald <- eval(parse(text = spec_to_code(spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

test_that("round-trip: headings uploaded file with a non-'heading' column", {
  set.seed(7)
  raw <- data.frame(
    angle_deg = c(10, 20, 30, 200, 210, 5, 95, 185, 275, 100),
    cond      = rep(c("a", "b"), 5),
    stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(raw, tmp, row.names = FALSE)

  hd <- build_headings_input(raw, col = "angle_deg", units = "degrees",
                             convention = "unit_circle", group = "cond")
  spec <- build_plot_spec(
    ts = NULL, hd = hd, method = NULL,
    data = list(source = "file", mode = "headings", path = tmp,
                col = "angle_deg", units = "degrees",
                convention = "unit_circle", group = "cond"),
    inputs = list(colour_by = "cond", cond_col = "cond",
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees", show_arrow = TRUE))

  set.seed(1L); live  <- spec_to_plot(spec, NULL, hd)
  env <- new.env(parent = globalenv())
  set.seed(1L); evald <- eval(parse(text = spec_to_code(spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

test_that("spec_to_plot honours spec$axial (double-headed arrow)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts   <- cpunctatus
  hd   <- derive_headings(ts, rule = "distal")
  spec <- list(
    data = list(source = "example", path = NULL, dialect = NULL),
    headings = list(rule = "distal"),
    group_col = ts@cols$id, facet_by = NULL, axial = TRUE,
    colour = list(by = "trajectory", cap = 20, legend = FALSE),
    theme = "void", angle_labels = "degrees", display = list(zero = 0),
    heading_display = "points",
    show = list(tracks = FALSE, arrow = TRUE, vectors = FALSE,
                rayleigh = FALSE, ci = FALSE, vtest = FALSE,
                quadrants = FALSE, rings = FALSE))
  p <- spec_to_plot(spec, ts = ts, hd = hd)
  # the mean arrow is the only GeomSegment whose data uses the .x/.xend columns
  arrow_layers <- Filter(function(l)
    inherits(l$geom, "GeomSegment") && ".x" %in% names(l$data), p$layers)
  expect_length(arrow_layers, 1L)
  seg <- arrow_layers[[1]]$data
  expect_equal(seg$.x, -seg$.xend)      # axial arrow: segment through the origin
})

test_that("round-trip: headings uploaded file, single colour (no group), stacked", {
  set.seed(8)
  raw <- data.frame(theta = c(0.1, 0.3, 0.35, 1.2, 1.25, 3.0, 3.05, 5.5),
                    stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(raw, tmp, row.names = FALSE)

  hd <- build_headings_input(raw, col = "theta", units = "radians",
                             convention = "unit_circle", group = NULL)
  spec <- build_plot_spec(
    ts = NULL, hd = hd, method = NULL,
    data = list(source = "file", mode = "headings", path = tmp,
                col = "theta", units = "radians",
                convention = "unit_circle", group = NULL),
    inputs = list(colour_by = NULL, cond_col = NULL,
                  heading_display = "stacked", plot_theme = "void",
                  angle_labels = "degrees"))

  set.seed(1L); live  <- spec_to_plot(spec, NULL, hd)
  env <- new.env(parent = globalenv())
  set.seed(1L); evald <- eval(parse(text = spec_to_code(spec)), envir = env)
  expect_equal(.fingerprint(evald), .fingerprint(live))
})

test_that("emitted code reproduces spec_to_plot (axial: double-headed arrow + CI)", {
  rt <- roundtrip_spec("stacked", "trajectory", NULL, arrow = TRUE,
                       vectors = FALSE, ci = TRUE, axial = TRUE)
  expect_roundtrip(rt)
})

# Axial individual-data round-trips: mirroring doubles the points/stacked rows,
# so these only pass if spec_to_code emits axial = TRUE and the eval'd figure
# mirrors identically. (Guard below asserts the fingerprint depends on the flag.)
.axial_doubles_rows <- function(rt) {
  on  <- rt
  off <- rt; off$spec$axial <- FALSE
  set.seed(1L); n_on  <- sum(vapply(ggplot2::ggplot_build(
    spec_to_plot(on$spec,  on$ts,  on$hd))$data,  nrow, integer(1)))
  set.seed(1L); n_off <- sum(vapply(ggplot2::ggplot_build(
    spec_to_plot(off$spec, off$ts, off$hd))$data, nrow, integer(1)))
  expect_gt(n_on, n_off)
}

test_that("emitted code reproduces spec_to_plot (points, axial)", {
  rt <- roundtrip_spec("points", "trajectory", NULL, FALSE, FALSE, axial = TRUE)
  .axial_doubles_rows(rt)   # mirroring must actually add rows (not vacuous)
  expect_roundtrip(rt)
})

test_that("emitted code reproduces spec_to_plot (stacked, faceted, axial)", {
  rt <- roundtrip_spec("stacked", "trajectory", "type", FALSE, FALSE, axial = TRUE)
  .axial_doubles_rows(rt)   # mirroring must actually add rows (not vacuous)
  expect_roundtrip(rt)
})

test_that("circular boxplot overlay round-trips (directional)", {
  rt <- roundtrip_spec("points", by = "trajectory", facet = NULL,
                       arrow = FALSE, vectors = FALSE, boxplot = TRUE)
  live <- suppressWarnings(spec_to_plot(rt$spec, rt$ts, rt$hd))
  expect_true(any(vapply(live$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))))
  code <- spec_to_code(rt$spec)
  expect_match(code, "add_circular_boxplot(hd)", fixed = TRUE)
  suppressWarnings(expect_roundtrip(rt))
})

test_that("circular boxplot overlay round-trips (axial) and emits axial = TRUE", {
  rt <- roundtrip_spec("points", by = "trajectory", facet = NULL,
                       arrow = FALSE, vectors = FALSE, axial = TRUE, boxplot = TRUE)
  code <- spec_to_code(rt$spec)
  expect_match(code, "add_circular_boxplot(hd, axial = TRUE)", fixed = TRUE)
  suppressWarnings(expect_roundtrip(rt))
})
