.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# structural fingerprint of a built ggplot: per-layer geom class + row count
.fingerprint <- function(p) {
  b <- ggplot2::ggplot_build(p)
  data.frame(
    geom = vapply(p$layers, function(l) class(l$geom)[1], character(1)),
    rows = vapply(b$data, nrow, integer(1))
  )
}

roundtrip_spec <- function(heading_display, by, facet, arrow, vectors) {
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
    theme = "bw", angle_labels = "degrees", display = list(zero = 0),
    heading_display = heading_display,
    show = list(tracks = TRUE, arrow = arrow, vectors = vectors))
  list(spec = spec, ts = ts, hd = hd)
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

test_that("emitted code reproduces spec_to_plot (crossing rule + heading vectors)", {
  # the vectors layer needs the crossing construction coords, so the emitted
  # derive_headings() must request return_coords = TRUE.
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus
  hd <- derive_headings(ts, rule = "crossing", circ0 = 0.3, circ1 = 0.6,
                        return_coords = TRUE)
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
  evald <- eval(parse(text = spec_to_code(spec)), envir = env)   # must not error
  expect_equal(.fingerprint(evald), .fingerprint(live))
})
