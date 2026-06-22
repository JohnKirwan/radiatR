# Source the app's plot_spec.R (installed under R CMD check, source tree under devtools).
.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# Fingerprint a built ggplot: per-layer geom + rounded positional columns.
.kfingerprint <- function(p) {
  b <- ggplot2::ggplot_build(p)
  pos <- c("x", "y", "xend", "yend")
  coords <- lapply(b$data, function(d) {
    cols <- intersect(pos, names(d))
    m <- as.matrix(d[, cols, drop = FALSE])
    round(m[order(m[, 1], m[, 2]), , drop = FALSE], 6)
  })
  list(geom = vapply(p$layers, function(l) class(l$geom)[1], character(1)),
       rows = vapply(b$data, nrow, integer(1)), coords = coords)
}

kin_ts <- function() {
  data(cpunctatus, package = "radiatR", envir = environment())
  cpunctatus
}

example_data_block <- list(source = "example", mode = "trajectories",
                           path = "your_tracks.csv", dialect = NULL)

test_that("build_kinematics_spec carries metric/units/fps and validates colour_by", {
  ts <- kin_ts()
  sp <- build_kinematics_spec(ts, list(kin_metric = "turning", kin_units = "degrees",
                                       kin_colour_by = "type", fps = 25,
                                       data = example_data_block))
  expect_identical(sp$metric, "turning")
  expect_identical(sp$units, "degrees")
  expect_identical(sp$fps, 25)
  expect_identical(sp$mode, "trajectories")
  expect_identical(sp$headings$rule, "none")
  expect_true(sp$colour_by %in% names(as.data.frame(ts)))

  expect_null(build_kinematics_spec(ts, list(kin_colour_by = "",  fps = 30,
                                             data = example_data_block))$colour_by)
  expect_null(build_kinematics_spec(ts, list(kin_colour_by = "nope", fps = 30,
                                             data = example_data_block))$colour_by)
})

test_that("kinematics_spec_to_plot is a faithful plot_profile wrapper", {
  ts <- kin_ts()
  sp <- build_kinematics_spec(ts, list(kin_metric = "speed", fps = 30,
                                       data = example_data_block))
  p <- kinematics_spec_to_plot(sp, ts)
  expect_s3_class(p, "ggplot")
  ref <- plot_profile(set_frame_rate(ts, 30), metric = "speed")
  expect_equal(.kfingerprint(p), .kfingerprint(ref))
})

test_that("spec_to_kinematics_code emits a runnable, exported-only script", {
  ts <- kin_ts()
  sp <- build_kinematics_spec(ts, list(kin_metric = "turning", kin_units = "degrees",
                                       kin_colour_by = "type", fps = 25,
                                       data = example_data_block))
  code <- spec_to_kinematics_code(sp)
  expect_silent(parse(text = code))
  expect_true(grepl("library(radiatR)", code, fixed = TRUE))
  expect_true(grepl("ts <- cpunctatus", code, fixed = TRUE))
  expect_false(grepl("derive_headings", code, fixed = TRUE))
  expect_true(grepl("set_frame_rate(ts, 25)", code, fixed = TRUE))
  expect_true(grepl('plot_profile(ts, metric = "turning", units = "degrees", colour_by = "type")',
                    code, fixed = TRUE))

  sp2 <- build_kinematics_spec(ts, list(kin_metric = "speed", fps = 30,
                                        data = example_data_block))
  code2 <- spec_to_kinematics_code(sp2)
  expect_true(grepl('plot_profile(ts, metric = "speed")', code2, fixed = TRUE))
  expect_false(grepl("units =", code2, fixed = TRUE))
})

test_that("emitted kinematics code reproduces kinematics_spec_to_plot", {
  ts <- kin_ts()
  sp <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_colour_by = "type",
                                       fps = 30, data = example_data_block))
  live  <- kinematics_spec_to_plot(sp, ts)
  evald <- eval(parse(text = spec_to_kinematics_code(sp)),
                envir = new.env(parent = globalenv()))
  expect_equal(.kfingerprint(evald), .kfingerprint(live))
})
