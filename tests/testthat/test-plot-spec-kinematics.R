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
  expect_false(grepl("smooth", code2, fixed = TRUE))                 # default omits smooth
})

test_that("kinematics spec carries and emits smoothing only when set", {
  ts <- kin_ts()
  sp <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_smooth = 7,
                                       kin_show_raw = TRUE, fps = 30,
                                       data = example_data_block))
  expect_equal(sp$smooth, 7)
  expect_true(isTRUE(sp$show_raw))
  expect_match(spec_to_kinematics_code(sp),
               'plot_profile(ts, metric = "speed", smooth = 7, show_raw = TRUE)',
               fixed = TRUE)
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

test_that("build_kinematics_spec carries a valid track id, else NULL", {
  ts  <- kin_ts()
  id1 <- as.character(ids(ts))[1]
  sp  <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_track = id1,
                                        fps = 30, data = example_data_block))
  expect_identical(sp$track, id1)
  expect_null(build_kinematics_spec(ts, list(kin_track = "",   fps = 30,
                                             data = example_data_block))$track)
  expect_null(build_kinematics_spec(ts, list(kin_track = "nope", fps = 30,
                                             data = example_data_block))$track)
})

test_that("kinematics_spec_to_plot filters to one track when track set", {
  ts  <- kin_ts()
  id1 <- as.character(ids(ts))[1]
  sp  <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_track = id1,
                                        fps = 30, data = example_data_block))
  p   <- kinematics_spec_to_plot(sp, ts)
  b   <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$group)), 1L)   # one track only
})

test_that("spec_to_kinematics_code emits the track subset; round-trips", {
  ts  <- kin_ts()
  id1 <- as.character(ids(ts))[1]
  sp  <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_track = id1,
                                        fps = 30, data = example_data_block))
  code <- spec_to_kinematics_code(sp)
  expect_true(grepl(sprintf('ts <- ts["%s"]', id1), code, fixed = TRUE))
  live  <- kinematics_spec_to_plot(sp, ts)
  evald <- eval(parse(text = code), envir = new.env(parent = globalenv()))
  expect_equal(.kfingerprint(evald), .kfingerprint(live))
})

test_that("spec_to_kinematics_code emits a comment, not a fabricated 30, when fps is unset", {
  ts <- kin_ts()
  sp <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_colour_by = "",
                                       fps = NA_real_, data = example_data_block))
  code <- paste(spec_to_kinematics_code(sp), collapse = "\n")
  expect_false(grepl("set_frame_rate(ts, 30)", code, fixed = TRUE))
  expect_match(code, "set a frame rate to enable kinematics")
})

test_that("spec_to_kinematics_code emits the real fps when set", {
  ts <- kin_ts()
  sp <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_colour_by = "",
                                       fps = 60, data = example_data_block))
  code <- paste(spec_to_kinematics_code(sp), collapse = "\n")
  expect_match(code, "set_frame_rate(ts, 60)", fixed = TRUE)
})

test_that("kinematics render with POSIXct time needs no fps (set_frame_rate not called on NA)", {
  t0 <- as.POSIXct("2020-01-01", tz = "UTC")
  d <- data.frame(id = "a", frame = t0 + c(0, 1, 2, 3),
                  x = c(0, 1, 2, 3), y = c(0, 1, 0, 1), angle = 0)
  ts <- methods::new("Tracks", data = d,
    cols = list(id = "id", time = "frame", angle = "angle", x = "x", y = "y"),
    angle_unit = "radians", meta = list())
  sp <- build_kinematics_spec(ts, list(kin_metric = "speed", kin_colour_by = "",
                                       fps = NA_real_, data = example_data_block))
  expect_s3_class(kinematics_spec_to_plot(sp, ts), "ggplot")
})
