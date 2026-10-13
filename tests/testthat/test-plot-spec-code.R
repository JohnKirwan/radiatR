.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

example_spec <- function(heading_display = "stacked", facet = "type",
                         by = "trajectory", arrow = TRUE, vectors = FALSE,
                         legend = FALSE, rule = "crossing") {
  list(
    data = list(source = "file", path = "tracks.csv", dialect = "ethovision"),
    headings = if (rule == "crossing") list(rule = "crossing", circ0 = 0.3, circ1 = 0.6)
               else list(rule = rule),
    group_col = "trial_id", facet_by = facet,
    colour = list(by = by, cap = 20, legend = legend),
    theme = "bw", angle_labels = "degrees", display = list(zero = 0),
    heading_display = heading_display,
    show = list(tracks = TRUE, arrow = arrow, vectors = vectors))
}

test_that("spec_to_code emits a runnable, exported-only script", {
  code <- spec_to_code(example_spec())
  expect_true(grepl("library(radiatR)", code, fixed = TRUE))
  expect_true(grepl('read_tracks("tracks.csv", dialect = "ethovision")', code, fixed = TRUE))
  expect_true(grepl('derive_headings(ts, rule = "crossing", circ0 = 0.3, circ1 = 0.6, coords = "relative")', code, fixed = TRUE))
  expect_true(grepl('by.x = "id"', code, fixed = TRUE))                       # facet merge onto hd
  expect_true(grepl('assign_colour_key(ts, by = "trajectory")', code, fixed = TRUE))
  expect_true(grepl("bin_angles(hd$heading", code, fixed = TRUE))            # stacked
  expect_true(grepl('add_stacked_headings(hd, colour_col = ".colour", group = "type"', code, fixed = TRUE))
  expect_true(grepl('compute_circ_mean(hd, facets = "type")', code, fixed = TRUE))
  expect_true(grepl('add_circ_mean(arrow_df, colour = "black")', code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("spec_to_code: example source emits data(cpunctatus)", {
  sp <- example_spec(); sp$data <- list(source = "example", path = NULL, dialect = NULL)
  code <- spec_to_code(sp)
  expect_true(grepl("data(cpunctatus)", code, fixed = TRUE))
  expect_false(grepl("read_tracks", code, fixed = TRUE))
})

test_that("spec_to_code: points display, distinct colour + legend, no facet", {
  code <- spec_to_code(example_spec(heading_display = "points", by = "arc",
                                    facet = NULL, arrow = FALSE, legend = TRUE))
  expect_true(grepl("add_heading_points(", code, fixed = TRUE))
  expect_false(grepl("bin_angles", code, fixed = TRUE))
  expect_true(grepl('labs(colour = "arc")', code, fixed = TRUE))
  expect_false(grepl("facets", code, fixed = TRUE))   # no facet
  expect_false(grepl("by.x =", code, fixed = TRUE))     # no facet merge
  expect_silent(parse(text = code))
})

test_that("spec_to_code: rule none -> tracks only, no derive_headings", {
  code <- spec_to_code(example_spec(rule = "none"))
  expect_false(grepl("derive_headings", code, fixed = TRUE))
  expect_false(grepl("add_stacked_headings|add_heading_points", code))
  expect_silent(parse(text = code))
})

test_that("spec_to_code: headings file mode emits headings_frame + frame-only radiate", {
  df <- data.frame(dir = c(0, 90, 180), cond = c("a","a","b"))
  hf <- build_headings_input(df, col = "dir", units = "degrees",
                             convention = "clock", group = "cond")
  spec <- build_plot_spec(
    ts = NULL, hd = hf, method = NULL,
    data = list(source = "file", mode = "headings", path = "angles.csv",
                col = "dir", units = "degrees", convention = "clock",
                group = "cond"),
    inputs = list(colour_by = "cond", cond_col = "cond",
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees")
  )
  code <- spec_to_code(spec)
  expect_match(code, "headings_frame\\(", fixed = FALSE)
  expect_match(code, "show_markers = FALSE", fixed = TRUE)
  expect_match(code, "read.csv\\(", fixed = FALSE)
})

test_that("spec_to_code: headings example mode emits derive_headings(cpunctatus)", {
  hf <- structure(
    data.frame(id = "t1", time = 1, heading = 0.5, cond = "a"),
    class = c("headings_frame", "data.frame"), heading_col = "heading")
  spec <- build_plot_spec(
    ts = NULL, hd = hf, method = NULL,
    data = list(source = "example", mode = "headings", path = NULL,
                col = "heading", units = "radians", convention = "unit_circle",
                group = "cond"),
    inputs = list(colour_by = "cond", cond_col = "cond",
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees")
  )
  code <- spec_to_code(spec)
  expect_match(code, "derive_headings(cpunctatus", fixed = TRUE)
})

test_that("spec_to_code: headings with no group emits a single-colour key", {
  df <- data.frame(dir = c(0, 90, 180, 270))
  hf <- build_headings_input(df, col = "dir", units = "degrees",
                             convention = "unit_circle", group = NULL)
  spec <- build_plot_spec(
    ts = NULL, hd = hf, method = NULL,
    data = list(source = "file", mode = "headings", path = "angles.csv",
                col = "dir", units = "degrees", convention = "unit_circle",
                group = NULL),
    inputs = list(colour_by = NULL, cond_col = NULL,
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees")
  )
  expect_identical(spec$colour$by, "trajectory")   # sentinel -> single colour
  expect_false(spec$colour$legend)
  code <- spec_to_code(spec)
  expect_match(code, "hd$.colour <- factor(\"all\")", fixed = TRUE)
})

test_that("spec_to_code always emits the heading frame; radiate coords only when absolute", {
  # derive_headings() defaults to "absolute" and radiate() to "relative", so the
  # heading frame is always explicit while radiate()'s is emitted only off-default.
  ts <- cpunctatus
  code_for <- function(frame) spec_to_code(build_plot_spec(
    ts = ts, hd = derive_headings(ts, rule = "distal", coords = frame),
    method = "distal",
    data = list(source = "example", mode = "trajectories",
                path = "x.csv", dialect = NULL),
    inputs = list(cond_col = NULL, colour_by = NULL, plot_theme = "void",
                  angle_labels = "degrees", show_tracks = TRUE,
                  show_arrow = FALSE, frame = frame)))

  rel <- code_for("relative")
  expect_true(grepl('derive_headings(ts, rule = "distal", coords = "relative")', rel, fixed = TRUE))
  expect_false(grepl('coords = "absolute"', rel, fixed = TRUE))

  abs <- code_for("absolute")
  expect_true(grepl('derive_headings(ts, rule = "distal", coords = "absolute")', abs, fixed = TRUE))
  expect_match(abs, 'radiate\\(ts, group_col[^\n]*coords = "absolute"')
})

test_that("spec_to_code emits axial = TRUE for axial headings specs", {
  sp <- example_spec(rule = "distal", arrow = TRUE)
  sp$data <- list(source = "example", path = NULL, dialect = NULL)
  sp$axial <- TRUE
  sp$show$ci <- TRUE
  code <- spec_to_code(sp)
  expect_true(grepl("compute_circ_mean(hd, facets = \"type\", axial = TRUE)", code, fixed = TRUE))
  expect_true(grepl("add_circ_mean(arrow_df, colour = \"black\", axial = TRUE)", code, fixed = TRUE))
  expect_true(grepl("add_heading_interval(hd, facets = \"type\", stat = \"bootstrap_ci\", axial = TRUE)", code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("spec_to_code: normalize_xy is emitted when the spec records it", {
  sp <- example_spec()
  sp$data$normalize_xy <- TRUE
  code <- spec_to_code(sp)
  expect_true(grepl(
    'read_tracks("tracks.csv", dialect = "ethovision", normalize_xy = TRUE)',
    code, fixed = TRUE))
  expect_silent(parse(text = code))

  sp$data$normalize_xy <- FALSE
  code <- spec_to_code(sp)
  expect_true(grepl(
    'read_tracks("tracks.csv", dialect = "ethovision", normalize_xy = FALSE)',
    code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("spec_to_code: an unrecorded normalize_xy emits no argument", {
  sp <- example_spec()
  sp$data$normalize_xy <- NULL
  code <- spec_to_code(sp)
  expect_false(grepl("normalize_xy", code, fixed = TRUE))
})

test_that("spec_to_code: example source never emits normalize_xy", {
  sp <- example_spec()
  sp$data <- list(source = "example", path = NULL, dialect = NULL,
                  normalize_xy = TRUE)
  code <- spec_to_code(sp)
  expect_true(grepl("data(cpunctatus)", code, fixed = TRUE))
  expect_false(grepl("normalize_xy", code, fixed = TRUE))
})

test_that("spec_to_code: a recorded row drop is emitted, and reproduces the load", {
  sp <- example_spec()
  expect_false(grepl("on_invalid|dialect_args", spec_to_code(sp)))

  sp$data$drop <- "unlinked"
  expect_true(grepl(
    'read_tracks("tracks.csv", dialect = "ethovision", dialect_args = list(unlinked = "drop"))',
    spec_to_code(sp), fixed = TRUE))

  path <- normalizePath(testthat::test_path("fixtures", "real", "idtrackerai_trajectories.csv"))
  sp$data <- list(source = "file", path = path, dialect = "idtrackerai_wide",
                  normalize_xy = FALSE, drop = "positions")
  code <- spec_to_code(sp)
  expect_silent(parse(text = code))
  load_line <- grep("^ts <- read_tracks\\(", strsplit(code, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_match(load_line, 'normalize_xy = FALSE, on_invalid = "drop")', fixed = TRUE)
  expect_warning(ts <- eval(parse(text = load_line)[[1]][[3]]), class = "radiatR_dropped_rows")
  expect_equal(nrow(as.data.frame(ts)), 86L)

  sp$data <- list(source = "example", path = NULL, dialect = NULL, drop = "positions")
  expect_false(grepl("on_invalid", spec_to_code(sp), fixed = TRUE))
})

test_that("dropped_note names the count and the kind of row, or is NULL", {
  expect_null(dropped_note(0L, character(0)))
  expect_null(dropped_note(NULL, "positions"))
  expect_identical(dropped_note(2L, "positions"),
                   "2 row(s) were dropped when this file was loaded (no x/y position).")
  expect_identical(
    dropped_note(5L, c("unlinked", "positions")),
    "5 row(s) were dropped when this file was loaded (spot in no track; no x/y position).")
})

test_that("build_plot_spec records the seed for the app's stochastic outputs", {
  df <- data.frame(dir = c(0, 90, 180), cond = c("a","a","b"))
  hf <- build_headings_input(df, col = "dir", units = "degrees",
                             convention = "clock", group = "cond")
  spec <- build_plot_spec(
    ts = NULL, hd = hf, method = NULL,
    data = list(source = "file", mode = "headings", path = "angles.csv",
                col = "dir", units = "degrees", convention = "clock",
                group = "cond"),
    inputs = list(colour_by = "cond", cond_col = "cond",
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees")
  )
  expect_identical(spec$seed, APP_SEED)
})
