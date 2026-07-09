# plot_spec.R is a shiny-free helper sourced by the app; source it directly.
.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

test_that("build_headings_input maps a degrees/compass column to a headings frame", {
  df  <- data.frame(dir = c(0, 90, 180, 270), cond = c("a","a","b","b"))
  hf  <- build_headings_input(df, col = "dir", units = "degrees",
                              convention = "clock", group = "cond")
  expect_s3_class(hf, "headings_frame")
  expect_true("heading" %in% names(hf))          # angle column renamed to "heading"
  expect_true(all(hf$heading >= 0 & hf$heading < 2 * pi))  # radians, wrapped
  expect_true("cond" %in% names(hf))             # group column carried through
})

test_that("build_headings_input coerces a plain derive_headings()-style frame", {
  df  <- data.frame(id = c("t1","t2"), time = c(5, 9),
                    heading = c(0.5, 2.1))        # already radians/unit-circle
  hf  <- build_headings_input(df, col = "heading", units = "radians",
                              convention = "unit_circle", group = NULL)
  expect_s3_class(hf, "headings_frame")
  expect_equal(hf$heading, c(0.5, 2.1))
})

test_that("build_plot_spec captures the figure choices", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "condition", colour_by = "__trajectory__",
                  plot_theme = "bw", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE)
  )
  expect_equal(spec$group_col, ts@cols$id)
  expect_equal(spec$facet_by, "condition")
  expect_equal(spec$colour$by, "trajectory")
  expect_false(spec$colour$legend)          # trajectory cycles -> no legend
  expect_equal(spec$theme, "bw")
  expect_equal(spec$heading_display, "points")
  expect_true(spec$show$arrow)
})

test_that("build_plot_spec: low-cardinality colour -> distinct + legend", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", colour_by = "condition",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "stacked",
                  show_tracks = TRUE, show_arrow = FALSE, show_vectors = FALSE)
  )
  expect_equal(spec$colour$by, "condition")
  expect_true(spec$colour$legend)           # 3 conditions <= cap -> legend
  expect_null(spec$facet_by)
})

test_that("build_plot_spec: rule 'none' -> no headings", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  spec <- build_plot_spec(
    ts = ts, hd = NULL, method = "none",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", colour_by = "__trajectory__",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE)
  )
  expect_equal(spec$headings$rule, "none")
})

# helper: does the plot have a layer with the given geom class?
.has_geom <- function(p, cls) any(vapply(p$layers,
  function(l) inherits(l$geom, cls), logical(1)))

test_that("spec_to_plot renders tracks + stacked markers + arrow for an example spec", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "condition", colour_by = "__trajectory__",
                  plot_theme = "bw", angle_labels = "degrees",
                  heading_display = "stacked",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE))
  p <- spec_to_plot(spec, ts, hd)
  expect_s3_class(p, "ggplot")
  expect_true(.has_geom(p, "GeomPoint"))     # stacked markers
  expect_true(.has_geom(p, "GeomSegment"))   # arrow
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("spec_to_plot: rule 'none' draws tracks only (no markers)", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  spec <- build_plot_spec(
    ts = ts, hd = NULL, method = "none",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", colour_by = "__trajectory__",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE))
  p <- spec_to_plot(spec, ts, NULL)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("build_plot_spec: headings mode records mode and headings data block", {
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
  expect_identical(spec$mode, "headings")
  expect_identical(spec$facet_by, "cond")
  expect_false(isTRUE(spec$show$tracks))     # no tracks in headings mode
})

test_that("attrition_note: derived wording names the rule and warns of bias", {
  msg <- attrition_note(n_total = 235, n_missing = 47, derived = TRUE, rule = "distal")
  expect_type(msg, "character")
  expect_match(msg, "188 of 235")
  expect_match(msg, "distal")
  expect_match(msg, "bias")
})

test_that("attrition_note: provided wording is neutral", {
  msg <- attrition_note(n_total = 200, n_missing = 12, derived = FALSE)
  expect_match(msg, "12 of 200")
  expect_false(grepl("bias", msg))
})

test_that("attrition_note: returns NULL when there is no attrition", {
  expect_null(attrition_note(n_total = 100, n_missing = 0, derived = TRUE, rule = "distal"))
  expect_null(attrition_note(n_total = 100, n_missing = NULL, derived = FALSE))
})

test_that("build_plot_spec carries the heading frame and spec_to_plot honours it", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus
  hd <- derive_headings(ts, rule = "distal", coords = "absolute")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", mode = "trajectories",
                path = NULL, dialect = NULL),
    inputs = list(cond_col = NULL, colour_by = NULL, frame = "absolute",
                  plot_theme = "void", angle_labels = "degrees",
                  show_tracks = TRUE, show_arrow = FALSE))
  expect_identical(spec$coords, "absolute")
  expect_s3_class(spec_to_plot(spec, ts, hd), "ggplot")

  # default (no frame input) is relative
  spec2 <- build_plot_spec(
    ts = ts, hd = derive_headings(ts, rule = "distal"), method = "distal",
    data = list(source = "example", mode = "trajectories",
                path = NULL, dialect = NULL),
    inputs = list(cond_col = NULL, colour_by = NULL,
                  plot_theme = "void", angle_labels = "degrees",
                  show_tracks = TRUE, show_arrow = FALSE))
  expect_identical(spec2$coords, "relative")
})

test_that("build_plot_spec maps show_boxplot into spec$show$boxplot", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus
  hd <- derive_headings(ts, rule = "distal")
  inputs <- list(show_boxplot = TRUE, show_tracks = TRUE)
  spec <- build_plot_spec(ts, hd, method = "distal",
                          data = list(mode = "trajectories"), inputs = inputs)
  expect_true(isTRUE(spec$show$boxplot))
  inputs$show_boxplot <- NULL
  spec2 <- build_plot_spec(ts, hd, method = "distal",
                           data = list(mode = "trajectories"), inputs = inputs)
  expect_false(isTRUE(spec2$show$boxplot))
})

test_that("build_plot_spec maps show_oob into spec$clip_tracks (inverted)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus; hd <- derive_headings(ts, rule = "distal")
  spec_on <- build_plot_spec(ts, hd, method = "distal",
                             data = list(mode = "trajectories"),
                             inputs = list(show_tracks = TRUE, show_oob = TRUE))
  expect_false(isTRUE(spec_on$clip_tracks))         # show_oob -> clip off
  spec_off <- build_plot_spec(ts, hd, method = "distal",
                              data = list(mode = "trajectories"),
                              inputs = list(show_tracks = TRUE))
  expect_true(isTRUE(spec_off$clip_tracks))         # default -> clip on
})

test_that("spec_to_code emits clip_tracks = FALSE only when off-default", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus; hd <- derive_headings(ts, rule = "distal")
  spec_on <- build_plot_spec(ts, hd, method = "distal",
                             data = list(mode = "trajectories"),
                             inputs = list(show_tracks = TRUE, show_oob = TRUE))
  expect_true(grepl("clip_tracks = FALSE", spec_to_code(spec_on), fixed = TRUE))
  spec_off <- build_plot_spec(ts, hd, method = "distal",
                              data = list(mode = "trajectories"),
                              inputs = list(show_tracks = TRUE))
  expect_false(grepl("clip_tracks", spec_to_code(spec_off), fixed = TRUE))
})

test_that("spec_to_plot honours clip_tracks (beyond-circumference points)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus; hd <- derive_headings(ts, rule = "distal")
  rho_max <- function(oob) {
    spec <- build_plot_spec(ts, hd, method = "none",
                            data = list(mode = "trajectories"),
                            inputs = list(show_tracks = TRUE, show_oob = oob))
    b <- ggplot2::ggplot_build(spec_to_plot(spec, ts, NULL))
    max(sqrt(b$data[[1]]$x^2 + b$data[[1]]$y^2), na.rm = TRUE)
  }
  expect_gt(rho_max(TRUE), 1)            # clip off -> points beyond the circumference
  expect_lte(rho_max(FALSE), 1 + 1e-6)   # clip on (default) -> within the unit circle
})

test_that("build_plot_spec keeps overlays on in grid mode (un-gated)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus; hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(ts, hd, method = "distal",
            data = list(mode = "trajectories"),
            inputs = list(show_tracks = TRUE, cond_col = "type", cond_col2 = "arc",
                          show_ci = TRUE, show_rayleigh = TRUE, show_vtest = TRUE,
                          show_boxplot = TRUE))
  expect_identical(spec$facet_by, "type")
  expect_identical(spec$facet_cols, "arc")
  expect_true(isTRUE(spec$show$ci))
  expect_true(isTRUE(spec$show$rayleigh))
  expect_true(isTRUE(spec$show$vtest))
  expect_true(isTRUE(spec$show$boxplot))
})

test_that("build_plot_spec keeps facet_cols NULL and overlays on in single-facet mode", {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus; hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(ts, hd, method = "distal",
            data = list(mode = "trajectories"),
            inputs = list(show_tracks = TRUE, cond_col = "type", show_ci = TRUE))
  expect_null(spec$facet_cols)
  expect_true(isTRUE(spec$show$ci))
})

test_that("headings-mode grid sets facet_cols and emits the both-column hd merge", {
  data(cpunctatus, package = "radiatR", envir = environment())
  hd0  <- derive_headings(cpunctatus, rule = "distal", coords = "relative")
  cond <- unique(as.data.frame(cpunctatus)[, c("trial_id", "type", "arc")])
  hd0  <- merge(hd0, cond, by.x = "id", by.y = "trial_id", all.x = TRUE)
  hd   <- build_headings_input(hd0, col = "heading", units = "radians",
                               convention = "unit_circle")
  spec <- build_plot_spec(
    ts = NULL, hd = hd, method = NULL,
    data = list(source = "example", mode = "headings", path = NULL,
                col = "heading", units = "radians", convention = "unit_circle"),
    inputs = list(colour_by = "type", cond_col = "type", hd_group2 = "arc",
                  heading_display = "points", plot_theme = "void",
                  angle_labels = "degrees", show_arrow = TRUE))
  expect_identical(spec$facet_by, "type")
  expect_identical(spec$facet_cols, "arc")
  code <- spec_to_code(spec)
  expect_match(code, 'c("trial_id", "type", "arc")', fixed = TRUE)  # merge covers both facet cols
})

test_that("build_plot_spec: group_by sets the analytical key and stats by_col", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", group_by = "condition",
                  colour_by = "__group__",
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE))
  expect_equal(spec$group_by, "condition")
  expect_equal(spec$group_col, ts@cols$id)      # ggplot path key unchanged
  expect_true(spec$colour$follows_group)        # Follow group + group set
  expect_equal(spec$colour$by, "condition")     # resolved to the group column
  expect_equal(spec$stats$by_col, "condition")  # stats key = group
})

test_that("build_plot_spec: group unset -> follows_group FALSE, default unchanged", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "condition", group_by = "",
                  colour_by = "__group__",       # Follow group, but no group
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE))
  expect_null(spec$group_by)
  expect_false(spec$colour$follows_group)
  expect_equal(spec$colour$by, "trajectory")     # Follow group -> trajectory
  expect_equal(spec$stats$by_col, "condition")   # falls back to facet col
})

test_that("build_plot_spec: colour = group column directly counts as follows_group", {
  ts <- simulate_tracks(n_points = 20, seed = 1, output = "trajset")
  hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", group_by = "condition",
                  colour_by = "condition",       # direct, not the sentinel
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points",
                  show_tracks = TRUE, show_arrow = TRUE, show_vectors = FALSE))
  expect_true(spec$colour$follows_group)
})

# built data of the LAST layer with the given geom (stat overlays are added last,
# after radiate()'s structural unit-circle/tick/label layers).
.last_layer_data_for <- function(p, cls) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))
  if (!length(idx)) return(NULL)
  b$data[[idx[[length(idx)]]]]
}

.grp_spec <- function(colour_by, group_by = "arc", ...) {
  data(cpunctatus, package = "radiatR", envir = environment())
  ts <- cpunctatus; hd <- derive_headings(ts, rule = "distal")
  spec <- build_plot_spec(
    ts = ts, hd = hd, method = "distal",
    data = list(source = "example", path = NULL, dialect = NULL),
    inputs = list(cond_col = "", group_by = group_by, colour_by = colour_by,
                  plot_theme = "void", angle_labels = "degrees",
                  heading_display = "points", show_tracks = FALSE,
                  show_arrow = FALSE, ...))
  list(spec = spec, ts = ts, hd = hd)
}

test_that("spec_to_plot: Follow-group -> one Rayleigh circle per group", {
  g <- .grp_spec("__group__", show_rayleigh = TRUE)
  n_grp <- length(unique(as.data.frame(g$ts)[["arc"]]))   # 8
  p  <- spec_to_plot(g$spec, g$ts, g$hd)
  rp <- .last_layer_data_for(p, "GeomPath")               # Rayleigh, not unit circle
  expect_false(is.null(rp))
  expect_equal(length(unique(rp$colour)), n_grp)          # colour-mapped per group
})

test_that("spec_to_plot: Follow-group -> marks + overlay share one group colour scale", {
  g <- .grp_spec("__group__", show_rayleigh = TRUE)
  g$spec$heading_display <- "points"
  p <- spec_to_plot(g$spec, g$ts, g$hd)
  n_grp <- length(unique(as.data.frame(g$ts)[["arc"]]))    # 8
  marks <- .last_layer_data_for(p, "GeomPoint")            # add_heading_points
  circ  <- .last_layer_data_for(p, "GeomPath")             # Rayleigh circle
  # Marks map colour from `.colour` (= factor of the group values) and the
  # overlay maps from the raw group column; ggplot builds ONE discrete scale over
  # those shared string values, so a given group's colour is identical in both
  # layers by construction. ggplot's internal `group` integer is NOT comparable
  # across the two layers (each orders by its own factor), so we assert the
  # observable consequence instead: both layers resolve to n_grp distinct hues
  # drawn from the SAME palette. This catches pooling (-> 1 colour), the wrong
  # column (-> wrong count), and a divergent scale (-> different palette).
  expect_equal(length(unique(marks$colour)), n_grp)
  expect_equal(length(unique(circ$colour)),  n_grp)
  expect_setequal(unique(circ$colour), unique(marks$colour))
})

test_that("spec_to_plot: group set but colour = trajectory -> overlays pooled", {
  g <- .grp_spec("__trajectory__", show_rayleigh = TRUE)
  expect_false(g$spec$colour$follows_group)
  p  <- spec_to_plot(g$spec, g$ts, g$hd)
  rp <- .last_layer_data_for(p, "GeomPath")
  expect_equal(unique(rp$colour), SPEC_CRIT_COLOUR)        # single pooled circle, fixed colour
})

test_that("spec_to_plot draws grouped boxplot rings coloured by group", {
  g <- .grp_spec("__group__", show_boxplot = TRUE)
  # A grouped cpunctatus cell is near-uniform, so the boxplot emits its
  # documented advisory; suppress it to keep the test output pristine.
  p <- suppressWarnings(spec_to_plot(g$spec, g$ts, g$hd))
  box <- Find(function(l) inherits(l$geom, "GeomPolygon") &&
                          "colour" %in% names(l$mapping), p$layers)
  expect_false(is.null(box))
  expect_identical(rlang::as_label(box$mapping$colour), g$spec$group_by)
})
