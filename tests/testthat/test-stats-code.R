.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# Build a stats spec for the bundled example, grouped by a condition column.
stats_spec <- function(by_col = "arc", omnibus = "rao", axial = FALSE,
                       headings = FALSE, n_groups = NULL) {
  list(
    mode     = if (headings) "headings" else "trajectory",
    data     = list(source = "example"),
    headings = list(rule = "distal"),
    facet_by = NULL, group_col = "trial_id",
    show     = list(vectors = FALSE),
    colour   = list(by = "trajectory"),
    stats    = list(by_col = by_col, pooled = is.null(by_col),
                    omnibus = omnibus, axial = axial, n_groups = n_groups)
  )
}

test_that("spec_to_stats_code emits the analysis calls and the chosen omnibus", {
  code <- spec_to_stats_code(stats_spec(omnibus = "hermans_rasson"))
  expect_match(code, "circ_summarise(", fixed = TRUE)
  expect_match(code, 'test_uniformity(hd, group_col = "arc", test = "rayleigh"', fixed = TRUE)
  expect_match(code, "set.seed(20260617L)", fixed = TRUE)
  expect_match(code, 'test_uniformity(hd, group_col = "arc", test = "hermans_rasson", n_sim = 999L, p_method = "monte_carlo")', fixed = TRUE)
  expect_match(code, "circ_model_select(", fixed = TRUE)
  expect_match(code, "straightness_index(ts)", fixed = TRUE)   # trajectory mode
  expect_match(code, "derive_headings(", fixed = TRUE)
  # rao when omnibus = rao
  expect_match(spec_to_stats_code(stats_spec(omnibus = "rao")),
               'test_uniformity(hd, group_col = "arc", test = "rao", n_sim = 999L, p_method = "monte_carlo")', fixed = TRUE)
  expect_silent(parse(text = code))   # parses
})

test_that("pooled spec emits ungrouped seeded uniformity calls", {
  code <- spec_to_stats_code(stats_spec(by_col = NULL))
  expect_match(code, 'test_uniformity(hd, test = "rayleigh"', fixed = TRUE)
  expect_match(code, "set.seed(20260617L)", fixed = TRUE)
  expect_match(code, ', test = "rao", n_sim = 999L, p_method = "monte_carlo")', fixed = TRUE)
  expect_false(grepl("group_col", code))
})

test_that("spec_to_stats_code value round-trip: emitted circ_summarise == app's internal", {
  data(cpunctatus, package = "radiatR", envir = environment())
  by_col <- "arc"
  spec <- stats_spec(by_col = by_col, axial = FALSE)

  code <- spec_to_stats_code(spec)
  e <- new.env(); suppressWarnings(suppressMessages(eval(parse(text = code), envir = e)))
  emitted <- e$summ                                    # the emitted circ_summarise result

  # app-internal equivalent for the same spec (mirrors the app's cond-col merge)
  hd_app <- derive_headings(cpunctatus, rule = "distal")
  cond_map <- unique(as.data.frame(cpunctatus)[, c("trial_id", by_col)])
  hd_app <- merge(hd_app, cond_map, by.x = "id", by.y = "trial_id", all.x = TRUE)
  ref <- circ_summarise(hd_app, "heading", units = "radians", .by = by_col,
                        stats = c("n", "n_missing", "mean_dir_deg", "resultant_R"),
                        display = circ_display(zero = 0), axial = FALSE)
  # same groups, same core numbers
  expect_setequal(emitted[[by_col]], ref[[by_col]])
  m <- merge(emitted, ref, by = by_col, suffixes = c(".e", ".r"))
  expect_equal(m$resultant_R.e, m$resultant_R.r, tolerance = 1e-6)
  expect_equal(m$mean_dir_deg.e, m$mean_dir_deg.r, tolerance = 1e-6)
})

test_that("headings-only spec omits straightness_index", {
  code <- spec_to_stats_code(stats_spec(headings = TRUE))
  expect_false(grepl("straightness_index", code, fixed = TRUE))
})

test_that("headings-grouped emitted script runs (regression: quoted source id)", {
  data(cpunctatus, package = "radiatR", envir = environment())
  spec <- stats_spec(by_col = "arc", headings = TRUE)   # mode="headings", source example
  code <- spec_to_stats_code(spec)
  expect_match(code, 'c("trial_id", "arc")', fixed = TRUE)   # quoted, not bare
  e <- new.env()
  expect_error(suppressWarnings(suppressMessages(eval(parse(text = code), envir = e))), NA)  # runs cleanly
  expect_true(is.data.frame(e$summ))
})

test_that("spec_to_stats_code emits the three group-comparison calls when grouped", {
  code <- spec_to_stats_code(stats_spec(n_groups = 3L))
  expect_match(code, 'test_mean_directions(hd, group_col = "arc"', fixed = TRUE)
  expect_match(code, 'test_concentration(hd, group_col = "arc"', fixed = TRUE)
  expect_match(code, 'test_distributions(hd, group_col = "arc"', fixed = TRUE)
  expect_match(code, 'method = "watson_wheeler"', fixed = TRUE)
  expect_silent(parse(text = code))
})

test_that("spec_to_stats_code picks watson_two for exactly two groups", {
  code <- spec_to_stats_code(stats_spec(n_groups = 2L))
  expect_match(code, 'method = "watson_two"', fixed = TRUE)
})

test_that("spec_to_stats_code omits group-comparison calls when pooled", {
  code <- spec_to_stats_code(stats_spec(by_col = NULL))
  expect_false(grepl("test_mean_directions(", code, fixed = TRUE))
  expect_false(grepl("test_concentration(", code, fixed = TRUE))
  expect_false(grepl("test_distributions(", code, fixed = TRUE))
})
