.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# Build a stats spec for the bundled example, grouped by a condition column.
stats_spec <- function(by_col = "arc", omnibus = "rao", axial = FALSE,
                       headings = FALSE) {
  list(
    mode     = if (headings) "headings" else "trajectory",
    data     = list(source = "example"),
    headings = list(rule = "distal"),
    facet_by = NULL, group_col = "trial_id",
    show     = list(vectors = FALSE),
    colour   = list(by = "trajectory"),
    stats    = list(by_col = by_col, pooled = is.null(by_col),
                    omnibus = omnibus, axial = axial)
  )
}

test_that("spec_to_stats_code emits the analysis calls and the chosen omnibus", {
  code <- spec_to_stats_code(stats_spec(omnibus = "hermans_rasson"))
  expect_match(code, "circ_summarise(", fixed = TRUE)
  expect_match(code, 'test_uniformity(hd, test = "rayleigh"', fixed = TRUE)
  expect_match(code, 'test_uniformity(hd, test = "hermans_rasson"', fixed = TRUE)
  expect_match(code, "circ_model_select(", fixed = TRUE)
  expect_match(code, "straightness_index(ts)", fixed = TRUE)   # trajectory mode
  expect_match(code, "derive_headings(", fixed = TRUE)
  # rao when omnibus = rao
  expect_match(spec_to_stats_code(stats_spec(omnibus = "rao")),
               'test_uniformity(hd, test = "rao"', fixed = TRUE)
  expect_silent(parse(text = code))   # parses
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
