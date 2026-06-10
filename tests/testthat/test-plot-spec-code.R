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
  expect_true(grepl('TrajSet_read("tracks.csv", dialect = "ethovision")', code, fixed = TRUE))
  expect_true(grepl('derive_headings(ts, rule = "crossing", circ0 = 0.3, circ1 = 0.6)', code, fixed = TRUE))
  expect_true(grepl('by.x = "id"', code, fixed = TRUE))                       # facet merge onto hd
  expect_true(grepl('assign_colour_key(ts, by = "trajectory")', code, fixed = TRUE))
  expect_true(grepl("bin_angles(hd$heading", code, fixed = TRUE))            # stacked
  expect_true(grepl('add_stacked_headings(hd, colour_col = ".colour", group = "type"', code, fixed = TRUE))
  expect_true(grepl('compute_circ_mean(hd, colour_col = "type")', code, fixed = TRUE))
  expect_true(grepl('add_circ_mean(arrow_df, colour = "black")', code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("spec_to_code: example source emits data(cpunctatus)", {
  sp <- example_spec(); sp$data <- list(source = "example", path = NULL, dialect = NULL)
  code <- spec_to_code(sp)
  expect_true(grepl("data(cpunctatus)", code, fixed = TRUE))
  expect_false(grepl("TrajSet_read", code, fixed = TRUE))
})

test_that("spec_to_code: points display, distinct colour + legend, no facet", {
  code <- spec_to_code(example_spec(heading_display = "points", by = "arc",
                                    facet = NULL, arrow = FALSE, legend = TRUE))
  expect_true(grepl("add_heading_points(", code, fixed = TRUE))
  expect_false(grepl("bin_angles", code, fixed = TRUE))
  expect_true(grepl('labs(colour = "arc")', code, fixed = TRUE))
  expect_false(grepl("panel_by", code, fixed = TRUE))   # no facet
  expect_false(grepl("by.x =", code, fixed = TRUE))     # no facet merge
  expect_silent(parse(text = code))
})

test_that("spec_to_code: rule none -> tracks only, no derive_headings", {
  code <- spec_to_code(example_spec(rule = "none"))
  expect_false(grepl("derive_headings", code, fixed = TRUE))
  expect_false(grepl("add_stacked_headings|add_heading_points", code))
  expect_silent(parse(text = code))
})
