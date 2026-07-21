.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# Minimal headings-mode spec built the way the app does (build_plot_spec), with
# data$delim/sheet/ext overridden to simulate a non-default upload. Exercises
# the public spec_to_code() seam only -- no fragile internal-accumulator capture.
.headings_spec <- function(delim = NULL, sheet = NULL, ext = "csv", path = "a.csv") {
  raw <- data.frame(
    angle_deg = c(10, 20, 30, 200, 210, 5, 95, 185, 275, 100),
    stringsAsFactors = FALSE)
  hd <- headings_frame(raw, col = angle_deg, units = "degrees",
                        angle_convention = "unit_circle")
  data <- list(source = "file", mode = "headings", path = path,
               col = "angle_deg", units = "degrees", convention = "unit_circle",
               delim = delim, sheet = sheet, ext = ext)
  build_plot_spec(NULL, hd, method = NULL, data = data,
                   inputs = list(colour_by = NULL, cond_col = NULL,
                                 heading_display = "points", plot_theme = "void",
                                 angle_labels = "degrees"))
}

test_that("spec_to_code emits a semicolon reader for non-default headings uploads", {
  code <- paste(spec_to_code(.headings_spec(delim = ";")), collapse = "\n")
  expect_match(code, 'read\\.csv2\\(')
})

test_that("spec_to_code emits read_excel + sheet for Excel headings uploads", {
  code <- paste(spec_to_code(
    .headings_spec(sheet = "two", ext = "xlsx", path = "b.xlsx")), collapse = "\n")
  expect_match(code, 'read_excel\\(')
  expect_match(code, 'sheet = "two"')
})
