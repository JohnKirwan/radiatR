.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# Minimal headings-mode spec built the way the app does (build_plot_spec), with
# data$delim/sheet/ext overridden to simulate a non-default upload. Exercises
# the public spec_to_code() seam only -- no fragile internal-accumulator capture.
.headings_spec <- function(delim = NULL, sheet = NULL, ext = "csv", path = "a.csv",
                           text_cols = NULL, decimal = NULL, latin1 = NULL) {
  raw <- data.frame(
    angle_deg = c(10, 20, 30, 200, 210, 5, 95, 185, 275, 100),
    stringsAsFactors = FALSE)
  hd <- headings_frame(raw, col = angle_deg, units = "degrees",
                        angle_convention = "unit_circle")
  data <- list(source = "file", mode = "headings", path = path,
               col = "angle_deg", units = "degrees", convention = "unit_circle",
               delim = delim, sheet = sheet, ext = ext, text_cols = text_cols,
               decimal = decimal, latin1 = latin1)
  build_plot_spec(NULL, hd, method = NULL, data = data,
                   inputs = list(colour_by = NULL, cond_col = NULL,
                                 heading_display = "points", plot_theme = "void",
                                 angle_labels = "degrees"))
}

test_that("spec_to_code emits a semicolon reader for non-default headings uploads", {
  code <- paste(spec_to_code(.headings_spec(delim = ";", decimal = ",")), collapse = "\n")
  expect_match(code, 'read\\.csv2\\(')
})

# The emitted read call, evaluated on a real file.
.emitted_read <- function(...) {
  rd <- grep("^df <- ", unlist(strsplit(spec_to_code(.headings_spec(...)), "\n")),
             value = TRUE)
  expect_length(rd, 1L)
  rd
}

test_that("spec_to_code emits the delimiter and decimal mark the app read with", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("g;angle", "a;10.5", "b;20.25"), p)
  rd <- .emitted_read(delim = ";", decimal = ".", path = p)
  expect_match(rd, 'read.csv("', fixed = TRUE)
  expect_match(rd, 'sep = ";"', fixed = TRUE)
  expect_identical(eval(parse(text = rd))$angle, c(10.5, 20.25))

  writeLines(c("g\tangle", "a\t10.5"), p)
  rd <- .emitted_read(delim = "\t", decimal = ".", path = p)
  expect_match(rd, "read.delim(", fixed = TRUE)
  expect_identical(eval(parse(text = rd))$angle, 10.5)

  writeLines(c("g,angle", "a,10.5"), p)
  rd <- .emitted_read(delim = ",", decimal = ".", path = p)
  expect_identical(rd, paste0("df <- read.csv(", encodeString(p, quote = '"'), ")"))
})

test_that("spec_to_code stops on a delimiter the app did not resolve", {
  expect_error(spec_to_code(.headings_spec(delim = "auto")), "unexpected delimiter")
})

test_that("spec_to_code reads a latin-1 file as latin-1", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(iconv("g,angle\n\u00b5m,10\n", "UTF-8", "latin1", toRaw = TRUE)[[1]], p)
  rd <- .emitted_read(delim = ",", decimal = ".", latin1 = TRUE, path = p)
  expect_match(rd, 'fileEncoding = "latin1"', fixed = TRUE)
  expect_identical(eval(parse(text = rd))$g, "\u00b5m")
  expect_false(grepl("fileEncoding", .emitted_read(delim = ",", decimal = ".")))
})

test_that("spec_to_code emits read_excel + sheet for Excel headings uploads", {
  code <- paste(spec_to_code(
    .headings_spec(sheet = "two", ext = "xlsx", path = "b.xlsx")), collapse = "\n")
  expect_match(code, 'read_excel\\(')
  expect_match(code, 'sheet = "two"')
})

test_that("spec_to_code reads the app's text label columns as text", {
  code <- paste(spec_to_code(.headings_spec(text_cols = "grp")), collapse = "\n")
  expect_match(code, 'read.csv("a.csv", colClasses = c(grp = "character"))', fixed = TRUE)
  expect_false(grepl("colClasses", paste(spec_to_code(.headings_spec()), collapse = "\n")))
  # keyed by the name read.csv() gives the column, or it would miss it
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("g1;g 2;angle", "1,1;1,1;10", "1,10;1,10;20"), p)
  code <- unlist(strsplit(spec_to_code(.headings_spec(delim = ";", path = p,
                                                     text_cols = c("g1", "g 2"))), "\n"))
  rd <- grep("^df <- ", code, value = TRUE)
  expect_length(rd, 1L)
  expect_match(rd, 'colClasses = c(g1 = "character", g.2 = "character")', fixed = TRUE)
  df <- eval(parse(text = rd))
  expect_identical(df$g1, c("1,1", "1,10"))
  expect_identical(df$g.2, c("1,1", "1,10"))
})

test_that("spec_to_code emits the trajectory decimal mark beside delim and sheet", {
  spec <- function(...) list(
    data = c(list(source = "file", mode = "trajectories", path = "t.tsv"), list(...)),
    headings = list(rule = "distal"), group_col = "id", facet_by = NULL,
    colour = list(by = "trajectory", cap = 20, legend = FALSE),
    theme = "bw", angle_labels = "degrees", display = list(zero = 0),
    heading_display = "stacked", show = list(tracks = TRUE, arrow = TRUE, vectors = FALSE))
  code <- paste(spec_to_code(spec(delim = "\t", decimal = ",")), collapse = "\n")
  expect_match(code, 'read_opts = list(delim = "\\t", decimal = ",")', fixed = TRUE)
  code <- paste(spec_to_code(spec()), collapse = "\n")
  expect_false(grepl("read_opts", code, fixed = TRUE))
})
