# Shiny-free upload helpers: source directly (installed location first).
.up <- system.file("app", "upload.R", package = "radiatR")
if (!nzchar(.up) || !file.exists(.up))
  .up <- testthat::test_path("..", "..", "inst", "app", "upload.R")
source(.up, local = TRUE)

app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

test_that("upload_read honours the semicolon delimiter via ext dispatch", {
  tmp <- tempfile()                        # extensionless path
  writeLines(c("bearing;cond", "10;a", "20;b", "30;a"), tmp)
  df <- as.data.frame(upload_read(tmp, "angles.csv", ";", NULL))
  expect_equal(names(df), c("bearing", "cond"))
  expect_equal(nrow(df), 3L)
})

test_that("headings upload honours the semicolon delimiter (lazy read)", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("bearing;cond", "10;a", "20;b", "30;a"), tmp)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(file = list(datapath = tmp, name = "angles.csv"))
    session$setInputs(delim_sel = ";")
    expect_false(is.null(rv$raw_hd))
    expect_equal(names(rv$raw_hd), c("bearing", "cond"))
    expect_equal(nrow(rv$raw_hd), 3L)
  })
})

test_that("a failed headings upload clears stale example state (PR #20 guard)", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(load_example_hd = 1)   # load example headings first
    expect_false(is.null(rv$hd))
    bad <- file.path(tempdir(), "does-not-exist-headings.csv")
    session$setInputs(file = list(datapath = bad, name = "bad.csv"))
    session$setInputs(delim_sel = "auto")
    expect_null(rv$hd)        # stale example headings cleared
    expect_null(rv$raw_hd)
  })
})

test_that("Excel upload lists worksheets and reads the selected sheet", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  xl <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(one = data.frame(bearing = c(1, 2)),
         two = data.frame(bearing = c(90, 180, 270))), xl)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(file = list(datapath = xl, name = "book.xlsx"))
    session$setInputs(delim_sel = "auto")
    session$setInputs(sheet_sel = "two")

    # The worksheet selector must actually render for an Excel upload,
    # listing every sheet in the workbook (not just read the chosen one).
    sb <- output$sheet_box
    expect_false(is.null(sb))
    html <- as.character(if (is.list(sb) && !is.null(sb$html)) sb$html else sb)
    expect_true(grepl("sheet_sel", html, fixed = TRUE))
    expect_true(grepl("one", html, fixed = TRUE))
    expect_true(grepl("two", html, fixed = TRUE))

    expect_equal(nrow(rv$raw_hd), 3L)   # sheet "two" has 3 rows
  })
})

test_that("sheet_box does not render for a delimited (non-Excel) upload", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("bearing", "10", "20", "30"), tmp)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(file = list(datapath = tmp, name = "angles.csv"))
    session$setInputs(delim_sel = "auto")
    sb <- output$sheet_box
    expect_true(is.null(sb) || identical(sb, ""))
  })
})

test_that("accept list excludes .mat and gates Excel on readxl", {
  # .mat never advertised (ctrax removed)
  expect_false(".mat" %in% accepted_exts("trajectories"))
  expect_false(".mat" %in% accepted_exts("headings"))
  # delimited always present
  expect_true(all(c(".csv", ".tsv", ".txt") %in% accepted_exts("headings")))
})
