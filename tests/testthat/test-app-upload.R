# Too slow for CRAN's check time budget; runs locally and on CI.
skip_on_cran()

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

test_that("a headings file with a column name used twice is refused, naming it", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("heading,cond,heading", "10,a,200", "20,b,210"), tmp)
  # a preview leaves repeated columns to the dialect
  expect_named(upload_read(tmp, "angles.csv"), c("heading", "cond", "heading"))
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(file = list(datapath = tmp, name = "angles.csv"))
    expect_null(rv$raw_hd)
    expect_match(rv$error, "column name(s) 'heading' appear more than once", fixed = TRUE)
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

test_that("upload_preview shows a dialect's file as its first lines when it is no table", {
  # the EthoVision preamble is one column under readr and an error under fread
  hidden <- c(list(character(0)),
              if (requireNamespace("data.table", quietly = TRUE)) list("readr"))
  for (hide in hidden) {
    testthat::local_mocked_bindings(
      .is_installed = function(pkg) !pkg %in% hide && requireNamespace(pkg, quietly = TRUE),
      .package = "radiatR")
    ev <- testthat::test_path("fixtures", "real", "ethovision_trial.txt")
    # the discarded table read's warnings (fread's) are discarded with it
    expect_no_warning(pv <- upload_preview(ev, "ethovision_trial.txt", dialect = "ethovision"))
    expect_identical(names(pv), "Line")
    expect_equal(nrow(pv), 10L)
    expect_match(pv$Line[1], "header", ignore.case = TRUE)
  }
  tr <- testthat::test_path("fixtures", "real", "trex_beetle.csv")
  expect_gt(ncol(upload_preview(tr, "trex_beetle.csv", dialect = "trex")), 1L)
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("id,time,x,y\na,1,.1,.2\n"), p)
  expect_identical(names(upload_preview(p, "t.csv", dialect = "generic")), c("id", "time", "x", "y"))
})

test_that("upload_preview re-emits the table read's warnings only when it shows the table", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("a,b\n1,2\n"), p)
  with_read <- function(tbl) {
    f <- upload_preview
    environment(f) <- list2env(list(upload_read = function(...) {
      warning("table read warned"); tbl }), parent = environment(upload_preview))
    f
  }
  shown <- with_read(data.frame(a = 1, b = 2))
  expect_warning(pv <- shown(p, "t.csv", dialect = "trex"), "table read warned")
  expect_named(pv, c("a", "b"))
  raw <- with_read(data.frame(a = 1))
  expect_no_warning(pv <- raw(p, "t.csv", dialect = "trex"))
  expect_named(pv, "Line")
})

test_that("upload_preview reads only the lines it shows", {
  ev <- testthat::test_path("fixtures", "real", "ethovision_trial.txt")
  seen <- NULL
  testthat::local_mocked_bindings(
    .read_text_lines = function(path, n = -1L) { seen <<- n; rep("line", max(n, 0L)) },
    .is_installed = function(pkg) pkg != "readr" && requireNamespace(pkg, quietly = TRUE),
    .package = "radiatR")
  pv <- upload_preview(ev, "ethovision_trial.txt", dialect = "ethovision", n = 4L)
  expect_identical(seen, 4L)
  expect_equal(nrow(pv), 4L)
})

test_that(".read_text_lines reads the first n lines in any of its encodings", {
  dir <- withr::local_tempdir()
  utf8 <- file.path(dir, "utf8.txt")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("a\u00e9\nb\nc\n")), utf8)
  latin <- file.path(dir, "latin.txt")
  writeBin(c(charToRaw("a"), as.raw(0xe9), charToRaw("\nb\nc\n")), latin)
  utf16 <- file.path(dir, "utf16.txt")
  writeBin(c(as.raw(c(0xff, 0xfe)), iconv("a\u00e9\nb\nc\n", "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]),
           utf16)
  for (p in c(utf8, latin, utf16)) {
    expect_identical(radiatR:::.read_text_lines(p, n = 2L), c("a\u00e9", "b"), info = basename(p))
    expect_length(radiatR:::.read_text_lines(p), 3L)
  }
})

test_that("upload_preview of a missing file stops before trying to open it", {
  bad <- file.path(withr::local_tempdir(), "no-such.txt")
  expect_no_warning(expect_error(upload_preview(bad, "x.txt", dialect = "ethovision"),
                                 "not found"))
})

test_that("upload_read keeps label columns it is told of as their text", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("grp,angle\n1.1,10\n1.10,20\n1.1,30\n"), p)
  expect_length(unique(upload_read(p, "h.csv")$grp), 1L)          # typed: merged
  expect_setequal(upload_read(p, "h.csv", id = "grp")$grp, c("1.1", "1.10"))
  # a label column that types without merging stays numeric
  q <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("grp,angle\n1,10\n2,20\n"), q)
  expect_true(is.numeric(upload_read(q, "h.csv", id = "grp")$grp))
})

test_that("a headings upload keeps its group labels apart and says so in the spec", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("grp,angle\n1.1,10\n1.10,20\n1.1,30\n1.10,40\n"), p)
  q <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("grp,angle\n1,10\n2,20\n1,30\n2,40\n"), q)
  go <- function(path, text_cols) shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(file = list(datapath = path, name = "h.csv"))
    session$setInputs(hd_col = "angle", hd_units = "degrees",
                      hd_convention = "unit_circle", hd_group = "grp", go3 = 1)
    expect_null(rv$error)
    expect_length(unique(rv$hd$grp), 2L)
    expect_identical(rv$hd_map$text_cols, text_cols)
    expect_identical(current_spec()$data$text_cols, text_cols)
  })
  go(p, "grp")             # 1.1 and 1.10: kept as their text
  go(q, character(0))      # 1 and 2: stay numbers
})

test_that("upload_read_format() resolves what .read_any() reads the file with", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("g;angle", "a;1,5", "b;2,5"), p)
  expect_identical(upload_read_format(p, "h.csv", "auto"),
                   list(delim = ";", decimal = ",", latin1 = FALSE))
  expect_identical(upload_read_format(p, "h.csv", "\t")$delim, "\t")
  writeBin(iconv("g,angle\nµm,10\n", "UTF-8", "latin1", toRaw = TRUE)[[1]], p)
  expect_true(upload_read_format(p, "h.csv", NULL)$latin1)
  expect_null(upload_read_format(p, "h.xlsx", NULL))
})

test_that("a headings upload on auto records the delimiter it was read with", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("grp;angle\na;10.5\nb;20.5\na;30.5\n"), p)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings", delim_sel = "auto")
    session$setInputs(file = list(datapath = p, name = "h.csv"))
    session$setInputs(hd_col = "angle", hd_units = "degrees",
                      hd_convention = "unit_circle", go3 = 1)
    expect_null(rv$error)
    d <- current_spec()$data
    expect_identical(list(d$delim, d$decimal, d$latin1), list(";", ".", FALSE))
    expect_null(attr(rv$hd, "decimal_evidence"))
  })
})

test_that("format_read_opts keeps only the controls not on auto", {
  expect_identical(format_read_opts(NULL, NULL), list())
  expect_identical(format_read_opts("auto", "auto"), list())
  expect_identical(format_read_opts(";", "auto"), list(delim = ";"))
  expect_identical(format_read_opts("auto", ","), list(decimal = ","))
  expect_identical(format_read_opts("\t", "."), list(delim = "\t", decimal = "."))
})

test_that("upload_read and upload_read_format honour the decimal mark control", {
  tmp <- tempfile(fileext = ".tsv")
  writeLines(c("x\ty", "1,5\t2,5", "1,6\t2,6"), tmp)
  expect_type(upload_read(tmp, "a.tsv")$x, "character")      # sniffed: point
  expect_equal(upload_read(tmp, "a.tsv", decimal_sel = ",")$x, c(1.5, 1.6))
  expect_identical(upload_read_format(tmp, "a.tsv")$decimal, ".")
  expect_identical(upload_read_format(tmp, "a.tsv", decimal_sel = ",")$decimal, ",")
  expect_identical(upload_read_format(tmp, "a.tsv", "auto", "auto")$delim, "\t")
})

test_that("upload_read_opts drops controls hidden for the file type", {
  # Shiny keeps a removed control's last value: a CSV after an Excel upload
  # still has sheet_sel, an Excel file after a CSV still has delim/decimal.
  expect_identical(upload_read_opts("a.csv", "auto", "auto", "Sheet2"), list())
  expect_identical(upload_read_opts("a.csv", ";", ",", "Sheet2"),
                   list(delim = ";", decimal = ","))
  expect_identical(upload_read_opts("a.xlsx", ";", ",", "Sheet2"), list(sheet = "Sheet2"))
  expect_identical(upload_read_opts("a.xlsx", ";", ",", ""), list())
  expect_identical(upload_read_opts("a.xlsx", NULL, NULL, NULL), list())
})

test_that("a decimal-comma tab file is refused on auto and loads on Comma", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  tsv <- tempfile(fileext = ".tsv")
  writeLines(c("id\tframe\tx\ty", paste0("a\t", 1:3, "\t0,", 1:3, "\t0,", 4:6)), tsv)
  shiny::testServer(app_dir(), {
    session$setInputs(file = list(datapath = tsv, name = "t.tsv"))
    session$setInputs(dialect_sel = "generic", delim_sel = "auto", decimal_sel = "auto")
    session$setInputs(map_x = "x", map_y = "y", map_time = "frame", map_id = "id",
                      normalize_sel = "FALSE")   # raw coordinates, to compare values
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_true(is.character(rv$error) && nzchar(rv$error))
    session$setInputs(decimal_sel = ",")
    session$setInputs(go2 = 2)
    expect_false(is.null(rv$ts))
    expect_equal(as.data.frame(rv$ts)[[rv$ts@cols$x]], c(0.1, 0.2, 0.3))
    # the downloaded scripts re-read the file with the same mark
    expect_identical(current_spec()$data$decimal, ",")
    expect_identical(kinematics_spec()$data$decimal, ",")
  })
})

test_that("an Excel upload with a text-export dialect says to upload the text export", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  xl <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Sheet1 = data.frame(a = 1:2)), xl)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = xl, name = "book.xlsx"))
    session$setInputs(dialect_sel = "ethovision", delim_sel = "auto", decimal_sel = "auto",
                      sheet_sel = "Sheet1")
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_match(rv$error, "read from its text export, not from an Excel workbook", fixed = TRUE)
    expect_false(grepl("read_opts", rv$error, fixed = TRUE))
  })
})

test_that("a headings file re-reads when the decimal mark changes", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  tsv <- tempfile(fileext = ".tsv")
  writeLines(c("bearing\tcond", "10,5\ta", "20,5\tb"), tsv)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(file = list(datapath = tsv, name = "h.tsv"))
    session$setInputs(delim_sel = "auto", decimal_sel = "auto")
    expect_type(rv$raw_hd$bearing, "character")
    session$setInputs(decimal_sel = ",")
    expect_equal(rv$raw_hd$bearing, c(10.5, 20.5))
  })
})

test_that("a DeepLabCut upload on auto controls sends no read_opts", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  # testServer runs in the app dir: the path must be absolute
  dlc <- normalizePath(testthat::test_path("fixtures", "real", "dlc_single_wasp_predictions.csv"))
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = dlc, name = "wasp.csv"))
    session$setInputs(dialect_sel = "auto", delim_sel = "auto", decimal_sel = "auto",
                      sheet_sel = "Sheet1")   # stale value from an earlier Excel upload
    session$setInputs(go2 = 1)
    # stopped only for its 9 masked positions (as in test-app-drop-control.R),
    # not for read_opts
    expect_identical(rv$drop_offer[c("kind", "n")], list(kind = "positions", n = 9L))
    session$setInputs(drop_btn = 1)
    expect_equal(nrow(as.data.frame(rv$ts)), 43L)
  })
})

test_that("hd_angle_choices() offers a text column of other-mark numbers, preselected", {
  df <- data.frame(id = c("1.1", "1.10"), heading = c("123.456", "10.125"),
                   n = 1:2, note = c("a", "b"), blank = NA_character_)
  attr(df, "decimal") <- ","
  ch <- hd_angle_choices(df)
  expect_identical(ch$choices, c("n", "heading"))   # not the id labels, not text
  expect_identical(ch$selected, "heading")
  expect_identical(ch$flagged, "heading")
  expect_match(hd_mark_advice(df, "heading"), "Set Decimal mark to Point (.)", fixed = TRUE)
  expect_null(hd_mark_advice(df, "n"))
  # every value must carry the other mark's reading, and one must need it
  df2 <- data.frame(a = c("1,5", "oops"), b = c("1", "2"), c = c("1,5", "2,25"),
                    stringsAsFactors = FALSE)
  attr(df2, "decimal") <- "."
  expect_identical(hd_angle_choices(df2)$flagged, "c")
  expect_match(hd_mark_advice(df2, "c"), "Set Decimal mark to Comma (,)", fixed = TRUE)
  # no mark recorded (Excel): no advice about a control the app hides
  attr(df2, "decimal") <- NULL
  expect_identical(hd_angle_choices(df2)$flagged, character(0))
  # nothing flagged: the first numeric column, as before
  df3 <- data.frame(id = 1:2, angle = c(10, 20))
  attr(df3, "decimal") <- "."
  expect_identical(hd_angle_choices(df3)[c("choices", "selected")],
                   list(choices = c("id", "angle"), selected = "id"))
  # the column the user chose stays chosen once it reads as numbers
  expect_identical(hd_angle_choices(df3, current = "angle")$selected, "angle")
  expect_identical(hd_angle_choices(df3, current = "gone")$selected, "id")
  # a numeric column with decimals confirms the mark the file was read with:
  # a text column of "1.1" / "1.10" beside it is labels, not angles
  df4 <- data.frame(grp = c("1.1", "1.10"), angle = c(123.5, 10.25))
  attr(df4, "decimal") <- ","
  expect_identical(hd_angle_choices(df4)[c("choices", "selected", "flagged")],
                   list(choices = "angle", selected = "angle", flagged = character(0)))
})

test_that("a headings upload with the other decimal mark is stopped with advice", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("id;heading\n1;123.456\n2;234.567\n3;10.125\n"), p)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings", delim_sel = "auto", decimal_sel = "auto")
    session$setInputs(file = list(datapath = p, name = "h.csv"))
    session$setInputs(hd_col = "heading", hd_units = "degrees",
                      hd_convention = "unit_circle", go3 = 1)
    expect_match(rv$error, "Set Decimal mark to Point (.) in step 1", fixed = TRUE)
    expect_null(rv$hd)
    session$setInputs(go2 = 1)            # step 2: the flagged column is preselected
    sel <- function() regmatches(output$wizard$html,
                                 regexpr('<option value="[^"]*" selected', output$wizard$html))
    expect_identical(sel(), '<option value="heading" selected')
    session$setInputs(decimal_sel = ".")  # now numeric: the user's choice stays
    expect_identical(sel(), '<option value="heading" selected')
    session$setInputs(go3 = 2)
    expect_null(rv$error)
    expect_equal(rv$hd$heading, c(123.456, 234.567, 10.125) * pi / 180)
  })
})

test_that("a headings angle column backed only by another column's point is stopped", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("id;heading;w\n1;123.456;0.5\n2;234.567;0.6\n3;10.125;0.7\n"), p)
  p <- normalizePath(p)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings", delim_sel = "auto", decimal_sel = "auto")
    session$setInputs(file = list(datapath = p, name = "h.csv"))
    session$setInputs(hd_col = "heading", hd_units = "degrees",
                      hd_convention = "unit_circle", go3 = 1)
    expect_match(rv$error, "Can't tell the decimal mark of column 'heading'", fixed = TRUE)
    expect_null(rv$hd)
    session$setInputs(decimal_sel = ".")
    session$setInputs(go3 = 2)
    expect_null(rv$error)
    expect_equal(rv$hd$heading, c(123.456, 234.567, 10.125) * pi / 180)
    expect_null(attr(rv$hd, "decimal_evidence"))
    expect_null(attr(rv$hd, "decimal"))
  })
})

test_that("a trajectory upload with an ambiguous decimal mark loads on Point", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;t;x;y;label", "a;1;1.234;2.345;1.5", "a;2;1.240;2.350;2.5",
               "a;3;1.250;2.360;3.5"), p)
  p <- normalizePath(p)
  shiny::testServer(app_dir(), {
    session$setInputs(file = list(datapath = p, name = "t.csv"))
    session$setInputs(dialect_sel = "generic", delim_sel = "auto", decimal_sel = "auto")
    session$setInputs(map_x = "x", map_y = "y", map_time = "t", map_id = "id",
                      normalize_sel = "FALSE")
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_match(rv$error, "set Decimal mark to Point (.)", fixed = TRUE)
    session$setInputs(decimal_sel = ".")
    session$setInputs(go2 = 2)
    expect_false(is.null(rv$ts))
    expect_equal(as.data.frame(rv$ts)[[rv$ts@cols$x]], c(1.234, 1.240, 1.250))
  })
})
