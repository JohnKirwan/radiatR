# Too slow for CRAN's check time budget; runs locally and on CI.
skip_on_cran()

app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

app_env <- function() {
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file)) app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  e
}

test_that("a failed trajectory ingest records the original exception in detail", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    bad <- file.path(tempdir(), "no-such-tracks.csv")
    session$setInputs(file = list(datapath = bad, name = "bad.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)      # trigger ingest (real trigger id)
    expect_true(!is.null(rv$error) && nzchar(rv$error))
    expect_true(is.character(rv$error))          # headline stays a string
    expect_true(!is.null(rv$error_detail))       # original exception captured
  })
})

test_that("a successful ingest's captured warnings are surfaced, not dropped", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    # Simulate the success path: rv$error cleared but rv$error_detail carries
    # warnings captured during a successful load (see app.R ~line 660-662).
    rv$step <- 1L
    rv$error <- NULL
    rv$error_detail <- "some warning message from ingest"
    html <- paste(as.character(output$wizard), collapse = "\n")
    expect_match(html, "Notes")
    expect_match(html, "some warning message from ingest", fixed = TRUE)
    expect_no_match(html, "alert-danger")
  })
})

test_that("invalid track data gets its own headline and lists rows and ids", {
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  d <- data.frame(id = c("a", "a", "b"), time = c(1, 1, 1), x = 0.1, y = 0.2)
  err <- tryCatch(tracks(d, id = "id", time = "time", x = "x", y = "y"),
                  error = function(cnd) cnd)
  expect_s3_class(err, "radiatR_invalid_tracks")
  detail <- e$ingest_detail(err)
  expect_match(detail, "time repeated within an id", fixed = TRUE)
  expect_match(detail, "Affected rows (1): 2", fixed = TRUE)
  expect_match(detail, "Affected ids: a", fixed = TRUE)
  expect_match(e$plain_error(err), "share the same ID and time", fixed = TRUE)

  d <- data.frame(id = "a", time = 1:30, x = NA_real_, y = 0.2)
  err <- tryCatch(tracks(d, id = "id", time = "time", x = "x", y = "y"),
                  error = function(cnd) cnd)
  expect_match(e$plain_error(err), "not valid track data", fixed = TRUE)
  expect_match(e$ingest_detail(err), "Affected rows (30): 1, 2, 3", fixed = TRUE)
  expect_match(e$ingest_detail(err), "19, 20, ...\nAffected ids: a", fixed = TRUE)
})

test_that("a malformed value gets a headline naming the column and an example", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  d <- data.frame(id = "a", time = 1:25, x = "oops", y = 0.2)
  err <- tryCatch(read_tracks(d), error = function(cnd) cnd)
  expect_s3_class(err, "radiatR_malformed_values")
  expect_identical(
    e$plain_error(err),
    "Column 'x' has values that are not numeric, such as 'oops'. See Technical details for the rows."
  )
  detail <- e$ingest_detail(err)
  expect_match(detail, "rows: 1, 2, 3", fixed = TRUE)
  expect_match(detail, "19, 20, ...\n  values: 'oops'", fixed = TRUE)
  expect_no_match(detail, "Affected", fixed = TRUE)
})

test_that("real exports are auto-detected and load through the app's loader", {
  skip_if_not_installed("shiny")
  app_file <- file.path(app_dir(), "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  fx <- function(f) normalizePath(testthat::test_path("fixtures", "real", f))
  expect_identical(e$guess_dialect(fx("trackmate_v7_spots.csv")), "trackmate")
  expect_identical(e$guess_dialect(fx("trackmate_v6_spots.csv")), "trackmate")
  expect_identical(e$guess_dialect(fx("idtrackerai_trajectories.csv")), "idtrackerai_wide")
  expect_identical(e$guess_dialect(fx("toxtrac_tracking_realspace.txt")), "toxtrac")
  expect_identical(e$guess_dialect(fx("dlc_single_wasp_predictions.csv")), "deeplabcut")
  expect_identical(e$guess_dialect(fx("dlc_two_mice_predictions.csv")), "deeplabcut")
  expect_identical(e$guess_dialect(fx("sleap_three_nodes.analysis.csv")), "sleap")
  expect_false("deeplabcut_multiheader" %in% e$DIALECT_CHOICES)
  # a flat <bodypart>_x table is offered to the deeplabcut dialect, which refuses it by name
  flat <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("frame,head_x,head_y,head_likelihood", "0,1,2,0.9"), flat)
  expect_identical(e$guess_dialect(flat), "deeplabcut")
  err <- tryCatch(radiatR::read_tracks(flat, dialect = "deeplabcut"), error = function(e) e)
  expect_match(e$plain_error(err), "is not a DeepLabCut output file", fixed = TRUE)
  # a binary file, or one in another encoding, is generic rather than an error
  bin <- withr::local_tempfile(fileext = ".xlsx")
  writeBin(as.raw(c(0x50, 0x4B, 0x03, 0x04, 0xFE, 0xFF, 0x0A, 0x80, 0x0A)), bin)
  expect_identical(suppressWarnings(e$guess_dialect(bin)), "generic")
  latin <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("id,time,x,y\na,1,"), as.raw(0xE9), charToRaw(",2\n")), latin)
  expect_identical(e$guess_dialect(latin), "generic")
  # a re-save with every header field quoted is still a DeepLabCut file
  quoted <- withr::local_tempfile(fileext = ".csv")
  lines <- readLines(fx("dlc_single_wasp_predictions.csv"))
  lines[1:3] <- vapply(strsplit(lines[1:3], ",", fixed = TRUE),
                       function(f) paste0("\"", f, "\"", collapse = ","), character(1))
  writeLines(lines, quoted)
  expect_identical(e$guess_dialect(quoted), "deeplabcut")
  # a dialect's refusal is the headline, not a guess at it from its wording
  err <- tryCatch(radiatR::read_tracks(fx("dlc_CollectedData_Pranav.csv"), dialect = "deeplabcut"),
                  error = function(e) e)
  expect_match(e$plain_error(err), "labelled training data", fixed = TRUE)
  expect_identical(e$guess_dialect(system.file("extdata", "sleap_example.csv", package = "radiatR")),
                   "sleap")
  # a plain table stays generic, and so does one that merely has x1/y1 columns
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("ID,Frame,X,Y", "a,1,0,0"), p)
  expect_identical(e$guess_dialect(p), "generic")
  writeLines(c("id,frame,x1,y1,x2,y2", "a,1,0,0,1,1"), p)
  expect_identical(e$guess_dialect(p), "generic")

  ts <- e$load_ts(fx("toxtrac_tracking_realspace.txt"), "toxtrac", normalize_xy = FALSE)
  expect_identical(ts@meta$time_unit, "seconds")
  ts <- e$load_ts(fx("trackmate_v7_spots.csv"), "trackmate", normalize_xy = FALSE)
  expect_equal(nrow(as.data.frame(ts)), 6L)

  # TrackMate spots in no track: a headline the user can act on in TrackMate
  err <- tryCatch(e$load_ts(fx("trackmate_v6_spots.csv"), "trackmate", normalize_xy = FALSE),
                  error = function(err) err)
  expect_match(e$plain_error(err), "Spots in tracks", fixed = TRUE)
  expect_false(grepl("Affected ids", e$ingest_detail(err), fixed = TRUE))
})

test_that("an uploaded TrackMate 7 file is detected, loaded in seconds, and needs no frame rate", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- normalizePath(testthat::test_path("fixtures", "real", "trackmate_v7_spots.csv"))
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "spots.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_null(rv$error)
    expect_identical(rv$dialect, "trackmate")
    expect_identical(rv$ts@meta$time_unit, "seconds")
    expect_true(kin_fps_ok())
  })
})

test_that("an uploaded idtracker.ai file is detected; its 'nan' positions stop the load", {
  # The app loads with on_invalid = "error" until the user agrees to drop the
  # rows (test-app-drop-control.R), so a file with missing positions is refused
  # by name rather than half-loaded.
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- normalizePath(testthat::test_path("fixtures", "real", "idtrackerai_trajectories.csv"))
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "trajectories.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_match(rv$error, "2 row(s) of this file have no x/y position", fixed = TRUE)
    expect_match(rv$error_detail, "non-finite coordinates", fixed = TRUE)
  })
})

test_that("a value written with the other decimal mark names the control", {
  skip_if_not_installed("shiny")
  e <- app_env()
  tsv <- withr::local_tempfile(fileext = ".tsv")
  writeLines(c("id\tframe\tx\ty", paste0("a\t", 1:3, "\t0,", 1:3, "\t0,5")), tsv)
  err <- tryCatch(read_tracks(tsv), error = function(cnd) cnd)
  expect_match(e$plain_error(err), "If the file's decimal mark is a comma, set Decimal mark to Comma (,).",
               fixed = TRUE)
  semi <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;frame;x;y", paste0("a;", 1:3, ";0.", 1:3, ";0,5")), semi)
  err <- tryCatch(read_tracks(semi, read_opts = list(decimal = ",")), error = function(cnd) cnd)
  expect_match(e$plain_error(err), "set Decimal mark to Point (.)", fixed = TRUE)
})

test_that("no decimal advice without a suggestion", {
  skip_if_not_installed("shiny")
  e <- app_env()
  err <- tryCatch(read_tracks(data.frame(id = "a", time = 1:3, x = "oops", y = 0)),
                  error = function(cnd) cnd)
  expect_null(err$suggested_decimal)      # no number under either mark
  expect_false(grepl("Decimal mark", e$plain_error(err), fixed = TRUE))
})

test_that("a delimiter a dialect cannot use is refused in app words", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  dlc <- normalizePath(testthat::test_path("fixtures", "real", "dlc_single_wasp_predictions.csv"))
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = dlc, name = "wasp.csv"))
    session$setInputs(dialect_sel = "deeplabcut", delim_sel = ";", decimal_sel = "auto")
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_match(rv$error, "set Delimiter to Auto-detect", fixed = TRUE)
  })
})

test_that("an option a dialect cannot use names the app control", {
  skip_if_not_installed("shiny")
  e <- app_env()
  dlc <- testthat::test_path("fixtures", "real", "dlc_single_wasp_predictions.csv")
  err <- tryCatch(read_tracks(dlc, dialect = "deeplabcut", read_opts = list(delim = ";")),
                  error = function(cnd) cnd)
  expect_identical(e$plain_error(err),
                   "This file's format sets its own delimiter: set Delimiter to Auto-detect.")
  # a refusal with no app control behind it is shown as the package words it
  register_loader_dialect("plain_app_test", function(x) utils::read.csv(x), overwrite = TRUE)
  withr::defer(rm(list = "plain_app_test", envir = radiatR:::.loader_registry))
  err <- tryCatch(read_tracks(dlc, dialect = "plain_app_test", read_opts = list(decimal = ",")),
                  error = function(cnd) cnd)
  expect_match(e$plain_error(err), "takes no read_opts", fixed = TRUE)
})

test_that("a text-export dialect given an Excel option says to upload the text export", {
  skip_if_not_installed("shiny")
  e <- app_env()
  txt <- "This software's files are read from its text export, not from an Excel workbook: upload the .txt or .csv export."
  dlc <- testthat::test_path("fixtures", "real", "dlc_single_wasp_predictions.csv")
  err <- tryCatch(read_tracks(dlc, dialect = "deeplabcut", read_opts = list(sheet = "Sheet1")),
                  error = function(cnd) cnd)
  expect_identical(e$plain_error(err), txt)
  err <- tryCatch(read_tracks(dlc, dialect = "deeplabcut", read_opts = list(ext = "xlsx")),
                  error = function(cnd) cnd)
  expect_identical(e$plain_error(err), txt)
})

test_that("an EthoVision delimiter mismatch names the Delimiter control", {
  skip_if_not_installed("shiny")
  e <- app_env()
  eth <- tempfile(fileext = ".txt")
  writeLines(c('"Number of header lines:";"2"', '"Experiment";"x"'), eth)
  err <- tryCatch(read_tracks(eth, dialect = "ethovision", read_opts = list(delim = ",")),
                  error = function(cnd) cnd)
  expect_identical(e$plain_error(err),
                   "This file's format sets its own delimiter: set Delimiter to Auto-detect.")
})

test_that("an ambiguous decimal mark names the Decimal mark control", {
  e <- app_env()
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;t;x;y;label", "a;1;1.234;2.345;1.5", "a;2;1.240;2.350;2.5"), p)
  err <- tryCatch(read_tracks(p, mapping = list(id = "id", time = "t", x = "x", y = "y")),
                  error = function(x) x)
  expect_s3_class(err, "radiatR_ambiguous_decimal")
  msg <- e$plain_error(err)
  expect_match(msg, "Can't tell the decimal mark of column 'x'", fixed = TRUE)
  expect_match(msg, "set Decimal mark to Point (.)", fixed = TRUE)
  expect_false(grepl("read_opts", msg, fixed = TRUE))
})

test_that("the no-whole-row ambiguous decimal variant has its own plain message", {
  e <- app_env()
  err <- tryCatch(radiatR:::radiatR_abort_ambiguous_decimal("f.csv", character(0),
                                                            character(0), character(0)),
                  error = function(x) x)
  expect_match(e$plain_error(err), "can't be told from the file's first lines", fixed = TRUE)
})
