# The app detects EthoVision and ANY-maze uploads and loads them.
# Too slow for CRAN's check time budget; runs locally and on CI.
skip_on_cran()

app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}
ev_path <- function()
  normalizePath(testthat::test_path("fixtures", "real", "ethovision_trial.txt"))
am_path <- function()
  normalizePath(testthat::test_path("fixtures", "anymaze_synthetic.csv"))

test_that("EthoVision and ANY-maze files are detected from their first line", {
  skip_if_not_installed("shiny")
  app_file <- file.path(app_dir(), "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  expect_identical(e$guess_dialect(ev_path()), "ethovision")
  expect_identical(e$guess_dialect(am_path()), "anymaze")
  # unquoted, comma-separated, and American spelling
  p <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Number of header lines:,3", "Trial time,X center,Y center,", "(s),(mm),(mm),",
               "0,1,2,"), p)
  expect_identical(e$guess_dialect(p), "ethovision")
  writeLines(c("Time,Center position X,Center position Y", "0:00:00.000,1,2"), p)
  expect_identical(e$guess_dialect(p), "anymaze")
  # a UTF-8 byte-order mark before the first line is not part of it
  with_bom <- function(src) {
    q <- withr::local_tempfile(fileext = ".txt", .local_envir = parent.frame())
    writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), readBin(src, "raw", file.size(src))), q)
    q
  }
  expect_identical(e$guess_dialect(with_bom(ev_path())), "ethovision")
  expect_identical(e$guess_dialect(with_bom(am_path())), "anymaze")
  # a table that only mentions a centre is not an ANY-maze export
  writeLines(c("time,centre_x,centre_y", "0,1,2"), p)
  expect_identical(e$guess_dialect(p), "generic")
})

test_that("an uploaded EthoVision export is detected, offers the drop, and loads in seconds", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- ev_path()
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "Track-Trial 1.txt"))
    session$setInputs(dialect_sel = "auto")
    expect_identical(rv$dialect, "ethovision")
    # the generic table preview cannot read the header block: it must
    # say so (or show something) without setting the load error
    preview <- output$preview_tbl
    expect_null(rv$error)
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_identical(rv$drop_offer$kind, "positions")
    expect_match(paste(as.character(output$wizard), collapse = "\n"),
                 "Drop these 31 row(s) and continue", fixed = TRUE)
    session$setInputs(drop_btn = 1)
    expect_null(rv$error)
    expect_equal(nrow(as.data.frame(rv$ts)), 40L)
    expect_identical(rv$ts@meta$time_unit, "seconds")
    expect_true(kin_fps_ok())
  })
})

test_that("an uploaded ANY-maze export is detected, offers the drop, and loads in seconds", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- am_path()
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "Test 6.csv"))
    session$setInputs(dialect_sel = "auto")
    expect_identical(rv$dialect, "anymaze")
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_identical(rv$drop_offer$kind, "positions")
    session$setInputs(drop_btn = 1)
    expect_null(rv$error)
    expect_equal(nrow(as.data.frame(rv$ts)), 11L)
    expect_identical(rv$ts@meta$time_unit, "seconds")
  })
})
