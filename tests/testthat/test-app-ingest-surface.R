# Too slow for CRAN's check time budget; runs locally and on CI.
skip_on_cran()

app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
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
