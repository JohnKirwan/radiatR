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
