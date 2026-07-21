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
