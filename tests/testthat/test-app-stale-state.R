app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

test_that("a failed headings upload clears the prior example analysis", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "headings")
    session$setInputs(load_example_hd = 1)      # example headings -> Configure
    expect_false(is.null(rv$hd))
    expect_identical(rv$source, "example")

    # Upload a file whose path cannot be read: read.csv() throws.
    bad <- file.path(tempdir(), "does-not-exist-headings.csv")
    session$setInputs(file = list(datapath = bad, name = "bad.csv"))

    expect_null(rv$raw_hd)
    expect_null(rv$hd)
    expect_null(rv$hd_map)
    expect_null(rv$method)
    expect_identical(rv$source, "file")
    expect_true(!is.null(rv$error) && nzchar(rv$error))

    # Clicking Next must NOT show stale example results.
    session$setInputs(go2 = 1)
    expect_equal(rv$step, 1L)
    expect_match(rv$error, "upload", ignore.case = TRUE)
  })
})
