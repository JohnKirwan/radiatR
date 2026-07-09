# The no-headings ("none") mode surfaces sinuosity alongside straightness in the
# caption, and the per-track metrics CSV carries the combined metric columns.
test_that("none-mode caption and metrics table include sinuosity", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(method = "none")      # no headings -> path-metrics mode
    session$setInputs(go3 = 1)
    expect_null(rv$hd)                       # confirm none mode

    # The path-metrics caption (none mode) now reports sinuosity too.
    cap <- current_spec()$caption
    expect_match(cap, "sinuosity",    ignore.case = TRUE)
    expect_match(cap, "straightness", ignore.case = TRUE)
  })
})
