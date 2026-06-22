app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

test_that("a single app-wide frame rate feeds the circular spec", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    session$setInputs(track_colour = "time", frame_rate = 25)
    expect_equal(fps_rv(), 25)
    expect_equal(current_spec()$frame_rate, 25)
  })
})
