test_that("app ships the logo and lean favicon assets", {
  www <- system.file("app", "www", package = "radiatR")
  if (!nzchar(www) || !dir.exists(www))
    www <- testthat::test_path("..", "..", "inst", "app", "www")
  expect_true(dir.exists(www))
  for (f in c("logo.png", "favicon.ico", "favicon-16x16.png",
              "favicon-32x32.png", "apple-touch-icon.png")) {
    expect_true(file.exists(file.path(www, f)), info = f)
  }
})

test_that("app.R parses without error", {
  app_r <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_r) || !file.exists(app_r))
    app_r <- testthat::test_path("..", "..", "inst", "app", "app.R")
  expect_true(file.exists(app_r))
  expect_silent(parse(app_r))
})
