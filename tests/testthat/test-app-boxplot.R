test_that("toggling show_boxplot flows into the built spec", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    expect_equal(rv$step, 2L)
    session$setInputs(go3 = 1)                 # derive headings -> Results
    session$setInputs(show_boxplot = TRUE)
    sp <- current_spec()
    expect_true(isTRUE(sp$show$boxplot))
  })
})

test_that("boxplot_note warns only when the boxplot is on and not drawable", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")
  note <- function(out) paste(as.character(out), collapse = " ")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    session$setInputs(show_boxplot = FALSE)
    expect_identical(note(output$boxplot_note), "")
    # Antipodal two-cluster headings: the circular median is non-unique, so the
    # boxplot is genuinely not drawable (drawable = FALSE).
    rv$hd <- data.frame(id = "a", time = seq_len(100),
                        heading = rep(c(0, pi), each = 50))
    session$setInputs(show_boxplot = TRUE)
    expect_match(note(output$boxplot_note), "not drawn")
  })
})
