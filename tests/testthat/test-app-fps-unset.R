app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

test_that("frame rate starts unset and blocks kinematics for numeric-frame data", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    # Example tracks are numeric-frame; clear any adopted rate to model unset.
    session$setInputs(frame_rate = NA)
    expect_false(.fps_is_set(fps_rv()))
    expect_false(isTRUE(kin_fps_ok()))
  })
})

test_that("clearing the frame rate re-engages gating and clears the kin input", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    session$setInputs(frame_rate = 25)
    expect_true(.fps_is_set(fps_rv()))
    session$setInputs(frame_rate = NA)
    expect_false(.fps_is_set(fps_rv()))
    expect_false(isTRUE(kin_fps_ok()))
    expect_true(is.na(input$kin_frame_rate) || is.null(input$kin_frame_rate))
  })
})

test_that("a data-provided fps is labelled 'data', not relabelled 'user' by the echo", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    if (.fps_is_set(frame_rate(rv$ts))) {
      expect_identical(fps_source_rv(), "data")
    } else {
      succeed("example carries no fps; data-source path not exercised")
    }
  })
})

test_that("a user-entered fps is labelled 'user'", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    session$setInputs(frame_rate = 42)
    expect_identical(fps_source_rv(), "user")
    expect_equal(fps_rv(), 42)
  })
})

test_that("kin_note prompts for a capture rate when fps is unset", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    session$setInputs(frame_rate = NA)
    html <- paste(as.character(output$kin_note), collapse = " ")
    expect_match(html, "frame rate", ignore.case = TRUE)
  })
})

test_that("kin_note confirms a data-provided rate", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    if (.fps_is_set(frame_rate(rv$ts))) {
      html <- paste(as.character(output$kin_note), collapse = " ")
      expect_match(html, "From data", ignore.case = TRUE)
    } else {
      succeed("example carries no fps; data-note path not exercised")
    }
  })
})
