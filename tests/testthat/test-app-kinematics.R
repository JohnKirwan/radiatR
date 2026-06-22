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

test_that("the Kinematics tab renders a plot and emits matching code", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    session$setInputs(kin_metric = "speed", kin_colour_by = "", kin_frame_rate = 30)
    expect_false(is.null(output$kinematics_plot))
    expect_match(output$kinematics_code, "plot_profile(", fixed = TRUE)
    expect_match(output$kinematics_code, "set_frame_rate(ts, 30)", fixed = TRUE)
  })
})

test_that("kin_frame_rate participates in the shared frame rate", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    session$setInputs(kin_frame_rate = 24)
    expect_equal(fps_rv(), 24)
    session$setInputs(frame_rate = 18)
    expect_equal(fps_rv(), 18)
  })
})

test_that("Kinematics needs trajectory data; headings-only shows a note", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  note <- function(out) paste(as.character(out), collapse = " ")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    rv$ts <- NULL
    session$setInputs(kin_metric = "speed")
    expect_match(note(output$kin_note), "trajectory")
  })
})

test_that("Kinematics defaults to one track; 'All' overlays every track", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1); session$setInputs(go3 = 1)
    id1 <- as.character(ids(rv$ts))[1]
    session$setInputs(kin_metric = "speed", kin_colour_by = "", kin_track = id1)
    expect_identical(kinematics_spec()$track, id1)
    b1 <- ggplot2::ggplot_build(kinematics_spec_to_plot(kinematics_spec(), rv$ts))
    expect_equal(length(unique(b1$data[[1]]$group)), 1L)        # one track
    session$setInputs(kin_track = "")                          # All tracks
    expect_null(kinematics_spec()$track)
    bA <- ggplot2::ggplot_build(kinematics_spec_to_plot(kinematics_spec(), rv$ts))
    expect_gt(length(unique(bA$data[[1]]$group)), 1L)          # many tracks
  })
})

test_that("loading the example adopts the data's frame rate (0.2 fps)", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(load_example = 1); session$setInputs(go3 = 1)
    expect_equal(fps_rv(), frame_rate(rv$ts))   # adopted from cpunctatus (0.2)
  })
})
