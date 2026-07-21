app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

test_that("downloads are gated while an ingest error is active", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    bad <- file.path(tempdir(), "no-such-tracks.csv")
    session$setInputs(file = list(datapath = bad, name = "bad.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_false(results_ok())
  })
})

test_that("results_ok() is TRUE after a successful example load", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(load_example = 1)   # match the real example-trigger id
    expect_true(results_ok())
  })
})

test_that("kinematics downloads stay gated on a re-upload-and-fail cycle", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  shiny::testServer(app_dir(), {
    # Successful load first, so rv$ts is populated ...
    session$setInputs(input_type = "trajectories")
    session$setInputs(load_example = 1)
    expect_true(results_ok())

    # ... then a failed re-upload leaves rv$ts stale while rv$error is set.
    bad <- file.path(tempdir(), "no-such-tracks.csv")
    session$setInputs(file = list(datapath = bad, name = "bad.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 2)
    expect_false(results_ok())

    # The two kinematics download handlers must honor the same gate rather
    # than exporting the stale rv$ts-derived code/plot.
    expect_error(output$dl_kinematics_code())
    expect_error(output$dl_kin_plot())
  })
})
