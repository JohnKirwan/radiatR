# Too slow for CRAN's check time budget; runs locally and on CI.
skip_on_cran()

# testServer() evaluates its block with this file's environment as parent, not
# the app's, so the plot_spec.R helpers the block calls are sourced here.
.p <- system.file("app", "plot_spec.R", package = "radiatR")
if (!nzchar(.p) || !file.exists(.p))
  .p <- testthat::test_path("..", "..", "inst", "app", "plot_spec.R")
source(.p, local = TRUE)

# The Kinematics subtab's image-export wiring: kin_plot_fmt drives the
# dl_kin_plot filename and the kinematics figure/code reactives still resolve
# after the sidebar was reorganised into accordion sections (no shinytest2).
test_that("kin_plot_fmt drives the kinematics plot-download filename", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "app dir not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)                       # -> Results
    session$setInputs(kin_metric = "speed", kin_frame_rate = 30)

    # The kinematics spec/code reactives resolve after the restructure.
    expect_s3_class(kinematics_spec_to_plot(kinematics_spec(), rv$ts), "ggplot")
    expect_true(nzchar(kinematics_code_text()))

    # Image-export format flows into the download filename extension.
    session$setInputs(kin_plot_fmt = "png")
    expect_match(output$dl_kin_plot, "\\.png$")
    session$setInputs(kin_plot_fmt = "svg")
    expect_match(output$dl_kin_plot, "\\.svg$")
  })
})
