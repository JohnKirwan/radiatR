# Unit tests for the shared Results-sidebar UI builders in inst/app/ui_helpers.R.
# Source the module standalone (no app launch), mirroring test-plot-spec.R.
.uh <- system.file("app", "ui_helpers.R", package = "radiatR")
if (!nzchar(.uh)) .uh <- file.path("..", "..", "inst", "app", "ui_helpers.R")
source(.uh, local = TRUE)

test_that("ui_image_export namespaces ids by prefix", {
  kin <- as.character(ui_image_export("kin_"))
  expect_true(grepl("kin_plot_fmt", kin, fixed = TRUE))
  expect_true(grepl("kin_plot_w",   kin, fixed = TRUE))
  expect_true(grepl("kin_plot_dpi", kin, fixed = TRUE))
  expect_true(grepl("dl_kin_plot",  kin, fixed = TRUE))
  expect_true(grepl("input.kin_plot_fmt", kin, fixed = TRUE))   # conditionalPanel
})

test_that("ui_image_export('') keeps the original Circular-plots ids", {
  cir <- as.character(ui_image_export(""))
  expect_true(grepl("plot_fmt", cir, fixed = TRUE))
  expect_true(grepl("dl_plot",  cir, fixed = TRUE))
  expect_false(grepl("kin_plot_fmt", cir, fixed = TRUE))        # not namespaced
})

test_that("ui_code_section wires the code output and both buttons", {
  s <- as.character(ui_code_section("kinematics_code", "dl_kinematics_code",
                                    "reproduces this profile"))
  expect_true(grepl("kinematics_code", s, fixed = TRUE))        # verbatim output id
  expect_true(grepl("dl_kinematics_code", s, fixed = TRUE))     # download id
  expect_true(grepl("Download .R", s, fixed = TRUE))
  expect_true(grepl("reproduces this profile", s, fixed = TRUE))
})
