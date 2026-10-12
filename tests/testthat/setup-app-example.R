# Load a small example in the in-process app tests (shiny::testServer and
# sourced app.R): the first 5 tracks of each `arc` level, 40 of cpunctatus's
# 235. Those tests check wiring, not the dataset, and drawing and summarising
# all 235 tracks dominated the suite's run time. See example_ts() in
# inst/app/app.R.
#
# The shinytest2 browser tests start the app in a separate R process, which
# does not inherit this option, so they still exercise the full example.

withr::local_options(
  radiatR.app_example_per_arc = 5L,
  .local_envir = testthat::teardown_env()
)
