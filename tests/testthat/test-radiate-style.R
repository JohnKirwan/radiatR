test_that("radiate classic style generates a ggplot", {
  ts <- simulate_tracks(n_points = 30, seed = 123, output = "trajset")
  expect_s3_class(
    radiate(
      ts,
      group_col  = "trial_id",
      colour_col = "trial_id",
      show_arrow = FALSE
    ),
    "ggplot"
  )
})
