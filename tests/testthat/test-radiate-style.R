test_that("radiate classic style generates a ggplot", {
  tracks <- simulate_tracks(n_trials = 3, n_points = 30, seed = 123)
  expect_s3_class(
    radiate(
      tracks,
      x_col = "rel_x",
      y_col = "rel_y",
      group_col = "trial_id",
      colour_col = "trial_id",
      label_col = "trial_id",
      show_arrow = FALSE
    ),
    "ggplot"
  )
})
