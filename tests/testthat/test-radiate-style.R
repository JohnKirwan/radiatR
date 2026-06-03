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

test_that("show_tracks toggles the trajectory layer", {
  ts <- simulate_tracks(n_points = 30, seed = 123, output = "trajset")

  # The track layer is the geom_path whose data has one row per track point;
  # the arena circle is also a geom_path but with its own small generated data.
  n_track_layers <- function(g) {
    n_pts <- nrow(ts@data)
    sum(vapply(g$layers, function(l) {
      inherits(l$geom, "GeomPath") &&
        is.data.frame(l$data) && nrow(l$data) == n_pts
    }, logical(1)))
  }

  g_on  <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, show_tracks = TRUE)
  g_off <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                   show_labels = FALSE, show_tracks = FALSE)

  expect_equal(n_track_layers(g_on), 1L)
  expect_equal(n_track_layers(g_off), 0L)
  # the arena still renders without tracks
  expect_s3_class(g_off, "ggplot")
  expect_silent(ggplot2::ggplot_build(g_off))
})
