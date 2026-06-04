test_that("radiate generates a ggplot for every named theme", {
  ts <- simulate_tracks(n_points = 30, seed = 123, output = "trajset")
  themes <- c("void", "minimal", "classic", "bw", "grey", "light",
              "dark", "linedraw")
  for (th in themes) {
    p <- radiate(
      ts,
      group_col  = "trial_id",
      colour_col = "trial_id",
      show_arrow = FALSE,
      theme      = th
    )
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("radial_theme returns the matching ggplot2 base theme", {
  expect_identical(radial_theme("bw"),      ggplot2::theme_bw(base_size = 11))
  expect_identical(radial_theme("minimal"), ggplot2::theme_minimal(base_size = 11))
  expect_identical(radial_theme("grey"),    ggplot2::theme_grey(base_size = 11))
  expect_identical(radial_theme("gray"),    radial_theme("grey"))   # alias
  expect_error(radial_theme("nope"))
})

test_that("dark theme uses light ink for the unit circle so it stays visible", {
  ts <- simulate_tracks(n_points = 20, seed = 7, output = "trajset")
  circle_colour <- function(p) {
    # the unit circle is the annotate('path') layer spanning the full circle
    for (l in p$layers) {
      d <- l$data
      if (is.data.frame(d) && all(c("x", "y") %in% names(d)) &&
          nrow(d) >= 999L &&
          isTRUE(all.equal(max(abs(d$x)), 1, tolerance = 1e-6))) {
        col <- if (!is.null(l$aes_params$colour)) l$aes_params$colour
               else l$aes_params$color
        if (!is.null(col)) return(col)
      }
    }
    NA_character_
  }
  p_dark  <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                     show_labels = FALSE, theme = "dark")
  p_light <- radiate(ts, group_col = "trial_id", show_arrow = FALSE,
                     show_labels = FALSE, theme = "minimal")
  expect_equal(circle_colour(p_dark),  "grey85")
  expect_equal(circle_colour(p_light), "black")
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
