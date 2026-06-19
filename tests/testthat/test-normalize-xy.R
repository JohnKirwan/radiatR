# normalize_xy = TRUE must arena-scale per trajectory (centre on the bounding-box
# midpoint, scale so the furthest point sits at radius 1), preserving trajectory
# shape -- NOT collapse every point onto the unit circle.

test_that("normalize_xy=TRUE yields the same headings regardless of raw scale/offset", {
  # A non-radial (L-shaped) trajectory, then the same trajectory in a shifted and
  # scaled "raw pixel" frame. Arena scaling must recover identical headings: that
  # is the whole point -- load any pixel data, get correct arena-relative headings.
  # non-radial track with a single, unambiguous furthest-from-centre point (so
  # `distal` is well defined and not sensitive to float ties).
  base <- data.frame(id = "a", time = 1:6,
                     x = c(0.1, -0.5, 0.3, 0.9, 0.2, 0.0),
                     y = c(-0.4, 0.1, 0.6, 0.9, 0.2, 0.3))
  raw  <- transform(base, x = x * 837 + 5123, y = y * 837 + 2987)

  tsB <- tracks(base, id = "id", time = "time", x = "x", y = "y", normalize_xy = TRUE)
  tsR <- tracks(raw,  id = "id", time = "time", x = "x", y = "y", normalize_xy = TRUE)

  for (rule in c("net", "distal", "origin_mean", "velocity_mean", "straight", "pca_axis")) {
    hB <- as.numeric(derive_headings(tsB, rule = rule, on_missing = "quiet")$heading)
    hR <- as.numeric(derive_headings(tsR, rule = rule, on_missing = "quiet")$heading)
    expect_equal(hR, hB, tolerance = 1e-6, info = rule)
  }
})

test_that("normalize_xy=TRUE preserves shape (radii span a range, max radius = 1)", {
  # a square in raw pixels: corners equidistant from centre -> all at radius 1,
  # but a square is not collapsed to a single rim arc.
  df <- data.frame(id = "a", time = 1:4,
                   x = c(100, 300, 300, 100), y = c(100, 100, 300, 300))
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y", normalize_xy = TRUE)
  X <- ts@data[[ts@cols$x]]; Y <- ts@data[[ts@cols$y]]
  expect_equal(max(sqrt(X^2 + Y^2)), 1, tolerance = 1e-9)  # furthest point on the rim
  expect_equal(mean(X), 0, tolerance = 1e-9)               # symmetric square -> centred
  expect_equal(mean(Y), 0, tolerance = 1e-9)
})

test_that("normalize_xy=TRUE: radius-based rules work on scaled coords (crossing not NA)", {
  s  <- seq(0, 1, length.out = 12)
  df <- data.frame(id = "a", time = seq_along(s), x = 500 + s * 300, y = 500)
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y", normalize_xy = TRUE)
  cr <- derive_headings(ts, rule = "crossing", circ0 = 0.3, circ1 = 0.6)$heading
  expect_false(is.na(cr))
})

test_that("normalize_xy=TRUE maps a degenerate (zero-extent) trajectory to the origin", {
  df <- data.frame(id = "a", time = 1:3, x = c(500, 500, 500), y = c(300, 300, 300))
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y", normalize_xy = TRUE)
  X <- ts@data[[ts@cols$x]]; Y <- ts@data[[ts@cols$y]]
  expect_true(all(X == 0) && all(Y == 0))
})

test_that("normalize_xy=TRUE scales each trajectory independently", {
  df <- rbind(
    data.frame(id = "a", time = 1:4, x = c(90, 110, 110, 90),     y = c(90, 90, 110, 110)),
    data.frame(id = "b", time = 1:4, x = c(800, 1200, 1200, 800), y = c(800, 800, 1200, 1200)))
  ts <- tracks(df, id = "id", time = "time", x = "x", y = "y", normalize_xy = TRUE)
  d  <- ts@data; idc <- ts@cols$id
  X <- d[[ts@cols$x]]; Y <- d[[ts@cols$y]]
  ra <- sqrt(X[d[[idc]] == "a"]^2 + Y[d[[idc]] == "a"]^2)
  rb <- sqrt(X[d[[idc]] == "b"]^2 + Y[d[[idc]] == "b"]^2)
  expect_equal(max(ra), 1, tolerance = 1e-9)
  expect_equal(max(rb), 1, tolerance = 1e-9)
})
