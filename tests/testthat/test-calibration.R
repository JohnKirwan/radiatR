test_that("camera calibration helpers preserve identity under neutral parameters", {
  K <- diag(c(1, 1, 1))
  K[3, 3] <- 1
  model <- new("CalModel", K = K, k = c(0, 0), F = c(1, 1))

  df <- data.frame(
    id = c("A", "A"),
    time = c(0, 1),
    x = c(1, 2),
    y = c(0, 0),
    angle = c(0, 0)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "angle", normalize_xy = FALSE)
  calibrated <- calibrate_positions(ts, model)
  expect_equal(calibrated@data$x, df$x)
  expect_equal(calibrated@data$y, df$y)
})

test_that("cam_cal_many matches repeated cam_cal_pt", {
  K <- matrix(c(2, 0, 0,
                0, 3, 0,
                4, 5, 1), ncol = 3, byrow = TRUE)
  k <- c(0.1, 0.01)
  F <- c(100, 120)
  points <- matrix(c(50, 60, 70, 80), ncol = 2, byrow = TRUE)

  single <- t(apply(points, 1, function(pt) cam_cal_pt(pt[1], pt[2], K, k, F)))
  batch <- cam_cal_many(points, K, k, F)
  expect_equal(batch, single)
})
