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
test_that("checkerboard_points matches MATLAB ordering", {
  pts <- checkerboard_points(c(7, 9), square_size = c(2, 3), origin = c(1, 5))
  expect_equal(nrow(pts), (7 - 1) * (9 - 1))
  expect_equal(pts$row[1], 0L)
  expect_equal(pts$col[1], 0L)
  expect_equal(pts$x[1], 1)
  expect_equal(pts$y[1], 5)
  last <- tail(pts, 1)
  expect_equal(last$row, 5L)
  expect_equal(last$col, 7L)
  expect_equal(last$x, 1 + 7 * 3)
  expect_equal(last$y, 5 + 5 * 2)
  mat <- checkerboard_points(c(4, 6), square_size = 10, as_tibble = FALSE)
  expect_equal(dim(mat), c((4 - 1) * (6 - 1), 2))
  expect_equal(mat[1, ], c(0, 0))
  expect_equal(mat[nrow(mat), ], c(4 * 10, 2 * 10))
})
test_that("calibrate_positions records calibration transform", {
  K <- diag(c(1, 1, 1))
  model <- new("CalModel", K = K, k = c(0, 0), F = c(1, 1))
  df <- data.frame(
    id = c("A", "A", "B", "B"),
    time = c(1, 2, 1, 2),
    x = c(1, 2, 3, 4),
    y = c(0, 1, 1, 2),
    angle = c(0, 0, 0, 0)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "angle", normalize_xy = FALSE)
  ts <- log_transform(ts, step = "preexisting", traj_ids = ids(ts), implementation = "test_step", params = list(list()))
  n_ids <- length(ids(ts))
  calibrated <- calibrate_positions(ts, model)
  hist <- transform_history(calibrated)
  expect_equal(hist$step[1], "calibration")
  expect_equal(hist$order[1], 0L)
  expect_true(all(hist$order[-seq_len(n_ids)] >= 1L))
  expect_s4_class(calibrated@meta$calibration_model, "CalModel")
})

test_that("calibration point helpers persist data", {
  board_dims <- c(3, 4)
  square_size <- 1
  template <- checkerboard_points(board_dims, square_size)
  world <- as.matrix(template[, c("x", "y")])
  image_points <- list(world + 0.1, world + 0.2)

  tbl <- calibration_points_tibble(image_points)
  expect_s3_class(tbl, "data.frame")
  expect_equal(calibration_points_from_tibble(tbl), image_points)

  tmp <- tempfile(fileext = ".csv")
  write_calibration_points(image_points, tmp)
  expect_true(file.exists(tmp))
  expect_equal(read_calibration_points(tmp), image_points)

  calib <- calibration_from_points(board_dims, square_size, tbl, quiet = TRUE)
  expect_s4_class(calib$model, "CalModel")
  expect_equal(length(calib$image_points), length(image_points))
  expect_s3_class(calib$points, "data.frame")

  sess <- calibration_session(board_dims = board_dims, square_size = square_size,
                               pattern = "chessboard", load_points = tbl, quiet = TRUE)
  expect_equal(sess$image_points, image_points)

  tmp2 <- tempfile(fileext = ".csv")
  calibration_session(board_dims = board_dims, square_size = square_size,
                      pattern = "chessboard", load_points = tbl, quiet = TRUE,
                      save_points = tmp2)
  expect_true(file.exists(tmp2))
  expect_equal(read_calibration_points(tmp2), image_points)
})

