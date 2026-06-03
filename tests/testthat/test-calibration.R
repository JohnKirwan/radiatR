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
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "angle", angle_unit = "radians", normalize_xy = FALSE)
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
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", angle = "angle", angle_unit = "radians", normalize_xy = FALSE)
  ts <- log_transform(ts, step = "preexisting", traj_ids = ids(ts), implementation = "test_step", params = list(list()))
  n_ids <- length(ids(ts))
  calibrated <- calibrate_positions(ts, model)
  hist <- transform_history(calibrated)
  expect_equal(hist$step[1], "calibration")
  expect_equal(hist$order[1], 0L)
  expect_true(all(hist$order[-seq_len(n_ids)] >= 1L))
  expect_s4_class(calibrated@meta$calibration_model, "CalModel")
})

# ---- cal_model() builder -----------------------------------------------------

test_that("cal_model assembles the transposed-K convention", {
  m <- cal_model(fx = 800, fy = 810, cx = 640, cy = 360,
                 k1 = -0.2, k2 = 0.05, p1 = 0.001, p2 = -0.002, k3 = 0.01)

  expect_s4_class(m, "CalModel")
  # focal lengths on the diagonal, principal point in the bottom row
  expect_equal(m@K[1, 1], 800)
  expect_equal(m@K[2, 2], 810)
  expect_equal(m@K[3, 1], 640)
  expect_equal(m@K[3, 2], 360)
  expect_equal(m@K[3, 3], 1)
  # distortion stored as a named radial-then-tangential vector
  expect_equal(unname(m@k[c("k1", "k2", "k3", "p1", "p2")]),
               c(-0.2, 0.05, 0.01, 0.001, -0.002))
})

test_that("cal_model defaults F to the focal lengths (undistorted pixels)", {
  m <- cal_model(fx = 800, fy = 810, cx = 640, cy = 360)
  expect_equal(m@F, c(800, 810))

  # with no distortion and F = focal lengths, output is undistorted pixels
  # measured from the principal point: the principal point maps to the origin
  expect_equal(cam_cal_pt(640, 360, m@K, m@k, m@F), c(0, 0))
  # and a pixel offset is preserved unchanged
  expect_equal(cam_cal_pt(700, 400, m@K, m@k, m@F), c(60, 40))
})

test_that("cal_model accepts a scalar or custom F", {
  expect_equal(cal_model(800, 810, 640, 360, F = 5)@F, c(5, 5))
  expect_equal(cal_model(800, 810, 640, 360, F = c(5, 6))@F, c(5, 6))
})

test_that("cal_model rejects non-scalar or non-finite intrinsics", {
  expect_error(cal_model(fx = c(1, 2), fy = 1, cx = 0, cy = 0), "fx")
  expect_error(cal_model(fx = NA_real_, fy = 1, cx = 0, cy = 0), "fx")
  expect_error(cal_model(fx = 1, fy = 1, cx = 0, cy = 0, F = c(1, 2, 3)), "F")
})

# ---- read_calibration(): MATLAB ----------------------------------------------

test_that("read_calibration parses a MATLAB IntrinsicMatrix (transposed)", {
  # MATLAB cameraParameters.IntrinsicMatrix: [fx 0 0; s fy 0; cx cy 1]
  mat <- list(
    IntrinsicMatrix = matrix(c(800, 0, 0,
                               0, 810, 0,
                               641, 361, 1), 3, 3, byrow = TRUE),
    RadialDistortion = c(-0.2, 0.05, 0.01),
    TangentialDistortion = c(0.001, -0.002)
  )
  params <- radiatR:::.parse_matlab_calib(mat)
  expect_equal(params$fx, 800)
  expect_equal(params$fy, 810)
  expect_equal(params$cx, 641)
  expect_equal(params$cy, 361)
  expect_equal(params$k1, -0.2)
  expect_equal(params$k3, 0.01)
  expect_equal(params$p1, 0.001)
  expect_equal(params$default_base, 1)
})

test_that("read_calibration parses a MATLAB standard K (R2022b+)", {
  mat <- list(
    K = matrix(c(800, 0, 641,
                 0, 810, 361,
                 0, 0, 1), 3, 3, byrow = TRUE),
    RadialDistortion = c(-0.2, 0.05)
  )
  params <- radiatR:::.parse_matlab_calib(mat)
  expect_equal(params$fx, 800)
  expect_equal(params$cx, 641)
  expect_equal(params$cy, 361)
  expect_equal(params$k3, 0)
})

test_that("read_calibration applies the 1-based MATLAB principal_base default", {
  skip_if_not_installed("R.matlab")
  tmp <- tempfile(fileext = ".mat")
  R.matlab::writeMat(tmp,
    IntrinsicMatrix = matrix(c(800, 0, 0,
                               0, 810, 0,
                               641, 361, 1), 3, 3, byrow = TRUE),
    RadialDistortion = c(-0.2, 0.05),
    TangentialDistortion = c(0, 0))
  m <- read_calibration(tmp, source = "matlab")
  # default base of 1 shifts the 1-based principal point to 0-based
  expect_equal(m@K[3, 1], 640)
  expect_equal(m@K[3, 2], 360)
})

# ---- read_calibration(): OpenCV ----------------------------------------------

test_that("read_calibration parses OpenCV JSON", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(
    camera_matrix = list(rows = 3, cols = 3, dt = "d",
      data = c(800, 0, 640, 0, 810, 360, 0, 0, 1)),
    distortion_coefficients = list(rows = 5, cols = 1, dt = "d",
      data = c(-0.2, 0.05, 0.001, -0.002, 0.01))
  ), auto_unbox = TRUE), tmp)

  m <- read_calibration(tmp, source = "opencv")
  expect_equal(m@K[1, 1], 800)
  expect_equal(m@K[2, 2], 810)
  expect_equal(m@K[3, 1], 640)   # 0-based default, no shift
  expect_equal(m@K[3, 2], 360)
  expect_equal(unname(m@k["k1"]), -0.2)
  expect_equal(unname(m@k["p1"]), 0.001)
  expect_equal(unname(m@k["k3"]), 0.01)
})

test_that("read_calibration parses OpenCV YAML with non-standard tags", {
  skip_if_not_installed("yaml")
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "%YAML:1.0",
    "---",
    "camera_matrix: !!opencv-matrix",
    "   rows: 3",
    "   cols: 3",
    "   dt: d",
    "   data: [ 800., 0., 640., 0., 810., 360., 0., 0., 1. ]",
    "distortion_coefficients: !!opencv-matrix",
    "   rows: 5",
    "   cols: 1",
    "   dt: d",
    "   data: [ -0.2, 0.05, 0.001, -0.002, 0.01 ]"
  ), tmp)

  m <- read_calibration(tmp, source = "opencv")
  expect_equal(m@K[1, 1], 800)
  expect_equal(m@K[3, 1], 640)
  expect_equal(unname(m@k["p2"]), -0.002)
})

# ---- read_calibration(): CSV -------------------------------------------------

test_that("read_calibration parses a wide CSV", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    fx = 800, fy = 810, cx = 640, cy = 360,
    k1 = -0.2, k2 = 0.05, k3 = 0.01, p1 = 0.001, p2 = -0.002
  ), tmp, row.names = FALSE)

  m <- read_calibration(tmp, source = "csv")
  expect_equal(m@K[1, 1], 800)
  expect_equal(m@K[3, 2], 360)
  expect_equal(unname(m@k["k2"]), 0.05)
})

test_that("read_calibration parses a long parameter/value CSV", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    parameter = c("fx", "fy", "cx", "cy", "k1"),
    value = c(800, 810, 640, 360, -0.2)
  ), tmp, row.names = FALSE)

  m <- read_calibration(tmp, source = "csv")
  expect_equal(m@K[2, 2], 810)
  expect_equal(unname(m@k["k1"]), -0.2)
  expect_equal(unname(m@k["k3"]), 0)
})

test_that("read_calibration auto-detects the source from the extension", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(fx = 1, fy = 1, cx = 0, cy = 0),
                   tmp, row.names = FALSE)
  expect_s4_class(read_calibration(tmp), "CalModel")
})

test_that("read_calibration errors clearly on a missing file or unknown type", {
  expect_error(read_calibration("does-not-exist.csv"), "not found")
  tmp <- tempfile(fileext = ".bogus")
  file.create(tmp)
  expect_error(read_calibration(tmp), "infer calibration source")
})

test_that("an imported model round-trips through calibrate_positions", {
  m <- cal_model(fx = 800, fy = 800, cx = 640, cy = 360)
  df <- data.frame(
    id = c("A", "A"), time = c(0, 1),
    x = c(640, 740), y = c(360, 360), angle = c(0, 0)
  )
  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y",
                angle = "angle", angle_unit = "radians", normalize_xy = FALSE)
  calibrated <- calibrate_positions(ts, m)
  # principal point maps to origin; a +100px offset stays +100 (no distortion)
  expect_equal(calibrated@data$x, c(0, 100))
  expect_equal(calibrated@data$y, c(0, 0))
})
