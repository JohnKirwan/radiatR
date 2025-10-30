test_that("unit circle mapping normalises and inverts pixel coordinates", {
  origin <- c(320, 240)
  reference <- c(320, 140)

  mapping <- build_unit_circle_mapping(origin, reference, flip_y = TRUE)
  expect_equal(mapping$radius, 100)
  expect_true(is.function(mapping$map))
  expect_true(is.function(mapping$inverse))

  mapped_origin <- mapping$map(origin[1], origin[2])
  mapped_reference <- mapping$map(reference[1], reference[2])

  expect_equal(mapped_origin$trans_x, 0)
  expect_equal(mapped_origin$trans_y, 0)
  expect_equal(mapped_reference$trans_x, 0)
  expect_equal(mapped_reference$trans_y, 1, tolerance = 1e-6)
  expect_equal(mapped_reference$rel_theta_unit, 0, tolerance = 1e-6)

  sample_pt <- mapping$map(origin[1] + 50, origin[2])
  expect_equal(sample_pt$trans_x, 0.5, tolerance = 1e-6)
  expect_equal(sample_pt$trans_y, 0, tolerance = 1e-6)

  recovered <- mapping$inverse(sample_pt$trans_x, sample_pt$trans_y)
  expect_equal(recovered$x, origin[1] + 50, tolerance = 1e-6)
  expect_equal(recovered$y, origin[2], tolerance = 1e-6)
})

test_that("TrajSet retains raw coordinates when normalising", {
  df <- data.frame(
    id = c("a", "a"),
    time = c(0, 1),
    x = c(320, 340),
    y = c(240, 220)
  )

  ts <- TrajSet(df, id = "id", time = "time", x = "x", y = "y", normalize_xy = TRUE)
  expect_s4_class(ts, "TrajSet")
  expect_true(ts@meta$normalize_xy)

  expect_false(is.null(ts@cols$raw_x))
  expect_false(is.null(ts@cols$raw_y))
  expect_true(all(c(ts@cols$raw_x, ts@cols$raw_y) %in% names(ts@data)))
  expect_equal(ts@meta$raw_xy_cols$x, ts@cols$raw_x)
  expect_equal(ts@meta$raw_xy_cols$y, ts@cols$raw_y)

  normalised <- ts@data[[ts@cols$x]]
  raw_vals <- ts@data[[ts@cols$raw_x]]
  expect_false(isTRUE(all.equal(normalised, raw_vals)))
})

test_that("transform history propagates from mapping pipeline", {
  landmarks <- tibble::tibble(
    frame = c(1, 1),
    x = c(0, 0),
    y = c(0, 100)
  )
  tracks <- tibble::tibble(
    frame = 1:5,
    x = c(0, 25, 50, 25, 0),
    y = c(0, 20, 0, -20, 0)
  )
  file_tbl <- tibble::tibble(
    basename = "demo",
    landmark = "demo_point01.txt",
    track = "demo_point02.txt"
  )

  limits <- get_trial_limits(landmarks, tracks, file_tbl, vid_num = 1)
  res <- get_tracked_object_pos(limits, tracks)
  expect_s4_class(res, "TrajSet")
  history <- transform_history(res)

  expect_s3_class(history, "tbl_df")
  expect_equal(nrow(history), 1)
  expect_equal(history$step, "unit_circle_mapping")
  expect_true(all(c("origin", "reference", "map", "inverse") %in% names(history$params[[1]])))

  ts <- res
  extra <- log_transform(ts, step = "calibration",
                         traj_ids = history$id,
                         implementation = "calibrate_positions",
                         params = list(list(model = "identity")))
  logged <- transform_history(extra)
  expect_true(any(logged$step == "calibration"))
  expect_equal(max(logged$order), 2)
})
