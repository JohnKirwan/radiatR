test_that("derive_coords reproduces the loader mapping's derived fields", {
  set.seed(1)
  origin <- c(100, 100); reference <- c(160, 100)   # radius 60, ref at East
  m  <- build_unit_circle_mapping(origin, reference, flip_y = TRUE)
  x  <- runif(20, 40, 160); y <- runif(20, 40, 160)
  mp <- m$map(x, y)

  dc <- derive_coords(mp$trans_x, mp$trans_y, reference = m$ref_theta_unit)
  expect_equal(dc$trans_rho,       mp$trans_rho)
  expect_equal(dc$abs_theta_clock, mp$abs_theta_clock)
  expect_equal(dc$abs_theta_unit,  mp$abs_theta_unit)
  expect_equal(dc$rel_theta_unit,  mp$rel_theta_unit)
  expect_equal(dc$rel_x,           mp$rel_x)
  expect_equal(dc$rel_y,           mp$rel_y)
})

test_that("derive_coords with reference 0 gives relative == absolute", {
  dc <- derive_coords(c(0.5, -0.3), c(0.2, 0.4), reference = 0)
  expect_equal(dc$rel_theta_unit, dc$abs_theta_unit)
  expect_equal(dc$rel_x, c(0.5, -0.3))
  expect_equal(dc$rel_y, c(0.2, 0.4))
})

test_that("reference() is empty by default and .reference_lookup defaults to 0", {
  ts <- simulate_tracks(n_points = 20, seed = 3, output = "trajset")
  expect_true(is.null(reference(ts)) || nrow(reference(ts)) == 0L)
  lut <- radiatR:::.reference_lookup(ts)
  expect_true(all(lut == 0))
  expect_equal(length(lut), length(ids(ts)))
})

test_that("the object-position pipeline records a per-trajectory reference", {
  tmp_dir <- tempfile("radiatr_ref"); dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  landmarks <- data.frame(frame = c(1, 1, 101, 101),
                          x = c(0, 45, 0, -42), y = c(0, 0, 0, 0))
  tracks <- data.frame(
    frame = 1:200,
    x = c(seq(0, 40, length.out = 100), seq(0, -38, length.out = 100)),
    y = c(seq(0, 8,  length.out = 100), seq(0, -6,  length.out = 100)))
  write.table(landmarks, file.path(tmp_dir, "video_demo_point01.txt"),
              sep = "\t", col.names = FALSE, row.names = FALSE)
  write.table(tracks, file.path(tmp_dir, "video_demo_point02.txt"),
              sep = "\t", col.names = FALSE, row.names = FALSE)
  landmarks_df <- read.delim(file.path(tmp_dir, "video_demo_point01.txt"), header = FALSE)[, 1:3]
  names(landmarks_df) <- c("frame", "x", "y")
  tracks_df <- read.delim(file.path(tmp_dir, "video_demo_point02.txt"), header = FALSE)[, 1:3]
  names(tracks_df) <- c("frame", "x", "y")
  file_tbl <- import_tracks(tmp_dir)

  ts <- suppressWarnings(get_all_object_pos(landmarks_df, tracks_df, file_tbl, tmp_dir))
  ref <- reference(ts)
  expect_true(is.data.frame(ref))
  expect_true(all(c("id", "ref_theta") %in% names(ref)))
  expect_setequal(ref$id, ids(ts))
})
