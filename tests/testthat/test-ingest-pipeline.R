test_that("file-based ingest pipeline produces normalised tracks", {
  tmp_dir <- tempfile("radiatr_test")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  landmarks <- data.frame(
    frame = c(1, 1, 101, 101),
    x = c(0, 45, 0, -42),
    y = c(0, 0, 0, 0)
  )
  tracks <- data.frame(
    frame = 1:200,
    x = c(seq(0, 40, length.out = 100), seq(0, -38, length.out = 100)),
    y = c(seq(0, 8, length.out = 100), seq(0, -6, length.out = 100))
  )

  write.table(
    landmarks,
    file = file.path(tmp_dir, "video_demo_point01.txt"),
    sep = "\t", col.names = FALSE, row.names = FALSE
  )
  write.table(
    tracks,
    file = file.path(tmp_dir, "video_demo_point02.txt"),
    sep = "\t", col.names = FALSE, row.names = FALSE
  )

  file_tbl <- import_tracks(tmp_dir)
  expect_equal(nrow(file_tbl), 1)
  expect_equal(file_tbl$landmark, "video_demo_point01.txt")
  expect_equal(file_tbl$track, "video_demo_point02.txt")

  manifest <- data.frame(
    file = file_tbl$basename,
    arc = 0,
    type = "demo",
    obstacle = "none",
    id = "subject_01",
    stringsAsFactors = FALSE
  )

  file_tbl_aug <- load_tracks(file_tbl, manifest, tmp_dir)
  expect_equal(file_tbl_aug$arc, manifest$arc)

  file_tbl_aug2 <- load_tracks2(
    file_tbl,
    manifest,
    tmp_dir,
    colnames = list(arc = "arc", type = "type", obstacle = "obstacle", id = "id")
  )
  expect_equal(file_tbl_aug2$type, manifest$type)

  landmarks_df <- read.delim(
    file.path(tmp_dir, file_tbl$landmark[1]),
    header = FALSE
  )[, 1:3]
  names(landmarks_df) <- c("frame", "x", "y")
  tracks_df <- read.delim(
    file.path(tmp_dir, file_tbl$track[1]),
    header = FALSE
  )[, 1:3]
  names(tracks_df) <- c("frame", "x", "y")

  trial_limits <- get_trial_limits(landmarks_df, tracks_df, file_tbl_aug, vid_num = 1)
  expect_equal(nrow(trial_limits), 2)
  expect_true(all(trial_limits$r_px > 0))

  track_ts <- get_tracked_object_pos(trial_limits, tracks_df)
  expect_s4_class(track_ts, "TrajSet")
  expect_true(all(track_ts@meta$trial_limits$valid_track %in% c(TRUE, FALSE)))

  combined <- get_all_object_pos(landmarks_df, tracks_df, file_tbl_aug, tmp_dir)
  expect_s4_class(combined, "TrajSet")
  combined_df <- as.data.frame(combined)
  expect_gt(nrow(combined_df), 0)
  expect_true(all(abs(combined_df$rel_x) <= 1.5 + 1e-8))
  expect_s3_class(combined@meta$trial_limits, "data.frame")
})
