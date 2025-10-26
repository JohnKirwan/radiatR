test_that("trial utilities summarise and transform tracks", {
  landmarks <- data.frame(
    frame = c(1, 2, 6, 7),
    x = c(0, 1, 0, -1),
    y = c(0, 0, 0, 0)
  )
  animal_track <- data.frame(
    frame = 1:10,
    x = seq(0, -1, length.out = 10),
    y = seq(0, 0.2, length.out = 10)
  )
  file_tbl <- tibble::tibble(
    basename = "video1",
    arc = 0,
    type = "Herm",
    obstacle = "none",
    id = "animal"
  )

  limits <- suppressWarnings(get_trial_limits(landmarks, animal_track, file_tbl, vid_num = 1))
  expect_equal(nrow(limits), 2)
  expect_equal(limits$video[1], "video1")
  expect_true(all(c("stim_theta", "r_px") %in% names(limits)))

  track_res <- suppressWarnings(get_tracked_object_pos(limits, animal_track))
  expect_type(track_res, "list")
  expect_length(track_res, 2)
  expect_s3_class(track_res[[2]], "data.frame")

  tmp <- withr::local_tempdir()
  track_path <- file.path(tmp, "video1_point02.txt")
  landmark_path <- file.path(tmp, "video1_point01.txt")
  utils::write.table(landmarks, landmark_path, sep = "\t", col.names = FALSE, row.names = FALSE)
  utils::write.table(animal_track, track_path, sep = "\t", col.names = FALSE, row.names = FALSE)

  file_tbl_disk <- tibble::tibble(
    basename = "video1",
    landmark = basename(landmark_path),
    track = basename(track_path)
  )

  agg <- suppressWarnings(get_all_object_pos(file_tbl = file_tbl_disk, track_dir = tmp))
  expect_type(agg, "list")
  expect_true(all(c("rel_x", "rel_y") %in% names(agg[[1]])))
})
