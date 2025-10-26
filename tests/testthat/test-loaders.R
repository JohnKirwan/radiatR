test_that("TrajSet loader assembles tracks with metadata", {
  tmp <- withr::local_tempdir()
  track_path <- file.path(tmp, "sample_track.csv")
  track_df <- data.frame(
    id = c("A", "A"),
    time = c(0, 1),
    x = c(0, 1),
    y = c(0, 1)
  )
  utils::write.csv(track_df, track_path, row.names = FALSE)

  file_tbl <- tibble::tibble(
    basename = "sample",
    track = basename(track_path)
  )
  manifest <- tibble::tibble(
    file = "sample",
    condition = "baseline"
  )

  ts <- TrajSet_load_manifest(
    file_tbl = file_tbl,
    track_dir = tmp,
    manifest = manifest,
    mapping = list(id = "id", time = "time", x = "x", y = "y")
  )
  expect_s4_class(ts, "TrajSet")
  expect_true("condition" %in% names(ts@data))
  expect_equal(unique(ts@data$condition), "baseline")
})

test_that("legacy loader wrappers still augment file tables", {
  tmp <- withr::local_tempdir()
  track_path <- file.path(tmp, "sample_point02.txt")
  landmark_path <- file.path(tmp, "sample_point01.txt")
  utils::write.table(data.frame(frame = c(1, 2), x = c(0, 0), y = c(0, 1)),
                     landmark_path, sep = "\t", col.names = FALSE, row.names = FALSE)
  utils::write.table(data.frame(frame = 1:3, x = 0:2, y = 0:2),
                     track_path, sep = "\t", col.names = FALSE, row.names = FALSE)

  file_tbl <- tibble::tibble(
    basename = "sample",
    landmark = basename(landmark_path),
    track = basename(track_path)
  )
  manifest <- tibble::tibble(
    file = "sample",
    arc = 45,
    type = "demo",
    obstacle = "none",
    id = "animal"
  )

  augmented <- load_tracks(file_tbl, manifest, tmp)
  expect_true(all(c("arc", "type", "obstacle", "id") %in% names(augmented)))

  custom <- load_tracks2(file_tbl, manifest, tmp,
                         colnames = list(group = "type"))
  expect_true("group" %in% names(custom))
  expect_equal(custom$group, "demo")
})
