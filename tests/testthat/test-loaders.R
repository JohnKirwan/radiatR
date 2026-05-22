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

test_that("dtrack_read() reads a headerless tab-sep file into a TrajSet", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c("1\t100.5\t200.3\t1", "2\t101.0\t201.1\t1", "3\t102.2\t199.8\t1"),
    tmp
  )
  ts <- dtrack_read(tmp)
  expect_s4_class(ts, "TrajSet")
  expect_equal(nrow(ts@data), 3L)
  expect_true(all(c("x", "y") %in% names(ts@data)))
  expect_false("V4" %in% names(ts@data))
})

test_that("dtrack_read() errors on a file with fewer than 3 columns", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("1\t100.5", "2\t101.0"), tmp)
  expect_error(dtrack_read(tmp), "at least 3 columns")
})

test_that("dtrack dialect processes a pre-parsed data frame", {
  df <- data.frame(V1 = 1:3, V2 = c(10.1, 10.2, 10.3), V3 = c(20.1, 20.2, 20.3),
                   stringsAsFactors = FALSE)
  result <- get("dtrack", envir = radiatR:::.loader_registry)(df)
  expect_equal(names(result), c("frame", "x", "y", "id"))
  expect_equal(nrow(result), 3L)
  expect_equal(result$id[1], "1")
})
