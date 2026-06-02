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

# ---- dialect_args forwarding -------------------------------------------------

test_that("dialect_args are forwarded to the dialect function", {
  df <- data.frame(frame = 1:4,
                   head_x = c(0, .1, .2, .3), head_y = rep(0.1, 4),
                   head_likelihood = rep(0.99, 4))
  ts <- TrajSet_read(df, dialect = "deeplabcut",
                     dialect_args = list(bodypart = "head"))
  expect_s4_class(ts, "TrajSet")
  d <- as.data.frame(ts)
  expect_true(all(c("head_x", "head_y") %in% names(d)))
})

# ---- deeplabcut multi-bodypart centroid --------------------------------------

test_that("deeplabcut multi-bodypart centroid is likelihood-weighted", {
  df <- data.frame(
    frame        = 1:3,
    head_x       = c(1, 1, 1), head_y       = c(0, 0, 0),
    head_likelihood = c(1, 1, 1),
    tail_x       = c(0, 0, 0), tail_y       = c(0, 0, 0),
    tail_likelihood = c(0, 0, 0)   # zero weight
  )
  ts <- TrajSet_read(df, dialect = "deeplabcut")
  d  <- as.data.frame(ts)
  # tail excluded by zero likelihood -> centroid = head position
  expect_equal(d$x_raw[1], 1, tolerance = 1e-9)
})

test_that("deeplabcut multi-bodypart appends per-bodypart columns", {
  df <- data.frame(frame = 1:3,
                   head_x = c(0, .1, .2), head_y = c(.1, .1, .1),
                   thorax_x = c(0, .1, .2), thorax_y = c(0, 0, 0))
  ts <- TrajSet_read(df, dialect = "deeplabcut",
                     dialect_args = list(bodypart = c("head", "thorax")))
  d  <- as.data.frame(ts)
  expect_true(all(c("head_x", "head_y", "thorax_x", "thorax_y") %in% names(d)))
  expect_equal(d$y_raw[1], 0.05, tolerance = 1e-9)  # mean of 0.1 and 0
})

# ---- ethovision zone detection -----------------------------------------------

test_that("ethovision detects prefix-style body zones and appends columns", {
  df <- data.frame(time = 1:3,
                   x_center = c(0, .1, .2), y_center = c(0, 0, 0),
                   x_nose   = c(.1, .2, .3), y_nose   = c(.1, .1, .1),
                   x_tail   = c(-.1, 0, .1), y_tail   = c(-.1, -.1, -.1))
  ts <- TrajSet_read(df, dialect = "ethovision")
  d  <- as.data.frame(ts)
  expect_true(all(c("nose_x", "nose_y", "tail_x", "tail_y") %in% names(d)))
})

test_that("ethovision zone centroid uses selected zones", {
  df <- data.frame(time = 1:3,
                   x_center = c(0, .1, .2), y_center = c(0, 0, 0),
                   x_nose   = c(1, 1, 1),   y_nose   = c(0, 0, 0),
                   x_tail   = c(-1, -1, -1), y_tail  = c(0, 0, 0))
  ts <- TrajSet_read(df, dialect = "ethovision",
                     dialect_args = list(zone = c("nose", "tail")))
  d  <- as.data.frame(ts)
  expect_equal(d$x_raw[1], 0, tolerance = 1e-9)  # centroid of 1 and -1
})

# ---- anymaze zone detection --------------------------------------------------

test_that("anymaze detects <zone> X Centre columns and normalises them", {
  df <- data.frame(
    "Trial time"    = 0:2,
    "X Centre"      = c(5, 6, 7),
    "Y Centre"      = c(10, 11, 12),
    "Nose X Centre" = c(6, 7, 8),
    "Nose Y Centre" = c(11, 12, 13),
    check.names = FALSE
  )
  ts <- TrajSet_read(df, dialect = "anymaze")
  d  <- as.data.frame(ts)
  expect_true(all(c("nose_x", "nose_y") %in% names(d)))
})

test_that("anymaze zone centroid averages selected zones", {
  df <- data.frame(
    "Trial time"    = 0:2,
    "X Centre"      = c(5, 6, 7),  "Y Centre"      = c(0, 0, 0),
    "Nose X Centre" = c(10, 10, 10), "Nose Y Centre" = c(0, 0, 0),
    "Tail X Centre" = c(0, 0, 0),   "Tail Y Centre" = c(0, 0, 0),
    check.names = FALSE
  )
  ts <- TrajSet_read(df, dialect = "anymaze",
                     dialect_args = list(zone = c("nose", "tail")))
  d  <- as.data.frame(ts)
  expect_equal(d$x_raw[1], 5, tolerance = 1e-9)  # mean of 10 and 0
})

# ---- ctrax theta/a/b preservation -------------------------------------------

test_that("ctrax dialect errors when called on a nonexistent file", {
  ctrax_fn <- get("ctrax", envir = radiatR:::.loader_registry)
  expect_error(ctrax_fn("nonexistent.mat"), "provide a path|R.matlab")
})

# ---- trex dialect ------------------------------------------------------------

test_that("trex dialect infers id from numeric filename suffix", {
  tmp <- tempfile(pattern = "fish_3_", fileext = ".csv")
  write.csv(data.frame(frame = 1:3, X = c(0, .1, .2), Y = c(0, 0, 0)), tmp,
            row.names = FALSE)
  on.exit(unlink(tmp))
  ts <- TrajSet_read(tmp, dialect = "trex")
  expect_s4_class(ts, "TrajSet")
  expect_equal(nrow(as.data.frame(ts)), 3L)
})

test_that("trex dialect accepts individual column", {
  df <- data.frame(individual = rep(c("A","B"), each = 3),
                   frame = rep(1:3, 2),
                   X = c(0,.1,.2, 1,1.1,1.2), Y = rep(0, 6))
  ts <- TrajSet_read(df, dialect = "trex")
  expect_equal(length(unique(as.data.frame(ts)$id)), 2L)
})

test_that("trex dialect auto-detects centroid-variant columns", {
  df <- data.frame(frame = 1:3,
                   "X#wcentroid" = c(0, 0.5, 1),
                   "Y#wcentroid" = c(0, 0, 0),
                   check.names = FALSE)
  ts <- TrajSet_read(df, dialect = "trex")
  expect_equal(as.data.frame(ts)$x_raw[3], 1, tolerance = 1e-9)
})

test_that("trex dialect prefers plain X/Y over centroid variants", {
  df <- data.frame(frame = 1:2,
                   X = c(9, 9), "X#wcentroid" = c(1, 1),
                   Y = c(0, 0), "Y#wcentroid" = c(0, 0),
                   check.names = FALSE)
  ts <- TrajSet_read(df, dialect = "trex")
  expect_equal(as.data.frame(ts)$x_raw[1], 9, tolerance = 1e-9)
})

test_that("trex centroid argument forces a specific source", {
  df <- data.frame(frame = 1:2,
                   X = c(9, 9), "X#wcentroid" = c(1, 1),
                   Y = c(0, 0), "Y#wcentroid" = c(0, 0),
                   check.names = FALSE)
  ts <- TrajSet_read(df, dialect = "trex",
                     dialect_args = list(centroid = "wcentroid"))
  expect_equal(as.data.frame(ts)$x_raw[1], 1, tolerance = 1e-9)
})

test_that("trex centroid argument errors when variant absent", {
  df <- data.frame(frame = 1:2, X = c(1, 1), Y = c(0, 0))
  expect_error(
    TrajSet_read(df, dialect = "trex",
                 dialect_args = list(centroid = "pcentroid")),
    "pcentroid"
  )
})

test_that("trex dialect strips TRex unit annotations in column headers", {
  # genuine TRex export headers carry a trailing unit, e.g. "X#wcentroid (cm)"
  df <- data.frame(
    frame              = 1:3,
    "X#wcentroid (cm)" = c(0, 0.5, 1),
    "Y#wcentroid (cm)" = c(0, 0, 0),
    check.names = FALSE
  )
  ts <- TrajSet_read(df, dialect = "trex")
  expect_s4_class(ts, "TrajSet")
  expect_equal(as.data.frame(ts)$x_raw[3], 1, tolerance = 1e-9)
})

# ---- tracktor dialect --------------------------------------------------------

test_that("tracktor dialect reads identity and frame columns", {
  df <- data.frame(frame    = rep(1:4, 2),
                   identity = rep(c("A","B"), each = 4),
                   x = c(0,.1,.2,.3, 1,1.1,1.2,1.3),
                   y = rep(0, 8))
  ts <- TrajSet_read(df, dialect = "tracktor")
  expect_s4_class(ts, "TrajSet")
  expect_equal(length(unique(as.data.frame(ts)$id)), 2L)
})

test_that("tracktor dialect reads the bundled real Tracktor export", {
  path <- system.file("extdata", "tracktor_example.csv", package = "radiatR")
  skip_if(nchar(path) == 0L, "tracktor_example.csv not found in extdata")
  ts <- suppressMessages(suppressWarnings(
    TrajSet_read(path, dialect = "tracktor")
  ))
  d <- as.data.frame(ts)
  expect_s4_class(ts, "TrajSet")
  expect_gt(nrow(d), 100L)               # ~547 frames
  expect_true(all(c("x", "y") %in% names(d)))
  # pos_x / pos_y mapped to x/y; first frame near (1528, 330) in pixels
  expect_equal(d$x_raw[1], 1528.27, tolerance = 1e-1)
})

# ---- sleap dialect -----------------------------------------------------------

test_that("sleap dialect detects nodes and computes score-weighted centroid", {
  df <- data.frame(
    frame_idx = 1:3,
    track     = "track_0",
    head_x    = c(1, 1, 1), head_y    = c(0, 0, 0), head_score    = c(0.99, 0.99, 0.99),
    thorax_x  = c(0, 0, 0), thorax_y  = c(0, 0, 0), thorax_score  = c(0.01, 0.01, 0.01)
  )
  ts <- TrajSet_read(df, dialect = "sleap",
                     dialect_args = list(score_min = 0.5))
  d  <- as.data.frame(ts)
  # thorax below score_min -> centroid = head position
  expect_equal(d$x_raw[1], 1, tolerance = 1e-9)
  expect_true(all(c("head_x", "head_y", "thorax_x", "thorax_y") %in% names(d)))
})

test_that("sleap dialect uses track as id and frame_idx as time", {
  df <- data.frame(frame_idx = 1:3, track = "A",
                   head_x = c(0,.1,.2), head_y = rep(0,3), head_score = rep(0.9,3))
  ts <- TrajSet_read(df, dialect = "sleap", dialect_args = list(bodypart = "head"))
  expect_equal(as.data.frame(ts)$id[1], "A")
})

test_that("deeplabcut_multiheader reads official DLC labeled-data fixture", {
  path <- system.file("extdata", "dlc_CollectedData_Pranav.csv", package = "radiatR")
  skip_if(nchar(path) == 0L, "dlc_CollectedData_Pranav.csv not found in extdata")
  ts <- suppressMessages(TrajSet_read(path, dialect = "deeplabcut_multiheader"))
  d  <- as.data.frame(ts)
  expect_s4_class(ts, "TrajSet")
  expect_equal(nrow(d), 116L)
  # four bodyparts should be detected and appended
  expect_true(all(c("snout_x","snout_y","leftear_x","leftear_y",
                    "rightear_x","rightear_y","tailbase_x","tailbase_y") %in% names(d)))
  # centroid of frame 1: mean(21.521, 33.819, 19.984, 87.11) ≈ 40.609
  expect_equal(d$x_raw[1], mean(c(21.521, 33.819, 19.984, 87.11)), tolerance = 1e-3)
})

test_that("sleap dialect reads official SLEAP CSV fixture and handles NA scores", {
  path <- system.file("extdata", "sleap_example.csv", package = "radiatR")
  skip_if(nchar(path) == 0L, "sleap_example.csv not found in extdata")
  ts <- TrajSet_read(path, dialect = "sleap")
  d  <- as.data.frame(ts)
  expect_s4_class(ts, "TrajSet")
  expect_equal(nrow(d), 1L)
  # nodes A and B should be detected (dots normalised to underscores)
  expect_true(all(c("a_x", "a_y", "b_x", "b_y") %in% names(d)))
  # centroid should be the mean of A and B positions (NA scores → equal weight)
  expect_equal(d$x_raw[1],
               mean(c(205.9300539013689, 278.63521449272383)),
               tolerance = 1e-6)
})
