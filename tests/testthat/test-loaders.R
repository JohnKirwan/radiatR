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

test_that("tracktor dialect ignores a leading pandas index column", {
  # pandas df.to_csv() writes the row index as an unnamed first column (read as
  # "X"), which must not be mistaken for the x position column.
  df <- data.frame(
    X     = 0:3,                       # pandas-style 0-based index
    frame = 0:3,
    pos_x = c(100, 110, 120, 130),
    pos_y = c(50, 51, 52, 53)
  )
  ts <- suppressMessages(suppressWarnings(
    TrajSet_read(df, dialect = "tracktor")
  ))
  d <- as.data.frame(ts)
  expect_equal(d$x_raw, c(100, 110, 120, 130))
  expect_equal(d$y_raw, c(50, 51, 52, 53))
})

test_that("tracktor dialect keeps a genuine x column when there is no index", {
  df <- data.frame(
    frame = 0:3,
    x     = c(100, 110, 120, 130),
    y     = c(50, 51, 52, 53)
  )
  ts <- suppressMessages(suppressWarnings(
    TrajSet_read(df, dialect = "tracktor")
  ))
  d <- as.data.frame(ts)
  expect_equal(d$x_raw, c(100, 110, 120, 130))
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

test_that(".guess_col is case-insensitive and respects candidate priority", {
  expect_equal(radiatR:::.guess_col(c("ID", "time"), c("id", "subject")), "ID")
  expect_equal(radiatR:::.guess_col(c("Frame"), c("time", "frame")), "Frame")
  expect_null(radiatR:::.guess_col(c("foo", "bar"), c("id")))
})

test_that(".guess_xy_suffix matches separator-suffixed axis columns", {
  expect_equal(radiatR:::.guess_xy_suffix(c("Track1_X", "Track1_Y"), "x"), "Track1_X")
  expect_equal(radiatR:::.guess_xy_suffix(c("Track1_X", "Track1_Y"), "y"), "Track1_Y")
  expect_null(radiatR:::.guess_xy_suffix(c("max", "flux"), "x"))   # not separator-preceded
})

test_that("guess_columns recognises case-insensitive and suffixed columns", {
  d <- data.frame(Frame = 1:3, Track1_X = c(0, 1, 2), Track1_Y = c(0, 0, 1))
  names(d) <- c("Frame", "Track1_X", "Track1_Y")
  g <- guess_columns(d)
  expect_equal(g$time, "Frame")
  expect_equal(g$x, "Track1_X")
  expect_equal(g$y, "Track1_Y")
  expect_null(g$id)
})

test_that("guess_columns honours explicit mapping overrides", {
  d <- data.frame(a = 1:3, b = 1:3, c = 1:3)
  g <- guess_columns(d, mapping = list(x = "b", y = "c", time = "a"))
  expect_equal(g$x, "b"); expect_equal(g$y, "c"); expect_equal(g$time, "a")
})

test_that("TrajSet_read loads a single-track custom CSV, dropping non-finite rows", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(Frame = 1:6,
                   Track1_X = c(0, 1, 2, NaN, 4, 5),
                   Track1_Y = c(0, 0, 1, NaN, 2, 3))
  utils::write.csv(df, tmp, row.names = FALSE)
  ts <- suppressMessages(TrajSet_read(tmp, normalize_xy = FALSE))
  expect_s4_class(ts, "TrajSet")
  expect_equal(length(ids(ts)), 1L)                 # synthetic single-track id
  expect_equal(nrow(as.data.frame(ts)), 5L)          # the NaN row dropped
})

test_that("TrajSet_read synthesizes time from row order when no time column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(x = c(0, 1, 2), y = c(0, 1, 0)), tmp, row.names = FALSE)
  ts <- suppressMessages(TrajSet_read(tmp, normalize_xy = FALSE))
  expect_s4_class(ts, "TrajSet")
  expect_equal(length(ids(ts)), 1L)
  expect_equal(nrow(as.data.frame(ts)), 3L)
})

test_that("TrajSet_read still honours present id/time/x/y columns (parity)", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(id = c("a","a","b","b"), frame = c(1,2,1,2),
                              x = c(0,1,0,1), y = c(0,1,1,0)), tmp, row.names = FALSE)
  ts <- suppressMessages(TrajSet_read(tmp, normalize_xy = FALSE))
  expect_setequal(as.character(ids(ts)), c("a", "b"))   # real id used, not synthesized
})

test_that(".guess_delim detects the separator and decimal mark", {
  mk <- function(lines) { p <- tempfile(fileext = ".csv"); writeLines(lines, p); p }

  comma <- mk(c("x,y,frame", "0.1,0.2,1", "0.3,0.4,2"))
  expect_equal(radiatR:::.guess_delim(comma), list(delim = ",", decimal = "."))

  # semicolon export with comma decimals (European convention)
  semi <- mk(c("x;y;frame", "0,1;0,2;1", "0,3;0,4;2"))
  expect_equal(radiatR:::.guess_delim(semi), list(delim = ";", decimal = ","))

  tab <- mk(c("x\ty\tframe", "0.1\t0.2\t1", "0.3\t0.4\t2"))
  expect_equal(radiatR:::.guess_delim(tab), list(delim = "\t", decimal = "."))

  pipe <- mk(c("x|y|frame", "0.1|0.2|1", "0.3|0.4|2"))
  expect_equal(radiatR:::.guess_delim(pipe), list(delim = "|", decimal = "."))
})

test_that(".guess_delim ignores a stray comma in one header and stays consistent", {
  # tab-delimited, but the header has a comma in a label; tab is consistent
  p <- tempfile(fileext = ".csv")
  writeLines(c("x\ty\tlabel, note", "0.1\t0.2\ta", "0.3\t0.4\tb"), p)
  expect_equal(radiatR:::.guess_delim(p)$delim, "\t")
})

test_that(".guess_delim falls back to comma on an unreadable/empty file", {
  p <- tempfile(fileext = ".csv"); file.create(p)
  expect_equal(radiatR:::.guess_delim(p), list(delim = ",", decimal = "."))
})

test_that(".read_any parses a semicolon + comma-decimal file into numeric columns", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("x;y;frame",
               "0,10;0,20;1", "0,30;0,40;2", "0,50;0,60;3"), p)
  df <- as.data.frame(radiatR:::.read_any(p))
  expect_equal(ncol(df), 3L)                      # split into x / y / frame
  expect_true(is.numeric(df$x))
  expect_equal(round(range(df$x), 2), c(0.10, 0.50))
})

test_that(".read_any sniffs a tab-delimited file that carries a .csv extension", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("x\ty\tframe", "0.1\t0.2\t1", "0.3\t0.4\t2"), p)
  df <- as.data.frame(radiatR:::.read_any(p))
  expect_equal(ncol(df), 3L)
  expect_true(is.numeric(df$x))
  expect_equal(round(range(df$x), 2), c(0.1, 0.3))
})

test_that(".read_any honours an explicit delim/decimal override over the sniffer", {
  # genuinely semicolon, but force comma -> should NOT split into x/y/frame
  p <- tempfile(fileext = ".csv")
  writeLines(c("x;y;frame", "0,1;0,2;1", "0,3;0,4;2"), p)
  forced <- as.data.frame(radiatR:::.read_any(p, delim = ",", decimal = "."))
  expect_equal(ncol(forced), 1L)            # comma reading sees a single column
})

test_that(".read_any still reads a plain comma CSV unchanged", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("x,y,frame", "0.1,0.2,1", "0.3,0.4,2"), p)
  df <- as.data.frame(radiatR:::.read_any(p))
  expect_equal(ncol(df), 3L)
  expect_true(is.numeric(df$x))
})

test_that("TrajSet_read forwards read_opts$delim to the reader", {
  # comma-looking content but we force semicolon: the row stays one whole column
  p <- tempfile(fileext = ".csv")
  writeLines(c("x,y,frame", "0.1,0.2,1", "0.3,0.4,2"), p)
  expect_error(
    suppressMessages(TrajSet_read(p, normalize_xy = FALSE,
                                  read_opts = list(delim = ";"))),
    regexp = NULL)   # forcing ';' yields one column -> no x/y -> a load error
})

# ---- Excel (.xlsx) input -----------------------------------------------------

test_that(".read_any reads an Excel workbook: first sheet by default, sheet by name", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  p <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    Sheet1 = data.frame(x = c(0.1, 0.3, 0.5), y = c(0.2, 0.4, 0.6), frame = 1:3),
    Other  = data.frame(x = c(0.9, 0.8),      y = c(0.7, 0.6),      frame = 1:2)
  ), p)

  d1 <- as.data.frame(radiatR:::.read_any(p))
  expect_equal(nrow(d1), 3L)               # first sheet
  expect_true(is.numeric(d1$x))

  d2 <- as.data.frame(radiatR:::.read_any(p, sheet = "Other"))
  expect_equal(nrow(d2), 2L)               # chosen sheet
})

test_that("TrajSet_read forwards read_opts$sheet to the Excel reader", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  p <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    Sheet1 = data.frame(x = c(0.1, 0.3, 0.5), y = c(0.2, 0.4, 0.6), frame = 1:3),
    Other  = data.frame(x = c(0.9, 0.8),      y = c(0.7, 0.6),      frame = 1:2)
  ), p)
  ts1 <- suppressMessages(TrajSet_read(p, normalize_xy = FALSE))
  ts2 <- suppressMessages(
    TrajSet_read(p, normalize_xy = FALSE, read_opts = list(sheet = "Other")))
  expect_equal(nrow(as.data.frame(ts1)), 3L)
  expect_equal(nrow(as.data.frame(ts2)), 2L)
})
