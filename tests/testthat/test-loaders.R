test_that("Tracks loader assembles tracks with metadata", {
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

  ts <- load_manifest(
    file_tbl = file_tbl,
    track_dir = tmp,
    manifest = manifest,
    mapping = list(id = "id", time = "time", x = "x", y = "y")
  )
  expect_s4_class(ts, "Tracks")
  expect_true("condition" %in% names(ts@data))
  expect_equal(unique(ts@data$condition), "baseline")
})

test_that("manifest columns are joined onto file tables", {
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

  augmented <- .augment_with_manifest(file_tbl, manifest, NULL)
  expect_true(all(c("arc", "type", "obstacle", "id") %in% names(augmented)))

  custom <- .augment_with_manifest(file_tbl, manifest, list(group = "type"))
  expect_true("group" %in% names(custom))
  expect_equal(custom$group, "demo")
})

# ---- dialect_args forwarding -------------------------------------------------

test_that("dialect_args are forwarded to the dialect function", {
  df <- data.frame(frame = 1:4,
                   head_x = c(0, .1, .2, .3), head_y = rep(0.1, 4),
                   head_likelihood = rep(0.99, 4))
  ts <- read_tracks(df, dialect = "deeplabcut",
                     dialect_args = list(bodypart = "head"), normalize_xy = TRUE)
  expect_s4_class(ts, "Tracks")
  d <- as.data.frame(ts)
  expect_true(all(c("head_x", "head_y") %in% names(d)))
})

# ---- deeplabcut multi-bodypart centroid --------------------------------------

test_that("deeplabcut multi-bodypart centroid is likelihood-weighted", {
  df <- data.frame(
    frame        = 1:3,
    head_x       = c(1, 1, 1), head_y       = c(0, 0, 0),
    head_likelihood = c(0.75, 0.75, 0.75),
    tail_x       = c(0, 0, 0), tail_y       = c(0, 0, 0),
    tail_likelihood = c(0.25, 0.25, 0.25)
  )
  ts <- read_tracks(df, dialect = "deeplabcut", normalize_xy = TRUE)
  d  <- as.data.frame(ts)
  # weights 0.75 and 0.25 -> centroid three quarters of the way to the head
  expect_equal(d$x_raw[1], 0.75, tolerance = 1e-9)
})

test_that("deeplabcut multi-bodypart appends per-bodypart columns", {
  df <- data.frame(frame = 1:3,
                   head_x = c(0, .1, .2), head_y = c(.1, .1, .1),
                   thorax_x = c(0, .1, .2), thorax_y = c(0, 0, 0))
  ts <- read_tracks(df, dialect = "deeplabcut",
                     dialect_args = list(bodypart = c("head", "thorax")), normalize_xy = TRUE)
  d  <- as.data.frame(ts)
  expect_true(all(c("head_x", "head_y", "thorax_x", "thorax_y") %in% names(d)))
  expect_equal(d$y_raw[1], 0.05, tolerance = 1e-9)  # mean of 0.1 and 0
})

# ---- ctrax dialect removed --------------------------------------------------
test_that("ctrax dialect is gone (unknown dialect error)", {
  expect_false(exists("ctrax", envir = radiatR:::.loader_registry, inherits = FALSE))
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("id,time,x,y", "1,0,0,0", "1,1,1,1"), tmp)
  expect_error(read_tracks(tmp, dialect = "ctrax"), "Unknown dialect")
})

test_that("ellipse_axis heading rule survives ctrax removal", {
  df <- data.frame(id = "a", time = 0:2, x = c(0, 1, 2), y = c(0, 0, 0),
                   theta = c(0, pi / 4, pi / 2))
  ts <- read_tracks(df)
  hd <- derive_headings(ts, rule = "ellipse_axis")
  expect_s3_class(hd, "headings_frame")
  expect_true(all(is.finite(hd$heading)))
})

# ---- trex dialect ------------------------------------------------------------

test_that("trex dialect infers id from numeric filename suffix", {
  tmp <- tempfile(pattern = "fish_3_", fileext = ".csv")
  write.csv(data.frame(frame = 1:3, X = c(0, .1, .2), Y = c(0, 0, 0)), tmp,
            row.names = FALSE)
  on.exit(unlink(tmp))
  ts <- read_tracks(tmp, dialect = "trex", normalize_xy = TRUE)
  expect_s4_class(ts, "Tracks")
  expect_equal(nrow(as.data.frame(ts)), 3L)
})

test_that("trex dialect accepts individual column", {
  df <- data.frame(individual = rep(c("A","B"), each = 3),
                   frame = rep(1:3, 2),
                   X = c(0,.1,.2, 1,1.1,1.2), Y = rep(0, 6))
  ts <- read_tracks(df, dialect = "trex", normalize_xy = TRUE)
  expect_equal(length(unique(as.data.frame(ts)$id)), 2L)
})

test_that("trex dialect auto-detects centroid-variant columns", {
  df <- data.frame(frame = 1:3,
                   "X#wcentroid" = c(0, 0.5, 1),
                   "Y#wcentroid" = c(0, 0, 0),
                   check.names = FALSE)
  ts <- read_tracks(df, dialect = "trex", normalize_xy = TRUE)
  expect_equal(as.data.frame(ts)$x_raw[3], 1, tolerance = 1e-9)
})

test_that("trex dialect prefers plain X/Y over centroid variants", {
  df <- data.frame(frame = 1:2,
                   X = c(9, 9), "X#wcentroid" = c(1, 1),
                   Y = c(0, 0), "Y#wcentroid" = c(0, 0),
                   check.names = FALSE)
  ts <- read_tracks(df, dialect = "trex", normalize_xy = TRUE)
  expect_equal(as.data.frame(ts)$x_raw[1], 9, tolerance = 1e-9)
})

test_that("trex centroid argument forces a specific source", {
  df <- data.frame(frame = 1:2,
                   X = c(9, 9), "X#wcentroid" = c(1, 1),
                   Y = c(0, 0), "Y#wcentroid" = c(0, 0),
                   check.names = FALSE)
  ts <- read_tracks(df, dialect = "trex",
                     dialect_args = list(centroid = "wcentroid"), normalize_xy = TRUE)
  expect_equal(as.data.frame(ts)$x_raw[1], 1, tolerance = 1e-9)
})

test_that("trex centroid argument errors when variant absent", {
  df <- data.frame(frame = 1:2, X = c(1, 1), Y = c(0, 0))
  expect_error(
    read_tracks(df, dialect = "trex",
                 dialect_args = list(centroid = "pcentroid"), normalize_xy = TRUE),
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
  ts <- read_tracks(df, dialect = "trex", normalize_xy = TRUE)
  expect_s4_class(ts, "Tracks")
  expect_equal(as.data.frame(ts)$x_raw[3], 1, tolerance = 1e-9)
})

# ---- sleap dialect -----------------------------------------------------------

test_that("sleap dialect detects nodes and computes score-weighted centroid", {
  df <- data.frame(
    frame_idx = 1:3,
    track     = "track_0",
    head_x    = c(1, 1, 1), head_y    = c(0, 0, 0), head_score    = c(0.99, 0.99, 0.99),
    thorax_x  = c(0, 0, 0), thorax_y  = c(0, 0, 0), thorax_score  = c(0.01, 0.01, 0.01)
  )
  ts <- suppressMessages(read_tracks(df, dialect = "sleap", normalize_xy = TRUE,
                                     dialect_args = list(score_min = 0.5, partial = "mean")))
  d  <- as.data.frame(ts)
  # thorax below score_min is masked; partial = "mean" leaves the head position
  expect_equal(d$x_raw[1], 1, tolerance = 1e-9)
  expect_true(all(c("head_x", "head_y", "thorax_x", "thorax_y") %in% names(d)))
})

test_that("sleap dialect uses track as id and frame_idx as time", {
  df <- data.frame(frame_idx = 1:3, track = "A",
                   head_x = c(0,.1,.2), head_y = rep(0,3), head_score = rep(0.9,3))
  ts <- read_tracks(df, dialect = "sleap", dialect_args = list(bodypart = "head"), normalize_xy = TRUE)
  expect_equal(as.data.frame(ts)$id[1], "A")
})

test_that("sleap dialect reads official SLEAP CSV fixture and handles NA scores", {
  path <- system.file("extdata", "sleap_example.csv", package = "radiatR")
  skip_if(nchar(path) == 0L, "sleap_example.csv not found in extdata")
  ts <- read_tracks(path, dialect = "sleap", normalize_xy = TRUE)
  d  <- as.data.frame(ts)
  expect_s4_class(ts, "Tracks")
  expect_equal(nrow(d), 1L)
  # nodes A and B should be detected (dots normalised to underscores)
  expect_true(all(c("a_x", "a_y", "b_x", "b_y") %in% names(d)))
  # centroid should be the mean of A and B positions (NA scores → equal weight)
  expect_equal(d$x_raw[1],
               mean(c(205.9300539013689, 278.63521449272383)),
               tolerance = 1e-6)
})

test_that("sleap dialect reads an empty track column as one trajectory", {
  sleap <- data.frame(track = NA, frame_idx = 0:2,
                      A.x = c(1, 2, 3), A.y = c(1, 1, 2), check.names = FALSE)
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sleap, path, row.names = FALSE, na = "")
  ts <- read_tracks(path, dialect = "sleap")
  expect_identical(as.character(ids(ts)), "1")
  expect_identical(nrow(as.data.frame(ts)), 3L)
  # a misspelt id column is still an error, not a silent single trajectory
  expect_error(read_tracks(path, dialect = "sleap",
                           dialect_args = list(id_col = "trak")))
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

test_that("read_tracks loads a single-track custom CSV, dropping non-finite rows", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(Frame = 1:6,
                   Track1_X = c(0, 1, 2, NaN, 4, 5),
                   Track1_Y = c(0, 0, 1, NaN, 2, 3))
  utils::write.csv(df, tmp, row.names = FALSE)
  ts <- suppressMessages(suppressWarnings(
    read_tracks(tmp, normalize_xy = FALSE, on_invalid = "drop")
  ))
  expect_s4_class(ts, "Tracks")
  expect_equal(length(ids(ts)), 1L)                 # synthetic single-track id
  expect_equal(nrow(as.data.frame(ts)), 5L)          # the NaN row dropped
})

test_that("read_tracks synthesizes time from row order when no time column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(x = c(0, 1, 2), y = c(0, 1, 0)), tmp, row.names = FALSE)
  ts <- suppressMessages(read_tracks(tmp, normalize_xy = FALSE))
  expect_s4_class(ts, "Tracks")
  expect_equal(length(ids(ts)), 1L)
  expect_equal(nrow(as.data.frame(ts)), 3L)
})

test_that("read_tracks still honours present id/time/x/y columns (parity)", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(id = c("a","a","b","b"), frame = c(1,2,1,2),
                              x = c(0,1,0,1), y = c(0,1,1,0)), tmp, row.names = FALSE)
  ts <- suppressMessages(read_tracks(tmp, normalize_xy = FALSE))
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
  # genuinely semicolon, but force comma: the header is one field and the rows
  # three, so the forced delimiter is taken and the file is not one table
  p <- tempfile(fileext = ".csv")
  writeLines(c("x;y;frame", "0,1;0,2;1", "0,3;0,4;2"), p)
  expect_error(suppressWarnings(radiatR:::.read_any(p, delim = ",", decimal = ".")),
               class = "radiatR_malformed_file")
  # a forced delimiter that fits is used over the sniffed one
  writeLines(c("x;y;frame", "0.1;0.2;1", "0.3;0.4;2"), p)
  expect_named(radiatR:::.read_any(p, delim = ";", decimal = "."), c("x", "y", "frame"))
})

test_that(".read_any still reads a plain comma CSV unchanged", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("x,y,frame", "0.1,0.2,1", "0.3,0.4,2"), p)
  df <- as.data.frame(radiatR:::.read_any(p))
  expect_equal(ncol(df), 3L)
  expect_true(is.numeric(df$x))
})

test_that("read_tracks forwards read_opts$delim to the reader", {
  # comma-looking content but we force semicolon: the row stays one whole column
  p <- tempfile(fileext = ".csv")
  writeLines(c("x,y,frame", "0.1,0.2,1", "0.3,0.4,2"), p)
  expect_error(
    suppressMessages(read_tracks(p, normalize_xy = FALSE,
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

test_that("read_tracks forwards read_opts$sheet to the Excel reader", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  p <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    Sheet1 = data.frame(x = c(0.1, 0.3, 0.5), y = c(0.2, 0.4, 0.6), frame = 1:3),
    Other  = data.frame(x = c(0.9, 0.8),      y = c(0.7, 0.6),      frame = 1:2)
  ), p)
  ts1 <- suppressMessages(read_tracks(p, normalize_xy = FALSE))
  ts2 <- suppressMessages(
    read_tracks(p, normalize_xy = FALSE, read_opts = list(sheet = "Other")))
  expect_equal(nrow(as.data.frame(ts1)), 3L)
  expect_equal(nrow(as.data.frame(ts2)), 2L)
})

# ---- loader fps persists to frame_rate ---------------------------------------

test_that("read_tracks persists a valid fps as frame_rate (frames)", {
  df <- data.frame(id = "A", frame = 1:4,
                   x = c(0, .1, .2, .3), y = c(0, 0, 0, 0))
  ts <- read_tracks(df,
                    mapping = list(id = "id", time = "frame", x = "x", y = "y"),
                    time_type = "frames", fps = 60)
  expect_equal(frame_rate(ts), 60)
})

test_that("read_tracks rejects non-positive or non-finite fps for time_type = 'frames'", {
  df <- data.frame(id = "A", frame = 1:4,
                   x = c(0, .1, .2, .3), y = c(0, 0, 0, 0))
  map <- list(id = "id", time = "frame", x = "x", y = "y")
  for (bad_fps in c(0, -5, Inf, NaN)) {
    expect_error(read_tracks(df, mapping = map, time_type = "frames", fps = bad_fps),
                 "positive, finite")
  }
  expect_error(read_tracks(df, mapping = map, time_type = "frames", fps = NULL),
               "positive, finite")
})

test_that("read_tracks does not store frame_rate when time is already in seconds", {
  # time_type = "seconds" means the column is real elapsed time already; a
  # stored frame_rate would make elapsed_seconds() divide it by fps again.
  df <- data.frame(id = "A", time = c(0, 1, 2, 3),
                   x = c(0, .1, .2, .3), y = c(0, 0, 0, 0))
  ts <- read_tracks(df,
                    mapping = list(id = "id", time = "time", x = "x", y = "y"),
                    time_type = "seconds", fps = 60)
  expect_null(frame_rate(ts))
})

test_that("read_tracks leaves frame_rate NULL when no fps given", {
  df <- data.frame(id = "A", time = c(0, 1, 2, 3),
                   x = c(0, .1, .2, .3), y = c(0, 0, 0, 0))
  ts <- read_tracks(df,
                    mapping = list(id = "id", time = "time", x = "x", y = "y"),
                    time_type = "seconds")
  expect_null(frame_rate(ts))
})

test_that("read_tracks ignores a non-positive or non-finite fps", {
  df <- data.frame(id = "A", time = c(0, 1, 2, 3),
                   x = c(0, .1, .2, .3), y = c(0, 0, 0, 0))
  map <- list(id = "id", time = "time", x = "x", y = "y")
  expect_null(frame_rate(read_tracks(df, mapping = map,
                                     time_type = "seconds", fps = 0)))
  expect_null(frame_rate(read_tracks(df, mapping = map,
                                     time_type = "seconds", fps = -5)))
  # non-finite: exercises the is.finite() clause of the guard directly
  expect_null(frame_rate(read_tracks(df, mapping = map,
                                     time_type = "seconds", fps = Inf)))
  expect_null(frame_rate(read_tracks(df, mapping = map,
                                     time_type = "seconds", fps = NaN)))
})

test_that("a Tracks loaded with fps feeds track_speed without set_frame_rate", {
  df <- data.frame(id = "A", frame = 1:4,
                   x = c(0, 1, 2, 3), y = c(0, 0, 0, 0))
  ts <- read_tracks(df,
                    mapping = list(id = "id", time = "frame", x = "x", y = "y"),
                    time_type = "frames", fps = 2, normalize_xy = FALSE)
  sp <- track_speed(ts)
  # frame indices 1:4 at 2 fps -> 0.5 s between steps; unit x-step of 1 -> 2 units/s.
  # A test asserting only is.finite() would not have caught the fps being
  # applied twice (elapsed time divided by fps at load AND again on demand).
  expect_equal(sp$speed[1], 2)
})

test_that("read_tracks(time_type = 'frames', fps = ) does not double-convert elapsed time", {
  df <- data.frame(id = "A", frame = 1:4,
                    x = c(0, 1, 2, 3), y = c(0, 0, 0, 0))
  ts <- read_tracks(df,
                     mapping = list(id = "id", time = "frame", x = "x", y = "y"),
                     time_type = "frames", fps = 60, normalize_xy = FALSE)
  expect_equal(frame_rate(ts), 60)
  expect_equal(elapsed_seconds(ts), (0:3) / 60)
  expect_equal(track_duration(ts)$duration, 3 / 60)
  sp <- track_speed(ts)
  expect_equal(sp$speed[1], 1 / (1 / 60))       # unit step distance / dt
})

test_that("read_tracks(time_type = 'auto', fps = ) on a numeric time column does not double-convert", {
  df <- data.frame(id = "A", t = 1:4, x = c(0, 1, 2, 3), y = c(0, 0, 0, 0))
  ts <- read_tracks(df,
                     mapping = list(id = "id", time = "t", x = "x", y = "y"),
                     fps = 2, normalize_xy = FALSE)     # time_type left at default "auto"
  expect_equal(frame_rate(ts), 2)
  expect_equal(elapsed_seconds(ts), (0:3) / 2)
  expect_equal(track_speed(ts)$speed[1], 2)
})

test_that("a dialect-only fps (dialect_args) still becomes frame_rate without double-converting", {
  df <- data.frame(id = "A", frame = 1:4, x = c(0, 1, 2, 3), y = c(0, 0, 0, 0))
  ts <- read_tracks(df, dialect = "toxtrac", dialect_args = list(fps = 2),
                     normalize_xy = FALSE)
  expect_equal(frame_rate(ts), 2)
  expect_equal(elapsed_seconds(ts), (0:3) / 2)
  expect_equal(track_speed(ts)$speed[1], 2)
})

test_that("manifest load persists fps as frame_rate", {
  # Two track files (same fps) so length(traj_list) >= 2 and the Tracks c()
  # merge path in load_manifest() actually runs (a single-file manifest would
  # short-circuit before the merge).
  tmp <- withr::local_tempdir()
  path_a <- file.path(tmp, "track_a.csv")
  path_b <- file.path(tmp, "track_b.csv")
  utils::write.csv(
    data.frame(id = "A", frame = 1:4,
               x = c(0, .1, .2, .3), y = c(0, 0, 0, 0)),
    path_a, row.names = FALSE)
  utils::write.csv(
    data.frame(id = "B", frame = 1:4,
               x = c(0, .1, .2, .3), y = c(.1, .1, .1, .1)),
    path_b, row.names = FALSE)
  file_tbl <- tibble::tibble(
    basename = c("track_a", "track_b"),
    track = c(basename(path_a), basename(path_b)))
  manifest <- tibble::tibble(
    file = c("track_a", "track_b"),
    condition = c("baseline", "baseline"))

  ts <- load_manifest(
    file_tbl = file_tbl, track_dir = tmp, manifest = manifest,
    mapping = list(id = "id", time = "frame", x = "x", y = "y"),
    time_type = "frames", fps = 60)
  expect_equal(length(ts), 2L)          # merge really combined two tracks
  expect_equal(frame_rate(ts), 60)
})

# ---- multi-file id preservation ----------------------------------------------

test_that("read_tracks(character vector) derives one id per file when no id column", {
  tmp <- withr::local_tempdir()
  path_a <- file.path(tmp, "a.csv")
  path_b <- file.path(tmp, "b.csv")
  utils::write.csv(data.frame(time = c(1, 2, 3), x = c(0, 1, 2), y = c(0, 0, 1)),
                   path_a, row.names = FALSE)
  utils::write.csv(data.frame(time = c(1, 2, 3), x = c(5, 6, 7), y = c(1, 1, 0)),
                   path_b, row.names = FALSE)

  ts <- suppressMessages(suppressWarnings(
    read_tracks(c(path_a, path_b), normalize_xy = FALSE)))

  found <- sort(as.character(ids(ts)))
  expect_equal(found, c("a", "b"))                 # file stems, not "(multiple)"
  expect_false("(multiple)" %in% found)

  d <- as.data.frame(ts)
  # rows of file a precede rows of file b -> no cross-file time interleaving
  expect_equal(as.character(d[["..id"]]), c("a", "a", "a", "b", "b", "b"))
})

test_that("read_tracks_dir() reads a directory of id-less files, one id per file", {
  tmp <- withr::local_tempdir()
  utils::write.csv(data.frame(time = c(1, 2), x = c(0, 1), y = c(0, 1)),
                   file.path(tmp, "left.csv"), row.names = FALSE)
  utils::write.csv(data.frame(time = c(1, 2), x = c(2, 3), y = c(1, 0)),
                   file.path(tmp, "right.csv"), row.names = FALSE)

  ts <- suppressMessages(suppressWarnings(
    read_tracks_dir(tmp, normalize_xy = FALSE)))
  expect_s4_class(ts, "Tracks")
  expect_equal(sort(as.character(ids(ts))), c("left", "right"))
})

test_that("id_collision = 'error' (default) stops on a real id shared across files", {
  tmp <- withr::local_tempdir()
  path_a <- file.path(tmp, "a.csv")
  path_b <- file.path(tmp, "b.csv")
  utils::write.csv(data.frame(id = "1", time = c(1, 2), x = c(0, 1), y = c(0, 1)),
                   path_a, row.names = FALSE)
  utils::write.csv(data.frame(id = "1", time = c(1, 2), x = c(2, 3), y = c(1, 0)),
                   path_b, row.names = FALSE)

  err <- tryCatch(
    suppressMessages(suppressWarnings(
      read_tracks(c(path_a, path_b), normalize_xy = FALSE))),
    error = function(e) conditionMessage(e))
  expect_match(err, "collide")
  expect_match(err, "id '1'")
  expect_match(err, "a.csv")
  expect_match(err, "b.csv")
})

test_that("id_collision = 'namespace' prefixes ids by file stem and succeeds", {
  tmp <- withr::local_tempdir()
  path_a <- file.path(tmp, "a.csv")
  path_b <- file.path(tmp, "b.csv")
  utils::write.csv(data.frame(id = "1", time = c(1, 2), x = c(0, 1), y = c(0, 1)),
                   path_a, row.names = FALSE)
  utils::write.csv(data.frame(id = "1", time = c(1, 2), x = c(2, 3), y = c(1, 0)),
                   path_b, row.names = FALSE)

  ts <- suppressMessages(suppressWarnings(
    read_tracks(c(path_a, path_b), normalize_xy = FALSE, id_collision = "namespace")))
  expect_equal(sort(as.character(ids(ts))), c("a::1", "b::1"))
})

test_that("read_tracks errors on a duplicate (id, time) key across combined files", {
  tmp <- withr::local_tempdir()
  path_a <- file.path(tmp, "a.csv")
  path_b <- file.path(tmp, "b.csv")
  # file a repeats (id = x, time = 1); id x lives only in file a (no collision),
  # so the duplicate-key check is what fires.
  utils::write.csv(data.frame(id = "x", time = c(1, 1), x = c(0, 9), y = c(0, 9)),
                   path_a, row.names = FALSE)
  utils::write.csv(data.frame(id = "y", time = c(2, 3), x = c(2, 3), y = c(1, 0)),
                   path_b, row.names = FALSE)

  expect_error(
    suppressMessages(suppressWarnings(
      read_tracks(c(path_a, path_b), normalize_xy = FALSE))),
    regexp = "duplicate \\(id, time\\)"
  )
})

test_that("read_tracks errors on multi-file input with no id column and id_from_filename = FALSE", {
  tmp <- withr::local_tempdir()
  path_a <- file.path(tmp, "a.csv")
  path_b <- file.path(tmp, "b.csv")
  utils::write.csv(data.frame(time = c(1, 2), x = c(0, 1), y = c(0, 1)),
                   path_a, row.names = FALSE)
  utils::write.csv(data.frame(time = c(1, 2), x = c(2, 3), y = c(1, 0)),
                   path_b, row.names = FALSE)

  expect_error(
    suppressMessages(suppressWarnings(
      read_tracks(c(path_a, path_b), normalize_xy = FALSE, id_from_filename = FALSE))),
    regexp = "id_from_filename = FALSE"
  )
})

test_that("read_tracks_dir(recursive = TRUE) detects a synthesized-id collision across same-named files in different subdirectories", {
  tmp <- withr::local_tempdir()
  sub1 <- file.path(tmp, "sub1")
  sub2 <- file.path(tmp, "sub2")
  dir.create(sub1)
  dir.create(sub2)
  # Same file name ("left.csv") in two different subdirectories, non-overlapping
  # time ranges so the duplicate-(id,time)-key safety net does NOT mask this:
  # only the id-collision check (keyed on full source path) can catch it.
  utils::write.csv(data.frame(time = c(1, 2), x = c(0, 1), y = c(0, 1)),
                   file.path(sub1, "left.csv"), row.names = FALSE)
  utils::write.csv(data.frame(time = c(100, 101), x = c(5, 6), y = c(2, 3)),
                   file.path(sub2, "left.csv"), row.names = FALSE)

  err <- tryCatch(
    suppressMessages(suppressWarnings(
      read_tracks_dir(tmp, recursive = TRUE, normalize_xy = FALSE))),
    error = function(e) conditionMessage(e))
  expect_match(err, "collide")
  expect_match(err, "id 'left'")
  expect_match(err, "left.csv")
})

test_that("id_collision = 'namespace' resolves ids before the duplicate-key check runs", {
  tmp <- withr::local_tempdir()
  path_a <- file.path(tmp, "a.csv")
  path_b <- file.path(tmp, "b.csv")
  # Same real id "1" reused across files AND overlapping times, so leaving the
  # id un-namespaced would trip both the id-collision check and (once ids
  # collide) the duplicate-(id,time)-key check. Namespacing must resolve the
  # id before the duplicate-key check runs, so this should succeed cleanly.
  utils::write.csv(data.frame(id = "1", time = c(1, 2), x = c(0, 1), y = c(0, 1)),
                   path_a, row.names = FALSE)
  utils::write.csv(data.frame(id = "1", time = c(1, 2), x = c(2, 3), y = c(1, 0)),
                   path_b, row.names = FALSE)

  ts <- suppressMessages(suppressWarnings(
    read_tracks(c(path_a, path_b), normalize_xy = FALSE, id_collision = "namespace")))
  expect_s4_class(ts, "Tracks")
  expect_equal(sort(as.character(ids(ts))), c("a::1", "b::1"))
})

test_that(".read_any dispatches on explicit ext, not the path suffix", {
  # delimited file whose PATH has a misleading (extension-stripped) name
  raw <- tempfile()                      # no extension at all
  writeLines(c("a;b", "1;2", "3;4"), raw)
  df <- radiatR:::.read_any(raw, ext = "csv", delim = ";")
  expect_equal(nrow(df), 2L)
  expect_equal(names(df), c("a", "b"))
})

test_that(".read_any ext override reads Excel from an extensionless path", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  raw <- tempfile()                      # no .xlsx suffix
  writexl::write_xlsx(data.frame(a = 1:2, b = 3:4), raw)
  df <- radiatR:::.read_any(raw, ext = "xlsx")
  expect_equal(as.data.frame(df), data.frame(a = 1:2, b = 3:4))
})

test_that("read_tracks forwards read_opts$ext to .read_any", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  raw <- tempfile()
  writexl::write_xlsx(
    data.frame(id = "a", time = 0:2, x = c(0, 1, 2), y = c(0, 1, 0)), raw)
  ts <- read_tracks(raw, read_opts = list(ext = "xlsx"))
  expect_s4_class(ts, "Tracks")
  expect_equal(nrow(as.data.frame(ts)), 3L)
})

test_that("a byte that is not UTF-8 does not stop the separator sniff", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("id,time,x,y,note\n1,1,0.1,0.2,caf"), as.raw(0xe9),
             charToRaw("\n1,2,0.2,0.3,b\n")), p)
  expect_identical(.guess_delim(p), list(delim = ",", decimal = "."))
  ts <- read_tracks(p)
  expect_equal(ts@data[[ts@cols$x]], c(.1, .2))
  # ';' with decimal commas, latin-1 in a label
  p2 <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("id;time;x;y;note\n1;1;0,1;0,2;caf"), as.raw(0xe9),
             charToRaw("\n1;2;0,2;0,3;b\n")), p2)
  expect_identical(.guess_delim(p2), list(delim = ";", decimal = ","))
})

test_that("the separator sniff ignores separators inside quotes", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw(paste0("id;time;x;y;note\n1;1;0,1;0,1;\"a; b\"\n",
                            "1;2;0,2;0,2;c\n1;3;0,3;0,3;\"d; e; f\"\n")), p)
  expect_identical(.guess_delim(p), list(delim = ";", decimal = ","))
  expect_equal(read_tracks(p)@data$x, c(.1, .2, .3))
  q <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw(paste0("id,time,x,y,note\n1,1,.1,.1,\"a, b\"\n",
                            "1,2,.2,.2,c\n1,3,.3,.3,\"d,\ne, f\"\n")), q)   # and across a line
  expect_identical(.guess_delim(q)$delim, ",")
  expect_equal(nrow(read_tracks(q)@data), 3L)
})

test_that("a tab file written with row names is named as such, not read as one column", {
  p <- withr::local_tempfile(fileext = ".tsv")
  utils::write.table(data.frame(id = 1, time = 1:3, x = c(.1, .2, .3), y = .1), p,
                     sep = "\t", quote = FALSE)            # row.names = TRUE
  e <- expect_error(read_tracks(p), class = "radiatR_malformed_file")
  expect_match(conditionMessage(e), "row names")
})

test_that("a file read as one column says the separator was not found", {
  p <- withr::local_tempfile(fileext = ".csv")
  # tabs and spaces mixed: no candidate is used consistently, so one column
  writeBin(charToRaw("id\ttime x\ty\n1 1\t0.1\t0.2\n1\t2 0.2 0.3\n1 3 0.3\t0.4\n"), p)
  e <- expect_error(read_tracks(p), class = "radiatR_malformed_file")
  expect_identical(e$problem, "one_column")
  expect_match(conditionMessage(e), "read_opts")
  expect_match(conditionMessage(e), "single column")
  expect_false(grepl(dirname(p), conditionMessage(e), fixed = TRUE))
  e2 <- expect_error(read_tracks(p, read_opts = list(delim = "|")),
                     class = "radiatR_malformed_file")
  expect_false(grepl("read_opts", conditionMessage(e2), fixed = TRUE))
})

test_that("an explicit ext exempts an extensionless path from the one-column error", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  # A real one-column workbook at a path with no extension: the read must reach
  # the column check and stop there, not with the one-column error.
  p <- withr::local_tempfile()
  writexl::write_xlsx(data.frame(id = 1:2), p)
  expect_error(read_tracks(p, read_opts = list(ext = "xlsx")),
               "angle column or both x and y columns")
})

test_that("namespace gives files that share a stem their own prefix", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "a")); dir.create(file.path(dir, "b"))
  rows <- "id,time,x,y\n1,1,.1,.1\n1,2,.2,.2\n"
  writeBin(charToRaw(rows), file.path(dir, "n1.csv"))
  writeBin(charToRaw(gsub(",", "\t", rows)), file.path(dir, "n1.tsv"))
  writeBin(charToRaw(rows), file.path(dir, "a", "n2.csv"))
  writeBin(charToRaw(rows), file.path(dir, "b", "n2.csv"))
  ts <- read_tracks(c(file.path(dir, "n1.csv"), file.path(dir, "n1.tsv"),
                      file.path(dir, "a", "n2.csv"), file.path(dir, "b", "n2.csv")),
                    id_collision = "namespace")
  ids <- unique(as.character(ts@data$id))
  expect_length(ids, 4L)
  expect_true(all(c("n1.csv::1", "n1.tsv::1") %in% ids))
  expect_true(all(c("a/n2.csv::1", "b/n2.csv::1") %in% ids))
  # a stem used once keeps the short prefix
  ts2 <- read_tracks(c(file.path(dir, "n1.csv"), file.path(dir, "a", "n2.csv")),
                     id_collision = "namespace")
  expect_setequal(unique(as.character(ts2@data$id)), c("n1::1", "n2::1"))
})

test_that(".source_labels picks the shortest prefix that is unique", {
  expect_identical(
    unname(.source_labels(c("x/n1.csv", "x/n1.tsv", "a/n2.csv", "b/n2.csv", "c/n3.csv"))),
    c("n1.csv", "n1.tsv", "a/n2.csv", "b/n2.csv", "n3"))
})

test_that(".read_names gives the names .read_any gives, from one record", {
  dir <- withr::local_tempdir()
  for (txt in c("id,time,x,y\n1,1,.1,.1\n", "id;time; x ;\"y, z\"\n1;1;0,1;0,1\n",
                "a\tb\tc\n1\t2\t3\n", "﻿id,time,x,y\n1,1,.1,.1\n")) {
    p <- file.path(dir, "f.csv"); writeBin(charToRaw(enc2utf8(txt)), p)
    expect_identical(radiatR:::.read_names(p), names(radiatR:::.read_any(p)), info = txt)
  }
  gz <- file.path(dir, "f.csv.gz"); con <- gzfile(gz, "wb")
  writeLines(c("id,time,x,y", "1,1,.1,.1"), con); close(con)
  expect_identical(radiatR:::.read_names(gz), c("id", "time", "x", "y"))
})

test_that("read_tracks_format resolves regex_mapping from names alone", {
  dir <- withr::local_tempdir(); p <- file.path(dir, "f.csv")
  writeBin(charToRaw("animal,sec,px,py\na,1,.1,.1\na,2,.2,.2\n"), p)
  ts <- read_tracks_format(p, list(regex_mapping = list(id = "^animal", time = "^sec",
                                                        x = "^px", y = "^py")))
  expect_identical(unique(as.character(ts@data$animal)), "a")
})
