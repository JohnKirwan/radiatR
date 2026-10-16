# The kept pass-through dialects against files the tracking programs wrote.
# fixtures/real/README.md gives each file's source and licence.

fx <- function(f) testthat::test_path("fixtures", "real", f)
cond <- function(expr) tryCatch(expr, error = function(e) e)
rows_of <- function(ts) as.data.frame(ts)
# A copy of a fixture with its lines rewritten by `edit`.
edited <- function(f, edit, env = parent.frame()) {
  p <- withr::local_tempfile(fileext = paste0(".", tools::file_ext(f)), .local_envir = env)
  writeLines(edit(readLines(fx(f))), p)
  p
}

# ---- TrackMate -----------------------------------------------------------------

test_that("TrackMate 7: the three descriptor rows are dropped and T is seconds", {
  ts <- read_tracks(fx("trackmate_v7_spots.csv"), dialect = "trackmate")
  d <- rows_of(ts)
  expect_equal(nrow(d), 6L)
  expect_setequal(as.character(d$id), c("0", "1"))
  expect_equal(sort(unique(d$time)), c(0, 25.4814553, 51.4829403))
  expect_equal(d$x[d$id == "1" & d$time == 0], 69.8944504353973)
  expect_null(frame_rate(ts))
  expect_equal(sort(unique(elapsed_seconds(ts))), c(0, 25.4814553, 51.4829403))
})

test_that("TrackMate seconds are not divided by a frame rate", {
  expect_message(ts <- read_tracks(fx("trackmate_v7_spots.csv"), dialect = "trackmate",
                                   dialect_args = list(fps = 2)),
                 "`fps` is not needed")
  expect_null(frame_rate(ts))
  expect_equal(max(elapsed_seconds(ts)), 51.4829403)
  e <- cond(read_tracks(fx("trackmate_v7_spots.csv"), dialect = "trackmate",
                        time_type = "frames", fps = 2))
  expect_s3_class(e, "radiatR_time_type_mismatch")
})

test_that("TrackMate 7: a bad spot cell is reported by spot row, header and file", {
  p <- edited("trackmate_v7_spots.csv", function(l) { l[6] <- sub(",59.5261955784358,", ",oops,", l[6], fixed = TRUE); l })
  e <- cond(read_tracks(p, dialect = "trackmate"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 2L)
  expect_identical(e$columns, "POSITION_X")
  expect_identical(e$source, basename(p))
  expect_identical(e$dialect, "trackmate")
})

test_that("rows are not dropped as TrackMate descriptors merely because they do not parse", {
  # One header row, then two bad spots and a third, real one: nothing here is
  # a units row, so the bad cells must be reported.
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("TRACK_ID,FRAME,POSITION_X,POSITION_Y", "1,0,bad,1", "1,1,worse,2", "1,2,3,3"), p)
  e <- cond(read_tracks(p, dialect = "trackmate"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 1:2)
})

test_that("TrackMate <= 6: spots in no track stop the load, or are dropped on request", {
  e <- cond(read_tracks(fx("trackmate_v6_spots.csv"), dialect = "trackmate"))
  expect_s3_class(e, "radiatR_unlinked_rows")
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_identical(e$rows, 1:3)
  expect_identical(unique(e$values), "None")
  expect_match(conditionMessage(e), "unlinked = \"drop\"", fixed = TRUE)

  w <- NULL
  ts <- withCallingHandlers(
    read_tracks(fx("trackmate_v6_spots.csv"), dialect = "trackmate",
                dialect_args = list(unlinked = "drop", fps = 10)),
    radiatR_dropped_rows = function(c) { w <<- c; invokeRestart("muffleWarning") })
  expect_identical(w$rows, 1:3)
  d <- rows_of(ts)
  expect_setequal(as.character(d$id), c("1", "100"))
  # no units row, so time is FRAME (POSITION_T here is 60 x FRAME)
  expect_equal(sort(d$time), c(10, 11, 12, 78, 79, 80))
  expect_equal(frame_rate(ts), 10)
  expect_equal(sort(elapsed_seconds(ts)), rep(c(0, 0.1, 0.2), each = 2))
})

# ---- ToxTrac -------------------------------------------------------------------

test_that("ToxTrac Tracking_RealSpace: mm positions, seconds, id from arena and track", {
  ts <- read_tracks(fx("toxtrac_tracking_realspace.txt"), dialect = "toxtrac")
  d <- rows_of(ts)
  expect_identical(unique(as.character(d$id)), "1_1")
  expect_equal(d$x, c(225.826, 223.924, 221.487, 218.581, 216.395))
  expect_equal(d$y, c(186.29, 186.02, 185.6, 185.362, 185.215))
  expect_equal(elapsed_seconds(ts), c(0, 0.0333333, 0.0666667, 0.1, 0.133333))
  expect_true(all(c("arena", "label") %in% names(d)))
})

test_that("ToxTrac Tracking.txt: with or without a header row, 11 or 6 columns, in frames", {
  with_header <- rows_of(read_tracks(fx("toxtrac_tracking.txt"), dialect = "toxtrac"))
  expect_equal(with_header$x, c(145.826, 143.924, 141.487, 138.581, 136.395))
  expect_equal(with_header$time, 0:4)
  expect_identical(unique(as.character(with_header$id)), "0_1")

  headerless <- edited("toxtrac_tracking.txt", function(l) l[-1])
  ts <- read_tracks(headerless, dialect = "toxtrac", dialect_args = list(fps = 30))
  expect_equal(rows_of(ts)[c("x", "y")], with_header[c("x", "y")])
  expect_equal(frame_rate(ts), 30)
  expect_equal(elapsed_seconds(ts), (0:4) / 30)

  six <- edited("toxtrac_tracking.txt", function(l)
    vapply(strsplit(l[-1], "\t", fixed = TRUE), function(v) paste(v[c(1:5, 11)], collapse = "\t"), ""))
  expect_equal(rows_of(read_tracks(six, dialect = "toxtrac"))[c("x", "y")], with_header[c("x", "y")])

  seven <- edited("toxtrac_tracking.txt", function(l)
    vapply(strsplit(l[-1], "\t", fixed = TRUE), function(v) paste(v[1:7], collapse = "\t"), ""))
  e <- cond(read_tracks(seven, dialect = "toxtrac"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "7 columns")
})

test_that("ToxTrac: a latin-1 label reads as its text, with or without a header row", {
  latin <- function(l) {
    l[length(l)] <- sub("1$", "\u00e9", l[length(l)])
    iconv(l, "UTF-8", "latin1")
  }
  for (drop in c(FALSE, TRUE)) {
    p <- withr::local_tempfile(fileext = ".txt")
    l <- readLines(fx("toxtrac_tracking.txt"))
    writeLines(latin(if (drop) l[-1] else l), p, useBytes = TRUE)
    d <- rows_of(read_tracks(p, dialect = "toxtrac"))
    expect_identical(d$label[nrow(d)], "\u00e9", info = paste("headerless:", drop))
  }
})

test_that("ToxTrac: a bad cell in a headerless file is reported by row and file", {
  p <- edited("toxtrac_tracking.txt", function(l) sub("^2\t0\t1\t141.487", "2\t0\t1\toops", l[-1]))
  e <- cond(read_tracks(p, dialect = "toxtrac"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 3L)
  expect_identical(e$columns, "x")
  expect_identical(e$source, basename(p))
})

test_that("ToxTrac files in seconds and in frames do not combine", {
  e <- cond(read_tracks(c(fx("toxtrac_tracking.txt"), fx("toxtrac_tracking_realspace.txt")),
                        dialect = "toxtrac"))
  expect_s3_class(e, "radiatR_time_type_mismatch")
})

# ---- idtracker.ai --------------------------------------------------------------

test_that("idtracker.ai trajectories.csv: x<id>/y<id> pairs, seconds, 'nan' is missing", {
  path <- fx("idtrackerai_trajectories.csv")
  e <- cond(read_tracks(path, dialect = "idtrackerai_wide"))
  expect_s3_class(e, "radiatR_invalid_rows")              # the two 'nan' positions
  expect_false(inherits(e, "radiatR_malformed_values"))
  ts <- suppressWarnings(read_tracks(path, dialect = "idtrackerai_wide", on_invalid = "drop"),
                         classes = "radiatR_dropped_rows")
  d <- rows_of(ts)
  expect_setequal(as.character(d$id), as.character(1:8))
  expect_equal(nrow(d), 86L)
  first <- d[d$id == "1" & d$time == 0, ]
  expect_equal(c(first$x, first$y), c(853.333, 173.355))  # y is y1, not x1 again
  expect_equal(utils::head(elapsed_seconds(ts)[d$id == "1"], 3), c(0, 0.036, 0.071))
})

test_that("idtracker.ai: a bad cell is reported against its wide column and file row", {
  p <- edited("idtrackerai_trajectories.csv", function(l) { l[3] <- sub("732.275", "oops", l[3], fixed = TRUE); l })
  e <- cond(read_tracks(p, dialect = "idtrackerai_wide"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "x3")
  expect_identical(e$rows, 2L)
})

test_that("idtracker.ai: x_<id> names, row-order frames, and loud pairing and id errors", {
  df <- data.frame(x_1 = c(0, 1), y_1 = c(5, 6), x_2 = c(2, 3), y_2 = c(7, 8))
  d <- rows_of(read_tracks(df, dialect = "idtrackerai_wide"))
  expect_equal(d$y[d$id == "2"], c(7, 8))
  expect_equal(d$time[d$id == "1"], 1:2)
  expect_equal(nrow(rows_of(read_tracks(df, dialect = "idtrackerai_wide",
                                        dialect_args = list(ids = 2)))), 2L)
  expect_error(read_tracks(df, dialect = "idtrackerai_wide", dialect_args = list(ids = 9)),
               "no such id")
  expect_error(read_tracks(df[-4], dialect = "idtrackerai_wide"), "no y column for x_2")
})

# ---- TRex ----------------------------------------------------------------------

test_that("TRex: the repeated frame column is the time, in frames", {
  path <- fx("trex_beetle.csv")
  ts <- suppressMessages(read_tracks(path, dialect = "trex", dialect_args = list(fps = 30)))
  d <- rows_of(ts)
  expect_equal(d$time, 0:4)
  expect_equal(frame_rate(ts), 30)
  expect_equal(d$x[1:2], c(9.491, 9.487))                 # plain X (cm), not X#wcentroid
  secs <- suppressMessages(read_tracks(path, dialect = "trex", dialect_args = list(time_col = "time")))
  expect_equal(elapsed_seconds(secs)[1:2], c(0, 0.033))
})

test_that("TRex: repeated frame columns that differ are an error", {
  df <- data.frame(frame_1 = 0:2, X = c(0, 1, 2), Y = 0, frame_5 = c(0, 1, 9))
  expect_error(read_tracks(df, dialect = "trex"), "repeated frame columns")
})

# ---- A wrong dialect is an error --------------------------------------------------

test_that("every real export loads under its own dialect only", {
  own <- list(dlc_CollectedData_Pranav.csv = character(0), # labelled training data: refused by all
              dlc_single_wasp_predictions.csv = "deeplabcut",
              dlc_two_mice_predictions.csv = "deeplabcut",
              ethovision_trial.txt = "ethovision",
              ethovision_truncated.txt = character(0), # truncated: refused by all
              idtrackerai_trajectories.csv = "idtrackerai_wide",
              sleap_three_mice.analysis.csv = "sleap",
              sleap_three_nodes.analysis.csv = "sleap",
              toxtrac_tracking.txt = "toxtrac",
              toxtrac_tracking_realspace.txt = "toxtrac",
              trackmate_v6_spots.csv = "trackmate",
              trackmate_v7_spots.csv = "trackmate",
              trex_beetle.csv = "trex")
  dialects <- c("anymaze", "deeplabcut", "ethovision", "idtrackerai_wide", "sleap",
                "toxtrac", "trackmate", "trex")
  loads <- function(f, d) {
    args <- if (d == "trackmate") list(unlinked = "drop") else list()
    !inherits(cond(suppressWarnings(suppressMessages(
      read_tracks(fx(f), dialect = d, dialect_args = args, on_invalid = "drop")))), "error")
  }
  for (f in names(own)) {
    got <- dialects[vapply(dialects, function(d) loads(f, d), logical(1))]
    expect_identical(got, own[[f]], label = f)
  }
})

test_that("SLEAP refuses a table with no frame_idx unless time_col names the time", {
  e <- cond(read_tracks(fx("toxtrac_tracking.txt"), dialect = "sleap"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "frame_idx", fixed = TRUE)
  expect_s3_class(cond(read_tracks(fx("trex_beetle.csv"), dialect = "sleap")),
                  "radiatR_dialect_error")
  df <- data.frame(frame = 1:3, track = "a", head.x = c(0, .1, .2), head.y = 0,
                   check.names = FALSE)
  expect_s3_class(cond(read_tracks(df, dialect = "sleap")), "radiatR_dialect_error")
  ts <- read_tracks(df, dialect = "sleap", dialect_args = list(time_col = "frame"))
  expect_equal(nrow(rows_of(ts)), 3L)
})

test_that("TRex refuses a track column it was not told to use", {
  e <- cond(read_tracks(fx("toxtrac_tracking.txt"), dialect = "trex"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "`Track`.*id_col")
  e <- cond(read_tracks(fx("trackmate_v6_spots.csv"), dialect = "trex"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "`TRACK_ID`.*id_col")
  df <- data.frame(Track = rep(c("a", "b"), each = 2), frame = rep(1:2, 2),
                   X = c(0, 1, 2, 3), Y = 0)
  ts <- read_tracks(df, dialect = "trex", dialect_args = list(id_col = "Track"))
  expect_setequal(as.character(rows_of(ts)$id), c("a", "b"))
  e <- cond(read_tracks(df, dialect = "trex", dialect_args = list(id_col = "trak")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "`id_col = \"trak\"` names no column", fixed = TRUE)
})

test_that("a dialect refusal in a multi-file read names the file", {
  e <- cond(read_tracks(rep(fx("toxtrac_tracking.txt"), 2), dialect = "sleap"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "frame_idx.*\\(file 'toxtrac_tracking.txt'\\)\\.$")
  e <- cond(read_tracks(rep(fx("trex_beetle.csv"), 2), dialect = "trex",
                        dialect_args = list(id_col = "trak")))
  expect_match(conditionMessage(e), "names no column.*\\(file 'trex_beetle.csv'\\)\\.$")
  e <- cond(read_tracks(fx("trex_beetle.csv"), dialect = "trex",
                        dialect_args = list(id_col = c("a", "b"))))
  expect_s3_class(e, "radiatR_dialect_error")
})

test_that("refusals of the generic-column dialects are typed", {
  df <- data.frame(a = 1:2, b = 1:2)
  for (d in c("idtrackerai_wide", "trackmate", "toxtrac", "trex"))
    expect_s3_class(cond(read_tracks(df, dialect = d)), "radiatR_dialect_error")
})
