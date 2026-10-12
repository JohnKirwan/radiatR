# Loader coercion: a missing value stays NA, a malformed one aborts.

real_fixture <- function(f) testthat::test_path("fixtures", "real", f)

malformed <- function(expr) {
  tryCatch(expr, radiatR_malformed_values = function(e) e)
}

xy_df <- function(...) {
  d <- data.frame(id = "a", frame = 1:3, x = c(0.1, 0.2, 0.3),
                  y = c(0.1, 0.2, 0.3), stringsAsFactors = FALSE)
  repl <- list(...)
  for (nm in names(repl)) d[[nm]] <- repl[[nm]]
  d
}

# ---- .as_numeric_strict ------------------------------------------------------

test_that(".as_numeric_strict returns numeric input unchanged", {
  expect_identical(radiatR:::.as_numeric_strict(c(1L, NA, 3L), "x"), c(1L, NA, 3L))
  expect_identical(radiatR:::.as_numeric_strict(c(1.5, NaN, Inf), "x"), c(1.5, NaN, Inf))
})

test_that(".as_numeric_strict reads blank and NA tokens as missing", {
  got <- radiatR:::.as_numeric_strict(c("1.5", "", "  ", "NA", "na", NA), "x")
  expect_identical(got, c(1.5, NA, NA, NA, NA, NA))
})

test_that(".as_numeric_strict keeps what as.numeric() parses", {
  got <- radiatR:::.as_numeric_strict(c(" 2.5 ", "1e-3", "nan", "NaN", "-inf"), "x")
  expect_identical(got, c(2.5, 0.001, NaN, NaN, -Inf))
  expect_identical(radiatR:::.as_numeric_strict(factor(c("1", "2")), "x"), c(1, 2))
})

test_that(".as_numeric_strict handles a zero-row and an all-missing column", {
  expect_identical(radiatR:::.as_numeric_strict(character(0), "x"), numeric(0))
  expect_identical(radiatR:::.as_numeric_strict(c(NA, NA), "x"), c(NA_real_, NA_real_))
})

test_that(".as_numeric_strict aborts on malformed values with their rows", {
  e <- malformed(radiatR:::.as_numeric_strict(
    c("0.5", "oops", "", "1 000", "TRUE"), "nose_x",
    source = "run1.csv", dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_identical(e$rows, c(2L, 4L, 5L))
  expect_identical(e$values, c("oops", "1 000", "TRUE"))
  expect_identical(e$columns, "nose_x")
  expect_identical(e$ids, character(0))
  expect_identical(e$source, "run1.csv")
  expect_identical(e$dialect, "deeplabcut")
  expect_match(conditionMessage(e),
               "Column 'nose_x' (file 'run1.csv', dialect 'deeplabcut') has 3 value(s) that are not numeric",
               fixed = TRUE)
})

test_that("long lists of rows and values are truncated with a marker", {
  e <- malformed(radiatR:::.as_numeric_strict(paste0("v", 1:25), "x"))
  expect_identical(e$rows, 1:25)
  expect_match(conditionMessage(e), "rows: 1, 2, 3", fixed = TRUE)
  expect_match(conditionMessage(e), "19, 20, ...\n", fixed = TRUE)
  expect_match(conditionMessage(e), "'v10', ...", fixed = TRUE)
})

test_that("a decimal comma from a file earns a hint; nothing else does", {
  e <- malformed(radiatR:::.as_numeric_strict(c("1,5", "2,25"), "x", source = "a.tsv"))
  expect_match(conditionMessage(e), "read_opts = list(decimal = \",\")", fixed = TRUE)
  e <- malformed(radiatR:::.as_numeric_strict(c("oops"), "x", source = "a.tsv"))
  expect_no_match(conditionMessage(e), "decimal")
  # read_opts does nothing for a data frame
  e <- malformed(radiatR:::.as_numeric_strict(c("1,5", "2,25"), "x"))
  expect_no_match(conditionMessage(e), "decimal")
})

test_that("text read with a decimal comma parses, and only the bad cell is named", {
  got <- radiatR:::.as_numeric_strict(c("1,5", "-0,25", "3", ",5"), "x", decimal = ",")
  expect_identical(got, c(1.5, -0.25, 3, 0.5))
  e <- malformed(radiatR:::.as_numeric_strict(c("1,5", "oops", "1,2,3"), "x",
                                              source = "a.csv", decimal = ","))
  expect_identical(e$rows, 2:3)
  expect_no_match(conditionMessage(e), "read_opts")
})

# ---- .coerce_time ------------------------------------------------------------

test_that(".coerce_time reads numeric strings as numeric time", {
  expect_identical(radiatR:::.coerce_time(c("0", "1", "")), c(0, 1, NA))
  expect_identical(radiatR:::.coerce_time(c("0", "1"), time_type = "seconds"), c(0, 1))
  expect_identical(radiatR:::.coerce_time(c("0", "1"), time_type = "frames", fps = 5), c(0, 1))
})

test_that(".coerce_time parses timestamps in full", {
  got <- radiatR:::.coerce_time(c("2024-01-01 10:00:01", NA, "2024-01-01 10:00:03"))
  expect_s3_class(got, "POSIXct")
  expect_identical(as.numeric(diff(got[c(1, 3)]), units = "secs"), 2)
  iso <- radiatR:::.coerce_time(c("2024-01-01T10:00:01Z", "2024-01-01T10:00:02.5Z"))
  expect_identical(format(iso, "%H:%M:%OS1"), c("10:00:01.0", "10:00:02.5"))
  expect_identical(format(radiatR:::.coerce_time(c("2024/01/01", "2024/01/02"))),
                   c("2024-01-01", "2024-01-02"))
})

test_that(".coerce_time honours tz, and reads a trailing Z as UTC whatever tz is", {
  tokyo <- radiatR:::.coerce_time("2024-01-01 09:00:00", tz = "Asia/Tokyo")
  zulu  <- radiatR:::.coerce_time("2024-01-01T00:00:00Z", tz = "Asia/Tokyo")
  expect_identical(as.numeric(tokyo), as.numeric(zulu))
  expect_identical(attr(zulu, "tzone"), "Asia/Tokyo")
})

test_that("a bare date beside date-times is midnight, as as.character() writes it", {
  ts <- as.POSIXct(c("2023-12-31 23:59:59", "2024-01-01 00:00:00"), tz = "UTC")
  got <- radiatR:::.coerce_time(c("2023-12-31 23:59:59", "2024-01-01"))
  expect_identical(as.numeric(got), as.numeric(ts))
})

test_that(".coerce_time keeps typed time and converts difftime and Date", {
  t0 <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC")
  expect_identical(radiatR:::.coerce_time(t0), t0)
  expect_identical(radiatR:::.coerce_time(c(0.5, 1)), c(0.5, 1))
  expect_identical(radiatR:::.coerce_time(as.difftime(c(1, 2), units = "mins")), c(60, 120))
  expect_identical(radiatR:::.coerce_time(as.Date("2024-01-02")),
                   as.POSIXct("2024-01-02", tz = "UTC"))
})

test_that(".coerce_time rejects clock time as frames and an invalid fps for any input", {
  expect_error(radiatR:::.coerce_time(Sys.time(), time_type = "frames", fps = 5),
               class = "radiatR_time_type_mismatch")
  expect_error(radiatR:::.coerce_time(as.difftime(1, units = "secs"),
                                      time_type = "frames", fps = 5),
               class = "radiatR_time_type_mismatch")
  expect_error(radiatR:::.coerce_time(c("0", "1"), fps = -5), "fps")
  expect_error(radiatR:::.coerce_time(c(0, 1), fps = -5), "fps")
})

test_that(".coerce_time aborts on a malformed timestamp instead of truncating it", {
  e <- malformed(radiatR:::.coerce_time(
    c("2024-01-01 10:00:01", "2024-01-01 garbage", "2024-01-01 10:00:03"),
    column = "stamp"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 2L)
  expect_identical(e$values, "2024-01-01 garbage")
  expect_identical(e$columns, "stamp")
  expect_identical(e$expected, "a date-time")
})

test_that(".coerce_time blames the minority reading of a mixed column", {
  e <- malformed(radiatR:::.coerce_time(c("1", "x", "3")))
  expect_identical(e$rows, 2L)
  expect_identical(e$expected, "numeric")
  e <- malformed(radiatR:::.coerce_time(c("a", "b")))
  expect_identical(e$rows, 1:2)
  expect_identical(e$expected, "a number or date-time")
  e <- malformed(radiatR:::.coerce_time(c("1", "2"), time_type = "posix"))
  expect_identical(e$rows, 1:2)
})

# ---- read_tracks(): generic path ---------------------------------------------

test_that("read_tracks names the malformed cell, not every row of the column", {
  e <- malformed(read_tracks(xy_df(x = c("0.1", "bad", "0.3"))))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 2L)
  expect_identical(e$values, "bad")
  expect_identical(e$columns, "x")
})

test_that("on_invalid = 'drop' never drops a malformed value", {
  for (col in c("x", "y")) {
    d <- xy_df(); d[[col]] <- c("0.1", "bad", "0.3")
    e <- malformed(read_tracks(d, on_invalid = "drop"))
    expect_s3_class(e, "radiatR_malformed_values")
    expect_identical(e$columns, col)
  }
})

test_that("malformed angle, weight and time abort too", {
  ang <- data.frame(id = "a", frame = 1:3, angle = c("0.1", "north", "0.3"))
  expect_identical(malformed(read_tracks(ang))$columns, "angle")
  w <- xy_df(w = c("1", "heavy", "1"))
  e <- malformed(read_tracks(w, mapping = list(weight = "w")))
  expect_identical(e$columns, "w")
  e <- malformed(read_tracks(xy_df(frame = c("0", "x", "2"))))
  expect_identical(e$columns, "frame")
  expect_identical(e$rows, 2L)
})

test_that("character columns of well-formed numbers load like numeric ones", {
  num <- read_tracks(xy_df())
  chr <- read_tracks(xy_df(x = c("0.1", " 0.2 ", "3e-1"), y = factor(c("0.1", "0.2", "0.3"))))
  expect_equal(as.data.frame(chr), as.data.frame(num))
})

test_that("missing tokens in a character column are missing, not malformed", {
  d <- xy_df(x = c("0.1", "", "nan"))
  e <- tryCatch(read_tracks(d), radiatR_invalid_rows = function(e) e)
  expect_false(inherits(e, "radiatR_malformed_values"))
  expect_identical(e$rows, 2:3)
  tr <- suppressWarnings(read_tracks(d, on_invalid = "drop"), classes = "radiatR_dropped_rows")
  expect_equal(nrow(as.data.frame(tr)), 1L)
})

test_that("numeric-string frames with fps are stored as frames; durations are not", {
  tr <- read_tracks(xy_df(frame = c("0", "1", "2")), fps = 10)
  expect_identical(frame_rate(tr), 10)
  expect_identical(frame_rate(read_tracks(xy_df(), fps = 10)), 10)
  dur <- read_tracks(xy_df(frame = as.difftime(c(0, 10, 20), units = "secs")), fps = 10)
  expect_null(frame_rate(dur))
  expect_identical(as.data.frame(dur)$frame, c(0, 10, 20))
})

test_that("a mapped column that is not in the data is named", {
  for (role in c("x", "angle", "weight", "time")) {
    map <- stats::setNames(list("nope"), role)
    e <- tryCatch(suppressMessages(read_tracks(xy_df(), mapping = map)),
                  radiatR_missing_columns = function(e) e)
    expect_s3_class(e, "radiatR_missing_columns")
    expect_identical(e$columns, "nope")
    expect_match(conditionMessage(e), paste0(role, " = 'nope'"), fixed = TRUE)
  }
})

test_that("an unused text column named x does not block an angle-only load", {
  d <- data.frame(id = "a", frame = 1:3, angle = c(0.1, 0.2, 0.3), x = c("p", "q", "r"))
  expect_s4_class(read_tracks(d), "Tracks")
})

test_that("zero-row input still gives an empty Tracks", {
  tr <- suppressMessages(read_tracks(xy_df()[0, ]))
  expect_equal(nrow(as.data.frame(tr)), 0L)
})

test_that("a malformed cell beyond the reader's type-guessing sample is caught", {
  path <- withr::local_tempfile(fileext = ".csv")
  d <- data.frame(id = "a", frame = 1:1500, x = as.character(seq(0, 1, length.out = 1500)),
                  y = 0.5, stringsAsFactors = FALSE)
  d$x[1200] <- "oops"
  utils::write.csv(d, path, row.names = FALSE, quote = FALSE)
  for (mode in c("error", "drop")) {
    e <- malformed(read_tracks(path, on_invalid = mode))
    expect_s3_class(e, "radiatR_malformed_values")
    expect_identical(e$rows, 1200L)
    expect_identical(e$source, basename(path))
  }
})

test_that("decimal commas in a file are malformed, not read as digit grouping", {
  path <- withr::local_tempfile(fileext = ".tsv")
  writeLines(c("id\tframe\tx\ty", "a\t1\t1,5\t0,25", "a\t2\t853,333\t173,355"), path)
  e <- malformed(read_tracks(path))
  expect_identical(e$values, c("1,5", "853,333"))
  expect_match(conditionMessage(e), "read_opts = list(decimal = \",\")", fixed = TRUE)
  ok <- read_tracks(path, read_opts = list(decimal = ","))
  expect_equal(as.data.frame(ok)$x, c(1.5, 853.333))
})

test_that("one bad cell in a comma-decimal file names that cell only", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;frame;x;y", "a;1;1,5;0,25", "a;2;2,25;0,5", "a;3;oops;0,75"), path)
  e <- malformed(read_tracks(path))
  expect_identical(e$rows, 3L)
  expect_identical(e$values, "oops")
  expect_no_match(conditionMessage(e), "read_opts")

  writeLines(c("id;frame;x;y", "a;0,5;1,5;0,25", "a;x;2,25;0,5", "a;1,5;3,5;0,75"), path)
  e <- malformed(read_tracks(path))
  expect_identical(e$columns, "frame")
  expect_identical(e$rows, 2L)
})

test_that("a malformed cell in a multi-file import is reported against its own file", {
  dir <- withr::local_tempdir()
  utils::write.csv(xy_df(), file.path(dir, "a.csv"), row.names = FALSE)
  utils::write.csv(xy_df(id = "b", x = c("0.1", "bad", "0.3")), file.path(dir, "b.csv"),
                   row.names = FALSE)
  e <- malformed(read_tracks(file.path(dir, c("a.csv", "b.csv"))))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 2L)
  expect_identical(e$source, "b.csv")

  utils::write.csv(xy_df(id = "b", frame = c("0", "x", "2")), file.path(dir, "b.csv"),
                   row.names = FALSE)
  e <- malformed(read_tracks(file.path(dir, c("a.csv", "b.csv"))))
  expect_identical(e$columns, "frame")
  expect_identical(e$rows, 2L)
  expect_identical(e$source, "b.csv")
})

# ---- real exports ------------------------------------------------------------

test_that("idtracker.ai's 'nan' cells are missing values, by path and as text", {
  path <- real_fixture("idtrackerai_trajectories.csv")
  map <- list(time = "seconds", x = "x6", y = "y6")
  e <- tryCatch(suppressMessages(read_tracks(path, mapping = map)),
                radiatR_invalid_rows = function(e) e)
  expect_false(inherits(e, "radiatR_malformed_values"))
  expect_identical(e$rows, 1L)

  load <- function(x) as.data.frame(suppressMessages(suppressWarnings(
    read_tracks(x, mapping = map, on_invalid = "drop"), classes = "radiatR_dropped_rows")))
  by_path <- load(path)
  as_text <- load(utils::read.csv(path, colClasses = "character"))
  expect_equal(nrow(by_path), 10L)
  for (cc in c("seconds", "x6", "y6")) expect_equal(as_text[[cc]], by_path[[cc]])
})

test_that("TrackMate 7's extra header rows are reported as malformed, with their rows", {
  # The trackmate dialect does not strip these rows yet; until it does, they
  # must fail by name rather than as an unparseable-timestamp error.
  e <- malformed(read_tracks(real_fixture("trackmate_v7_spots.csv"), dialect = "trackmate"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 1:3)
  expect_identical(e$values, c("T", "T", "(sec)"))
})

# ---- empty guessed id column -------------------------------------------------

test_that("an empty guessed id column is ignored; SLEAP's example loads without a dialect", {
  path <- system.file("extdata", "sleap_example.csv", package = "radiatR")
  skip_if(!nzchar(path), "sleap_example.csv not found in extdata")
  msgs <- testthat::capture_messages(tr <- read_tracks(path))
  expect_match(msgs, "Id column 'track' is empty", fixed = TRUE, all = FALSE)
  expect_equal(nrow(as.data.frame(tr)), 1L)
})

test_that("blank ids count as empty, a partly filled or mapped id column does not", {
  blank <- suppressMessages(read_tracks(xy_df(id = c("", " ", NA))))
  expect_identical(unique(as.data.frame(blank)[["..id"]]), "1")
  expect_error(suppressMessages(read_tracks(xy_df(id = c("a", NA, "a")))),
               class = "radiatR_invalid_tracks")
  expect_error(suppressMessages(read_tracks(xy_df(id = c(NA, NA, NA)),
                                            mapping = list(id = "id"))),
               class = "radiatR_invalid_tracks")
})

test_that("the next id-like column stands in for an empty one", {
  d <- xy_df(); names(d)[1] <- "track"; d$track <- NA
  d$animal <- c("p", "p", "q")
  tr <- suppressMessages(read_tracks(d))
  expect_identical(as.data.frame(tr)$animal, c("p", "p", "q"))
  expect_false("..id" %in% names(as.data.frame(tr)))
})

# ---- validate ----------------------------------------------------------------

test_that("validate is deprecated and invariants are checked regardless", {
  expect_warning(tr <- read_tracks(xy_df(), validate = FALSE), class = "deprecatedWarning")
  expect_s4_class(tr, "Tracks")
  expect_no_warning(read_tracks(xy_df(), validate = TRUE))
  expect_error(suppressWarnings(read_tracks(xy_df(frame = c(1, 1, 2)), validate = FALSE)),
               class = "radiatR_invalid_tracks")
})
