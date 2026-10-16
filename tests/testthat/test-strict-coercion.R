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

test_that("a decimal comma earns a hint; nothing else does", {
  e <- malformed(radiatR:::.as_numeric_strict(c("1,5", "2,25"), "x", source = "a.tsv"))
  expect_match(conditionMessage(e), "read_opts = list(decimal = \",\")", fixed = TRUE)
  e <- malformed(radiatR:::.as_numeric_strict(c("oops"), "x", source = "a.tsv"))
  expect_no_match(conditionMessage(e), "decimal")
  # a data frame takes read_opts$decimal too
  e <- malformed(radiatR:::.as_numeric_strict(c("1,5", "2,25"), "x"))
  expect_match(conditionMessage(e), "read_opts = list(decimal = \",\")", fixed = TRUE)
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

# ---- time zone of file timestamps --------------------------------------------

# Two files with the same wall-clock times: readr types the first layout
# itself, the second stays text and is parsed by .coerce_time().
tz_files <- function(dir) {
  rows <- function(id, day) sprintf("%s,%s 10:00:0%d,0.%d,0.2", id, day, 1:2, 1:2)
  writeLines(c("id,time,x,y", rows("a", "2024-01-01")), file.path(dir, "a.csv"))
  writeLines(c("id,time,x,y", rows("b", "2024/01/01")), file.path(dir, "b.csv"))
  file.path(dir, c("a.csv", "b.csv"))
}
ny_epoch <- as.numeric(as.POSIXct("2024-01-01 10:00:01", tz = "America/New_York")) + 0:1

test_that("tz applies to a file's timestamps whichever layout they use", {
  f <- tz_files(withr::local_tempdir())
  for (p in f)
    expect_identical(as.numeric(read_tracks(p, tz = "America/New_York")@data$time),
                     ny_epoch, info = basename(p))
})

test_that("the files of one import are read in the same tz", {
  f <- tz_files(withr::local_tempdir())
  got <- read_tracks(f, tz = "America/New_York")
  expect_identical(as.numeric(got@data$time), rep(ny_epoch, 2))
})

test_that("a timestamp marked Z keeps its instant under another tz", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id,time,x,y", "a,2024-01-01T10:00:01Z,0.1,0.2",
               "a,2024-01-01T10:00:02Z,0.2,0.2"), path)
  utc <- as.numeric(as.POSIXct("2024-01-01 10:00:01", tz = "UTC")) + 0:1
  expect_identical(as.numeric(read_tracks(path, tz = "America/New_York")@data$time), utc)
})

test_that("tz is honoured by the fallback readers too", {
  f <- tz_files(withr::local_tempdir())
  for (hide in list("readr", c("readr", "data.table"))) {
    testthat::local_mocked_bindings(
      .is_installed = function(pkg) !pkg %in% hide && requireNamespace(pkg, quietly = TRUE))
    got <- read_tracks(f, tz = "America/New_York")
    expect_identical(as.numeric(got@data$time), rep(ny_epoch, 2),
                     info = paste("without", paste(hide, collapse = ", ")))
  }
})

test_that("a timestamp that names its zone is that instant under every reader and tz", {
  dir <- withr::local_tempdir()
  z <- file.path(dir, "z.csv")
  writeBin(charToRaw(paste0("id,time,x,y\na,2024-01-01T10:00:00Z,.1,.1\n",
                            "a,2024-01-01T11:00:00.5Z,.2,.2\n")), z)
  off <- file.path(dir, "off.csv")
  writeBin(charToRaw(paste0("id,time,x,y\na,2024-01-01T10:00:00+02:00,.1,.1\n",
                            "a,2024-01-01T11:00:00-0130,.2,.2\n",
                            "a,2024-01-01T12:00:00+02,.3,.3\n")), off)
  want_z   <- as.numeric(as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 11:00:00.5"),
                                    tz = "UTC"))
  # rows come back in time order: 08:00, 10:00, 12:30 UTC
  want_off <- as.numeric(as.POSIXct(c("2024-01-01 08:00:00", "2024-01-01 10:00:00",
                                      "2024-01-01 12:30:00"), tz = "UTC"))
  for (hide in list(character(0), "readr", c("readr", "data.table"))) {
    testthat::local_mocked_bindings(
      .is_installed = function(pkg) !pkg %in% hide && requireNamespace(pkg, quietly = TRUE))
    for (tz in c("UTC", "America/New_York")) {
      info <- paste("tz", tz, "without", paste(hide, collapse = ", "))
      got <- read_tracks(z, tz = tz)@data$time
      expect_equal(as.numeric(got), want_z, tolerance = 1e-6, info = info)
      expect_identical(attr(got, "tzone"), tz, info = info)
      expect_equal(as.numeric(read_tracks(off, tz = tz)@data$time), want_off,
                   tolerance = 1e-6, info = info)
    }
  }
})

test_that("text time reads a zone designator and refuses a column that mixes zoned and unzoned", {
  t1 <- radiatR:::.coerce_time(c("2024-01-01T10:00:00+05:30", "2024-01-01T10:00:00Z"),
                               tz = "Europe/Paris")
  expect_equal(as.numeric(t1),
               as.numeric(as.POSIXct(c("2024-01-01 04:30:00", "2024-01-01 10:00:00"),
                                     tz = "UTC")))
  # a bare date beside zoned times is midnight in tz, as beside Z
  t2 <- radiatR:::.coerce_time(c("2024-01-01", "2024-01-01T10:00:00+01:00"),
                               tz = "America/New_York")
  expect_equal(as.numeric(t2[1]),
               as.numeric(as.POSIXct("2024-01-01", tz = "America/New_York")))
  e <- malformed(radiatR:::.coerce_time(
    c("2024-01-01 10:00:00", "2024-01-01 11:00:00", "2024-01-01T12:00:00+01:00"),
    tz = "UTC", column = "time"))
  expect_identical(e$rows, 3L)
  # in one layout the zone mix is named, the minority kind reported
  e <- malformed(radiatR:::.coerce_time(
    c("2024-01-01T10:00:00", "2024-01-01T11:00:00", "2024-01-01T12:00:00+01:00"),
    tz = "UTC", column = "time"))
  expect_identical(e$rows, 3L)
  expect_identical(e$values, "2024-01-01T12:00:00+01:00")
  expect_match(conditionMessage(e), "date-times that all carry a zone, or none", fixed = TRUE)
  e <- malformed(radiatR:::.coerce_time(
    c("2024-01-01T10:00:00Z", "2024-01-01T11:00:00Z", "2024-01-01T12:00:00"),
    tz = "UTC", column = "time"))
  expect_identical(e$values, "2024-01-01T12:00:00")
  expect_match(conditionMessage(e), "date-times that all carry a zone, or none", fixed = TRUE)
  # an offset outside +-14:00 is no offset
  expect_error(radiatR:::.coerce_time(c("2024-01-01T10:00:00+15:00"), tz = "UTC",
                                      column = "time"),
               class = "radiatR_malformed_values")
})

# ---- a dot where the decimal mark is a comma ---------------------------------

test_that("a dot is not a second decimal mark in comma-decimal text", {
  e <- malformed(radiatR:::.as_numeric_strict(c("1,5", "1.234", "2,5"), "x", decimal = ","))
  expect_identical(e$rows, 2L)
  expect_identical(e$values, "1.234")
})

test_that("a dot in a comma-decimal file is malformed and earns a hint", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;frame;x;y", "a;1;1,5;0,25", "a;2;1.234;0,5", "a;3;2,5;0,75"), path)
  e <- malformed(read_tracks(path))
  expect_identical(e$columns, "x")
  expect_identical(e$rows, 2L)
  expect_identical(e$values, "1.234")
  expect_match(conditionMessage(e), "read_opts = list(decimal = \".\")", fixed = TRUE)
})

test_that("a semicolon file that shows decimal points is read with them", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;frame;x;y", "a;1;1.5;0.25", "a;2;2.25;0.5", "a;3;3.125;0.75"), path)
  expect_equal(as.data.frame(read_tracks(path))$x, c(1.5, 2.25, 3.125))
  # through a dialect too, which sniffs it the same way
  writeLines(c("TRACK_ID;FRAME;POSITION_X;POSITION_Y", "0;0;1.5;0.25", "0;1;2.25;0.5"), path)
  tm <- suppressMessages(read_tracks(path, dialect = "trackmate"))
  expect_equal(as.data.frame(tm)$x, c(1.5, 2.25))
})

test_that("dots that could be digit grouping are no evidence of decimal points", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;frame;x;y", "a;1;1.234;5.125", "a;2;2.345;6.250", "a;3;3.456;7.375"), path)
  e <- malformed(read_tracks(path))
  expect_identical(e$rows, 1:3)
  expect_match(conditionMessage(e), "read_opts = list(decimal = \".\")", fixed = TRUE)
  ok <- read_tracks(path, read_opts = list(decimal = "."))
  expect_equal(as.data.frame(ok)$x, c(1.234, 2.345, 3.456))
  # one comma-decimal value among the first rows settles it the other way
  writeLines(c("id;frame;x;y", "a;1;1.5;0,25", "a;2;2.25;0,5"), path)
  expect_identical(malformed(read_tracks(path))$columns, "x")
})

test_that("scientific notation with a dot earns the hint too", {
  e <- malformed(radiatR:::.as_numeric_strict(c("1,5", "1.5e3"), "x", source = "a.csv",
                                              decimal = ","))
  expect_identical(e$values, "1.5e3")
  expect_match(conditionMessage(e), "read_opts = list(decimal = \".\")", fixed = TRUE)
})

test_that("dotted numeric time in a comma-decimal file is malformed too", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;frame;x;y", "a;0.5;1,5;0,25", "a;1.5;2,25;0,5"), path)
  e <- malformed(read_tracks(path))
  expect_identical(e$columns, "frame")
  expect_match(conditionMessage(e), "read_opts = list(decimal = \".\")", fixed = TRUE)
})

# ---- smaller findings --------------------------------------------------------

test_that("read_tracks returns its Tracks visibly", {
  expect_visible(read_tracks(xy_df()))
})

test_that("reader attributes do not leak into the Tracks data", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(xy_df(), path, row.names = FALSE)
  expect_null(attr(read_tracks(path)@data, "decimal"))

  skip_if_not_installed("tibble")
  register_loader_dialect("zz_tibble", overwrite = TRUE, fun = function(x, ...)
    structure(tibble::as_tibble(x), time_unit = "seconds", decimal = ","))
  withr::defer(rm(list = "zz_tibble", envir = radiatR:::.loader_registry))
  tr <- read_tracks(xy_df(), dialect = "zz_tibble")
  expect_identical(tr@meta$time_unit, "seconds")
  expect_null(attr(tr@data, "time_unit"))
  expect_null(attr(tr@data, "decimal"))
})

test_that("bare dates count toward the date-time layout they sit beside", {
  got <- radiatR:::.coerce_time(c("2024-01-01", "2024-01-02", "2024-01-02 12:00:00"))
  expect_identical(format(got, "%Y-%m-%d %H:%M:%S"),
                   c("2024-01-01 00:00:00", "2024-01-02 00:00:00", "2024-01-02 12:00:00"))
  # another separator is still another layout
  e <- malformed(radiatR:::.coerce_time(c("2024/01/01", "2024/01/02", "2024-01-02 12:00:00")))
  expect_identical(e$rows, 3L)
})

test_that("a bare date is midnight in tz, also beside timestamps marked Z", {
  got <- radiatR:::.coerce_time(c("2024-01-01", "2024-01-02T12:00:00Z"),
                                tz = "America/New_York")
  expect_identical(as.numeric(got), as.numeric(c(
    as.POSIXct("2024-01-01", tz = "America/New_York"),
    as.POSIXct("2024-01-02 12:00:00", tz = "UTC"))))
})

xy_cols <- function(tr) unlist(tr@cols[c("x", "y")], use.names = FALSE)

test_that("a regex_mapping resolves columns by pattern", {
  d <- stats::setNames(xy_df(), c("id", "frame", "pos_h", "pos_v"))
  spec <- list(regex_mapping = list(x = "_h$", y = "_v$"))
  expect_identical(xy_cols(read_tracks_format(d, spec)), c("pos_h", "pos_v"))
  expect_identical(xy_cols(read_tracks(d, format = spec)), c("pos_h", "pos_v"))
})

test_that("read_tracks(format = ) keeps the format's mapping over guessed columns", {
  d <- cbind(xy_df(), pos_h = c(0.5, 0.6, 0.7), pos_v = c(0.5, 0.6, 0.7))
  spec <- list(mapping = list(x = "pos_h", y = "pos_v"))
  expect_identical(xy_cols(read_tracks(d, format = spec)), c("pos_h", "pos_v"))
  # an explicit mapping still overrides the format's
  got <- read_tracks(d, format = spec, mapping = list(x = "x", y = "y"))
  expect_identical(xy_cols(got), c("x", "y"))
  expect_identical(xy_cols(read_tracks(d, format = spec, mapping = NULL)), c("pos_h", "pos_v"))
})

test_that("the caller's mapping spares a role its regex_mapping pattern", {
  d <- cbind(xy_df(), pos_v = c(0.5, 0.6, 0.7))
  spec <- list(regex_mapping = list(x = "_nope$", y = "_v$", angle = character(0)))
  expect_identical(xy_cols(read_tracks_format(d, spec, mapping = list(x = "x"))),
                   c("x", "pos_v"))
  expect_identical(xy_cols(read_tracks(d, format = spec, mapping = list(x = "x"))),
                   c("x", "pos_v"))
})

test_that("read_tracks(format = ) keeps the format's other fields", {
  d <- data.frame(id = "a", frame = 1:3, theta = c(90, 180, 270))
  spec <- list(angle = list(unit = "degrees"), time = list(fps = 25))
  tr <- read_tracks(d, format = spec)
  expect_equal(tr@data$theta, c(90, 180, 270) * pi / 180)
  expect_equal(frame_rate(tr), 25)
  # what the caller supplies still overrides the format
  expect_equal(frame_rate(read_tracks(d, format = spec, fps = 50)), 50)
  # a format with no angle unit leaves read_tracks()'s default, not a guess
  expect_identical(suppressWarnings(read_tracks(d, format = list()))@angle_unit, "radians")

  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id;frame;x;y", "a;1;1.234;5.125", "a;2;2.345;6.250"), path)
  ok <- read_tracks(path, format = list(), read_opts = list(decimal = "."))
  expect_equal(as.data.frame(ok)$x, c(1.234, 2.345))
})

test_that("a regex_mapping pattern that matches no column is named", {
  spec <- list(regex_mapping = list(x = "^x$", y = "^y$", angle = "theta"))
  e <- tryCatch(read_tracks_format(xy_df(), spec), radiatR_missing_columns = function(e) e)
  expect_s3_class(e, "radiatR_missing_columns")
  expect_match(conditionMessage(e), "angle = 'theta'", fixed = TRUE)
})

test_that("Excel date-times, which carry no zone, are read in tz", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  d <- xy_df(frame = as.POSIXct("2024-01-01 10:00:01", tz = "UTC") + c(0, 0.1, 3.001))
  writexl::write_xlsx(d, path)
  # the same wall-clock times, five hours later; epoch seconds resolve ~2e-7 s
  got <- as.numeric(read_tracks(path, tz = "America/New_York")@data$frame)
  expect_lt(max(abs(got - as.numeric(d$frame) - 5 * 3600)), 5e-7)
  expect_identical(as.numeric(read_tracks(path)@data$frame), as.numeric(d$frame))
})

test_that("a wall-clock time that does not exist in tz is not moved an hour on", {
  # 02:30 on 2024-03-10 was skipped in New York
  s <- sprintf("2024-03-10 0%d:30:00", 1:3)
  e <- malformed(radiatR:::.coerce_time(s, tz = "America/New_York"))
  expect_identical(e$rows, 2L)
  expect_s3_class(radiatR:::.coerce_time(s, tz = "UTC"), "POSIXct")

  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(t = as.POSIXct(s, tz = "UTC")), path)
  got <- radiatR:::.read_any(path, tz = "America/New_York")
  expect_identical(is.na(got$t), c(FALSE, TRUE, FALSE))
  expect_identical(attr(got, "tz_gaps")$t$rows, 2L)
  expect_null(attr(radiatR:::.read_any(path), "tz_gaps"))

  # as the time column it stops the load, naming the value and the cause
  writexl::write_xlsx(xy_df(frame = as.POSIXct(s, tz = "UTC")), path)
  e <- malformed(read_tracks(path, tz = "America/New_York"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "frame")
  expect_identical(e$rows, 2L)
  expect_identical(e$values, "2024-03-10 02:30:00")
  expect_match(conditionMessage(e), "America/New_York", fixed = TRUE)
  expect_s3_class(read_tracks(path)@data$frame, "POSIXct")
})

test_that("a kept Excel date-time column is not blanked by a time that tz skips", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  stamp <- as.POSIXct(sprintf("2024-03-10 0%d:30:00", 1:3), tz = "UTC")
  writexl::write_xlsx(xy_df(stamp = stamp), path)
  e <- malformed(read_tracks(path, keep = "stamp", tz = "America/New_York"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "stamp")
  expect_identical(e$rows, 2L)
  expect_identical(e$source, basename(path))
  # a column that is not used does not stop the load
  expect_identical(nrow(read_tracks(path, tz = "America/New_York")@data), 3L)
  # nor does it in one file of several
  other <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(xy_df(id = "b", stamp = stamp + 86400), other)
  expect_identical(nrow(read_tracks(c(path, other), tz = "America/New_York")@data), 6L)
  e <- malformed(read_tracks(c(other, path), keep = "stamp", tz = "America/New_York"))
  expect_identical(e$source, basename(path))
  # files of one name are told apart: the skipped time is in the second
  dirs <- file.path(withr::local_tempdir(), c("d1", "d2"))
  for (d in dirs) dir.create(d)
  same <- file.path(dirs, "a.xlsx")
  writexl::write_xlsx(xy_df(id = "b", stamp = stamp + 86400), same[1])
  writexl::write_xlsx(xy_df(stamp = stamp), same[2])
  e <- malformed(read_tracks(same, keep = "stamp", tz = "America/New_York"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 2L)
})

test_that("a time that tz skips, read on the text path, is named and not blanked", {
  skip_if_not_installed("readr")
  dir <- withr::local_tempdir()
  stamp <- sprintf("2024-03-10 0%d:30:00", 1:3)
  path <- file.path(dir, "kept.csv")
  utils::write.csv(xy_df(stamp = stamp), path, row.names = FALSE)
  ny <- function(...) suppressWarnings(read_tracks(..., tz = "America/New_York"))
  e <- malformed(ny(path, keep = "stamp"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "stamp")
  expect_identical(e$rows, 2L)
  expect_identical(e$values, "2024-03-10 02:30:00")
  expect_identical(e$source, "kept.csv")
  expect_identical(nrow(ny(path)@data), 3L)
  expect_s3_class(read_tracks(path, keep = "stamp")@data$stamp, "POSIXct")
  # as the time column
  timed <- file.path(dir, "timed.csv")
  utils::write.csv(xy_df(frame = stamp), timed, row.names = FALSE)
  e <- malformed(ny(timed))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$values, "2024-03-10 02:30:00")
})

# ---- ids are labels ------------------------------------------------------------

id_file <- function(dir, a, b, sep = ",") {
  path <- tempfile(tmpdir = dir, fileext = ".csv")
  writeLines(c(paste("id", "frame", "x", "y", sep = sep),
               paste(a, 1:2, c(10, 11), c(20, 21), sep = sep),
               paste(b, 3:4, c(30, 31), c(40, 41), sep = sep)), path)
  path
}
# readr, then data.table::fread(), then utils::read.table(); the fread() pass
# needs data.table, or it would be a second read.table() pass under its name
readers <- c(list(character(0)),
             if (requireNamespace("data.table", quietly = TRUE)) list("readr"),
             list(c("readr", "data.table")))
hide_readers <- function(hide, env = parent.frame())
  testthat::local_mocked_bindings(
    .is_installed = function(pkg) !pkg %in% hide && requireNamespace(pkg, quietly = TRUE),
    .env = env)

# ---- one date-time parser for every reader -------------------------------------

test_that("every reader gives the same instants: one parser decides", {
  dir <- withr::local_tempdir()
  mixed <- file.path(dir, "mixed.csv")     # zoned beside unzoned: malformed on the text path
  writeBin(charToRaw(paste0("id,time,x,y\na,2024-01-01 10:00:00,.1,.1\n",
                            "a,2024-01-01 11:00:00,.2,.2\na,2024-01-01T12:00:00Z,.3,.3\n")), mixed)
  layouts <- list(c("2024-01-01T10:00", "2024-01-01T10:01"),
                  c("20240101T100000", "20240101T100100"),
                  c("2024-01-01T1000", "2024-01-01T1001"),
                  # ISO 8601 basic, zoned: the zone says the instant
                  c("20240101T100000Z", "20240101T100100Z"),
                  c("20240101T120000+0200", "20240101T120100+0200"),
                  c("2024-01-01T1000Z", "2024-01-01T1001Z"),
                  c("2024-01-01T1200+02:00", "2024-01-01T1201+02:00"))
  for (v in layouts) {
    p <- file.path(dir, "lay.csv")
    writeBin(charToRaw(sprintf("id,time,x,y\na,%s,.1,.1\na,%s,.2,.2\n", v[1], v[2])), p)
    for (hide in readers) {
      hide_readers(hide)
      expect_equal(as.numeric(read_tracks(p)@data$time),
                   as.numeric(as.POSIXct(c("2024-01-01 10:00", "2024-01-01 10:01"), tz = "UTC")),
                   info = paste(v[1], "without", paste(hide, collapse = " ")))
    }
  }
  for (hide in readers) {
    hide_readers(hide)
    expect_error(read_tracks(mixed), class = "radiatR_malformed_values")
  }
  hours <- file.path(dir, "hours.csv")      # hours only: refused, named
  writeBin(charToRaw("id,time,x,y\na,2024-01-01 10,.1,.1\na,2024-01-01 11,.2,.2\n"), hours)
  for (hide in readers) {
    hide_readers(hide)
    expect_error(read_tracks(hours), class = "radiatR_malformed_values")
  }
})

test_that("a kept timestamp column is POSIXct in tz under every reader; an unused one is not read", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "kept.csv")
  writeBin(charToRaw(paste0("id,time,x,y,stamp,slash,junk\n",
                            "a,1,.1,.1,2024-01-01 10:00:00,2024/01/01 10:00:00,2024-01-01 10:00:00\n",
                            "a,2,.2,.2,2024-01-01 11:00:00,2024/01/01 11:00:00,2024-02-30 99:00:00\n")), p)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    ts <- read_tracks(p, tz = "America/New_York", keep = c("stamp", "slash"))
    for (cc in c("stamp", "slash")) {
      expect_s3_class(ts@data[[cc]], "POSIXct")
      expect_identical(attr(ts@data[[cc]], "tzone"), "America/New_York", info = info)
      expect_equal(as.numeric(ts@data[[cc]][1]),
                   as.numeric(as.POSIXct("2024-01-01 10:00:00", tz = "America/New_York")),
                   info = paste(info, cc))
    }
  }
})

test_that("a kept column not all in one date-time layout is kept as read, under every reader", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "loose.csv")
  writeBin(charToRaw(paste0("id,time,x,y,hours,mixed\n",
                            "a,1,.1,.1,2024-01-01 10,2024-01-01 10:00:00\n",
                            "a,2,.2,.2,2024-01-01 11,2024-01-01T12:00:00Z\n")), p)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    ts <- read_tracks(p, keep = c("hours", "mixed"))
    expect_identical(ts@data$hours, c("2024-01-01 10", "2024-01-01 11"), info = info)
    expect_identical(ts@data$mixed, c("2024-01-01 10:00:00", "2024-01-01T12:00:00Z"), info = info)
  }
})

test_that("files whose date-time columns are typed differently still bind", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.csv"); b <- file.path(dir, "b.csv")
  writeBin(charToRaw("id,time,x,y,day\na,1,.1,.1,2024-01-01\na,2,.2,.2,2024-01-02\n"), a)
  writeBin(charToRaw("id,time,x,y,day\nb,1,.1,.1,2024-01-01 10:00:00\nb,2,.2,.2,2024-01-01 11:00:00\n"), b)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    expect_no_error(read_tracks(c(a, b)))                      # `day` unused
    ts <- read_tracks(c(a, b), keep = "day")                   # `day` kept: POSIXct
    expect_s3_class(ts@data$day, "POSIXct")
  }
  skip_if_not_installed("readxl"); skip_if_not_installed("writexl")
  x <- file.path(dir, "c.xlsx")
  writexl::write_xlsx(data.frame(id = "c", time = 1:2, x = c(.1, .2), y = c(.1, .2),
                                 day = as.POSIXct(c("2024-01-01 10:00", "2024-01-01 11:00"),
                                                  tz = "UTC")), x)
  for (hide in readers) {
    hide_readers(hide)
    expect_no_error(read_tracks(c(b, x)))
    expect_s3_class(read_tracks(c(b, x), keep = "day")@data$day, "POSIXct")
  }
})

test_that("ids that differ as text are not merged by reading them as numbers", {
  dir <- withr::local_tempdir()
  cases <- list(c("1.1", "1.10", ";"), c("1.1", "1.10", ","), c("1", "1.0", ","),
                c("1e1", "10", ","), c("01", "1", ","), c("1.1", "1.10", "\t"),
                c("T", "TRUE", ","))
  for (hide in readers) {
    hide_readers(hide)
    for (k in cases) {
      # two rows per id, in whatever order the ids sort
      got <- read_tracks(id_file(dir, k[1], k[2], k[3]))@data$id
      expect_identical(sort(got), sort(rep(k[1:2], each = 2L)),
                       info = paste(c(k, "without", hide), collapse = " "))
    }
  }
})

test_that("ids that read as distinct numbers are numbers under every reader", {
  dir <- withr::local_tempdir()
  for (hide in readers) {
    hide_readers(hide)
    # as the reader typed them: integer or double by reader
    for (k in list(c("1", "2"), c("1.0", "2.0"), c("1.5", "10"))) {
      got <- read_tracks(id_file(dir, k[1], k[2]))@data$id
      expect_true(is.numeric(got), info = paste(c(k, "without", hide), collapse = " "))
      expect_equal(got, rep(as.numeric(k), each = 2L),
                   info = paste(c(k, "without", hide), collapse = " "))
    }
    # an id-like column that is not the id is read the same way
    path <- file.path(dir, "subject.csv")
    writeLines(c("id,frame,x,y,subject", "a,1,1,2,1.1", "a,2,2,3,1.10"), path)
    expect_identical(read_tracks(path, keep = "subject")@data$subject, c("1.1", "1.10"))
  }
})

test_that("a mapped id column is read as labels too", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "who.csv")
  writeLines(c("who,frame,x,y", "1.1,1,1,2", "1.1,2,2,3", "1.10,3,3,4", "1.10,4,4,5"), path)
  for (hide in readers) {
    hide_readers(hide)
    expect_identical(read_tracks(path, mapping = list(id = "who"))@data$who,
                     rep(c("1.1", "1.10"), each = 2L))
  }
})

test_that("files that disagree on the id type are combined as text ids", {
  dir <- withr::local_tempdir()
  files <- c(id_file(dir, "1.1", "1.10"), id_file(dir, "7", "8"))
  # the ids of a file typed as numbers are taken from its text, not from the
  # numbers: 1.10 is not 1.1, nor 300000 3e+05
  other <- c(id_file(dir, "1.1", "x"), id_file(dir, "1.10", "300000"))
  for (hide in readers) {
    hide_readers(hide)
    expect_identical(unique(read_tracks(files)@data$id), c("1.1", "1.10", "7", "8"))
    expect_identical(sort(unique(read_tracks(other)@data$id)),
                     sort(c("1.1", "x", "1.10", "300000")))
    # whole numbers in one file and fractions in another are numbers in both
    got <- read_tracks(c(id_file(dir, "1", "2"), id_file(dir, "1.5", "2.5")))@data$id
    expect_equal(sort(unique(got)), c(1, 1.5, 2, 2.5))
  }
})

test_that("ids written differently in two files are two ids, not a collision", {
  dir <- withr::local_tempdir()
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    # 1.1 and 1.10 are one number but two labels
    got <- read_tracks(c(id_file(dir, "1.1", "2"), id_file(dir, "1.10", "3")))@data$id
    expect_identical(sort(unique(got)), c("1.1", "1.10", "2", "3"), info = info)
    # the same label in both files still collides
    expect_error(read_tracks(c(id_file(dir, "1", "2"), id_file(dir, "1", "3"))),
                 "collide", info = info)
    got <- read_tracks(c(id_file(dir, "1", "2"), id_file(dir, "1", "3")),
                       id_collision = "namespace")@data$id
    expect_length(unique(got), 4L)
  }
})

test_that("an id typed as a date or time keeps the labels it was read from", {
  dir <- withr::local_tempdir()
  path <- id_file(dir, "2024-01-01 00:00", "2024-01-01 00:00:00")
  for (hide in readers) {
    hide_readers(hide)
    expect_identical(sort(unique(as.character(read_tracks(path)@data$id))),
                     c("2024-01-01 00:00", "2024-01-01 00:00:00"),
                     info = paste(c("without", hide), collapse = " "))
  }
  # distinct dates are left as the reader typed them
  got <- read_tracks(id_file(dir, "2024-01-01", "2024-01-02"))@data$id
  expect_identical(unique(as.character(got)), c("2024-01-01", "2024-01-02"))
})

test_that("an id type the files disagree on is read as text, or stops the load", {
  dir <- withr::local_tempdir()
  csv <- id_file(dir, "a", "b")
  # a dialect's tables cannot be read again for the text of their ids
  register_loader_dialect("ids_as_read", function(path) utils::read.csv(path),
                          overwrite = TRUE)
  withr::defer(rm("ids_as_read", envir = radiatR:::.loader_registry))
  e <- expect_error(read_tracks(c(id_file(dir, "1", "2"), csv), dialect = "ids_as_read"),
                    class = "radiatR_id_type_mismatch")
  expect_identical(e$types, c("number", "text"))
  # an Excel file's ids are read as the text of its cells
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  xl <- file.path(dir, "num.xlsx")
  writexl::write_xlsx(data.frame(id = c(1.5, 1.5, 1e5, 1e5), frame = 1:4, x = 1:4, y = 5:8), xl)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    expect_identical(unique(read_tracks(c(xl, csv))@data$id), c("1.5", "100000", "a", "b"),
                     info = info)
    # the same id in an Excel file and a CSV still collides
    expect_error(read_tracks(c(xl, id_file(dir, "100000", "9"))), "collide", info = info)
  }
})

test_that("ids that differ only by surrounding whitespace stop the read", {
  dir <- withr::local_tempdir()
  quoted <- id_file(dir, "\" 1\"", "1")
  bare <- id_file(dir, " a", "a")
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    for (p in c(quoted, bare)) {
      e <- expect_error(read_tracks(p), class = "radiatR_ambiguous_ids", info = info)
      expect_setequal(trimws(e$ids), c("1", "a")[c(p == quoted, p == bare)])
    }
  }
  # a data frame too
  expect_error(read_tracks(data.frame(id = c("x ", "x"), frame = 1:2, x = 1:2, y = 1:2)),
               class = "radiatR_ambiguous_ids")
  # one id with whitespace and no twin is that id
  expect_identical(unique(read_tracks(data.frame(id = c(" x", " x"), frame = 1:2, x = 1:2,
                                                 y = 1:2))@data$id), " x")
})

test_that("padded numeric ids with no twin get the same labels under every reader", {
  dir <- withr::local_tempdir()
  pad <- file.path(dir, "pad.csv")
  writeBin(charToRaw(paste0("id,time,x,y\n 1,1,.1,.1\n 1,2,.2,.2\n",
                            " 10,1,.1,.1\n 10,2,.2,.3\n")), pad)
  merge <- file.path(dir, "merge.csv")
  writeBin(charToRaw(paste0("id,time,x,y\n 1.1,1,.1,.1\n 1.1,2,.2,.2\n",
                            " 1.10,1,.1,.1\n 1.10,2,.2,.3\n")), merge)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    expect_identical(sort(unique(as.character(read_tracks(pad)@data$id))),
                     c("1", "10"), info = info)
    expect_length(unique(read_tracks(merge)@data$id), 2L)
  }
})

# ---- every reader reads the same table ------------------------------------------

test_that("a row with more or fewer fields than the header stops every reader", {
  dir <- withr::local_tempdir()
  w <- function(name, lines) { p <- file.path(dir, name); writeLines(lines, p); p }
  hdr <- "id,frame,x,y"
  ok <- sprintf("1,%d,%d,2", 1:7, 1:7)
  cases <- list(
    short = w("short.csv", c(hdr, ok[1:2], "1,9,3", ok[3])),
    long = w("long.csv", c(hdr, ok[1:2], "1,9,3,4,5", ok[3])),
    # read.table() sizes rows from the first five lines and wraps a later long row
    late = w("late.csv", c(hdr, ok, "1,9,3,4,5,6,7,8", "1,10,1,2")),
    rownames = w("rownames.csv", c(hdr, sprintf("r%d,1,%d,%d,2", 1:3, 1:3, 1:3))),
    # fread() shifted the columns of this one
    trailing = w("trailing.csv", c(hdr, paste0(ok[1:3], ","))),
    # and filled this one
    widehead = w("widehead.csv", c(paste0(hdr, ",z"), ok[1:3])),
    # and skipped this first line, which the other readers take as the header
    preamble = w("preamble.csv", c("# exported by a tracker", hdr, ok[1:3])),
    # readr dropped a last line cut off mid-row, without a warning
    cut = w("cut.csv", c(hdr, ok[1:3], "1,9,3")),
    # a quote that is never closed
    quote = w("quote.csv", c(hdr, ok[1:2], "\"1,9,3,4", ok[3])))
  for (hide in readers) {
    hide_readers(hide)
    for (k in names(cases)) {
      e <- expect_error(suppressWarnings(read_tracks(cases[[k]])),
                        class = "radiatR_malformed_file",
                        info = paste(c(k, "without", hide), collapse = " "))
      expect_identical(e$problem, "ragged")
      expect_identical(grepl("row names", conditionMessage(e)), k %in% c("rownames", "trailing"),
                       info = paste(c(k, "without", hide), collapse = " "))
      # the line in the file, header = 1, whichever reader found it
      if (k %in% c("short", "long", "late", "cut"))
        expect_identical(e$lines, c(short = 4L, long = 4L, late = 9L, cut = 5L)[[k]],
                         info = paste(c(k, "without", hide), collapse = " "))
    }
  }
})

test_that("quoted line breaks and latin-1 bytes are no ragged rows", {
  dir <- withr::local_tempdir()
  qhdr <- file.path(dir, "qhdr.csv")
  lines <- c("id,\"fr", "ame\",x,y,note", "a,1,1,2,\"line", "break\"", "a,2,2,3,z",
             "b,3,3,4,w")
  writeLines(lines, qhdr)
  # readr and fread() kept the "\r\n" of a quoted line break, read.table() "\n"
  qcrlf <- file.path(dir, "qcrlf.csv")
  writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), qcrlf)
  latin <- file.path(dir, "latin.csv")
  # past the first 20 lines, which the delimiter sniffer reads as UTF-8
  writeBin(c(charToRaw(paste0("id,frame,x,y,note\n",
                              paste0(sprintf("a,%d,%d,2,b\n", 1:30, 1:30), collapse = ""),
                              "a,31,31,2,")),
             as.raw(0xe9), charToRaw("\n")), latin)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    for (p in c(qhdr, qcrlf)) {
      tr <- read_tracks(p, mapping = list(time = "fr\name"), keep = "note")
      expect_identical(tr@data$note, c("line\nbreak", "z", "w"),
                       info = paste(info, basename(p)))
    }
    expect_identical(nrow(read_tracks(latin)@data), 31L, info = info)
  }
})

test_that("a latin-1 file reads as its text under every reader", {
  dir <- withr::local_tempdir()
  latin1 <- function(name, text, open = file) {
    p <- file.path(dir, name)
    con <- open(p, "wb")
    writeBin(iconv(text, "UTF-8", "latin1", toRaw = TRUE)[[1]], con)
    close(con)
    p
  }
  text <- paste0("id,frame,x (\u00b5m),y,note\nren\u00e9,1,1,2,caf\u00e9\n",
                 "ren\u00e9,2,2,3,b\nb,1,3,4,c\n")
  files <- c(latin1("latin.csv", text), latin1("latin.csv.gz", text, gzfile))
  # one latin-1 byte makes the whole file latin-1, as .read_text_lines() reads
  # a dialect's file: the UTF-8 header of this one reads as latin-1 too
  mixed <- file.path(dir, "mixed.csv")
  writeBin(c(charToRaw(enc2utf8("id,frame,x (\u00b5m),y,note\na,1,1,2,")),
             as.raw(0xe9), charToRaw("\n")), mixed)
  # a UTF-8 file with the same characters is not taken for latin-1
  utf8 <- file.path(dir, "utf8.csv")
  writeBin(charToRaw(enc2utf8(text)), utf8)
  files <- c(files, utf8)
  # a zip archive, whose first member read.table() reads through a connection
  zp <- file.path(dir, "latin.zip")
  suppressWarnings(try(withr::with_dir(dir, utils::zip("latin.zip", "latin.csv", flags = "-q")),
                       silent = TRUE))
  if (file.exists(zp)) files <- c(files, zp)
  nm <- c("id", "frame", "x (\u00b5m)", "y", "note")
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    for (p in files) {
      i <- paste(info, basename(p))
      df <- .read_any(p)
      expect_identical(names(df), nm, info = i)
      expect_identical(df$id, c("ren\u00e9", "ren\u00e9", "b"), info = i)
      expect_identical(df$note, c("caf\u00e9", "b", "c"), info = i)
      expect_true(all(validUTF8(c(names(df), df$id, df$note))), info = i)
      expect_identical(.read_names(p), nm, info = i)
      tr <- read_tracks(p, mapping = list(time = "frame", x = "x (\u00b5m)"), keep = "note")
      expect_setequal(as.character(tr@data[[tr@cols$id]]), c("ren\u00e9", "b"))
    }
    df <- .read_any(mixed)
    expect_identical(names(df)[3], "x (\u00c2\u00b5m)", info = info)
    expect_identical(df$note, "\u00e9", info = info)
    expect_identical(.read_names(mixed), names(df), info = info)
  }
})

test_that("a compressed file is counted as the table it holds", {
  dir <- withr::local_tempdir()
  txt <- c("id,frame,x,y", "a,1,1,2", "a,2,2,3", "a,3,3,4")
  # fread() reads neither gzip nor bzip2 without R.utils, nor xz at all
  packed <- file.path(dir, c("t.csv.gz", "t.csv.bz2", "t.csv.xz"))
  Map(function(path, open) {
    con <- open(path, "w"); writeLines(txt, con); close(con)
  }, packed, list(gzfile, bzfile, xzfile))
  writeLines(txt, file.path(dir, "t.csv"))
  zp <- file.path(dir, "t.zip")
  suppressWarnings(try(withr::with_dir(dir, utils::zip("t.zip", "t.csv", flags = "-q")),
                       silent = TRUE))
  skip_if_not(file.exists(zp), "no zip program")
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    for (p in packed)
      expect_identical(nrow(read_tracks(p)@data), 3L, info = paste(info, basename(p)))
    expect_identical(nrow(read_tracks(zp)@data), 3L, info = info)
  }
})

test_that("an empty cell is missing under every reader", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "blank_id.csv")
  writeLines(c("id,frame,x,y", "a,1,1,2", ",2,2,3", "a,3,3,4"), path)
  for (hide in readers) {
    hide_readers(hide)
    # read.table() read the blank id as "", a track of its own
    e <- expect_error(read_tracks(path), class = "radiatR_invalid_tracks",
                      info = paste(c("without", hide), collapse = " "))
    expect_identical(e$rows, 2L)
  }
})

test_that("blank lines, quotes, apostrophes and # are read alike by every reader", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "cells.csv")
  writeLines(c("id,frame,x,y,note", "1,1,1,2,it's", "", "1,2,2,3,a#b",
               "1,3,3,4,\"c,d\"", "1,4,4,5,\" e \""), path)
  for (hide in readers) {
    hide_readers(hide)
    tr <- read_tracks(path, keep = "note")
    expect_identical(tr@data$note, c("it's", "a#b", "c,d", " e "),
                     info = paste(c("without", hide), collapse = " "))
  }
})

test_that("column names are trimmed, and a name used twice stops the read", {
  dir <- withr::local_tempdir()
  spaced <- file.path(dir, "spaced.csv")
  writeLines(c(" id ,frame,x,y", "a,1,1,2", "b,2,2,3"), spaced)
  twice <- file.path(dir, "twice.csv")
  writeLines(c("id,frame,x,y,id", "1,1,1,2,a", "2,2,2,3,b"), twice)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    expect_identical(unique(read_tracks(spaced)@data$id), c("a", "b"), info = info)
    e <- expect_error(read_tracks(twice), class = "radiatR_malformed_file", info = info)
    expect_identical(e$problem, "duplicate_columns")
    expect_identical(e$columns, "id")
  }
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  xl <- file.path(dir, "twice.xlsx")
  df <- data.frame(id = 1:2, frame = 1:2, x = 1:2, y = 1:2, id2 = c("a", "b"))
  names(df)[5] <- "id"
  writexl::write_xlsx(df, xl)
  expect_error(read_tracks(xl), class = "radiatR_malformed_file")
})

test_that("the text of a label column is the text of the same cells", {
  # text that does not read as the values the reader typed is not attached
  df <- data.frame(id = c(1, 1, 2), x = 1:3)
  expect_error(radiatR:::.keep_labels(df, NULL, function(pos) data.frame(id = c("7", "8", "9"))),
               class = "radiatR_label_misread")
  expect_identical(radiatR:::.keep_labels(df, NULL, function(pos) data.frame(id = c("1", "1.0", "2")))$id,
                   c("1", "1.0", "2"))
})

# ---- what the caller did not supply does not override a format ---------------

test_that("an explicit NULL beside format = does not delete the format's setting", {
  d <- cbind(xy_df(), cond = "u")
  spec <- list(time = list(type = "frames", fps = 25), keep = "cond")
  for (tr in list(read_tracks(d, format = spec, fps = NULL, keep = NULL),
                  read_tracks_format(d, spec, fps = NULL, keep = NULL))) {
    expect_equal(frame_rate(tr), 25)
    expect_true("cond" %in% names(tr@data))
  }
})

test_that("the caller's dialect overrides the format's", {
  for (nm in c("zz_format", "zz_caller"))
    local({
      nm <- nm
      register_loader_dialect(nm, overwrite = TRUE, fun = function(x, ...)
        transform(x, who = nm))
    })
  withr::defer(rm(list = c("zz_format", "zz_caller"), envir = radiatR:::.loader_registry))
  spec <- list(dialect = "zz_format", dialect_args = list())
  expect_identical(unique(read_tracks(xy_df(), format = spec)@data$who), "zz_format")
  # the caller's dialect_args are not its dialect: the format's still runs
  expect_identical(unique(read_tracks(xy_df(), format = spec,
                                      dialect_args = list(tag = 1))@data$who), "zz_format")
  expect_identical(unique(read_tracks(xy_df(), format = spec, dialect = "zz_caller")@data$who),
                   "zz_caller")
  expect_identical(unique(read_tracks(xy_df(), format = spec, dialect = NULL)@data$who),
                   "zz_format")
})

test_that("a format's dialect_args reach its dialect", {
  register_loader_dialect("zz_tag", overwrite = TRUE, fun = function(x, tag = "default", ...)
    transform(x, who = tag))
  withr::defer(rm(list = "zz_tag", envir = radiatR:::.loader_registry))
  spec <- list(dialect = "zz_tag", dialect_args = list(tag = "format"))
  who <- function(tr) unique(tr@data$who)
  expect_identical(who(read_tracks(xy_df(), format = spec)), "format")
  expect_identical(who(read_tracks_format(xy_df(), spec)), "format")
  expect_identical(who(read_tracks(xy_df(), format = spec, dialect_args = list(tag = "caller"))),
                   "caller")
  expect_identical(who(read_tracks(xy_df(), format = list(dialect = "zz_tag"))), "default")
})

test_that("load_manifest() passes on only what its caller supplied", {
  skip_if_not_installed("tibble")
  dir <- withr::local_tempdir()
  utils::write.csv(data.frame(id = "a", frame = 1:3, theta = c(90, 180, 270), cond = "u"),
                   file.path(dir, "one.csv"), row.names = FALSE)
  file_tbl <- tibble::tibble(basename = "one", track = "one.csv")
  spec <- list(angle = list(unit = "degrees"), time = list(type = "frames", fps = 25),
               keep = "cond")
  tr <- load_manifest(file_tbl, dir, format = spec)
  expect_equal(tr@data$theta, c(90, 180, 270) * pi / 180)
  expect_equal(frame_rate(tr), 25)
  expect_true("cond" %in% names(tr@data))
  # what the caller supplies still overrides the format
  expect_equal(frame_rate(load_manifest(file_tbl, dir, format = spec, fps = 50)), 50)
  # and without a format the defaults are read_tracks()'s own
  expect_identical(suppressWarnings(load_manifest(file_tbl, dir))@angle_unit, "radians")
})

test_that(".wall_clock gives the instants ISOdatetime gives, gaps NA", {
  ref <- function(lt, tz) {   # the implementation before 2026-10-14
    out <- ISOdatetime(lt$year + 1900, lt$mon + 1, lt$mday, lt$hour, lt$min, lt$sec, tz = tz)
    back <- as.POSIXlt(out, tz = tz)
    out[which(back$hour != lt$hour | back$min != lt$min)] <- NA
    out
  }
  s <- c("2024-01-01 00:00:00", "2024-03-10 02:30:00", "2024-03-10 03:00:00",
         "2024-11-03 01:30:00", "2024-04-26 00:00:00", "2024-06-30 23:59:59.25",
         "2024-10-27 02:30:00", "1970-01-01 00:00:00", "2038-01-19 03:14:08", NA)
  for (tz in c("UTC", "America/New_York", "Africa/Cairo", "Europe/London",
               "Australia/Lord_Howe", "America/Sao_Paulo")) {
    lt <- strptime(s, "%Y-%m-%d %H:%M:%OS", tz = tz)
    expect_identical(as.numeric(radiatR:::.wall_clock(lt, tz)), as.numeric(ref(lt, tz)),
                     info = tz)
    expect_length(radiatR:::.wall_clock(lt[0], tz), 0L)
  }
  # an all-zoned column calls .wall_clock() on no unzoned values
  z <- radiatR:::.coerce_time(c("2024-01-01T10:00:00Z", "2024-01-01T11:00:00Z"),
                              tz = "America/New_York")
  expect_equal(as.numeric(z), as.numeric(as.POSIXct(c("2024-01-01 10:00:00",
                                                       "2024-01-01 11:00:00"), tz = "UTC")))
})

# ---- a bare date is the first instant of its day -------------------------------

test_that(".day_start is midnight, or the first instant after a gap", {
  d <- as.Date(c("2024-04-25", "2024-04-26", NA))
  got <- radiatR:::.day_start(d, "Africa/Cairo")       # Cairo skipped 00:00-01:00 on 26 April 2024
  expect_identical(format(got, "%Y-%m-%d %H:%M:%S", tz = "Africa/Cairo"),
                   c("2024-04-25 00:00:00", "2024-04-26 01:00:00", NA))
  expect_identical(attr(got, "tzone"), "Africa/Cairo")
  sp <- radiatR:::.day_start(as.Date("2018-11-04"), "America/Sao_Paulo")
  expect_identical(format(sp, "%H:%M", tz = "America/Sao_Paulo"), "01:00")
  expect_error(radiatR:::.day_start(as.Date("2011-12-30"), "Pacific/Apia"),
               class = "radiatR_malformed_values")
})

test_that("a bare date in a DST gap is its day's first instant under every reader", {
  dir <- withr::local_tempdir()
  bare  <- file.path(dir, "bare.csv")
  writeBin(charToRaw("id,time,x,y\na,2024-04-25,.1,.1\na,2024-04-26,.2,.2\n"), bare)
  slash <- file.path(dir, "slash.csv")
  writeBin(charToRaw("id,time,x,y\na,2024/04/25,.1,.1\na,2024/04/26,.2,.2\n"), slash)
  mixed <- file.path(dir, "mixed.csv")
  writeBin(charToRaw("id,time,x,y\na,2024-04-26,.1,.1\na,2024-04-26 10:00:00,.2,.2\n"), mixed)
  want <- function(s) as.numeric(as.POSIXct(s, tz = "Africa/Cairo"))
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    for (p in c(bare, slash))
      expect_equal(as.numeric(read_tracks(p, tz = "Africa/Cairo")@data$time),
                   want(c("2024-04-25 00:00:00", "2024-04-26 01:00:00")), info = paste(info, p))
    expect_equal(as.numeric(read_tracks(mixed, tz = "Africa/Cairo")@data$time),
                 want(c("2024-04-26 01:00:00", "2024-04-26 10:00:00")), info = info)
  }
})

test_that("an Excel date cell at a midnight that is skipped is its day's first instant", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(xy_df(frame = as.POSIXct(c("2024-04-24", "2024-04-25", "2024-04-26"),
                                               tz = "UTC")), path)
  got <- read_tracks(path, tz = "Africa/Cairo")@data$frame
  expect_identical(format(got, "%Y-%m-%d %H:%M", tz = "Africa/Cairo"),
                   c("2024-04-24 00:00", "2024-04-25 00:00", "2024-04-26 01:00"))
})

test_that("a time in a DST gap and a leap second are named under every reader", {
  dir <- withr::local_tempdir()
  csv <- function(name, rows) {
    p <- file.path(dir, name)
    writeBin(charToRaw(paste0("id,time,x,y\n", paste0("a,", rows, ",.1,.1\n", collapse = ""))), p)
    p
  }
  gap  <- csv("gap.csv", c("2024-03-10 01:30:00", "2024-03-10 02:30:00"))
  leap <- csv("leap.csv", c("2016-12-31T23:59:59Z", "2016-12-31T23:59:60Z"))
  basic <- csv("basic.csv", c("20161231T235959Z", "20161231T235960Z"))
  basic_local <- csv("basic_local.csv", c("20161231T235959", "20161231T235960"))
  bad  <- csv("bad.csv", c("2024-02-28 10:00:00", "2024-02-30 10:00:00"))
  apia <- csv("apia.csv", c("2011-12-29 10:00:00", "2011-12-30 10:00:00"))
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    e <- malformed(read_tracks(gap, tz = "America/New_York"))
    expect_s3_class(e, "radiatR_malformed_values")
    expect_identical(e$values, "2024-03-10 02:30:00", info = info)
    expect_match(conditionMessage(e), "exist in time zone 'America/New_York'", info = info)
    e <- malformed(read_tracks(leap))
    expect_identical(e$values, "2016-12-31T23:59:60Z", info = info)
    expect_match(conditionMessage(e), "leap second", info = info)
    for (p in c(basic, basic_local)) {    # ISO 8601 basic: no ':' before the seconds
      e <- malformed(read_tracks(p))
      expect_identical(e$rows, 2L, info = paste(info, basename(p)))
      expect_match(conditionMessage(e), "leap second", info = paste(info, basename(p)))
    }
    e <- malformed(read_tracks(bad, tz = "America/New_York"))
    expect_identical(e$values, "2024-02-30 10:00:00", info = info)
    expect_false(grepl("exist in time zone", conditionMessage(e)), info = info)
    # a day the zone skips altogether is named, not moved a day back
    e <- malformed(read_tracks(apia, tz = "Pacific/Apia"))
    expect_identical(e$values, "2011-12-30 10:00:00", info = info)
    expect_match(conditionMessage(e), "exist in time zone 'Pacific/Apia'", info = info)
  }
})


test_that("a bare date on a day the zone skips reports its own row under every reader", {
  dir <- withr::local_tempdir()
  beside <- file.path(dir, "beside.csv")    # a bare date beside date-times
  writeBin(charToRaw(paste0("id,time,x,y\na,2011-12-28 10:00:00,.1,.1\n",
                            "a,2011-12-29 10:00:00,.2,.2\na,2011-12-30,.3,.3\n")), beside)
  dates <- file.path(dir, "dates.csv")      # bare dates throughout
  writeBin(charToRaw(paste0("id,time,x,y\na,2011-12-28,.1,.1\n",
                            "a,2011-12-29,.2,.2\na,2011-12-30,.3,.3\n")), dates)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    for (p in c(beside, dates)) {
      e <- malformed(read_tracks(p, tz = "Pacific/Apia"))
      expect_s3_class(e, "radiatR_malformed_values")
      expect_identical(e$rows, 3L, info = paste(info, basename(p)))
      expect_match(conditionMessage(e), "exist in time zone 'Pacific/Apia'", info = info)
    }
  }
})

test_that("bare dates in one file and date-times in another bind under every reader", {
  dir <- withr::local_tempdir()
  csv <- function(name, rows) {
    p <- file.path(dir, name)
    writeBin(charToRaw(paste0("id,time,x,y\n", paste0(rows, ",.1,.1\n", collapse = ""))), p)
    p
  }
  bare <- csv("bare.csv", c("a,2024-04-25", "a,2024-04-26"))
  dt   <- csv("dt.csv", c("b,2024-04-26 10:00:00", "b,2024-04-26 11:00:00"))
  apia <- csv("apia.csv", c("a,2011-12-29", "a,2011-12-30"))
  apia_dt <- csv("apia_dt.csv", c("b,2011-12-29 10:00:00", "b,2011-12-31 10:00:00"))
  want <- function(s) as.numeric(as.POSIXct(s, tz = "Africa/Cairo"))
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    got <- read_tracks(c(bare, dt), tz = "Africa/Cairo")@data$time
    expect_equal(as.numeric(got), want(c("2024-04-25 00:00:00", "2024-04-26 01:00:00",
                                         "2024-04-26 10:00:00", "2024-04-26 11:00:00")),
                 info = info)
    e <- malformed(read_tracks(c(apia, apia_dt), tz = "Pacific/Apia"))
    expect_s3_class(e, "radiatR_malformed_values")
    expect_identical(e$rows, 2L, info = info)
    expect_identical(e$source, "apia.csv", info = info)
    expect_match(conditionMessage(e), "exist in time zone 'Pacific/Apia'", info = info)
  }
  skip_if_not_installed("readxl"); skip_if_not_installed("writexl")
  x <- file.path(dir, "dt.xlsx")
  writexl::write_xlsx(data.frame(id = "c", time = as.POSIXct(
    c("2024-04-26 10:00:00", "2024-04-26 11:00:00"), tz = "UTC"), x = .1, y = .1), x)
  for (hide in readers) {
    hide_readers(hide)
    info <- paste(c("without", hide), collapse = " ")
    got <- read_tracks(c(bare, x), tz = "Africa/Cairo")@data$time
    expect_equal(as.numeric(got), want(c("2024-04-25 00:00:00", "2024-04-26 01:00:00",
                                         "2024-04-26 10:00:00", "2024-04-26 11:00:00")),
                 info = info)
  }
})

test_that("an Excel cell on a day the zone skips stops the load only in a column it uses", {
  skip_if_not_installed("readxl"); skip_if_not_installed("writexl")
  skipped <- as.POSIXct(c("2011-12-28", "2011-12-29", "2011-12-30"), tz = "UTC")
  used <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(xy_df(frame = skipped), used)
  e <- malformed(read_tracks(used, tz = "Pacific/Apia"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$rows, 3L)
  expect_identical(e$columns, "frame")
  expect_match(conditionMessage(e), "exist in time zone 'Pacific/Apia'")
  unused <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(xy_df(note = skipped), unused)
  expect_false("note" %in% names(read_tracks(unused, tz = "Pacific/Apia")@data))
  e <- malformed(read_tracks(unused, tz = "Pacific/Apia", keep = "note"))
  expect_identical(e$rows, 3L)
  expect_identical(e$columns, "note")
})
