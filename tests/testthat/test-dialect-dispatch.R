# Dialect dispatch: a dialect is the only reader of its input and runs once
# per file; built-in dialects that were removed fail by name.

# Register a dialect for the duration of the calling test.
local_dialect <- function(name, fun, env = parent.frame()) {
  register_loader_dialect(name, fun, overwrite = TRUE)
  withr::defer(rm(list = name, envir = radiatR:::.loader_registry), envir = env)
}

# The custom loader shown in vignette("loaders"): dtrack's headerless,
# tab-separated frame / x / y [/ flag] file, id taken from the file name.
dtrack_loader <- function(x) {
  d <- utils::read.delim(x, header = FALSE, colClasses = "numeric")
  if (ncol(d) < 3L) stop("dtrack: expected at least 3 columns (frame, x, y)")
  data.frame(id = tools::file_path_sans_ext(basename(x)),
             frame = d[[1]], x = d[[2]], y = d[[3]])
}

write_dtrack <- function(dir, stems, lines = c("1\t10\t20\t1", "2\t11\t21\t1", "3\t12\t22\t1")) {
  paths <- file.path(dir, paste0(stems, ".txt"))
  for (p in paths) writeLines(lines, p)
  paths
}

test_that("removed built-in dialects fail by name and point to custom loaders", {
  removed <- c("tracktor", "boris_xy", "dtrack", "motchallenge", "csv_bbox",
               "wide_prefix_xy", "tum_traj", "euroc_mav", "gpx",
               "geojson_linestring", "nmea_gprmc")
  expect_false(any(removed %in% list_loader_dialects()))
  expect_false(exists("dtrack_read", envir = asNamespace("radiatR"), inherits = FALSE))
  df <- data.frame(id = "a", frame = 1:2, x = c(0, 1), y = c(0, 1))
  for (d in removed) {
    e <- tryCatch(read_tracks(df, dialect = d), error = function(e) e)
    expect_s3_class(e, "radiatR_removed_dialect")
    expect_match(conditionMessage(e), d, fixed = TRUE)
    expect_match(conditionMessage(e), "register_loader_dialect", fixed = TRUE)
  }
  expect_error(read_tracks(df, dialect = "no_such_dialect"), "Unknown dialect")
})

test_that("a user-registered loader may reuse a removed name", {
  local_dialect("dtrack", dtrack_loader)
  path <- write_dtrack(withr::local_tempdir(), "trial_7")
  d <- as.data.frame(read_tracks(path, dialect = "dtrack"))
  expect_equal(d$frame, 1:3)
  expect_equal(d$x, 10:12)
  expect_identical(unique(d$id), "trial_7")
})

test_that("a dialect is the only reader of its file", {
  calls <- 0L
  local_dialect("dtrack", function(x) { calls <<- calls + 1L; dtrack_loader(x) })
  path <- write_dtrack(withr::local_tempdir(), "a")
  # A generic read of this headerless file would take row 1 as the header and
  # announce the repaired duplicate name "1".
  expect_no_message(read_tracks(path, dialect = "dtrack"))
  expect_identical(calls, 1L)
})

test_that("a dialect runs once per file in a multi-file import", {
  seen <- list()
  local_dialect("dtrack", function(x) { seen[[length(seen) + 1L]] <<- x; dtrack_loader(x) })
  dir <- withr::local_tempdir()
  paths <- write_dtrack(dir, c("a", "b"))
  d <- as.data.frame(read_tracks_dir(dir, pattern = "\\.txt$", dialect = "dtrack"))
  expect_identical(unlist(seen), paths)
  expect_identical(sort(unique(d$id)), c("a", "b"))
  expect_equal(nrow(d), 6L)
  expect_identical(unique(d[["..source_file"]][d$id == "b"]), "b.txt")
})

test_that("ids repeated across files under a dialect follow id_collision", {
  local_dialect("one_id", function(x) transform(dtrack_loader(x), id = "1"))
  paths <- write_dtrack(withr::local_tempdir(), c("a", "b"))
  expect_error(read_tracks(paths, dialect = "one_id"), "collide across source files")
  d <- as.data.frame(read_tracks(paths, dialect = "one_id", id_collision = "namespace"))
  expect_identical(sort(unique(d$id)), c("a::1", "b::1"))
})

test_that("a dialect failure in a multi-file import names the file", {
  local_dialect("dtrack", dtrack_loader)
  dir <- withr::local_tempdir()
  paths <- c(write_dtrack(dir, "a"), write_dtrack(dir, "b", lines = c("1\t10", "2\t11")))
  e <- tryCatch(read_tracks(paths, dialect = "dtrack"), error = function(e) e)
  expect_match(conditionMessage(e), "b.txt", fixed = TRUE)
  expect_match(conditionMessage(e$parent), "at least 3 columns")
})

test_that("TRex per-individual files load together, ids from the file names", {
  dir <- withr::local_tempdir()
  for (i in 0:1)
    utils::write.csv(data.frame(frame = 0:2, X = c(0, .1, .2) + i, Y = c(0, .1, .2)),
                     file.path(dir, sprintf("run_fish%d.csv", i)), row.names = FALSE)
  ts <- read_tracks_dir(dir, dialect = "trex", dialect_args = list(fps = 25))
  d <- as.data.frame(ts)
  expect_identical(sort(unique(as.character(d$id))), c("0", "1"))
  expect_equal(frame_rate(ts), 25)
  expect_equal(nrow(d), 6L)
})

test_that("dialect tables are coerced file by file, and ids come from the file names", {
  # A loader that returns text columns and no id: a bad cell must be reported
  # against its own file and row, not as a type clash when the files are bound.
  local_dialect("raw3", function(x)
    utils::read.delim(x, header = FALSE, col.names = c("frame", "x", "y"),
                      colClasses = "character"))
  dir <- withr::local_tempdir()
  paths <- c(write_dtrack(dir, "a", c("1\t10\t20", "2\t11\t21")),
             write_dtrack(dir, "b", c("1\t10\t20", "2\toops\t21")))
  e <- tryCatch(read_tracks(paths, dialect = "raw3"), error = function(e) e)
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$source, "b.txt")
  expect_identical(e$rows, 2L)
  expect_identical(e$dialect, "raw3")

  write_dtrack(dir, "b", c("1\t10\t20", "2\t11\t21"))
  d <- as.data.frame(read_tracks(paths, dialect = "raw3"))
  expect_identical(sort(unique(d[[1]])), c("a", "b"))
  expect_false("..source_path" %in% names(d))
  expect_error(read_tracks(paths, dialect = "raw3", id_from_filename = FALSE),
               "no id column")
})

test_that("a missing file is named before any dialect runs", {
  local_dialect("dtrack", dtrack_loader)
  path <- write_dtrack(withr::local_tempdir(), "a")
  gone <- file.path(dirname(path), "typo.txt")
  expect_error(read_tracks(c(path, gone), dialect = "dtrack"), "not found: .*typo.txt")
  expect_error(read_tracks(gone, dialect = "dtrack"), "not found: .*typo.txt")
})

test_that("single-animal SLEAP files in one directory need id_collision = 'namespace'", {
  src <- system.file("extdata", "sleap_example.csv", package = "radiatR")
  skip_if(!nzchar(src), "sleap_example.csv not found in extdata")
  dir <- withr::local_tempdir()
  file.copy(src, file.path(dir, c("a.csv", "b.csv")))
  expect_error(suppressMessages(read_tracks_dir(dir, dialect = "sleap")),
               "collide across source files")
  ts <- suppressMessages(read_tracks_dir(dir, dialect = "sleap", id_collision = "namespace"))
  expect_setequal(sub("::.*$", "", unique(as.character(as.data.frame(ts)[[ts@cols$id]]))),
                  c("a", "b"))
})

test_that("a text column from a dialect is parsed with its file's decimal mark", {
  # One bad cell leaves the column as text; only that cell is malformed, in a
  # single file and in a multi-file import alike.
  dir <- withr::local_tempdir()
  write_tm <- function(stem, x) {
    p <- file.path(dir, paste0(stem, ".csv"))
    writeLines(c("TRACK_ID;FRAME;POSITION_X;POSITION_Y",
                 paste0(stem, ";", 0:2, ";", x, ";2,5")), p)
    p
  }
  bad <- write_tm("b", c("1,5", "oops", "1,7"))
  for (x in list(bad, c(write_tm("a", c("1,5", "1,6", "1,7")), bad))) {
    e <- tryCatch(read_tracks(x, dialect = "trackmate"), error = function(e) e)
    expect_s3_class(e, "radiatR_malformed_values")
    expect_identical(e$rows, 2L)
    expect_identical(e$values, "oops")
    expect_identical(e$source, "b.csv")
  }
})

test_that("a custom loader for a non-text file is not sniffed as text", {
  path <- withr::local_tempfile(fileext = ".bin")
  writeBin(as.raw(c(0x89, 0xff, 0xfe, 0x0a, 0xc3, 0x28, 0x0a)), path)
  local_dialect("binary", function(x) {
    readBin(x, "raw", 8L)
    data.frame(id = "a", frame = 1:2, x = c(0, 1), y = c(0, 1))
  })
  expect_equal(nrow(as.data.frame(read_tracks(path, dialect = "binary"))), 2L)
})

test_that("a dialect's declared time unit is checked and cannot be contradicted", {
  tbl <- function(unit) function(x)
    structure(data.frame(id = "a", time = c(0, 0.5, 1), x = c(0, 1, 2), y = 0), time_unit = unit)
  df <- data.frame(dummy = 1)
  local_dialect("secs", tbl("seconds"))
  expect_equal(elapsed_seconds(read_tracks(df, dialect = "secs")), c(0, 0.5, 1))
  for (tt in c("frames", "posix"))
    expect_s3_class(tryCatch(read_tracks(df, dialect = "secs", time_type = tt, fps = 2),
                             error = function(e) e), "radiatR_time_type_mismatch")
  local_dialect("frames", tbl("frames"))
  expect_equal(elapsed_seconds(read_tracks(df, dialect = "frames", fps = 2)), c(0, 0.25, 0.5))
  expect_s3_class(tryCatch(read_tracks(df, dialect = "frames", time_type = "seconds"),
                           error = function(e) e), "radiatR_time_type_mismatch")
  local_dialect("minutes", tbl("minutes"))
  e <- tryCatch(read_tracks(df, dialect = "minutes"), error = function(e) e)
  expect_s3_class(e, "radiatR_time_type_mismatch")
  expect_match(conditionMessage(e), "must be \"seconds\" or \"frames\"", fixed = TRUE)
  # without a dialect the attribute means nothing
  plain <- tbl("minutes")(NULL)
  expect_error(elapsed_seconds(read_tracks(plain)), "frame rate")
})

test_that("a dialect that ignores its path content reads an unreadable path", {
  local_dialect("toy_nopath", function(x)
    data.frame(id = "a", frame = 1:3, x = c(.1, .2, .3), y = c(.1, .2, .3)))
  d <- withr::local_tempdir()
  expect_equal(nrow(read_tracks(d, dialect = "toy_nopath")@data), 3L)
})
