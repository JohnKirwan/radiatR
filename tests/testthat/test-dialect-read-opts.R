# read_opts reach the built-in dialects: what each honours, and the loud
# refusal of what it cannot.

fx <- function(f) testthat::test_path("fixtures", "real", f)
cond <- function(expr) tryCatch(expr, error = function(e) e)
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
col_of <- function(ts, role) as.data.frame(ts)[[ts@cols[[role]]]]
tm_file <- function(lines, env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(lines, p)
  p
}
# TrackMate, tab-separated with decimal commas: the sniff reads a point.
tm_tab_comma <- function(env = parent.frame())
  tm_file(c("TRACK_ID\tFRAME\tPOSITION_X\tPOSITION_Y",
            paste0("a\t", 0:2, "\t", c("1,5", "1,6", "1,7"), "\t2,5")), env)

test_that("a .read_any() dialect reads with the given decimal mark", {
  p <- tm_tab_comma()
  expect_s3_class(cond(read_tracks(p, dialect = "trackmate")), "radiatR_malformed_values")
  ts <- read_tracks(p, dialect = "trackmate",
                    dialect_args = list(read_opts = list(decimal = ",")))
  expect_equal(col_of(ts, "x"), c(1.5, 1.6, 1.7))
})

test_that("a .read_any() dialect reads with the given delimiter", {
  p <- tm_file(c("TRACK_ID|FRAME|POSITION_X|POSITION_Y", paste0("a|", 0:2, "|1.5|2.5")))
  ts <- read_tracks(p, dialect = "trackmate",
                    dialect_args = list(read_opts = list(delim = "|")))
  expect_equal(col_of(ts, "x"), rep(1.5, 3))
})

test_that("NULL entries are no option at all", {
  p <- tm_file(c("TRACK_ID,FRAME,POSITION_X,POSITION_Y", paste0("a,", 0:2, ",1.5,2.5")))
  ts <- read_tracks(p, dialect = "trackmate",
                    dialect_args = list(read_opts = list(decimal = NULL, delim = NULL)))
  expect_equal(col_of(ts, "x"), rep(1.5, 3))
})

test_that("an unknown entry or a bad decimal mark is refused by name", {
  p <- tm_file(c("TRACK_ID,FRAME,POSITION_X,POSITION_Y", paste0("a,", 0:2, ",1.5,2.5")))
  e <- cond(read_tracks(p, dialect = "trackmate",
                        dialect_args = list(read_opts = list(decimals = ","))))
  expect_s3_class(e, "radiatR_invalid_argument")
  expect_match(conditionMessage(e), "trackmate: read_opts has no entry 'decimals'", fixed = TRUE)
  e <- cond(read_tracks(p, dialect = "trackmate",
                        dialect_args = list(read_opts = list(decimal = ";"))))
  expect_s3_class(e, "radiatR_invalid_argument")
  expect_match(conditionMessage(e), 'read_opts$decimal must be "." or ","', fixed = TRUE)
})

test_that("DeepLabCut and ToxTrac refuse a delimiter; DeepLabCut honours a decimal mark", {
  e <- cond(read_tracks(fx("dlc_single_wasp_predictions.csv"), dialect = "deeplabcut",
                        dialect_args = list(read_opts = list(delim = ";"))))
  expect_s3_class(e, "radiatR_invalid_argument")
  expect_match(conditionMessage(e),
               "deeplabcut: read_opts$delim cannot be used: this dialect reads its own file layout and takes only read_opts$decimal",
               fixed = TRUE)
  e <- cond(read_tracks(fx("toxtrac_tracking_realspace.txt"), dialect = "toxtrac",
                        dialect_args = list(read_opts = list(delim = "\t"))))
  expect_s3_class(e, "radiatR_invalid_argument")
  ref <- quiet(read_tracks(fx("dlc_single_wasp_predictions.csv"), dialect = "deeplabcut",
                           on_invalid = "drop"))
  same <- quiet(read_tracks(fx("dlc_single_wasp_predictions.csv"), dialect = "deeplabcut",
                            on_invalid = "drop",
                            dialect_args = list(read_opts = list(decimal = "."))))
  expect_identical(as.data.frame(same), as.data.frame(ref))
  # a comma mark makes the file's decimal points malformed: the mark was used
  expect_s3_class(cond(quiet(read_tracks(fx("dlc_single_wasp_predictions.csv"),
                                         dialect = "deeplabcut", on_invalid = "drop",
                                         dialect_args = list(read_opts = list(decimal = ","))))),
                  "radiatR_malformed_values")
})

test_that("ToxTrac: a headerless decimal-comma Tracking.txt reads with read_opts", {
  comma <- withr::local_tempfile(fileext = ".txt")
  writeLines(gsub(".", ",", readLines(fx("toxtrac_tracking.txt")), fixed = TRUE), comma)
  ref <- quiet(read_tracks(fx("toxtrac_tracking.txt"), dialect = "toxtrac"))
  ts <- quiet(read_tracks(comma, dialect = "toxtrac",
                          dialect_args = list(read_opts = list(decimal = ","))))
  expect_equal(col_of(ts, "x"), col_of(ref, "x"))
})

test_that("a data frame takes a decimal mark but no file option", {
  d <- data.frame(TRACK_ID = "a", FRAME = 0:2, POSITION_X = c("1,5", "1,6", "1,7"),
                  POSITION_Y = "2,5")
  ts <- read_tracks(d, dialect = "trackmate",
                    dialect_args = list(read_opts = list(decimal = ",")))
  expect_equal(col_of(ts, "x"), c(1.5, 1.6, 1.7))
  e <- cond(read_tracks(d, dialect = "trackmate",
                        dialect_args = list(read_opts = list(delim = ";"))))
  expect_s3_class(e, "radiatR_invalid_argument")
  expect_match(conditionMessage(e), "the input is a data frame, not a file", fixed = TRUE)
})
