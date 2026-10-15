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
  expect_identical(e$option, "delim")
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

local_dialect <- function(name, fun, env = parent.frame()) {
  register_loader_dialect(name, fun, overwrite = TRUE)
  withr::defer(rm(list = name, envir = radiatR:::.loader_registry), envir = env)
}

test_that("read_tracks() hands read_opts to a built-in dialect", {
  p <- tm_tab_comma()
  ts <- read_tracks(p, dialect = "trackmate", read_opts = list(decimal = ","))
  expect_equal(col_of(ts, "x"), c(1.5, 1.6, 1.7))
  # multi-file: every file
  q <- tm_tab_comma()
  ts <- read_tracks(c(p, q), dialect = "trackmate", read_opts = list(decimal = ","),
                    id_collision = "namespace")
  expect_equal(col_of(ts, "x"), rep(c(1.5, 1.6, 1.7), 2))
})

test_that("read_opts given twice is an error", {
  e <- cond(read_tracks(tm_tab_comma(), dialect = "trackmate",
                        read_opts = list(decimal = ","),
                        dialect_args = list(read_opts = list(decimal = ","))))
  expect_s3_class(e, "radiatR_invalid_argument")
  expect_match(conditionMessage(e), "read_opts is given both", fixed = TRUE)
})

test_that("a custom dialect without a read_opts argument refuses them", {
  local_dialect("plain", function(x) utils::read.delim(x, colClasses = "character"))
  p <- tm_tab_comma()
  e <- cond(read_tracks(p, dialect = "plain", read_opts = list(decimal = ",")))
  expect_s3_class(e, "radiatR_invalid_argument")
  expect_match(conditionMessage(e), "the 'plain' dialect takes no read_opts", fixed = TRUE)
  # all-NULL read_opts (the default) are no options: the load goes on and
  # stops on the commas instead
  e <- cond(read_tracks(p, dialect = "plain",
                        read_opts = list(delim = NULL, decimal = NULL, sheet = NULL),
                        mapping = list(id = "TRACK_ID", time = "FRAME",
                                       x = "POSITION_X", y = "POSITION_Y")))
  expect_s3_class(e, "radiatR_malformed_values")
})

test_that("a custom dialect with read_opts gets them, and its table keeps the mark", {
  seen <- NULL
  local_dialect("plain2", function(x, read_opts = list()) {
    seen <<- read_opts
    utils::read.delim(x, colClasses = "character")   # sets no decimal attr
  })
  ts <- read_tracks(tm_tab_comma(), dialect = "plain2", read_opts = list(decimal = ","),
                    mapping = list(id = "TRACK_ID", time = "FRAME",
                                   x = "POSITION_X", y = "POSITION_Y"))
  expect_identical(seen, list(decimal = ","))
  expect_equal(col_of(ts, "x"), c(1.5, 1.6, 1.7))
})

test_that("read_opts are checked on the generic path too", {
  p <- tm_file(c("id,frame,x,y", paste0("a,", 0:2, ",1.5,2.5")))
  e <- cond(read_tracks(p, read_opts = list(decimals = ",")))   # not a partial match
  expect_s3_class(e, "radiatR_invalid_argument")
  expect_match(conditionMessage(e), "read_tracks: read_opts has no entry 'decimals'", fixed = TRUE)
  expect_s3_class(cond(read_tracks(p, read_opts = list(decimal = ";"))),
                  "radiatR_invalid_argument")
})

test_that("a duration with the other decimal mark gets the suggestion", {
  d <- tm_file(c("Time\tCentre position X\tCentre position Y",
                 "0:00:00,320\t410\t352", "0:00:01,000\t411\t353"))
  e <- cond(quiet(read_tracks(d, dialect = "anymaze")))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$decimal, ".")
  expect_identical(e$suggested_decimal, ",")
  e <- cond(radiatR:::.parse_duration("0:00:00.320", "Time", source = "a.csv",
                                      dialect = "anymaze", decimal = ","))
  expect_identical(e$decimal, ",")
  expect_identical(e$suggested_decimal, ".")
})

test_that("an Excel source gets no decimal suggestion (read_opts cannot change it)", {
  e <- cond(radiatR:::radiatR_abort_malformed_values("x", 1L, "1,5", source = "a.xlsx",
                                                     decimal_hint = TRUE, decimal = "."))
  expect_null(e$suggested_decimal)
  expect_false(grepl("read_opts", conditionMessage(e), fixed = TRUE))
})

test_that("a dialect's malformed values carry the mark and a suggestion", {
  e <- cond(read_tracks(tm_tab_comma(), dialect = "trackmate"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$decimal, ".")
  expect_identical(e$suggested_decimal, ",")
  expect_match(conditionMessage(e), 'Pass read_opts = list(decimal = ",")', fixed = TRUE)
  # a value that is no number in either mark suggests nothing
  p <- tm_file(c("TRACK_ID,FRAME,POSITION_X,POSITION_Y", paste0("a,", 0:2, ",oops,2.5")))
  e <- cond(read_tracks(p, dialect = "trackmate"))
  expect_null(e$suggested_decimal)
})
