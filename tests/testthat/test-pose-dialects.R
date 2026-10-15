# Pose dialects (DeepLabCut, SLEAP): the shared point-selection rule, the
# attrition record, and the files the programs write.
# fixtures/real/README.md gives each file's source and licence.

fx <- function(f) testthat::test_path("fixtures", "real", f)
cond <- function(expr) tryCatch(expr, error = function(e) e)
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
rows_of <- function(ts) as.data.frame(ts)
# A copy of a fixture with its lines rewritten by `edit`.
edited <- function(f, edit, env = parent.frame()) {
  p <- withr::local_tempfile(fileext = paste0(".", tools::file_ext(f)), .local_envir = env)
  writeLines(edit(readLines(fx(f))), p)
  p
}
# x, y and score matrices for .dialect_position(), one named column per point
pts <- function(x, y, score = NULL) {
  m <- function(l) if (!is.null(l)) do.call(cbind, l)
  list(x = m(x), y = m(y), score = m(score))
}
position <- function(...) radiatR:::.dialect_position(..., dialect = "test")

# ---- the point-selection rule --------------------------------------------------

test_that("the position is the score-weighted mean of the selected points", {
  p <- position(pts(x = list(head = c(1, 1), tail = c(0, 0)),
                    y = list(head = c(0, 2), tail = c(0, 0)),
                    score = list(head = c(0.75, 0.5), tail = c(0.25, 0.5))))
  expect_equal(p$x, c(0.75, 0.5))
  expect_equal(p$y, c(0, 1))
  expect_equal(p$attrition$masked, c(0L, 0L))
  expect_equal(p$attrition$frames_no_position, c(0L, 0L))
})

test_that("a masked point leaves the frame without a position unless partial = 'mean'", {
  x <- list(head = c(1, NA, 1, 1), tail = c(0, 0, 0, 0))
  y <- list(head = c(0, 0, 0, 0), tail = c(0, 0, NA, 0))
  s <- list(head = c(1, 1, 1, 0), tail = c(1, 1, 1, 1))
  p <- position(pts(x, y, s))
  expect_equal(p$x, c(0.5, NA, NA, NA))
  expect_identical(p$attrition$point, c("head", "tail"))
  expect_equal(p$attrition$masked, c(2L, 1L))   # head: a missing x and a score of 0
  expect_equal(p$attrition$frames, c(4L, 4L))
  expect_equal(p$attrition$frames_no_position, c(3L, 3L))
  expect_equal(sum(is.na(p$x)), p$attrition$frames_no_position[1])
  # masked cells are blanked in the per-point coordinates too
  expect_identical(names(p$points), c("head_x", "head_y", "tail_x", "tail_y"))
  expect_equal(p$points$head_x, c(1, NA, 1, NA))
  expect_equal(p$points$tail_y, c(0, 0, NA, 0))

  m <- position(pts(x, y, s), partial = "mean")
  expect_equal(m$x, c(0.5, 0, 1, 0))
  expect_equal(m$attrition$frames_no_position, c(0L, 0L))
  expect_identical(m$attrition$partial, c("mean", "mean"))
})

test_that("a threshold masks points below it; every point masked is no position", {
  x <- list(head = c(1, 1, 1), tail = c(0, 0, 0))
  y <- list(head = c(0, 0, 0), tail = c(0, 0, 0))
  s <- list(head = c(0.95, 0.4, 0.4), tail = c(0.95, 0.95, NA))
  p <- position(pts(x, y, s), score_min = 0.9, partial = "mean")
  expect_equal(p$x, c(0.5, 0, NA))
  expect_equal(p$attrition$masked, c(2L, 1L))   # a missing score cannot pass a threshold
  expect_equal(p$attrition$frames_no_position, c(1L, 1L))
  expect_equal(sum(is.na(p$x)), 1L)
  expect_equal(p$attrition$score_min, c(0.9, 0.9))
  # without a threshold a missing score weighs 1
  q <- position(pts(x, y, s))
  expect_equal(q$x[3], 0.4 / 1.4)
  expect_equal(q$attrition$masked, c(0L, 0L))
  expect_true(all(is.na(q$attrition$score_min)))
})

test_that("one point follows the same rule, and rows are never removed", {
  p <- position(pts(list(head = c(1, 2, 3)), list(head = c(0, 0, 0)),
                    list(head = c(0.99, 0.2, 0))), score_min = 0.9)
  expect_equal(p$x, c(1, NA, NA))
  expect_length(p$y, 3L)
  expect_equal(p$attrition$masked, 2L)
  # a coordinate that is not finite is missing, and counted
  inf <- position(pts(list(head = c(1, Inf)), list(head = c(0, 0))))
  expect_equal(inf$x, c(1, NA))
  expect_equal(inf$attrition$masked, 1L)
})

test_that("a threshold needs a score for every selected point and must be one number", {
  one <- pts(list(head = 1, tail = 0), list(head = 0, tail = 0), list(head = 0.9))
  e <- cond(position(one, score_min = 0.5))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "no score column for: tail", fixed = TRUE)
  expect_s3_class(cond(position(one, score_min = "0.5")), "radiatR_dialect_error")
  # no threshold: the point without a score weighs 1
  expect_equal(position(one)$x, 0.9 / 1.9)
})

test_that("score columns are read strictly and a negative score is malformed", {
  inp <- radiatR:::.dialect_input(
    data.frame(head_x = c("1", "2"), head_y = c(0, 0), head_score = c("1e-3", "0.9"),
               stringsAsFactors = FALSE), "test")
  p <- radiatR:::.dialect_points(inp, c(head = "head_x"), c(head = "head_y"),
                                 c(head = "head_score"))
  expect_equal(p$x[, "head"], c(1, 2))
  expect_equal(p$score[, "head"], c(0.001, 0.9))
  # read as a number, "1e-3" is below 0.5 (compared as text it was above it)
  expect_equal(position(p, score_min = 0.5)$attrition$masked, 1L)
  expect_null(radiatR:::.dialect_points(inp, c(head = "head_x"), c(head = "head_y"))$score)

  bad <- radiatR:::.dialect_input(data.frame(head_x = 1, head_y = 0, head_score = -0.5), "test")
  e <- cond(radiatR:::.dialect_points(bad, c(head = "head_x"), c(head = "head_y"),
                                      c(head = "head_score")))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "head_score")
  expect_equal(e$rows, 1L)
})

test_that("the points to use are distinct names among those found", {
  select <- function(sel) radiatR:::.dialect_select(sel, c("head", "tail"), "node", "test")
  expect_identical(select(NULL), c("head", "tail"))
  expect_identical(select("tail"), "tail")
  expect_identical(radiatR:::.dialect_point_names(c("id", "head_x", "head_y", "tail_x", "wing_y")),
                   "head")
  e <- cond(select("wing"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "node(s) not found: wing; available: head, tail", fixed = TRUE)
  # a repeated point would be weighed twice and masked once
  for (sel in list(c("head", "head"), character(0), NA, NA_character_, 1))
    expect_s3_class(cond(select(sel)), "radiatR_dialect_error")
  expect_error(position(pts(list(head = 1, head = 1), list(head = 0, head = 0))))
})

test_that("a column named in an argument must exist", {
  inp <- radiatR:::.dialect_input(data.frame(a = 1), "test")
  expect_identical(radiatR:::.dialect_require(inp, "a", "id_col"), "a")
  expect_null(radiatR:::.dialect_require(inp, NULL, "id_col"))
  e <- cond(radiatR:::.dialect_require(inp, "b", "id_col"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "id_col = \"b\"", fixed = TRUE)
})

# ---- the attrition record ------------------------------------------------------

# A dialect that reports attrition the way the built-in pose dialects do.
local_attrition_dialect <- function(env = parent.frame()) {
  register_loader_dialect("attrition_test", function(x) {
    inp <- radiatR:::.dialect_input(x, "attrition_test")
    p <- radiatR:::.dialect_points(inp, c(head = "head_x", tail = "tail_x"),
                                   c(head = "head_y", tail = "tail_y"))
    pos <- radiatR:::.dialect_position(p, dialect = "attrition_test", source = inp$source)
    radiatR:::.dialect_output(
      data.frame(id = inp$df$id, time = inp$df$frame, x = pos$x, y = pos$y),
      "frames", pos$attrition)
  }, overwrite = TRUE)
  withr::defer(rm("attrition_test", envir = radiatR:::.loader_registry), envir = env)
}
# Four frames of two points; the head is missing in the frames named by `gap`.
two_point <- function(id = "a", gap = 2L) {
  d <- data.frame(id = id, frame = 1:4, head_x = c(1, 2, 3, 4), head_y = 0,
                  tail_x = c(0, 1, 2, 3), tail_y = 0)
  d$head_x[gap] <- NA
  d
}

test_that("read_tracks() stores the attrition record and reports it once", {
  local_attrition_dialect()
  err_msg <- testthat::capture_messages(e <- cond(read_tracks(two_point(), dialect = "attrition_test")))
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_equal(e$rows, 2L)
  expect_match(err_msg, "on_invalid = \"drop\"", fixed = TRUE, all = FALSE)
  expect_match(err_msg, "partial = \"mean\"", fixed = TRUE, all = FALSE)

  msg <- testthat::capture_messages(
    ts <- suppressWarnings(read_tracks(two_point(), dialect = "attrition_test",
                                       on_invalid = "drop")))
  expect_equal(sum(grepl("have no position", msg, fixed = TRUE)), 1L)
  expect_match(msg, "attrition_test: 1 of 4 row(s) have no position (no threshold, partial = \"na\")",
               fixed = TRUE, all = FALSE)
  expect_match(msg, "Masked points: head 1.", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("Pass on_invalid", msg, fixed = TRUE)))
  a <- ts@meta$attrition
  expect_identical(names(a), c("source", "point", "masked", "frames", "frames_no_position",
                               "score_min", "partial"))
  expect_equal(a$masked, c(1L, 0L))
  expect_equal(a$frames_no_position, c(1L, 1L))
  expect_true(all(is.na(a$source)))
  expect_equal(nrow(rows_of(ts)), 3L)
})

test_that("nothing masked is recorded without a message", {
  local_attrition_dialect()
  d <- two_point(gap = integer(0))
  expect_no_message(ts <- read_tracks(d, dialect = "attrition_test"), class = "radiatR_attrition")
  expect_equal(ts@meta$attrition$masked, c(0L, 0L))
  # a load with no dialect has no record
  expect_null(read_tracks(data.frame(id = "c", time = 1:3, x = c(0, 1, 2), y = 0))@meta$attrition)
})

test_that("several files give one record per file, and the message sums them", {
  local_attrition_dialect()
  dir <- withr::local_tempdir()
  utils::write.csv(two_point("a", 2L), file.path(dir, "a.csv"), row.names = FALSE)
  utils::write.csv(two_point("b", 2:3), file.path(dir, "b.csv"), row.names = FALSE)
  msg <- testthat::capture_messages(
    ts <- suppressWarnings(read_tracks_dir(dir, dialect = "attrition_test", on_invalid = "drop")))
  expect_match(msg, "3 of 8 row(s) have no position", fixed = TRUE, all = FALSE)
  expect_match(msg, "Masked points: head 3.", fixed = TRUE, all = FALSE)
  a <- ts@meta$attrition
  expect_identical(a$source, c("a.csv", "a.csv", "b.csv", "b.csv"))
  expect_equal(a$masked, c(1L, 0L, 2L, 0L))
  expect_equal(a$frames_no_position, c(1L, 1L, 2L, 2L))
  expect_equal(nrow(rows_of(ts)), 5L)
})

test_that("the record survives subsetting and is row-bound by c()", {
  local_attrition_dialect()
  load <- function(id) quiet(read_tracks(two_point(id), dialect = "attrition_test",
                                         on_invalid = "drop"))
  a <- load("a"); b <- load("b")
  expect_identical(a["a"]@meta$attrition, a@meta$attrition)
  both <- c(a, b)
  expect_equal(nrow(both@meta$attrition), 4L)
  expect_equal(both@meta$attrition$masked, c(1L, 0L, 1L, 0L))
  # an input without a record adds nothing, whichever side it is on
  plain <- read_tracks(data.frame(id = "c", time = 1:3, x = c(0, 1, 2), y = 0))
  expect_identical(c(plain, a)@meta$attrition, a@meta$attrition)
  expect_identical(c(a, plain)@meta$attrition, a@meta$attrition)
})

test_that("an attrition record of the wrong shape is a dialect error", {
  register_loader_dialect("attrition_bad", function(x, record)
    structure(data.frame(id = "a", time = 1:2, x = c(0, 1), y = 0), attrition = record),
    overwrite = TRUE)
  withr::defer(rm("attrition_bad", envir = radiatR:::.loader_registry))
  load <- function(record) read_tracks(data.frame(z = 1), dialect = "attrition_bad",
                                       dialect_args = list(record = record))
  good <- data.frame(source = NA_character_, point = c("head", "tail"), masked = 0L,
                     frames = 2L, frames_no_position = 0L, score_min = NA_real_,
                     partial = "na", stringsAsFactors = FALSE)
  e <- cond(load(list(masked = 1)))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "attrition", fixed = TRUE)
  # counts are numbers, and the per-file counts are one value
  expect_s3_class(cond(load(transform(good, masked = "0"))), "radiatR_dialect_error")
  expect_s3_class(cond(load(transform(good, frames = c(2L, 3L)))), "radiatR_dialect_error")
  # counts are whole and non-negative, partial is a known mode, points are named
  expect_s3_class(cond(load(transform(good, masked = 1.5))), "radiatR_dialect_error")
  expect_s3_class(cond(load(transform(good, masked = -1L))), "radiatR_dialect_error")
  expect_s3_class(cond(load(transform(good, partial = NA_character_))), "radiatR_dialect_error")
  expect_s3_class(cond(load(transform(good, point = NA_character_))), "radiatR_dialect_error")
  expect_s3_class(cond(load(good[0, ])), "radiatR_dialect_error")
  expect_s3_class(cond(load(good[setdiff(names(good), "frames")])), "radiatR_dialect_error")
  # extra columns are left out, so that records row-bind
  expect_identical(names(load(cbind(good, note = "x"))@meta$attrition), names(good))
})

# ---- DeepLabCut ----------------------------------------------------------------

wasp <- "dlc_single_wasp_predictions.csv"
mice <- "dlc_two_mice_predictions.csv"

test_that("DeepLabCut: a latin-1 body-part name reads as its text", {
  # a re-save by a Windows program writes the header in latin-1
  p <- withr::local_tempfile(fileext = ".csv")
  l <- readLines(fx(wasp))
  l[2] <- gsub("head", "t\u00eate", l[2], fixed = TRUE)
  writeLines(iconv(l, "UTF-8", "latin1"), p, useBytes = TRUE)
  read <- function(path, part)
    rows_of(quiet(read_tracks(path, dialect = "deeplabcut", on_invalid = "drop",
                              dialect_args = list(bodypart = part))))
  d <- read(p, "t\u00eate")
  expect_equal(d[c("x", "y")], read(fx(wasp), "head")[c("x", "y")])
  expect_true("t\u00eate_x" %in% names(d))
})

test_that("DeepLabCut: a predictions file loads as one track in frames", {
  msg <- testthat::capture_messages(
    ts <- suppressWarnings(read_tracks(fx(wasp), dialect = "deeplabcut", fps = 40,
                                       on_invalid = "drop")))
  d <- rows_of(ts)
  expect_equal(nrow(d), 43L)                    # 52 frames, 9 with likelihood 0
  expect_identical(as.character(ids(ts)), "1")
  expect_equal(d$time[1:3], c(0, 1, 2))
  expect_equal(frame_rate(ts), 40)
  expect_error(read_tracks(fx(mice), dialect = "deeplabcut", time_type = "seconds"),
               class = "radiatR_time_type_mismatch")
  # likelihood-weighted centroid of head and stinger in frame 0
  expect_equal(d$x[1], 1084.858797688071)
  expect_equal(d$y[1], 421.386680542047)
  expect_true(all(c("head_x", "head_y", "stinger_x", "stinger_y") %in% names(d)))
  a <- ts@meta$attrition
  expect_identical(a$point, c("head", "stinger"))
  expect_identical(a$source, rep(wasp, 2))
  expect_equal(a$masked, c(9L, 9L))
  expect_equal(a$frames, c(52L, 52L))
  expect_equal(a$frames_no_position, c(9L, 9L))
  expect_match(msg, "deeplabcut: 9 of 52 row(s) have no position (no threshold",
               fixed = TRUE, all = FALSE)
})

test_that("DeepLabCut: frames with no detection stop the default load", {
  e <- cond(suppressMessages(read_tracks(fx(wasp), dialect = "deeplabcut")))
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_equal(e$rows, 44:52)
})

test_that("DeepLabCut: likelihood_min masks points, counts them and removes no row itself", {
  dl <- function(...) quiet(read_tracks(fx(wasp), dialect = "deeplabcut",
                                        dialect_args = list(...), on_invalid = "drop"))
  a <- dl(likelihood_min = 0.5)@meta$attrition
  expect_equal(a$masked, c(14L, 16L))
  expect_equal(a$frames_no_position, c(16L, 16L))
  expect_equal(a$score_min, c(0.5, 0.5))

  m <- dl(likelihood_min = 0.5, partial = "mean")
  expect_equal(m@meta$attrition$frames_no_position, c(14L, 14L))
  expect_equal(nrow(rows_of(m)), 38L)
  # in two kept frames only the stinger was masked: its columns are blank there
  expect_equal(sum(is.na(rows_of(m)$stinger_x)), 2L)
  expect_equal(sum(is.na(rows_of(m)$head_x)), 0L)

  one <- dl(bodypart = "head", likelihood_min = 0.9)
  expect_equal(one@meta$attrition$masked, 18L)
  expect_equal(nrow(rows_of(one)), 34L)
  expect_false("stinger_x" %in% names(rows_of(one)))
})

test_that("DeepLabCut: a multi-animal file gives one track per individual", {
  ts <- read_tracks(fx(mice), dialect = "deeplabcut", fps = 100,
                    dialect_args = list(bodypart = "snout"))
  d <- rows_of(ts)
  expect_setequal(as.character(ids(ts)), c("individual1", "individual2"))
  expect_equal(nrow(d), 40L)
  expect_equal(d$x[d$id == "individual1" & d$time == 0], 1076.505)
  expect_equal(d$x[d$id == "individual2" & d$time == 0], 1123.813)
  expect_equal(max(elapsed_seconds(ts)), 0.19)
  expect_equal(ts@meta$attrition$frames, 40L)

  all_parts <- read_tracks(fx(mice), dialect = "deeplabcut")
  expect_equal(nrow(rows_of(all_parts)), 40L)
  expect_true(all(c("snout_x", "tailend_y") %in% names(rows_of(all_parts))))

  only <- read_tracks(fx(mice), dialect = "deeplabcut",
                      dialect_args = list(individuals = "individual2"))
  expect_identical(as.character(ids(only)), "individual2")
  expect_equal(nrow(rows_of(only)), 20L)

  a <- quiet(read_tracks(fx(mice), dialect = "deeplabcut", on_invalid = "drop",
                         dialect_args = list(likelihood_min = 0.9)))@meta$attrition
  expect_equal(sum(a$masked), 2L)
  expect_equal(a$masked[a$point == "tailend"], 2L)
  expect_equal(a$frames_no_position[1], 2L)
})

test_that("DeepLabCut: selection errors name what the file has", {
  dl <- function(...) cond(read_tracks(fx(mice), dialect = "deeplabcut",
                                       dialect_args = list(...)))
  e <- dl(individuals = "bob")
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "the file has individual1, individual2", fixed = TRUE)
  e <- dl(bodypart = "nose")
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "available: snout, leftear", fixed = TRUE)
  e <- dl(bodypart = c("snout", "snout"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "distinct bodyparts", fixed = TRUE)
  e <- dl(id_col = "x")
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "do not apply to a DeepLabCut file", fixed = TRUE)
  e <- cond(read_tracks(fx(wasp), dialect = "deeplabcut",
                        dialect_args = list(individuals = "individual1")))
  expect_match(conditionMessage(e), "no individuals row", fixed = TRUE)
  e <- dl(individuals = character(0))
  expect_s3_class(e, "radiatR_dialect_error")
})

test_that("DeepLabCut: individuals with different bodyparts need `individuals`", {
  # the last two columns become DLC's holder of unique bodyparts
  p <- edited(mice, function(l) {
    h <- strsplit(l[2:4], ",")
    h[[1]][72:73] <- "single"; h[[2]][72:73] <- "corner"; h[[3]][72:73] <- c("x", "y")
    l[2:4] <- vapply(h, paste, "", collapse = ",")
    l
  })
  e <- cond(read_tracks(p, dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "individual 'individual1' has no x/y columns for bodypart(s): corner",
               fixed = TRUE)
  expect_match(conditionMessage(e), "individuals = c(...)", fixed = TRUE)
  ts <- read_tracks(p, dialect = "deeplabcut", dialect_args = list(individuals = "individual1"))
  expect_identical(as.character(ids(ts)), "individual1")
})

test_that("DeepLabCut: files that are not tracking output are refused by name", {
  e <- cond(read_tracks(fx("dlc_CollectedData_Pranav.csv"), dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "labelled training data", fixed = TRUE)

  img <- edited(wasp, function(l) { l[-(1:3)] <- sub("^[0-9]+", "img.png", l[-(1:3)]); l })
  e <- cond(read_tracks(img, dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "folder of images", fixed = TRUE)

  # a flat <bodypart>_x table is accepted in memory, not as a file
  flat <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("frame,head_x,head_y,head_likelihood", "0,1,2,0.9"), flat)
  e <- cond(read_tracks(flat, dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "is not a DeepLabCut output file", fixed = TRUE)
  expect_match(conditionMessage(e), "'frame', '0'", fixed = TRUE)

  expect_match(conditionMessage(e), "read as a data frame", fixed = TRUE)

  empty <- edited(wasp, function(l) l[1:3])
  e <- cond(read_tracks(empty, dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "has no data rows", fixed = TRUE)

  # newer labelled data carries extra index columns under blank header fields
  blank <- edited(wasp, function(l) { l[3] <- sub("coords,x", "coords,", l[3], fixed = TRUE); l })
  e <- cond(read_tracks(blank, dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "no coords label", fixed = TRUE)
})

test_that("DeepLabCut: a row with too few or too many fields is refused, not padded", {
  short <- edited(wasp, function(l) { l[12] <- "8,100,1"; l })
  e <- cond(read_tracks(short, dialect = "deeplabcut", dialect_args = list(bodypart = "head")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "data row 9 of", fixed = TRUE)
  expect_match(conditionMessage(e), "has 3 field(s); its header names 7", fixed = TRUE)
  long <- edited(wasp, function(l) { l[12] <- paste0(l[12], ",77,88,0.9"); l })
  e <- cond(read_tracks(long, dialect = "deeplabcut", dialect_args = list(bodypart = "head")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "has 10 field(s)", fixed = TRUE)
  # quoted header fields and blank trailing lines are neither
  quoted <- edited(wasp, function(l) {
    l[1:3] <- vapply(strsplit(l[1:3], ","), function(f) paste0('"', f, '"', collapse = ","), "")
    c(l, "", "")
  })
  expect_equal(nrow(rows_of(quiet(read_tracks(quoted, dialect = "deeplabcut",
                                              on_invalid = "drop")))), 43L)
})

test_that("DeepLabCut: a malformed cell is reported by header, row and file", {
  p <- edited(wasp, function(l) { l[5] <- sub("^1,[^,]+,", "1,oops,", l[5]); l })
  e <- cond(read_tracks(p, dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "head_x")
  expect_equal(e$rows, 2L)
  expect_identical(e$dialect, "deeplabcut")
  expect_identical(e$source, basename(p))

  neg <- edited(wasp, function(l) {
    l[5] <- sub(",0.03531652316451073,", ",-0.5,", l[5], fixed = TRUE); l })
  e <- cond(read_tracks(neg, dialect = "deeplabcut"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "head_likelihood")
  expect_identical(e$values, "-0.5")
})

test_that("deeplabcut_multiheader is removed and names its replacement", {
  e <- cond(read_tracks(fx(wasp), dialect = "deeplabcut_multiheader"))
  expect_s3_class(e, "radiatR_removed_dialect")
  expect_match(conditionMessage(e), "dialect = \"deeplabcut\"", fixed = TRUE)
  expect_false("deeplabcut_multiheader" %in% list_loader_dialects())
})

test_that("DeepLabCut: a flat table in memory loads, with ids and a threshold on text", {
  df <- data.frame(id = rep(c("a", "b"), each = 2), frame = c(1, 2, 1, 2),
                   head_x = c(1, 2, 3, 4), head_y = 0,
                   head_likelihood = c("0.95", "1e-3", "0.95", "0.95"),
                   stringsAsFactors = FALSE)
  ts <- quiet(read_tracks(df, dialect = "deeplabcut", on_invalid = "drop",
                          dialect_args = list(likelihood_min = 0.9)))
  expect_equal(nrow(rows_of(ts)), 3L)
  expect_setequal(as.character(ids(ts)), c("a", "b"))
  expect_equal(ts@meta$attrition$masked, 1L)
  expect_true(is.na(ts@meta$attrition$source))
  e <- cond(read_tracks(df, dialect = "deeplabcut", dialect_args = list(id_col = "trak")))
  expect_s3_class(e, "radiatR_dialect_error")
  e <- cond(read_tracks(df[c("id", "frame", "head_x", "head_y")], dialect = "deeplabcut",
                        dialect_args = list(likelihood_min = 0.9)))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "`likelihood_min` was given", fixed = TRUE)
})

test_that("DeepLabCut: a directory of single-animal files needs id_collision = 'namespace'", {
  dir <- withr::local_tempdir()
  file.copy(fx(wasp), file.path(dir, c("a.csv", "b.csv")))
  expect_error(quiet(read_tracks_dir(dir, dialect = "deeplabcut", on_invalid = "drop")),
               "id_collision")
  ts <- quiet(read_tracks_dir(dir, dialect = "deeplabcut", on_invalid = "drop",
                              id_collision = "namespace"))
  expect_setequal(as.character(ids(ts)), c("a::1", "b::1"))
  expect_identical(ts@meta$attrition$source, rep(c("a.csv", "b.csv"), each = 2))
  expect_equal(ts@meta$attrition$masked, rep(9L, 4))
})

test_that("the bundled DeepLabCut example loads with no arguments", {
  path <- system.file("extdata", "dlc_single_wasp_predictions.csv", package = "radiatR")
  expect_no_message(ts <- read_tracks(path, dialect = "deeplabcut"), class = "radiatR_attrition")
  expect_equal(nrow(rows_of(ts)), 300L)
  a <- quiet(read_tracks(path, dialect = "deeplabcut", on_invalid = "drop",
                         dialect_args = list(likelihood_min = 0.9)))@meta$attrition
  expect_equal(a$masked, c(104L, 57L))
  expect_equal(a$frames_no_position, c(104L, 104L))
})

# ---- SLEAP ---------------------------------------------------------------------

nodes3 <- "sleap_three_nodes.analysis.csv"
mice3 <- "sleap_three_mice.analysis.csv"
sleap <- function(...) get("sleap", envir = radiatR:::.loader_registry)(...)

test_that("SLEAP: blank nodes leave a frame without a position, counted per node", {
  msg <- testthat::capture_messages(
    ts <- suppressWarnings(read_tracks(fx(nodes3), dialect = "sleap", fps = 30,
                                       on_invalid = "drop")))
  d <- rows_of(ts)
  expect_equal(nrow(d), 50L)                    # 59 rows, 9 with a blank node
  expect_setequal(as.character(ids(ts)), c("track_0", "track_1"))
  # score-weighted centroid of the three nodes in frame 0
  expect_equal(d$x[d$id == "track_0" & d$time == 0], 490.701857232182)
  expect_equal(frame_rate(ts), 30)
  expect_error(read_tracks(fx(mice3), dialect = "sleap", time_type = "seconds"),
               class = "radiatR_time_type_mismatch")
  a <- ts@meta$attrition
  expect_identical(a$point, c("head", "torso", "tail_base"))
  expect_equal(a$masked, c(8L, 0L, 8L))
  expect_equal(a$frames, rep(59L, 3))
  expect_equal(a$frames_no_position, rep(9L, 3))
  expect_match(msg, "sleap: 9 of 59 row(s) have no position", fixed = TRUE, all = FALSE)
  e <- cond(suppressMessages(read_tracks(fx(nodes3), dialect = "sleap")))
  expect_s3_class(e, "radiatR_invalid_rows")
})

test_that("SLEAP: score_min, partial and node selection", {
  out <- sleap(fx(nodes3), score_min = 0.8)
  expect_identical(attr(out, "time_unit"), "frames")
  expect_equal(nrow(out), 59L)                  # the dialect removes no row
  a <- attr(out, "attrition")
  expect_equal(a$masked, c(39L, 8L, 55L))
  expect_equal(a$frames_no_position, rep(57L, 3))
  expect_equal(sum(is.na(out$x)), 57L)
  m <- attr(sleap(fx(nodes3), score_min = 0.8, partial = "mean"), "attrition")
  expect_lt(m$frames_no_position[1], 57L)

  # a node is matched as its header is normalised, however it is typed
  one <- quiet(read_tracks(fx(nodes3), dialect = "sleap", on_invalid = "drop",
                           dialect_args = list(bodypart = "Tail Base")))
  expect_equal(nrow(rows_of(one)), 51L)
  expect_true("tail_base_x" %in% names(rows_of(one)))
  expect_false("head_x" %in% names(rows_of(one)))

  e <- cond(read_tracks(fx(nodes3), dialect = "sleap", dialect_args = list(bodypart = "wing")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "available: head, torso, tail_base", fixed = TRUE)
  e <- cond(read_tracks(fx(nodes3), dialect = "sleap", dialect_args = list(id_col = "trak")))
  expect_s3_class(e, "radiatR_dialect_error")
})

test_that("SLEAP: tracks and one node load whole, scores above 1 included", {
  expect_no_message(ts <- read_tracks(fx(mice3), dialect = "sleap"), class = "radiatR_attrition")
  d <- rows_of(ts)
  expect_equal(nrow(d), 60L)
  expect_setequal(as.character(ids(ts)), c("AEON3B_NTP", "AEON3B_TP1", "AEON3B_TP2"))
  expect_equal(d$x[d$id == "AEON3B_NTP" & d$time == 0], 773.189331054688)
  expect_equal(ts@meta$attrition$masked, 0L)
  expect_gt(max(utils::read.csv(fx(mice3))$centroid.score), 1)
  expect_identical(attr(sleap(fx(mice3)), "time_unit"), "frames")
})

test_that("SLEAP: a malformed cell is reported by header, row and file", {
  p <- edited(mice3, function(l) {
    l[3] <- sub(",1117.61926269531,", ",oops,", l[3], fixed = TRUE); l })
  e <- cond(read_tracks(p, dialect = "sleap"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_identical(e$columns, "centroid.x")
  expect_equal(e$rows, 2L)
  expect_identical(e$dialect, "sleap")
})

test_that("SLEAP: a threshold needs score columns", {
  df <- data.frame(frame_idx = 1:2, track = "a", head_x = c(1, 2), head_y = 0)
  expect_equal(nrow(rows_of(read_tracks(df, dialect = "sleap"))), 2L)
  e <- cond(read_tracks(df, dialect = "sleap", dialect_args = list(score_min = 0.5)))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "`score_min` was given", fixed = TRUE)
})

test_that("SLEAP: nodes that share a normalised name are refused, as is one named twice", {
  df <- data.frame(frame_idx = 1:2, track = "a", `Tail Base.x` = c(1, 2), `Tail Base.y` = 0,
                   `tail_base.x` = c(5, 6), `tail_base.y` = 0, head.x = 0, head.y = 0,
                   check.names = FALSE)
  e <- cond(sleap(df))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "Tail Base.x, Tail Base.y, tail_base.x, tail_base.y",
               fixed = TRUE)
  e <- cond(sleap(fx(nodes3), bodypart = c("Tail Base", "tail_base", "head")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "distinct nodes", fixed = TRUE)
})

test_that("SLEAP: only frame_idx and frame columns are declared as frames", {
  df <- data.frame(frame_time = c(0.1, 0.2), track = "a", head_x = c(1, 2), head_y = 0)
  expect_null(attr(sleap(df, time_col = "frame_time"), "time_unit"))
  names(df)[1] <- "frame_idx"
  expect_identical(attr(sleap(df), "time_unit"), "frames")
})
