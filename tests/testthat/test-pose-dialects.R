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
  expect_message(e <- cond(read_tracks(two_point(), dialect = "attrition_test")),
                 class = "radiatR_attrition")
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_equal(e$rows, 2L)

  msg <- testthat::capture_messages(
    ts <- suppressWarnings(read_tracks(two_point(), dialect = "attrition_test",
                                       on_invalid = "drop")))
  expect_equal(sum(grepl("have no position", msg, fixed = TRUE)), 1L)
  expect_match(msg, "attrition_test: 1 of 4 row(s) have no position (no threshold, partial = \"na\")",
               fixed = TRUE, all = FALSE)
  expect_match(msg, "Masked points: head 1.", fixed = TRUE, all = FALSE)
  expect_match(msg, "on_invalid = \"drop\"", fixed = TRUE, all = FALSE)
  expect_match(msg, "partial = \"mean\"", fixed = TRUE, all = FALSE)
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
  # extra columns are left out, so that records row-bind
  expect_identical(names(load(cbind(good, note = "x"))@meta$attrition), names(good))
})
