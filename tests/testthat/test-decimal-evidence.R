# A decimal point sniffed from a ';' file must be backed by a role column
# (x, y, time, angle, weight): see .check_decimal_evidence().

cond <- function(expr) tryCatch(expr, error = function(e) e)
semi <- function(lines, env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(lines, p)
  p
}
map <- list(id = "id", time = "t", x = "x", y = "y")
col_of <- function(ts, role) as.data.frame(ts)[[ts@cols[[role]]]]

test_that("a point found only in a label column does not back grouping-shaped x/y", {
  p <- semi(c("id;t;x;y;label",
              "a;1;1.234;2.345;1.5", "a;2;1.240;2.350;2.5", "a;3;1.250;2.360;3.5"))
  e <- cond(read_tracks(p, mapping = map))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_identical(e$columns, c("x", "y"))
  expect_identical(e$evidence, "label")
  expect_identical(e$values[1], "1.234")
  expect_identical(e$source, basename(p))
  expect_identical(e$decimal, ".")
  expect_match(conditionMessage(e), 'read_opts = list(decimal = ".")', fixed = TRUE)
  expect_match(conditionMessage(e), "1234", fixed = TRUE)
  ts <- read_tracks(p, mapping = map, read_opts = list(decimal = "."))
  expect_equal(col_of(ts, "x"), c(1.234, 1.240, 1.250))
})

test_that("a %.3f pixel export backed by its time column loads unchanged", {
  p <- semi(c("id;t;x;y",
              "a;0.000;523.117;401.250", "a;0.040;524.310;402.115",
              "a;0.080;525.002;403.999"))
  ts <- read_tracks(p, mapping = map)
  expect_equal(col_of(ts, "x"), c(523.117, 524.310, 525.002))
})

test_that("a clear point in one role column backs a grouping-shaped one", {
  p <- semi(c("id;t;x;y", "a;1;523.117;0.5", "a;2;524.310;0.6", "a;3;525.002;0.7"))
  ts <- read_tracks(p, mapping = map)
  expect_equal(col_of(ts, "x"), c(523.117, 524.310, 525.002))
})

test_that("grouping-shaped values with no point anywhere are read with a comma, as before", {
  p <- semi(c("id;t;x;y", "a;1;523.117;401.250", "a;2;524.310;402.115"))
  expect_null(radiatR:::.guess_delim(p)$cells)
  e <- cond(read_tracks(p, mapping = map))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_false(inherits(e, "radiatR_ambiguous_decimal"))
})

test_that("integer role columns need no backing", {
  p <- semi(c("id;t;x;y;label", "a;1;523;401;1.5", "a;2;524;402;2.5"))
  ts <- read_tracks(p, mapping = map)
  expect_equal(col_of(ts, "x"), c(523, 524))
})

test_that("a quoted ';' does not shift the evidence to another column", {
  # Split naively, the note's ';' would credit y's 0.5 to a column past the
  # end and x's 1.234 to y, and the load would stop.
  p <- semi(c("id;t;note;x;y", 'a;1;"p;q";1.234;0.5', 'a;2;"r;s";1.240;0.6'))
  ts <- read_tracks(p, mapping = map)
  expect_equal(col_of(ts, "x"), c(1.234, 1.240))
})

test_that("a quoted line break cut by the 20-line sample leaves the tally whole", {
  body <- sprintf("a;%d;1.%03d;0.5;x", 1:18, 1:18)
  p <- semi(c("id;t;x;y;note", body, 'a;19;1.019;0.5;"line', 'break"', "a;20;1.020;0.5;x"))
  g <- radiatR:::.guess_delim(p)
  expect_identical(g$decimal, ".")
  expect_length(g$cells, 5L)
  expect_identical(g$cells[[3]], sprintf("1.%03d", 1:18))   # the cut record is left out
})

test_that("a first record cut by the sample stops unless the mark is given", {
  # its quoted field runs past line 20, so no whole data record is sniffed
  p <- semi(c("id;t;x;y;note", 'a;1;1.234;2.345;"1.5', rep("more", 20), 'end"',
              "a;2;1.240;2.350;x"))
  e <- cond(read_tracks(p, mapping = map))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_length(e$columns, 0L)
  expect_match(conditionMessage(e), "no whole row", fixed = TRUE)
  ts <- read_tracks(p, mapping = map, read_opts = list(decimal = "."))
  expect_equal(col_of(ts, "x"), c(1.234, 1.240))
})

test_that("a mapped weight column with a point backs grouping-shaped x/y", {
  p <- semi(c("id;t;x;y;w", "a;1;523.117;401.250;0.5", "a;2;524.310;402.115;0.7"))
  ts <- read_tracks(p, mapping = c(map, weight = "w"))
  expect_equal(col_of(ts, "x"), c(523.117, 524.310))
})

test_that("read_tracks_dir() checks each file", {
  d <- withr::local_tempdir()
  writeLines(c("id;t;x;y;label", "b;1;1.234;2.345;1.5", "b;2;1.240;2.350;2.5"),
             file.path(d, "bad.csv"))
  e <- cond(read_tracks_dir(d, mapping = map))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_identical(e$source, "bad.csv")
})

test_that("a data frame carries no evidence and is not checked", {
  df <- data.frame(id = "a", t = 1:2, x = c("1.234", "1.240"), y = c("2.345", "2.350"),
                   label = c("1.5", "2.5"))
  ts <- read_tracks(df, mapping = map)
  expect_equal(col_of(ts, "x"), c(1.234, 1.240))
})

test_that("each file of a multi-file import is checked and named", {
  ok <- semi(c("id;t;x;y", "a;0.000;523.117;401.250", "a;0.040;524.310;402.115"))
  bad <- semi(c("id;t;x;y;label", "b;1;1.234;2.345;1.5", "b;2;1.240;2.350;2.5"))
  e <- cond(read_tracks(c(ok, bad), mapping = map))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_identical(e$source, basename(bad))
})

test_that("no evidence is kept when the mark is given or not sniffed", {
  p <- semi(c("id;t;x;y;label", "a;1;1.234;2.345;1.5"))
  expect_null(attr(radiatR:::.read_any(p, decimal = "."), "decimal_evidence"))
  expect_false(is.null(attr(radiatR:::.read_any(p), "decimal_evidence")))
  tab <- semi(c("id\tt\tx\ty\tlabel", "a\t1\t1.234\t2.345\t1.5", "a\t2\t1.240\t2.350\t2.5"))
  expect_null(attr(radiatR:::.read_any(tab), "decimal_evidence"))
  ts <- read_tracks(tab, mapping = map)            # a tab file's point is assumed
  expect_equal(col_of(ts, "x"), c(1.234, 1.240))
})

test_that(".decimal_evidence sorts columns by what their cells show", {
  ev <- radiatR:::.decimal_evidence(list(
    a = c("1.234", "2.345", NA, "7"), b = c("0.5", "1.234"), c = c("x", "1"),
    d = c("1.234.567", "12"), e = c("0:00:00.320", "1.000")))
  expect_identical(ev$point, c("b", "e"))
  expect_identical(names(ev$grouping), c("a", "d"))
  expect_identical(ev$grouping$a, c("1.234", "2.345"))
})
