# A decimal point sniffed from a ';' file must be backed by a role column
# (x, y, time, angle, weight): see .check_decimal_evidence().

cond <- function(expr) tryCatch(expr, error = function(e) e)
semi <- function(lines, env = parent.frame(), ext = ".csv") {
  p <- withr::local_tempfile(fileext = ext, .local_envir = env)
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

# AnyMaze: Time is a duration; a fraction there is a clear point.
am <- function(rows, env = parent.frame())
  semi(c("Time;Centre position X;Centre position Y;Label", rows), env)

test_that("anymaze: grouping-shaped positions backed only by a label stop", {
  p <- am(c("0:00:00;1.234;2.345;1.5", "0:00:01;1.240;2.350;2.5"))
  e <- cond(suppressMessages(read_tracks(p, dialect = "anymaze")))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_identical(e$dialect, "anymaze")
  expect_identical(e$columns, c("Centre position X", "Centre position Y"))
  ts <- suppressMessages(read_tracks(p, dialect = "anymaze", read_opts = list(decimal = ".")))
  expect_equal(col_of(ts, "x"), c(1.234, 1.240))
})

test_that("anymaze: a duration with a fraction backs %.3f positions", {
  p <- am(c("0:00:00.000;523.117;401.250;1.5", "0:00:00.040;524.310;402.115;2.5"))
  ts <- suppressMessages(read_tracks(p, dialect = "anymaze"))
  expect_equal(col_of(ts, "x"), c(523.117, 524.310))
})

ev_file <- function(rows, env = parent.frame()) {
  # minimal EthoVision text export: header line count, preamble, names, units
  semi(c('"Number of header lines:";"5"', '"Experiment";"x"', "",
         '"Trial time";"X center";"Y center";"Label"', '"s";"mm";"mm";""', rows), env)
}

test_that("ethovision: grouping-shaped centres backed only by a label stop", {
  p <- ev_file(c('"0";"1.234";"2.345";"1.5"', '"1";"1.240";"2.350";"2.5"'))
  e <- cond(suppressMessages(read_tracks(p, dialect = "ethovision")))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_identical(e$dialect, "ethovision")
  ts <- suppressMessages(read_tracks(p, dialect = "ethovision", read_opts = list(decimal = ".")))
  expect_equal(col_of(ts, "x"), c(1.234, 1.240))
})

test_that("ethovision: a time with a fraction backs %.3f centres", {
  p <- ev_file(c('"0.000";"523.117";"401.250";"1.5"', '"0.040";"524.310";"402.115";"2.5"'))
  ts <- suppressMessages(read_tracks(p, dialect = "ethovision"))
  expect_equal(col_of(ts, "x"), c(523.117, 524.310))
})

test_that("trackmate (a .read_any() dialect) checks the evidence, v7 descriptor rows too", {
  p <- semi(c("LABEL;TRACK_ID;FRAME;POSITION_X;POSITION_Y",
              "1.5;a;0;1.234;2.345", "2.5;a;1;1.240;2.350"))
  e <- cond(read_tracks(p, dialect = "trackmate"))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_identical(e$dialect, "trackmate")
  v7 <- semi(c("LABEL;TRACK_ID;FRAME;POSITION_X;POSITION_Y",
               "Label;Track ID;Frame;X;Y", "Label;Track ID;Frame;X;Y", ";;;(micron);(micron)",
               "1.5;a;0;1.234;2.345", "2.5;a;1;1.240;2.350"))
  e <- cond(read_tracks(v7, dialect = "trackmate"))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  ts <- read_tracks(v7, dialect = "trackmate", read_opts = list(decimal = "."))
  expect_equal(col_of(ts, "x"), c(1.234, 1.240))
})

# The remaining sniffing dialects: each stops when only a label shows the point.
dialect_stops <- function(lines, dialect, ext = ".csv") {
  p <- semi(lines, ext = ext)
  e <- cond(read_tracks(p, dialect = dialect))
  expect_s3_class(e, "radiatR_ambiguous_decimal")
  expect_identical(e$dialect, dialect)
  expect_identical(e$evidence, if (dialect == "toxtrac") "Label" else "lab")
  expect_no_error(read_tracks(p, dialect = dialect, read_opts = list(decimal = ".")))
}

test_that("idtrackerai_wide: grouping-shaped positions backed only by a label stop", {
  dialect_stops(c("seconds;x1;y1;lab", "0;1.234;2.345;1.5", "1;1.240;2.350;2.5"),
                "idtrackerai_wide")
})

test_that("idtrackerai_wide: a time with a point backs %.3f positions", {
  p <- semi(c("seconds;x1;y1;lab", "0.000;1.234;2.345;1.5", "0.036;1.240;2.350;2.5"))
  expect_no_error(read_tracks(p, dialect = "idtrackerai_wide"))
})

test_that("toxtrac: header path stops on grouping-shaped positions backed by a label", {
  dialect_stops(c("Time (sec);Arena;Track;Pos. X (mm);Pos. Y (mm);Label",
                  "0;1;1;1.234;2.345;1.5", "1;1;1;1.240;2.350;2.5"),
                "toxtrac", ext = ".txt")
})

test_that("trex: grouping-shaped positions backed only by a label stop", {
  dialect_stops(c("frame;X;Y;lab", "0;1.234;2.345;1.5", "1;1.240;2.350;2.5"), "trex")
})

test_that("sleap: grouping-shaped positions backed only by a label stop", {
  dialect_stops(c("track;frame_idx;lab;centroid.x;centroid.y",
                  "t0;0;1.5;1.234;2.345", "t0;1;2.5;1.240;2.350"), "sleap")
})

test_that("sleap: a score column with a clear point backs %.3f node positions", {
  p <- semi(c("track;frame_idx;instance.score;centroid.x;centroid.y;centroid.score",
              "t0;0;0.9;1.234;2.345;0.8", "t0;1;0.9;1.240;2.350;0.7"))
  expect_no_error(read_tracks(p, dialect = "sleap"))
})

test_that("ethovision: a given mark keeps no evidence and skips the pass", {
  hdr <- c('"Number of header lines:";"4"', '"Experiment";"x"',
           '"Trial time";"Recording time";"X center";"Y center"', '"s";"s";"cm";"cm"')
  p <- semi(c(hdr, '"0.000";"0.000";"1.234";"2.345"', '"0.040";"0.040";"1.240";"2.350"'),
            ext = ".txt")
  expect_false(is.null(attr(radiatR:::.read_ethovision(p), "decimal_evidence")))
  d <- radiatR:::.read_ethovision(p, decimal = ".")
  expect_null(attr(d, "decimal_evidence"))
  expect_identical(attr(d, "decimal"), ".")
})
