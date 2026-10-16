# Zone dialects (EthoVision XT, ANY-maze) and the per-file `source_meta` record.
# fixtures/real/README.md gives the source and licence of the real excerpts.

fx <- function(f) testthat::test_path("fixtures", "real", f)
cond <- function(expr) tryCatch(expr, error = function(e) e)
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
rows_of <- function(ts) as.data.frame(ts)

# ---- source_meta -------------------------------------------------------------

# A dialect that reads `id,time,x,y` and keeps the file's first comment line.
local_meta_dialect <- function(source_meta = NULL, env = parent.frame()) {
  register_loader_dialect("meta_test", function(x) {
    first <- sub("^# *", "", readLines(x, n = 1L))
    d <- utils::read.csv(x, comment.char = "#")
    attr(d, "source_meta") <- source_meta %||% list(note = first)
    d
  }, overwrite = TRUE)
  withr::defer(rm("meta_test", envir = radiatR:::.loader_registry), envir = env)
}
meta_file <- function(dir, name, note, id = "a") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  p <- file.path(dir, name)
  writeLines(c(paste("#", note), "id,time,x,y", sprintf("%s,1,0.1,0.2", id),
               sprintf("%s,2,0.2,0.3", id)), p)
  p
}

test_that("read_tracks() keeps a dialect's source_meta as one record for the file", {
  local_meta_dialect()
  p <- meta_file(withr::local_tempdir(), "trial.csv", "arena 1")
  ts <- read_tracks(p, dialect = "meta_test")
  expect_identical(ts@meta$source_meta,
                   list(list(source = "trial.csv", path = p, note = "arena 1")))
  expect_null(attr(ts@data, "source_meta"))
})

test_that("files of the same name in different folders each keep their record", {
  local_meta_dialect()
  dir <- withr::local_tempdir()
  p <- c(meta_file(file.path(dir, "day1"), "trial.csv", "arena 1", "a"),
         meta_file(file.path(dir, "day2"), "trial.csv", "arena 2", "b"))
  sm <- read_tracks(p, dialect = "meta_test")@meta$source_meta
  expect_null(names(sm))
  expect_identical(vapply(sm, `[[`, "", "source"), c("trial.csv", "trial.csv"))
  expect_identical(vapply(sm, `[[`, "", "path"), p)
  expect_identical(vapply(sm, `[[`, "", "note"), c("arena 1", "arena 2"))
})

test_that("c() concatenates the records of its inputs, and a subset keeps them", {
  local_meta_dialect()
  dir <- withr::local_tempdir()
  a <- read_tracks(meta_file(dir, "a.csv", "first", "a"), dialect = "meta_test")
  b <- read_tracks(meta_file(dir, "b.csv", "second", "b"), dialect = "meta_test")
  expect_identical(vapply(c(a, b)@meta$source_meta, `[[`, "", "note"), c("first", "second"))
  expect_identical(a[1, ]@meta$source_meta, a@meta$source_meta)
})

test_that("a load without source_meta has no such entry", {
  ts <- read_tracks(data.frame(id = "a", time = 1:2, x = c(.1, .2), y = c(.2, .3)))
  expect_false("source_meta" %in% names(ts@meta))
})

test_that("a source_meta that is not a named list is a dialect error", {
  p <- meta_file(withr::local_tempdir(), "trial.csv", "arena 1")
  for (bad in list("text", list(1, 2), list(source = "x"), list(a = 1, a = 2),
                   data.frame(a = 1))) {
    e <- local({
      local_meta_dialect(bad)
      cond(read_tracks(p, dialect = "meta_test"))
    })
    expect_s3_class(e, "radiatR_dialect_error")
    expect_match(conditionMessage(e), "source_meta")
  }
})

# ---- EthoVision reader ---------------------------------------------------------

ev_read <- function(p) {
  d <- radiatR:::.read_ethovision(p)
  attr(d, "decimal_evidence") <- NULL
  d
}
# The lines of an EthoVision fixture (latin-1 in the file) as UTF-8 text.
ev_lines <- function(f = "ethovision_trial.txt")
  iconv(readLines(fx(f), warn = FALSE), "latin1", "UTF-8")
# A file holding `lines` in `encoding`, with the CRLF line ends EthoVision writes.
ev_file <- function(lines, encoding = "latin1", env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".txt", .local_envir = env)
  text <- paste0(enc2utf8(lines), "\r\n", collapse = "")
  bom <- switch(encoding, "UTF-16LE" = as.raw(c(0xff, 0xfe)),
                "UTF-8-BOM" = as.raw(c(0xef, 0xbb, 0xbf)))
  con <- file(p, open = "wb")
  if (!is.null(bom)) writeBin(bom, con)
  writeBin(iconv(text, "UTF-8", sub("-BOM$", "", encoding), toRaw = TRUE)[[1L]], con)
  close(con)
  p
}
# The data lines of the fixture (after its 33 header lines) rewritten by `edit`.
ev_data_edited <- function(edit, env = parent.frame()) {
  l <- ev_lines()
  ev_file(c(l[1:33], edit(l[-(1:33)])), env = env)
}

test_that("EthoVision reader: the real export is a text table with its header block", {
  d <- ev_read(fx("ethovision_trial.txt"))
  expect_identical(dim(d), c(71L, 10L))
  expect_identical(names(d), c("Trial time", "Recording time", "X center", "Y center", "Area",
                               "Areachange", "Elongation", "Distance moved", "Velocity",
                               "Result 1"))
  expect_true(all(vapply(d, is.character, logical(1))))
  expect_identical(unlist(d[1, 1:4], use.names = FALSE),
                   c("1.501", "0.000", "-47.5671", "36.301"))
  expect_identical(attr(d, "decimal"), ".")
  lay <- attr(d, "ethovision")
  expect_identical(lay$header[["Subject name"]], "Subject 1")
  expect_identical(lay$header[["Start time"]], "02/09/2014 18:04:58,4")
  expect_identical(lay$header[["Subject not found"]], "3,6 %")
  expect_identical(lay$header[["Video file"]], "")
  expect_false("" %in% names(lay$header))
  expect_identical(lay$units[["X center"]], "(mm)")
  expect_identical(lay$units[["Area"]], "(mm\u00b2)")
  expect_identical(lay$units[["Result 1"]], "")
})

test_that("EthoVision reader: \"-\" is a missing value and nothing else is", {
  d <- ev_read(fx("ethovision_trial.txt"))
  expect_identical(sum(is.na(d[["X center"]])), 31L)
  expect_identical(is.na(d[["X center"]]), is.na(d[["Y center"]]))
  expect_true(is.na(d[["Distance moved"]][1]))
  expect_false(anyNA(d[["Trial time"]]))
  p <- ev_data_edited(function(x) sub("^1\\.501;", "-1.501;", sub(";-47\\.5671;", ";--;", x)))
  expect_identical(unlist(ev_read(p)[1, c(1, 3)], use.names = FALSE), c("-1.501", "--"))
})

test_that("EthoVision reader: the decimal mark is that of the data rows", {
  p <- ev_data_edited(function(x) gsub(".", ",", x, fixed = TRUE))
  d <- ev_read(p)
  expect_identical(attr(d, "decimal"), ",")
  expect_identical(d[["Trial time"]][1], "1,501")
  # one decimal point among decimal commas is not grounds to read commas
  mixed <- ev_data_edited(function(x) {
    x <- gsub(".", ",", x, fixed = TRUE)
    x[2] <- sub("^2,002;", "2.002;", x[2])
    x
  })
  expect_identical(attr(ev_read(mixed), "decimal"), ".")
})

test_that("EthoVision reader: other separators and encodings give the same table", {
  ref <- ev_read(fx("ethovision_trial.txt"))
  l <- ev_lines()
  expect_identical(ev_read(ev_file(l)), ref)
  expect_identical(ev_read(ev_file(gsub(";", ",", l, fixed = TRUE))), ref)
  expect_identical(ev_read(ev_file(gsub(";", "\t", l, fixed = TRUE))), ref)
  for (enc in c("UTF-8", "UTF-8-BOM", "UTF-16LE"))
    expect_identical(ev_read(ev_file(l, enc)), ref, info = enc)
  # no trailing separator
  expect_identical(ev_read(ev_file(sub(";$", "", l))), ref)
})

test_that("EthoVision reader: a file that is not an export is refused by name", {
  flat <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Trial time,X center,Y center", "0,1,2"), flat)
  empty <- withr::local_tempfile(fileext = ".txt")
  file.create(empty)
  for (p in c(flat, empty)) {
    e <- cond(ev_read(p))
    expect_s3_class(e, "radiatR_dialect_error")
    expect_match(conditionMessage(e), "not an EthoVision text export")
    expect_identical(e$source, basename(p))
  }
  l <- ev_lines()
  short <- cond(ev_read(ev_file(l[1:20])))
  expect_s3_class(short, "radiatR_dialect_error")
  expect_match(conditionMessage(short), "33 header lines; it has 20")
  none <- cond(ev_read(ev_file(l[1:33])))
  expect_s3_class(none, "radiatR_dialect_error")
  expect_match(conditionMessage(none), "no data rows")
})

test_that("EthoVision reader: the header-line count is a whole number of at least 3", {
  l <- ev_lines()
  count <- function(n, env = parent.frame())
    ev_file(replace(l, 1, sprintf("\"Number of header lines:\";\"%s\";", n)), env = env)
  for (bad in c("33.5", "33abc", "", "99999999999")) {
    e <- cond(ev_read(count(bad)))
    expect_s3_class(e, "radiatR_dialect_error")
    expect_match(conditionMessage(e), sprintf("is '%s', not a whole number", bad), fixed = TRUE)
  }
  e <- cond(ev_read(count("2")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "gives 2 header lines; an export has at least 3")
})

test_that("EthoVision reader: a row cut off or over-long stops the read, naming its line", {
  e <- cond(ev_read(fx("ethovision_truncated.txt")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "line 105 of 'ethovision_truncated.txt' has 3 field(s)",
               fixed = TRUE)
  expect_match(conditionMessage(e), "names row has 11")
  long <- cond(ev_read(ev_data_edited(function(x) { x[3] <- paste0(x[3], "9;"); x })))
  expect_match(conditionMessage(long), "line 36 of .* has 12 field")
  blank <- cond(ev_read(ev_data_edited(function(x) append(x, "", 2))))
  expect_match(conditionMessage(blank), "line 36 of ")
  # blank lines after the last row are not rows
  ref <- ev_read(fx("ethovision_trial.txt"))
  expect_identical(ev_read(ev_file(c(ev_lines(), "", ""))), ref)
})

test_that("EthoVision reader: a value after the last column is refused, not dropped", {
  e <- cond(ev_read(ev_data_edited(function(x) { x[3] <- sub(";$", ";999", x[3]); x })))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "line 36 of .* has a value after its last column")
})

test_that("EthoVision reader: the units row must match the names row", {
  l <- ev_lines()
  for (bad in c(sub(";$", "", l[33]), paste0(l[33], "\"x\";"))) {
    e <- cond(ev_read(ev_file(replace(l, 33, bad))))
    expect_s3_class(e, "radiatR_dialect_error")
    expect_match(conditionMessage(e), "line 33 of .* has \\d+ field\\(s\\); the names row has")
  }
})

test_that("EthoVision reader: column names must be present and distinct", {
  l <- ev_lines()
  for (bad in list(sub("\"Y center\"", "\"X center\"", l[32]), sub("\"Area\"", "\"\"", l[32]))) {
    e <- cond(ev_read(ev_file(replace(l, 32, bad))))
    expect_s3_class(e, "radiatR_dialect_error")
    expect_match(conditionMessage(e), "line 32 of .* should name the columns")
  }
})

# ---- EthoVision dialect --------------------------------------------------------

ev_tracks <- function(x = fx("ethovision_trial.txt"), ..., on_invalid = "drop")
  quiet(read_tracks(x, dialect = "ethovision", dialect_args = list(...),
                    on_invalid = on_invalid))
# The center positions of the real excerpt as numbers (NA where "-").
ev_center <- function() {
  d <- ev_read(fx("ethovision_trial.txt"))
  list(x = as.numeric(d[["X center"]]), y = as.numeric(d[["Y center"]]))
}
# The real excerpt with nose and tail points added (not written by EthoVision):
# nose = center + (1, 2), tail = center - (1, 2); the nose has no x in row 2.
ev_multipoint <- function(env = parent.frame()) {
  l <- ev_lines()
  ctr <- ev_center()
  cell <- function(v) ifelse(is.na(v), "\"-\"", sprintf("%.4f", v))
  nose_x <- ctr$x + 1
  nose_x[2] <- NA
  add <- paste(cell(nose_x), cell(ctr$y + 2), cell(ctr$x - 1), cell(ctr$y - 2), sep = ";")
  ev_file(c(l[1:31],
            paste0(l[32], "\"X nose\";\"Y nose\";\"X tail\";\"Y tail\";"),
            paste0(l[33], "\"(mm)\";\"(mm)\";\"(mm)\";\"(mm)\";"),
            paste0(l[-(1:33)], add, ";")), env = env)
}

test_that("EthoVision: the real export loads as one track in seconds", {
  ts <- ev_tracks()
  d <- rows_of(ts)
  expect_identical(nrow(d), 40L)
  expect_identical(unique(as.character(d[[ts@cols$id]])), "1")
  expect_equal(d[[ts@cols$time]][1:2], c(1.501, 2.002))
  expect_equal(c(d[[ts@cols$x]][1], d[[ts@cols$y]][1]), c(-47.5671, 36.301))
  expect_identical(ts@meta$time_unit, "seconds")
  expect_true(all(c("center_x", "center_y") %in% names(d)))
  expect_equal(d$center_x, d[[ts@cols$x]])
})

test_that("EthoVision: the header block and units are kept as source_meta", {
  sm <- ev_tracks()@meta$source_meta
  expect_length(sm, 1L)
  expect_identical(sm[[1]]$source, "ethovision_trial.txt")
  expect_identical(sm[[1]]$header[["Subject name"]], "Subject 1")
  expect_identical(sm[[1]]$header[["Trial name"]], "Trial     1")
  expect_identical(sm[[1]]$units[["X center"]], "(mm)")
})

test_that("EthoVision: samples without a position are counted and stop the default load", {
  a <- ev_tracks()@meta$attrition
  expect_identical(a$point, "center")
  expect_identical(c(a$masked, a$frames, a$frames_no_position), c(31L, 71L, 31L))
  msgs <- list()
  e <- withCallingHandlers(
    cond(read_tracks(fx("ethovision_trial.txt"), dialect = "ethovision")),
    radiatR_attrition = function(m) {
      msgs[[length(msgs) + 1L]] <<- conditionMessage(m)
      invokeRestart("muffleMessage")
    })
  expect_s3_class(e, "radiatR_invalid_rows")
  expect_length(msgs, 1L)
  expect_match(msgs[[1]], "31 of 71 row")
})

test_that("EthoVision: a malformed cell is reported by header, row and file", {
  p <- ev_data_edited(function(x) sub(";36\\.301;", ";36.3o1;", x))
  e <- cond(read_tracks(p, dialect = "ethovision", on_invalid = "drop"))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_match(conditionMessage(e), "Column 'Y center'")
  expect_match(conditionMessage(e), "rows: 1\n")
  expect_match(conditionMessage(e), "36.3o1", fixed = TRUE)
})

test_that("EthoVision: a decimal-comma export gives the same positions", {
  comma <- ev_data_edited(function(x) gsub(".", ",", x, fixed = TRUE))
  expect_equal(rows_of(ev_tracks(comma))$center_x, rows_of(ev_tracks())$center_x)
  # a decimal point in a decimal-comma file is malformed, not a number
  mixed <- ev_data_edited(function(x) {
    x <- gsub(".", ",", x, fixed = TRUE)
    x[2] <- sub("^2,002;", "2.002;", x[2])
    x
  })
  expect_s3_class(cond(ev_tracks(mixed)), "radiatR_malformed_values")
})

test_that("EthoVision: read_opts$decimal overrides the data-row sniff", {
  # A decimal-comma export with one decimal point in an unused column
  # (Elongation): the sniff reads points, so the positions stop.
  p <- ev_data_edited(function(x) {
    x <- gsub(".", ",", x, fixed = TRUE)
    x[1] <- sub("0,616467", "0.5", x[1], fixed = TRUE)
    x
  })
  expect_identical(attr(ev_read(p), "decimal"), ".")
  expect_s3_class(cond(ev_tracks(p)), "radiatR_malformed_values")
  expect_equal(rows_of(ev_tracks(p, read_opts = list(decimal = ",")))$center_x,
               rows_of(ev_tracks())$center_x)
})

test_that("EthoVision: read_opts$delim must be the export's own separator", {
  expect_equal(rows_of(ev_tracks(read_opts = list(delim = ";")))$center_x,
               rows_of(ev_tracks())$center_x)
  e <- cond(ev_tracks(read_opts = list(delim = ",")))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "is separated by ';', not by the read_opts delimiter ','",
               fixed = TRUE)
  expect_identical(c(e$arg, e$option, e$refused_by), c("read_opts", "delim", "ethovision"))
  expect_s3_class(cond(ev_tracks(read_opts = list(sheet = 1))), "radiatR_invalid_argument")
})

test_that("EthoVision: the time column is Trial time, in seconds, unless named", {
  expect_equal(rows_of(ev_tracks(time_col = "Recording time"))[[2]][1:2], c(0, 0.5))
  expect_equal(rows_of(ev_tracks(time_col = "recording_time"))[[2]][1:2], c(0, 0.5))
  l <- ev_lines()
  minutes <- ev_file(replace(l, 33, sub("^\"\\(s\\)\"", "\"(min)\"", l[33])))
  e <- cond(ev_tracks(minutes))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "'Trial time' is in '(min)'", fixed = TRUE)
  expect_identical(nrow(rows_of(ev_tracks(minutes, time_col = "Recording time"))), 40L)
  for (bad in list("Elapsed", c("Trial time", "Recording time"), 1))
    expect_s3_class(cond(ev_tracks(time_col = bad)), "radiatR_dialect_error")
  no_time <- ev_file(replace(l, 32, sub("Trial time", "Clock", l[32])))
  expect_match(conditionMessage(cond(ev_tracks(no_time))), "no `Trial time` column")
})

test_that("EthoVision: bodypart selects points; the default is the center", {
  p <- ev_multipoint()
  ctr <- ev_center()
  keep <- !is.na(ctr$x)
  d <- rows_of(ev_tracks(p))
  expect_equal(d$center_x, ctr$x[keep])
  expect_true(all(c("nose_x", "nose_y", "tail_x", "tail_y") %in% names(d)))
  for (bp in list("center", "centre", "Center"))
    expect_equal(rows_of(ev_tracks(p, bodypart = bp))$center_x, ctr$x[keep])
  nose <- ev_tracks(p, bodypart = "Nose")
  expect_equal(rows_of(nose)[[nose@cols$x]], (ctr$x + 1)[keep][-2])
  expect_identical(nose@meta$attrition$masked, 32L)
})

test_that("EthoVision: several points average; one missing coordinate masks the point", {
  p <- ev_multipoint()
  ctr <- ev_center()
  keep <- !is.na(ctr$x)
  both <- ev_tracks(p, bodypart = c("nose", "tail"))
  # the added points are written to four decimals
  expect_equal(rows_of(both)[[both@cols$x]], ctr$x[keep][-2], tolerance = 1e-4)
  expect_equal(rows_of(both)[[both@cols$y]], ctr$y[keep][-2], tolerance = 1e-4)
  expect_identical(both@meta$attrition$masked, c(32L, 31L))
  expect_identical(both@meta$attrition$frames_no_position, c(32L, 32L))
  # row 2: the nose has a y but no x, so the position is the tail's alone
  mean <- ev_tracks(p, bodypart = c("nose", "tail"), partial = "mean")
  expect_equal(rows_of(mean)[[mean@cols$x]][2], ctr$x[2] - 1)
  expect_equal(rows_of(mean)[[mean@cols$y]][2], ctr$y[2] - 2)
  expect_identical(nrow(rows_of(mean)), 40L)
})

test_that("EthoVision: selection errors say what the table has, and zone is refused", {
  p <- ev_multipoint()
  e <- cond(ev_tracks(p, bodypart = "paw"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "body point(s) not found: paw; available: center, nose, tail",
               fixed = TRUE)
  expect_s3_class(cond(ev_tracks(p, bodypart = c("nose", "nose"))), "radiatR_dialect_error")
  z <- cond(ev_tracks(p, zone = "nose"))
  expect_s3_class(z, "radiatR_dialect_error")
  expect_match(conditionMessage(z), "`zone` is now `bodypart`")
  expect_match(conditionMessage(cond(ev_tracks(partial = "some"))), "should be one of")
})

test_that("EthoVision: a table without a center point needs bodypart", {
  df <- data.frame(`Trial time` = c(0, 0.5), `X nose` = c(1, 2), `Y nose` = c(3, 4),
                   check.names = FALSE)
  e <- cond(ev_tracks(df))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "no center point; .*`bodypart`: nose")
  expect_equal(rows_of(ev_tracks(df, bodypart = "nose"))$nose_x, c(1, 2))
  none <- cond(ev_tracks(data.frame(`Trial time` = 0, x = 1, y = 2, check.names = FALSE)))
  expect_match(conditionMessage(none), "no `X <point>` / `Y <point>` column pairs")
})

test_that("EthoVision: a data frame may name an id column, kept as written", {
  df <- data.frame(`Trial time` = c(0, 0.5, 0, 0.5), `X center` = c(1, 2, 3, 4),
                   `Y center` = c(1, 1, 2, 2), Fish = c("1.1", "1.1", "1.10", "1.10"),
                   check.names = FALSE)
  ts <- ev_tracks(df, id_col = "fish")
  expect_identical(sort(unique(as.character(rows_of(ts)[[ts@cols$id]]))), c("1.1", "1.10"))
  expect_identical(ts@meta$time_unit, "seconds")
  expect_false("source_meta" %in% names(ts@meta))
  one <- ev_tracks(df[1:2, ])
  expect_identical(unique(as.character(rows_of(one)[[one@cols$id]])), "1")
  e <- cond(ev_tracks(id_col = "Trial time"))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "`id_col` does not apply to an EthoVision file")
})

test_that("EthoVision: a table the caller read still has \"-\" for a missing value", {
  l <- ev_lines()
  df <- utils::read.csv2(text = l[c(32, 34:104)], check.names = FALSE, dec = ".")
  expect_type(df[["X center"]], "character")
  ts <- ev_tracks(df)
  expect_identical(nrow(rows_of(ts)), 40L)
  expect_identical(ts@meta$attrition$masked, 31L)
  df[["X center"]][1] <- "--"
  expect_s3_class(cond(ev_tracks(df)), "radiatR_malformed_values")
  # read.csv(stringsAsFactors = TRUE) gives factors, with "-" a level
  fac <- utils::read.csv2(text = l[c(32, 34:104)], check.names = FALSE, dec = ".",
                          stringsAsFactors = TRUE)
  expect_s3_class(fac[["X center"]], "factor")
  expect_identical(nrow(rows_of(ev_tracks(fac))), 40L)
})

test_that("EthoVision: columns that are one column once normalised are refused", {
  l <- ev_lines()
  two_times <- ev_file(replace(l, 32, sub("\"Recording time\"", "\"Trial-Time\"", l[32])))
  e <- cond(ev_tracks(two_times))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "same column .*: Trial time, Trial-Time")
  both <- data.frame(`Trial time` = 0:1, `X center` = 1:2, `Y center` = 1:2,
                     `X centre` = 3:4, `Y centre` = 3:4, check.names = FALSE)
  e <- cond(ev_tracks(both))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "both X center and X centre; they are the same point")
})

test_that("EthoVision: a sample with no time is an error, not a dropped row", {
  e <- cond(ev_tracks(ev_data_edited(function(x) sub("^2\\.002;", "\"-\";", x))))
  expect_s3_class(e, "radiatR_invalid_tracks")
  expect_match(conditionMessage(e), "missing or non-finite time")
})

test_that("EthoVision: a folder of trials needs id_collision = 'namespace'", {
  p <- file.path(withr::local_tempdir(), c("trial1.txt", "trial2.txt"))
  file.copy(fx("ethovision_trial.txt"), p)
  expect_error(quiet(read_tracks(p, dialect = "ethovision", on_invalid = "drop")),
               "id_collision")
  ts <- quiet(read_tracks(p, dialect = "ethovision", on_invalid = "drop",
                          id_collision = "namespace"))
  expect_identical(length(unique(rows_of(ts)[[ts@cols$id]])), 2L)
  expect_identical(vapply(ts@meta$source_meta, `[[`, "", "path"), p)
  expect_identical(sum(ts@meta$attrition$masked), 62L)
})

# ---- durations -----------------------------------------------------------------

duration <- function(v, decimal = ".")
  radiatR:::.parse_duration(v, "Time", "f.csv", "anymaze", decimal)

test_that("a duration H:MM:SS.fff is read as seconds", {
  expect_equal(duration(c("0:00:00.000", "0:00:00.320", "0:01:00.016", "1:02:03.5",
                          "100:00:00", " 0:00:01 ")),
               c(0, 0.32, 60.016, 3723.5, 360000, 1))
  expect_equal(duration(c("0:00:00,320", "1:02:03,5"), decimal = ","), c(0.32, 3723.5))
  expect_identical(duration(c("0:00:01", "", NA, "NA")), c(1, NA, NA, NA))
})

test_that("plain numbers are refused, as text or as numbers: their unit is unknown", {
  for (v in list(c("0", "0.32", "60"), c(0, 0.5), 1:2)) {
    e <- cond(duration(v))
    expect_s3_class(e, "radiatR_malformed_values")
    expect_match(conditionMessage(e), "as.difftime(<seconds>, units = \"secs\")", fixed = TRUE)
  }
  expect_equal(duration(as.difftime(c(0, 0.5), units = "secs")), c(0, 0.5))
})

test_that("anything else is malformed, and the two forms are not mixed", {
  for (bad in list("0:60:00", "0:00:61", "12:30", "-0:00:01", "0:0:01", "noon", "0:00:01.",
                   "0:00:01,5")) {
    e <- cond(duration(c("0:00:00.000", bad)))
    expect_s3_class(e, "radiatR_malformed_values")
    expect_match(conditionMessage(e), "rows: 2\n")
    expect_match(conditionMessage(e), "a duration H:MM:SS.fff")
  }
  e <- cond(duration(c("0:00:01", "2", "0:00:03", "x")))
  expect_match(conditionMessage(e), "rows: 2, 4\n")
  expect_s3_class(cond(duration(c("1", "x"))), "radiatR_malformed_values")
  expect_s3_class(cond(duration("0:00:01.5", decimal = ",")), "radiatR_malformed_values")
})

# ---- ANY-maze dialect ----------------------------------------------------------

am_fixture <- function() testthat::test_path("fixtures", "anymaze_synthetic.csv")
am_tracks <- function(x = am_fixture(), ..., on_invalid = "drop")
  quiet(read_tracks(x, dialect = "anymaze", dialect_args = list(...), on_invalid = on_invalid))
# A copy of the synthetic ANY-maze file with its lines rewritten by `edit`.
am_edited <- function(edit, env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(edit(readLines(am_fixture())), p)
  p
}

test_that("ANY-maze: the export layout loads as one track in seconds", {
  ts <- am_tracks()
  d <- rows_of(ts)
  expect_identical(nrow(d), 11L)
  expect_identical(unique(as.character(d[[ts@cols$id]])), "1")
  expect_equal(d[[ts@cols$time]], c(0.32, 0.352, 0.384, 1, 2.016, 5.504, 10, 30.25, 59.984,
                                    60.016, 3723.5))
  expect_equal(c(d[[ts@cols$x]][1], d[[ts@cols$y]][1]), c(410, 352))
  expect_identical(ts@meta$time_unit, "seconds")
  expect_false("source_meta" %in% names(ts@meta))
  expect_false(any(grepl("^in_|^In ", names(d))))
})

test_that("ANY-maze: the blank first sample is counted, not stripped", {
  a <- am_tracks()@meta$attrition
  expect_identical(a$point, "center")
  expect_identical(c(a$masked, a$frames, a$frames_no_position), c(1L, 12L, 1L))
  e <- cond(quiet(read_tracks(am_fixture(), dialect = "anymaze")))
  expect_s3_class(e, "radiatR_invalid_rows")
  # a first row that has a position is a sample like any other
  full <- am_edited(function(x) sub("^0:00:00.000,,,", "0:00:00.000,409,351,", x))
  expect_identical(nrow(rows_of(am_tracks(full, on_invalid = "error"))), 12L)
})

test_that("the attrition message offers partial = 'mean' only where points can be averaged", {
  said <- function(...) {
    msg <- NULL
    withCallingHandlers(cond(read_tracks(...)), radiatR_attrition = function(m) {
      msg <<- conditionMessage(m)
      invokeRestart("muffleMessage")
    })
    msg
  }
  # anymaze reads one point and has no `partial` argument
  one <- said(am_fixture(), dialect = "anymaze")
  expect_match(one, "1 of 12 row(s) have no position", fixed = TRUE)
  expect_match(one, "on_invalid = \"drop\"", fixed = TRUE)
  expect_no_match(one, "partial = \"mean\"", fixed = TRUE)
  expect_no_match(said(fx("ethovision_trial.txt"), dialect = "ethovision"),
                  "partial = \"mean\"", fixed = TRUE)
  two <- said(ev_multipoint(), dialect = "ethovision",
              dialect_args = list(bodypart = c("nose", "tail")))
  expect_match(two, "dialect_args = list(partial = \"mean\")", fixed = TRUE)
})

test_that("ANY-maze: malformed cells and short rows stop the load, naming where", {
  e <- cond(am_tracks(am_edited(function(x) sub(",411,353,", ",4l1,353,", x))))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_match(conditionMessage(e), "Column 'Centre position X'")
  expect_match(conditionMessage(e), "rows: 3\n")
  t <- cond(am_tracks(am_edited(function(x) sub("^0:00:01.000,", "0:00:61.000,", x))))
  expect_s3_class(t, "radiatR_malformed_values")
  expect_match(conditionMessage(t), "Column 'Time'")
  r <- cond(am_tracks(am_edited(function(x) { x[4] <- "0:00:00.352,411"; x })))
  expect_s3_class(r, "radiatR_dialect_error")
  expect_match(conditionMessage(r), "line 4 of .* has 2 field\\(s\\); its header names 8")
  n <- cond(am_tracks(am_edited(function(x) x[1])))
  expect_match(conditionMessage(n), "no data rows")
})

test_that("ANY-maze: the columns it reads must be there", {
  old <- am_edited(function(x) sub("Centre position X,Centre position Y", "X Centre,Y Centre", x))
  e <- cond(am_tracks(old))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "no `Centre position X` / `Centre position Y` columns")
  expect_match(conditionMessage(e), "X Centre, Y Centre, In Q1")
  us <- am_edited(function(x) gsub("Centre position", "Center position", x))
  expect_identical(nrow(rows_of(am_tracks(us))), 11L)
  clock <- am_edited(function(x) sub("^Time,", "Clock,", x))
  expect_match(conditionMessage(cond(am_tracks(clock))), "no `Time` column")
  expect_equal(rows_of(am_tracks(clock, time_col = "clock"))[[2]][1], 0.32)
  expect_s3_class(cond(am_tracks(time_col = "Elapsed")), "radiatR_dialect_error")
})

test_that("ANY-maze: a named id column keeps its text, whatever its case", {
  fish <- am_edited(function(x)
    paste0(x, c(",Fish", rep(",1.1", 6), rep(",1.10", 6))))
  ts <- am_tracks(fish, id_col = "fish")
  expect_identical(sort(unique(as.character(rows_of(ts)[[ts@cols$id]]))), c("1.1", "1.10"))
  expect_identical(as.vector(table(rows_of(ts)[[ts@cols$id]])), c(5L, 6L))
  expect_s3_class(cond(am_tracks(fish, id_col = "animal")), "radiatR_dialect_error")
})

test_that("ANY-maze: a semicolon file with decimal commas reads the same", {
  eu <- am_edited(function(x) {
    x <- gsub("(:[0-9]{2})\\.([0-9]{3});", "\\1,\\2;", gsub(",", ";", x, fixed = TRUE))
    sub(",320;410;", ",320;410,5;", x, fixed = TRUE)
  })
  d <- rows_of(am_tracks(eu))
  expect_equal(d[[2]][1:2], c(0.32, 0.352))
  expect_equal(d[[3]][1], 410.5)
})

test_that("ANY-maze: a file with every field quoted reads the same", {
  quoted <- am_edited(function(x) gsub("([^,]+)", "\"\\1\"", x))
  expect_equal(rows_of(am_tracks(quoted)), rows_of(am_tracks()))
})

test_that("ANY-maze: latin-1 text, a byte-order mark and blank last lines read the same", {
  ref <- rows_of(am_tracks())
  l <- readLines(am_fixture())
  # a zone named "Zone m<e-acute>diane", written in latin-1
  latin <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw(sub("In Platform", "In Zone m", l[1], fixed = TRUE)), as.raw(0xe9),
             charToRaw(paste0("diane\n", paste(l[-1], collapse = "\n"), "\n"))), latin)
  expect_equal(rows_of(am_tracks(latin)), ref)
  bom <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)),
             charToRaw(paste0(paste(l, collapse = "\r\n"), "\r\n  \r\n\r\n"))), bom)
  expect_equal(rows_of(am_tracks(bom)), ref)
})

test_that("ANY-maze: semicolons with decimal points in the times read the same", {
  # whole-number positions leave the durations as the only decimal evidence
  semi <- am_edited(function(x) gsub(",", ";", x, fixed = TRUE))
  expect_equal(rows_of(am_tracks(semi)), rows_of(am_tracks()))
})

test_that("ANY-maze: a time given as difftime or hms is converted, not read as seconds", {
  df <- utils::read.csv(am_fixture(), check.names = FALSE)
  df$Time <- as.difftime(seq_len(nrow(df)), units = "mins")
  ts <- am_tracks(df)
  expect_equal(rows_of(ts)[[ts@cols$time]][1:2], c(120, 180))
  df$Time <- structure(seq_len(nrow(df)) * 1.5, units = "secs", class = c("hms", "difftime"))
  ts <- am_tracks(df)
  expect_equal(rows_of(ts)[[ts@cols$time]][1:2], c(3, 4.5))
})

test_that("ANY-maze: a table in memory loads as the file does; a numeric time is refused", {
  df <- utils::read.csv(am_fixture(), check.names = FALSE)
  expect_equal(rows_of(am_tracks(df)), rows_of(am_tracks()))
  df$Time <- seq(0, by = 0.5, length.out = nrow(df))
  e <- cond(am_tracks(df))
  expect_s3_class(e, "radiatR_malformed_values")
  expect_match(conditionMessage(e), "Column 'Time'")
  df$Time <- as.difftime(df$Time, units = "secs")
  ts <- am_tracks(df)
  expect_equal(rows_of(ts)[[ts@cols$time]][1:2], c(0.5, 1))
  expect_identical(ts@meta$time_unit, "seconds")
})

test_that("ANY-maze: removed arguments are refused with the reason", {
  for (args in list(list(zone = "nose"), list(bodypart = "head"))) {
    e <- cond(do.call(am_tracks, c(list(am_fixture()), args)))
    expect_s3_class(e, "radiatR_dialect_error")
    expect_match(conditionMessage(e), "reads the centre position")
  }
  e <- cond(am_tracks(skip_units_row = FALSE))
  expect_s3_class(e, "radiatR_dialect_error")
  expect_match(conditionMessage(e), "`skip_units_row` was removed")
})

test_that("ANY-maze: read_opts give the delimiter and decimal mark", {
  # tab-separated with decimal commas: sniffed as decimal points
  tab_comma <- am_edited(function(l) gsub(".", ",", gsub(",", "\t", l, fixed = TRUE),
                                          fixed = TRUE))
  expect_s3_class(cond(am_tracks(tab_comma)), "radiatR_malformed_values")
  ts <- am_tracks(tab_comma, read_opts = list(decimal = ","))
  expect_equal(rows_of(ts)[[ts@cols$time]], rows_of(am_tracks())[[am_tracks()@cols$time]])
  ts <- am_tracks(tab_comma, read_opts = list(delim = "\t", decimal = ","))
  expect_equal(rows_of(ts)[[ts@cols$x]], rows_of(am_tracks())[[am_tracks()@cols$x]])
  # the delimiter is used: ';' on the comma file is one column
  expect_s3_class(cond(am_tracks(read_opts = list(delim = ";"))), "radiatR_dialect_error")
  expect_s3_class(cond(am_tracks(read_opts = list(sheet = 1))), "radiatR_invalid_argument")
})
