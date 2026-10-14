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

ev_read <- function(p) radiatR:::.read_ethovision(p)
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

test_that("EthoVision reader: column names must be present and distinct", {
  l <- ev_lines()
  for (bad in list(sub("\"Y center\"", "\"X center\"", l[32]), sub("\"Area\"", "\"\"", l[32]))) {
    e <- cond(ev_read(ev_file(replace(l, 32, bad))))
    expect_s3_class(e, "radiatR_dialect_error")
    expect_match(conditionMessage(e), "line 32 of .* should name the columns")
  }
})
