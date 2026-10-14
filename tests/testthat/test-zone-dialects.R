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
