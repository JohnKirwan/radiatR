# Too slow for CRAN's check time budget; runs locally and on CI.
skip_on_cran()

app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}
fx <- function(f) normalizePath(testthat::test_path("fixtures", "real", f))

test_that("only missing positions and unlinked TrackMate spots can be dropped", {
  skip_if_not_installed("shiny")
  app_file <- file.path(app_dir(), "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  err_of <- function(expr) tryCatch(expr, error = function(cnd) cnd)

  err <- err_of(read_tracks(fx("idtrackerai_trajectories.csv"), dialect = "idtrackerai_wide"))
  expect_identical(e$drop_offer(err), "positions")
  expect_match(e$plain_error(err), "2 row(s) of this file have no x/y position", fixed = TRUE)

  err <- err_of(read_tracks(fx("trackmate_v6_spots.csv"), dialect = "trackmate"))
  expect_identical(e$drop_offer(err), "unlinked")
  expect_match(e$plain_error(err), "3 spot(s) that belong to no track", fixed = TRUE)

  # a malformed value and a broken track are never dropped
  err <- err_of(read_tracks(data.frame(id = "a", time = 1:25, x = "oops", y = 0.2)))
  expect_s3_class(err, "radiatR_malformed_values")
  expect_null(e$drop_offer(err))
  d <- data.frame(id = c("a", "a", "b"), time = c(1, 1, 1), x = 0.1, y = 0.2)
  err <- err_of(tracks(d, id = "id", time = "time", x = "x", y = "y"))
  expect_s3_class(err, "radiatR_invalid_tracks")
  expect_null(e$drop_offer(err))
  expect_null(e$drop_offer(simpleError("no such file")))
})

test_that("missing positions stop the load with an offer; accepting it loads the rest", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- fx("idtrackerai_trajectories.csv")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "trajectories.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_identical(rv$drop_offer$kind, "positions")
    html <- paste(as.character(output$wizard), collapse = "\n")
    expect_match(html, "Drop these 2 row(s) and continue", fixed = TRUE)

    session$setInputs(drop_btn = 1)
    expect_null(rv$error)
    expect_equal(rv$step, 2L)
    expect_equal(nrow(as.data.frame(rv$ts)), 86L)
    expect_identical(rv$dropped, 2L)
    html <- paste(as.character(output$wizard), collapse = "\n")
    expect_match(html, "2 row(s) were dropped when this file was loaded", fixed = TRUE)
    expect_no_match(html, "and continue", fixed = TRUE)
    expect_match(paste(as.character(output$dropped_banner), collapse = "\n"),
                 "2 row(s) were dropped when this file was loaded (no x/y position)",
                 fixed = TRUE)

    # every downloaded script loads the file the way the app did
    session$setInputs(method = "distal", go3 = 1)
    expect_match(figure_code_text(), ', on_invalid = "drop")', fixed = TRUE)
    expect_match(kinematics_code_text(), ', on_invalid = "drop")', fixed = TRUE)
    expect_match(stats_code_text(), ', on_invalid = "drop")', fixed = TRUE)
  })
})

test_that("TrackMate spots in no track can be dropped, and the script says so", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- fx("trackmate_v6_spots.csv")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "spots.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_null(rv$ts)
    expect_identical(rv$drop_offer$kind, "unlinked")
    expect_match(paste(as.character(output$wizard), collapse = "\n"),
                 "Drop these 3 row(s) and continue", fixed = TRUE)

    session$setInputs(drop_btn = 1)
    expect_null(rv$error)
    expect_equal(nrow(as.data.frame(rv$ts)), 6L)
    expect_identical(rv$dropped, 3L)
    session$setInputs(method = "distal", go3 = 1)
    code <- figure_code_text()
    expect_match(code, 'dialect_args = list(unlinked = "drop")', fixed = TRUE)
    # the user agreed to drop unlinked spots, not rows with no position
    expect_no_match(code, "on_invalid", fixed = TRUE)
  })
})

test_that("each kind of drop is agreed to separately", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  # the v6 fixture (3 spots in no track) with the x of one linked spot blanked
  lines <- readLines(fx("trackmate_v6_spots.csv"))
  lines[5] <- sub(",31.985,", ",,", lines[5], fixed = TRUE)
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(lines, path)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "spots.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_identical(rv$drop_offer[c("kind", "n")], list(kind = "unlinked", n = 3L))
    session$setInputs(drop_btn = 1)
    # dropping the unlinked spots did not also drop the row with no position
    expect_equal(rv$step, 1L)
    expect_identical(rv$drop_offer[c("kind", "n")], list(kind = "positions", n = 1L))
    session$setInputs(drop_btn = 2)
    expect_null(rv$error)
    expect_equal(nrow(as.data.frame(rv$ts)), 5L)
    expect_identical(rv$dropped, 4L)
    expect_match(paste(as.character(output$dropped_banner), collapse = "\n"),
                 "4 row(s) were dropped when this file was loaded (spot in no track; no x/y position)",
                 fixed = TRUE)
    session$setInputs(method = "distal", go3 = 1)
    expect_match(figure_code_text(),
                 ', dialect_args = list(unlinked = "drop"), on_invalid = "drop")', fixed = TRUE)
  })
})

test_that("pose frames with a masked point are offered as missing positions", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- fx("dlc_single_wasp_predictions.csv")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "wasp.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_identical(rv$drop_offer[c("kind", "n")], list(kind = "positions", n = 9L))
    session$setInputs(drop_btn = 1)
    expect_identical(rv$dialect, "deeplabcut")
    expect_equal(nrow(as.data.frame(rv$ts)), 43L)
    expect_identical(rv$dropped, 9L)
  })
})

test_that("a load with nothing dropped reports and emits no drop", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- fx("trackmate_v7_spots.csv")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "spots.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_null(rv$error)
    expect_identical(rv$dropped, 0L)
    expect_null(output$dropped_banner)
    session$setInputs(method = "distal", go3 = 1)
    expect_no_match(figure_code_text(), "on_invalid|unlinked")
  })
})

test_that("an error that is not about droppable rows offers no drop", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id,time,x,y", "a,1,oops,0.2", "a,2,0.1,0.2"), bad)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = bad, name = "bad.csv"))
    session$setInputs(dialect_sel = "generic")
    session$setInputs(map_x = "x", map_y = "y", map_time = "time", map_id = "id")
    session$setInputs(go2 = 1)
    expect_match(rv$error, "Column 'x' has values that are not numeric", fixed = TRUE)
    expect_null(rv$drop_offer)
    expect_no_match(paste(as.character(output$wizard), collapse = "\n"),
                    "and continue", fixed = TRUE)
  })
})

test_that("the offer goes when its error is replaced by another", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  path <- fx("idtrackerai_trajectories.csv")
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = path, name = "trajectories.csv"))
    session$setInputs(dialect_sel = "auto")
    session$setInputs(go2 = 1)
    expect_identical(rv$drop_offer$kind, "positions")
    # what a failed example load does while the upload's error is still shown
    rv$error <- "Could not load the bundled example dataset."
    session$flushReact()
    expect_null(rv$drop_offer)
    expect_no_match(paste(as.character(output$wizard), collapse = "\n"),
                    "and continue", fixed = TRUE)

    session$setInputs(go2 = 2)
    expect_identical(rv$drop_offer$kind, "positions")
    session$setInputs(input_type = "headings")
    expect_null(rv$error)
    expect_null(rv$drop_offer)
  })
})

test_that("agreeing to a drop covers that load only: a later load asks again", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id,time,xa,xb,y",
               "a,1,0.1,0.1,0.2", "a,2,,0.2,0.3", "a,3,0.3,,0.4",
               "a,4,0.4,,0.5", "a,5,0.5,,0.6", "a,6,0.6,0.6,0.7"), csv)
  shiny::testServer(app_dir(), {
    session$setInputs(input_type = "trajectories")
    session$setInputs(file = list(datapath = csv, name = "tracks.csv"))
    session$setInputs(dialect_sel = "generic")
    session$setInputs(map_x = "xa", map_y = "y", map_time = "time", map_id = "id")
    session$setInputs(go2 = 1)
    expect_identical(rv$drop_offer[c("kind", "n")], list(kind = "positions", n = 1L))
    session$setInputs(drop_btn = 1)
    expect_equal(rv$step, 2L)
    expect_identical(rv$dropped, 1L)

    # Back, read another column: its three missing rows are not dropped unasked
    session$setInputs(back_btn = 1)
    session$setInputs(map_x = "xb")
    session$setInputs(go2 = 2)
    expect_equal(rv$step, 1L)
    expect_identical(rv$drop_offer[c("kind", "n")], list(kind = "positions", n = 3L))
    session$setInputs(drop_btn = 2)
    expect_identical(rv$dropped, 3L)

    # and "Next" on the very same read asks again
    session$setInputs(back_btn = 2)
    session$setInputs(go2 = 3)
    expect_equal(rv$step, 1L)
    expect_identical(rv$drop_offer[c("kind", "n")], list(kind = "positions", n = 3L))
  })
})

test_that("in a browser, the drop button loads an idtracker.ai file", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")
  skip_on_os(c("mac", "windows"))  # browser-timing flakes off Ubuntu; see test-app-example.R

  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir),
          "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(
    app_dir,
    name         = "drop-control",
    load_timeout = 60 * 1000,
    timeout      = 30 * 1000
  )
  withr::defer(app$stop())

  app$upload_file(file = fx("idtrackerai_trajectories.csv"))
  app$wait_for_idle(timeout = 30 * 1000)
  app$click("go2")
  app$wait_for_idle(timeout = 30 * 1000)
  # The error re-renders the wizard; the button must survive that and its click
  # must re-run the load with the format still selected.
  expect_match(app$get_html("#drop_btn"), "Drop these 2 row(s) and continue", fixed = TRUE)
  app$click("drop_btn")
  app$wait_for_idle(timeout = 30 * 1000)
  expect_match(app$get_html("#wizard"), "2 row(s) were dropped when this file was loaded",
               fixed = TRUE)
  expect_false(is.null(app$get_value(input = "method")))   # reached Configure
})

test_that("a file that is not one table, or ambiguous ids, are explained as the reader says", {
  skip_if_not_installed("shiny")
  app_file <- file.path(app_dir(), "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))
  err_of <- function(expr) tryCatch(expr, error = function(cnd) cnd)
  path <- withr::local_tempfile(fileext = ".csv")

  writeLines(c("id,frame,x,y,id", "1,1,1,2,a", "2,2,2,3,b"), path)
  err <- err_of(read_tracks(path))
  expect_s3_class(err, "radiatR_malformed_file")
  expect_match(e$plain_error(err), sprintf(
    "^file '%s' is not one table: column name\\(s\\) 'id' appear more than once", basename(path)))
  expect_null(e$drop_offer(err))

  err <- err_of(read_tracks(data.frame(id = c(" 1", "1"), frame = 1:2, x = 1:2, y = 1:2)))
  expect_s3_class(err, "radiatR_ambiguous_ids")
  expect_match(e$plain_error(err), "^ids in column 'id' differ only by surrounding whitespace")
})
