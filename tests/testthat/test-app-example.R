# Browser-level smoke test for the Shiny companion app, driven through a real
# headless Chrome via shinytest2. Guards the example-data path and the Results
# step features (layer toggles, vector download) against regressions that the
# server-only testServer cannot catch -- DOM wiring, input registration, and
# actual plot rendering.
#
# shinytest2 launches the app in a separate R process that runs
# library(radiatR), so the package must be INSTALLED (this runs under
# R CMD check, not bare devtools::test()) and a Chrome/Chromium binary must be
# present. The chromote launch flags are configured in setup-shinytest2.R. The
# test skips cleanly when any prerequisite is missing.

test_that("the app walks example -> Results and honours the layer toggles", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")

  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir),
          "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(
    app_dir,
    name         = "millipede-example",
    load_timeout = 60 * 1000,
    timeout      = 30 * 1000
  )
  withr::defer(app$stop())

  # Step 1 -> 2: load the bundled example instead of uploading a file.
  app$click("load_example")
  app$wait_for_idle(timeout = 30 * 1000)
  expect_false(is.null(app$get_value(input = "method")))

  # Step 2 -> 3: run the analysis. The auto-detected `arc` condition is joined
  # here; a broken join used to leave the app stuck on Configure.
  app$click("go3")
  app$wait_for_idle(timeout = 30 * 1000)

  # Results rendered: the grouped summary is present and populated.
  summary_html <- app$get_value(output = "summary_tbl")
  expect_false(is.null(summary_html))
  expect_match(summary_html, "Direction")
  expect_match(summary_html, "Rayleigh")
  expect_false(grepl("Summary not available", summary_html))

  # The track plot rendered without falling back to "Plot unavailable". The
  # render path logs "track_plot render failed" on error.
  expect_false(grepl(
    "track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")
  ))

  # Turning on the bootstrap-CI overlay must not error the plot.
  app$set_inputs(show_ci = TRUE)
  app$wait_for_idle(timeout = 30 * 1000)
  expect_false(grepl(
    "track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")
  ))

  # Switching the heading display to stacked dots must not error the plot.
  app$set_inputs(heading_display = "stacked")
  app$wait_for_idle(timeout = 30 * 1000)
  expect_false(grepl(
    "track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")
  ))

  # Hiding every layer (no heading markers either) still renders the empty
  # arena (no crash).
  app$set_inputs(show_tracks = FALSE, heading_display = "none",
                 show_arrow = FALSE, show_ci = FALSE)
  app$wait_for_idle(timeout = 30 * 1000)
  expect_false(grepl(
    "track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")
  ))
})

test_that("selecting the Headings input type sticks across wizard re-renders", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")

  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir),
          "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(
    app_dir,
    name         = "headings-input-type",
    load_timeout = 60 * 1000,
    timeout      = 30 * 1000
  )
  withr::defer(app$stop())

  # Switch the upload type to headings. Changing it re-renders the whole wizard
  # (rv$mode is a dependency); the selection must persist. A hardcoded `selected`
  # used to snap it back to "trajectories" on that re-render, making the headings
  # upload path unreachable.
  app$set_inputs(input_type = "headings")
  app$wait_for_idle(timeout = 30 * 1000)
  expect_identical(app$get_value(input = "input_type"), "headings")

  # The headings-only "Load example headings" link renders only while in
  # headings mode, so its presence confirms the mode actually took effect.
  expect_match(app$get_html("#load_example_hd"), "Load example headings")
})

test_that("example headings land on Configure with a facet selector (arc/type)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")

  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir),
          "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(
    app_dir,
    name         = "headings-facet",
    load_timeout = 60 * 1000,
    timeout      = 30 * 1000
  )
  withr::defer(app$stop())

  app$set_inputs(input_type = "headings")
  app$wait_for_idle(timeout = 30 * 1000)
  app$click("load_example_hd")
  app$wait_for_idle(timeout = 30 * 1000)

  # The example no longer skips Configure: a Facet-by selector is present and
  # defaults to arc (not locked to type).
  expect_identical(app$get_value(input = "hd_group"), "arc")
  # `type` is also offered (selecting it sticks; it is not the only option).
  app$set_inputs(hd_group = "type")
  app$wait_for_idle(timeout = 30 * 1000)
  expect_identical(app$get_value(input = "hd_group"), "type")
  app$set_inputs(hd_group = "arc")
  app$wait_for_idle(timeout = 30 * 1000)

  # Choosing arc and analysing renders results without error.
  app$click("go3")
  app$wait_for_idle(timeout = 30 * 1000)
  expect_false(is.null(app$get_value(output = "summary_tbl")))
  expect_false(grepl(
    "track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")
  ))
})

test_that("the app exports a vector plot download", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")

  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir),
          "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(
    app_dir,
    name         = "millipede-download",
    load_timeout = 60 * 1000,
    timeout      = 30 * 1000
  )
  withr::defer(app$stop())

  app$click("load_example")
  app$wait_for_idle(timeout = 30 * 1000)
  app$click("go3")
  app$wait_for_idle(timeout = 30 * 1000)

  # The Download controls live in an accordion panel that is collapsed by
  # default (to keep the option column compact). Expand every collapsed panel
  # so the download link is interactable before triggering it.
  app$run_js(
    "document.querySelectorAll('.accordion-button.collapsed').forEach(function(b){ b.click(); });"
  )
  app$wait_for_idle(timeout = 30 * 1000)

  # Format affects only the download, not any rendered output, so don't wait
  # for an output update. PDF is the default; set it explicitly for clarity.
  app$set_inputs(plot_fmt = "pdf", wait_ = FALSE)
  out <- app$get_download("dl_plot")
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
  # PDF magic bytes
  expect_equal(rawToChar(readBin(out, "raw", 4L)), "%PDF")
})

test_that("the axial toggle renders example headings without error", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")
  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir), "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(app_dir, name = "axial-toggle",
                                   load_timeout = 60 * 1000, timeout = 30 * 1000)
  withr::defer(app$stop())

  app$set_inputs(input_type = "headings"); app$wait_for_idle(timeout = 30 * 1000)
  app$click("load_example_hd");            app$wait_for_idle(timeout = 30 * 1000)
  app$click("go3");                        app$wait_for_idle(timeout = 30 * 1000)
  app$set_inputs(axial = TRUE);            app$wait_for_idle(timeout = 30 * 1000)
  expect_identical(app$get_value(input = "axial"), TRUE)
  expect_false(grepl("track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")))
})

# Server-level test (no browser): a Generic CSV upload renders the column-mapping
# dropdowns pre-filled from guess_columns(), and the chosen mapping loads a
# single-track TrajSet through the go2 handler. (Driven via shiny::testServer
# rather than shinytest2 because browser file-upload is flaky in CI.)
test_that("a generic CSV maps columns and loads through the server", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  csv <- tempfile(fileext = ".csv")
  set.seed(1)
  utils::write.csv(data.frame(Frame = 1:30,
                              Track1_X = cumsum(rnorm(30)),
                              Track1_Y = cumsum(rnorm(30))),
                   csv, row.names = FALSE)

  shiny::testServer(app_dir, {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    session$setInputs(dialect_sel = "generic")

    # the mapping panel renders with the guessed columns and the single-track option
    html <- paste(unlist(output$mapping_box), collapse = " ")
    expect_match(html, "map_x")
    expect_match(html, "Track1_X")
    expect_match(html, "single track")

    # supply the (pre-filled) mapping and load
    session$setInputs(map_x = "Track1_X", map_y = "Track1_Y",
                      map_time = "Frame", map_id = "")
    session$setInputs(go2 = 1)

    expect_equal(rv$step, 2L)               # advanced to Configure
    expect_false(is.null(rv$ts))            # a TrajSet was built
    expect_equal(length(ids(rv$ts)), 1L)    # treated as a single trajectory
  })
})

test_that("the velocity_axis method renders the example trajectory without error", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")
  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir), "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(app_dir, name = "velocity-axis-method",
                                   load_timeout = 60 * 1000, timeout = 30 * 1000)
  withr::defer(app$stop())

  app$click("load_example");          app$wait_for_idle(timeout = 30 * 1000)
  app$set_inputs(method = "velocity_axis"); app$wait_for_idle(timeout = 30 * 1000)
  app$click("go3");                   app$wait_for_idle(timeout = 30 * 1000)
  expect_identical(app$get_value(input = "method"), "velocity_axis")
  expect_false(grepl("track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")))
})

test_that("a semicolon CSV maps split columns (preview/mapping read via .read_any)", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  csv <- tempfile(fileext = ".csv")
  writeLines(c("Track1_X;Track1_Y;Frame",
               "0,10;0,20;1", "0,30;0,40;2", "0,50;0,60;3"), csv)

  shiny::testServer(app_dir, {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    session$setInputs(dialect_sel = "generic", delim_sel = "auto")

    map <- paste(unlist(output$mapping_box), collapse = " ")
    # Discriminating: when correctly split, each column is its own selectable
    # option, so the quoted token "Track1_Y" appears. A comma-misread of a ';'
    # file yields ONE column literally named "Track1_X;Track1_Y;Frame", in which
    # the quoted token "Track1_Y" never appears (no quote precedes it).
    expect_true(grepl('"Track1_Y"', map, fixed = TRUE))
    expect_true(grepl('"Frame"',    map, fixed = TRUE))
  })
})

test_that("the delimiter override forces the reader and loads a semicolon file", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  csv <- tempfile(fileext = ".csv")
  writeLines(c("Track1_X;Track1_Y;Frame",
               "0,10;0,20;1", "0,30;0,40;2", "0,50;0,60;3"), csv)

  shiny::testServer(app_dir, {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    session$setInputs(dialect_sel = "generic", delim_sel = ";")
    session$setInputs(map_x = "Track1_X", map_y = "Track1_Y",
                      map_time = "Frame", map_id = "")
    session$setInputs(go2 = 1)
    expect_equal(rv$step, 2L)
    expect_false(is.null(rv$ts))
    expect_equal(length(ids(rv$ts)), 1L)
  })
})
