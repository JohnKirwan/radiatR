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
#
# Also skipped on macOS: some CI macOS runners ship a usable Chrome, so these
# tests don't skip via the chromote guard there the way they do on Windows --
# but shinytest2's download flow has shown macOS-specific timing flakiness
# (a client-side Shiny input/output binding race after expanding a collapsed
# accordion via raw JS; wait_for_idle() only waits on server busy/idle, not on
# the browser finishing binding newly-visible inputs/outputs) that Ubuntu's
# reference Chrome does not exhibit. Skip explicitly rather than chase
# platform-specific browser-automation timing on a platform these tests were
# never validated against.

test_that("the app walks example -> Results and honours the layer toggles", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")
  skip_on_os("mac")  # macOS-specific shinytest2 download-timing flake; see file header

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
  # circle (no crash).
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
  skip_on_os("mac")  # macOS-specific shinytest2 download-timing flake; see file header

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
  skip_on_os("mac")  # macOS-specific shinytest2 download-timing flake; see file header

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
  skip_on_os("mac")  # macOS-specific shinytest2 download-timing flake; see file header

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

  # Format affects only the download, not any rendered output, so the
  # default set_inputs() wait (which waits for an output to invalidate)
  # would time out here. Use wait_ = FALSE, but then wait for the server's
  # busy/idle signal explicitly -- otherwise the download request can race
  # ahead of the session processing the input change on slower CI runners.
  # PDF is the default; set it explicitly for clarity.
  app$set_inputs(plot_fmt = "pdf", wait_ = FALSE)
  app$wait_for_idle(timeout = 30 * 1000)
  out <- app$get_download("dl_plot")
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
  # PDF magic bytes
  expect_equal(rawToChar(readBin(out, "raw", 4L)), "%PDF")
})

test_that("the Data model = Axial selection renders example headings without error", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")
  skip_on_os("mac")  # macOS-specific shinytest2 download-timing flake; see file header
  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir), "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(app_dir, name = "axial-data-model",
                                   load_timeout = 60 * 1000, timeout = 30 * 1000)
  withr::defer(app$stop())

  # The old headings-only `axial` checkbox was replaced by the `data_model`
  # selector ("Directional"/"Axial"), which lives in the Configure step (step 2).
  # It must be set BEFORE clicking go3 (which advances to Results).
  app$set_inputs(input_type = "headings");  app$wait_for_idle(timeout = 30 * 1000)
  app$click("load_example_hd");             app$wait_for_idle(timeout = 30 * 1000)
  app$set_inputs(data_model = "axial");     app$wait_for_idle(timeout = 30 * 1000)
  expect_identical(app$get_value(input = "data_model"), "axial")
  app$click("go3");                         app$wait_for_idle(timeout = 30 * 1000)
  expect_false(grepl("track_plot render failed",
    paste(utils::capture.output(print(app$get_logs())), collapse = "\n")))
})

test_that("an axial heading method soft-syncs Data model, and the override sticks", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")
  skip_on_os("mac")  # macOS-specific shinytest2 download-timing flake; see file header
  app_dir <- system.file("app", package = "radiatR")
  skip_if(!nzchar(app_dir), "radiatR app directory not found (package installed?)")

  app <- shinytest2::AppDriver$new(app_dir, name = "axial-method-softsync",
                                   load_timeout = 60 * 1000, timeout = 30 * 1000)
  withr::defer(app$stop())

  # Trajectory mode: load_example lands on Configure (step 2) with both the
  # `method` dropdown and the `data_model` selector present. The soft sync
  # relies on updateSelectInput propagating to the client, so it is genuinely
  # browser-only (testServer cannot exercise it).
  app$click("load_example");                  app$wait_for_idle(timeout = 30 * 1000)  # trajectory mode, Configure
  app$set_inputs(method = "velocity_axis");   app$wait_for_idle(timeout = 30 * 1000)
  expect_identical(app$get_value(input = "data_model"), "axial")   # soft sync fired
  app$set_inputs(data_model = "directional"); app$wait_for_idle(timeout = 30 * 1000)
  app$set_inputs(method = "distal");          app$wait_for_idle(timeout = 30 * 1000)
  expect_identical(app$get_value(input = "data_model"), "directional")  # not forced back
})

# Server-level test (no browser): a Generic CSV upload renders the column-mapping
# dropdowns pre-filled from guess_columns(), and the chosen mapping loads a
# single-track Tracks through the go2 handler. (Driven via shiny::testServer
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
    expect_false(is.null(rv$ts))            # a Tracks was built
    expect_equal(length(ids(rv$ts)), 1L)    # treated as a single trajectory
  })
})

test_that("the velocity_axis method renders the example trajectory without error", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium binary found")
  skip_on_os("mac")  # macOS-specific shinytest2 download-timing flake; see file header
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

# Server-level (no browser): the Data model selector is the single source of
# truth for axial mode. testServer exercises the is_axial() resolver and that it
# propagates into current_spec()$axial and the relabelled summary column. (The
# method -> data_model soft sync relies on updateSelectInput reaching the client
# and is covered by the shinytest2 test above, not here.)
test_that("data_model selector drives the is_axial resolver", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    session$setInputs(data_model = "axial")
    expect_true(is_axial())
    session$setInputs(data_model = "directional")
    expect_false(is_axial())
  })
})

test_that("data_model drives current_spec()$axial and the summary relabel (example headings)", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    # Drive the documented example-headings load path to a real Results state.
    session$setInputs(input_type = "headings")
    session$setInputs(load_example_hd = 1)   # builds rv$hd, lands on Configure
    expect_false(is.null(rv$hd))
    session$setInputs(go3 = 1)                # advance to Results
    expect_equal(rv$step, 3L)

    # Axial: current_spec()$axial flips and the summary's Rayleigh column is
    # relabelled. output$summary_tbl is the rendered renderTable HTML (a
    # character string), so the relabelled header text appears in it.
    session$setInputs(data_model = "axial")
    expect_true(isTRUE(current_spec()$axial))
    html_ax <- paste(output$summary_tbl, collapse = " ")
    expect_true(grepl("Rayleigh (axial) p", html_ax, fixed = TRUE))

    # Directional: axial off, header reverts to "Rayleigh p".
    session$setInputs(data_model = "directional")
    expect_false(isTRUE(current_spec()$axial))
    html_dir <- paste(output$summary_tbl, collapse = " ")
    expect_true(grepl("Rayleigh p", html_dir, fixed = TRUE))
    expect_false(grepl("Rayleigh (axial) p", html_dir, fixed = TRUE))
  })
})

# Track-colour selector threads through current_spec() in trajectory mode.
test_that("track_colour selector drives current_spec()$track_colour", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)   # trajectory example, lands on Configure
    session$setInputs(go3 = 1)            # advance to Results
    expect_false(is.null(rv$ts))

    session$setInputs(track_colour = "sequence")
    expect_equal(current_spec()$track_colour, "sequence")
    session$setInputs(track_colour = "trajectory")
    expect_equal(current_spec()$track_colour, "trajectory")
  })
})

# Track colour "By elapsed time": the selection + frame rate flow into the spec,
# and an invalid frame rate surfaces the not-drawable note (graceful fallback).
test_that("track_colour = 'time' + frame rate flows into the spec; invalid fps shows the note", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)              # trajectory example, lands on Configure
    session$setInputs(go3 = 1)                        # advance to Results
    expect_false(is.null(rv$ts))

    session$setInputs(track_colour = "time", frame_rate = 30)
    expect_equal(current_spec()$track_colour, "time")
    expect_equal(current_spec()$frame_rate, 30)
    # valid fps -> effective mode is "time" -> note is empty
    expect_null(output$track_time_note)
    # zero fps -> falls back to sequence, note appears
    session$setInputs(frame_rate = 0)
    expect_true(!is.null(output$track_time_note))
  })
})

# Track colour "By speed": mirrors the "By elapsed time" wiring -- the selection
# + frame rate flow into the spec, and an invalid frame rate surfaces the note.
test_that("track_colour = 'speed' + frame rate flows into the spec; invalid fps shows the note", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)              # trajectory example, lands on Configure
    session$setInputs(go3 = 1)                        # advance to Results
    expect_false(is.null(rv$ts))

    session$setInputs(track_colour = "speed", frame_rate = 30)
    expect_equal(current_spec()$track_colour, "speed")
    # valid fps -> effective mode is "speed" -> note is empty
    expect_null(output$track_time_note)
    # zero fps -> falls back to sequence, note appears
    session$setInputs(frame_rate = 0)
    expect_true(!is.null(output$track_time_note))
  })
})

# rao_spacing_fmt: omnibus Rao spacing bracket, parsed from circular's print output.
test_that("rao_spacing_fmt brackets clustered, uniform, and tiny samples", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(1)
  clustered <- rnorm(40, 0, 0.2)                 # strongly non-uniform
  uniform   <- seq(0, 2 * pi, length.out = 41)[-41]
  tiny      <- c(0.1, 0.2, 0.3)                  # n < 4

  expect_match(e$rao_spacing_fmt(clustered), "^< 0\\.")      # significant bracket
  expect_identical(e$rao_spacing_fmt(uniform), "> 0.10")     # not significant
  expect_identical(e$rao_spacing_fmt(tiny), "—")             # below Rao's n floor
})

test_that("rao_spacing_fmt is raw-angle (axial bimodal data reads as non-uniform)", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(2)
  bimodal <- c(rnorm(20, 0, 0.15), rnorm(20, pi, 0.15))  # two antipodal clusters
  expect_match(e$rao_spacing_fmt(bimodal), "^< 0\\.")
})

test_that("the summary table carries a Rao spacing column for both data models", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    session$setInputs(input_type = "headings")
    session$setInputs(load_example_hd = 1)
    session$setInputs(go3 = 1)
    expect_equal(rv$step, 3L)

    html_dir <- paste(output$summary_tbl, collapse = " ")
    expect_true(grepl("Rao spacing", html_dir, fixed = TRUE))
    expect_true(grepl("Rayleigh p", html_dir, fixed = TRUE))

    session$setInputs(data_model = "axial")
    html_ax <- paste(output$summary_tbl, collapse = " ")
    expect_true(grepl("Rao spacing", html_ax, fixed = TRUE))          # still present
    expect_true(grepl("Rayleigh (axial) p", html_ax, fixed = TRUE))   # focused row relabelled
  })
})

test_that("hermans_p_fmt formats a Monte-Carlo p and is render-stable", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(1)
  clustered <- rnorm(40, 1, 0.3) %% (2 * pi)
  p1 <- e$hermans_p_fmt(clustered, n_sim = 199)
  p2 <- e$hermans_p_fmt(clustered, n_sim = 199)
  expect_identical(p1, p2)                                   # fixed seed -> identical
  expect_true(grepl("^(< 0\\.001|[01]\\.[0-9]{3})$", p1))    # bracket or 3dp
  expect_identical(e$hermans_p_fmt(c(0.1, 0.2), n_sim = 199), "—")   # n < 3
})

test_that("circ_summary_table omnibus arg switches the omnibus column", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(2)
  hd <- data.frame(heading = rnorm(40, 1, 0.3) %% (2 * pi), grp = "a")
  r_rao <- e$circ_summary_table(hd, "grp", omnibus = "rao")
  r_hr  <- e$circ_summary_table(hd, "grp", omnibus = "hermans_rasson")
  expect_true("Rao spacing" %in% names(r_rao))
  expect_true("Hermans-Rasson p" %in% names(r_hr))
  expect_false("Rao spacing" %in% names(r_hr))
})

test_that("circ_summary_table adds a Best model column from model_sel", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(3)
  hd <- data.frame(heading = rnorm(60, 1, 0.3) %% (2 * pi), grp = "a")
  ms <- circ_model_select(hd, group_col = "grp")
  r  <- e$circ_summary_table(hd, "grp", model_sel = ms)
  expect_true("Best model" %in% names(r))
  expect_match(r[["Best model"]][1], "^(uniform|unimodal|axial) \\([01]\\.[0-9]{2}\\)$")
  expect_false("Best model" %in% names(e$circ_summary_table(hd, "grp")))
})

test_that("group_compare_table returns three labelled rows for two groups", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(10)
  hd <- data.frame(
    heading = c(rnorm(20, 1, 0.3), rnorm(20, 2.5, 0.3)) %% (2 * pi),
    grp     = rep(c("a", "b"), each = 20)
  )
  r <- e$group_compare_table(hd, "grp")
  expect_equal(nrow(r), 3L)
  expect_equal(r$Test, c("Mean direction", "Concentration", "Distribution"))
  expect_true(all(c("Method", "Statistic", "df", "p-value") %in% names(r)))
})

test_that("group_compare_table picks watson_two for exactly two groups", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(11)
  hd <- data.frame(
    heading = c(rnorm(20, 1, 0.3), rnorm(20, 2.5, 0.3)) %% (2 * pi),
    grp     = rep(c("a", "b"), each = 20)
  )
  r <- e$group_compare_table(hd, "grp")
  expect_equal(r$Method[r$Test == "Distribution"], "watson_two")
})

test_that("group_compare_table picks watson_wheeler for three or more groups", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  set.seed(12)
  hd <- data.frame(
    heading = c(rnorm(15, 0, 0.3), rnorm(15, 2, 0.3), rnorm(15, 4, 0.3)) %% (2 * pi),
    grp     = rep(c("a", "b", "c"), each = 15)
  )
  r <- e$group_compare_table(hd, "grp")
  expect_equal(r$Method[r$Test == "Distribution"], "watson_wheeler")
})

test_that("group_compare_table degrades one row to a dash instead of erroring", {
  skip_if_not_installed("shiny")
  app_file <- system.file("app", "app.R", package = "radiatR")
  if (!nzchar(app_file))
    app_file <- testthat::test_path("..", "..", "inst", "app", "app.R")
  skip_if(!file.exists(app_file), "app.R not found")
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(app_file, envir = e, chdir = TRUE)))

  # Group "b" has only 1 observation: after the package's own internal
  # >=2-observations filter, fewer than 2 groups remain, so each of the three
  # package functions throws. group_compare_table must catch each independently
  # and still return a 3-row table with dashes, not propagate the error.
  set.seed(13)
  hd <- data.frame(heading = c(rnorm(20, 1, 0.3), 2.1) %% (2 * pi),
                    grp = c(rep("a", 20), "b"))
  expect_no_error(r <- e$group_compare_table(hd, "grp"))
  expect_equal(nrow(r), 3L)
  expect_true(all(r$Statistic == "—"))
  expect_true(all(r[["p-value"]] == "—"))
})

test_that("the omnibus selector switches the summary omnibus column", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")
  shiny::testServer(app_dir, {
    session$setInputs(input_type = "headings")
    session$setInputs(load_example_hd = 1)
    session$setInputs(go3 = 1)
    expect_equal(rv$step, 3L)
    session$setInputs(omnibus_test = "rao")
    h_rao <- paste(output$summary_tbl, collapse = " ")
    expect_true(grepl("Rao spacing", h_rao, fixed = TRUE))
    session$setInputs(omnibus_test = "hermans_rasson")
    h_hr <- paste(output$summary_tbl, collapse = " ")
    expect_true(grepl("Hermans-Rasson p", h_hr, fixed = TRUE))
    expect_false(grepl("Rao spacing", h_hr, fixed = TRUE))
  })
})

test_that("summary has a Best model column and the model-selection card renders", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")
  shiny::testServer(app_dir, {
    session$setInputs(input_type = "headings")
    session$setInputs(load_example_hd = 1)
    session$setInputs(go3 = 1)
    expect_equal(rv$step, 3L)
    sm <- paste(output$summary_tbl, collapse = " ")
    expect_true(grepl("Best model", sm, fixed = TRUE))
    card <- paste(output$model_sel_tbl, collapse = " ")
    expect_true(grepl("weight", card, fixed = TRUE))
    expect_true(grepl("unimodal", card) || grepl("axial", card) || grepl("uniform", card))
  })
})

test_that("the method dropdown is grouped into Directional and Axial optgroups", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    expect_equal(rv$step, 2L)
    wiz <- paste(as.character(output$wizard), collapse = " ")
    expect_true(grepl('optgroup label="Axial methods"', wiz, fixed = TRUE))
    expect_true(grepl('optgroup label="Directional methods"', wiz, fixed = TRUE))
    expect_true(grepl('value="velocity_axis"', wiz, fixed = TRUE))
    expect_true(grepl('value="distal"', wiz, fixed = TRUE))
  })
})

test_that("Results sub-tabs: plot, summary, and figure-code outputs all render after the restructure", {
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)                       # advance to Results
    expect_no_error(current_spec())
    expect_no_error(output$track_plot)               # Circular plots sub-tab
    expect_no_error(output$summary_tbl)              # Summary & stats sub-tab
    expect_no_error(output$figure_code)              # figure code (Plots sub-tab)
  })
})

test_that("method_model_note hints, warns, and stays silent appropriately", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  note <- function(out) paste(as.character(out), collapse = " ")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    expect_equal(rv$step, 2L)

    # Hint: axial model + directional method
    session$setInputs(data_model = "axial", method = "distal")
    expect_true(grepl("folded", note(output$method_model_note), fixed = TRUE))

    # Warning: directional model + axial method
    session$setInputs(data_model = "directional", method = "velocity_axis")
    expect_true(grepl("biases the circular statistics",
                      note(output$method_model_note), fixed = TRUE))

    # Matched combos -> nothing
    session$setInputs(data_model = "axial", method = "velocity_axis")
    expect_false(grepl("folded|biases", note(output$method_model_note)))
    session$setInputs(data_model = "directional", method = "distal")
    expect_false(grepl("folded|biases", note(output$method_model_note)))

    # None -> nothing
    session$setInputs(data_model = "axial", method = "none")
    expect_false(grepl("folded|biases", note(output$method_model_note)))
  })
})

test_that("Summary & stats sub-tab emits reproducible R code", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if(!dir.exists(app_dir), "radiatR app directory not found")

  shiny::testServer(app_dir, {
    session$setInputs(load_example = 1)
    session$setInputs(go3 = 1)
    code <- spec_to_stats_code(current_spec())
    expect_match(code, "circ_summarise(", fixed = TRUE)
    expect_no_error(output$stats_code)
  })
})
