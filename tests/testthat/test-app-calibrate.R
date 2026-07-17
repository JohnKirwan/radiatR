# Fixed-reference unit-circle calibration is a load-time choice on the same
# Coordinates radio as normalize_xy (mutually exclusive by construction): the
# "calibrate" mode reads origin_x/origin_y/radius once at Next and records them
# on rv, forwarding (xy - origin)/radius uniformly to every row.

app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

# A minimal single-track generic CSV in raw pixel-like coordinates. With
# origin = c(500, 300) and radius = 400 the x-extent 100..900 maps to -1..1 and
# the y-extent 100..500 maps to -0.5..0.5 -- clean values for exact assertions.
write_demo_csv <- function() {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(Frame = 1:30,
                              Track1_X = seq(100, 900, length.out = 30),
                              Track1_Y = seq(100, 500, length.out = 30)),
                   csv, row.names = FALSE)
  csv
}

load_calibrate <- function(session, csv, origin_x, origin_y, radius) {
  session$setInputs(file = list(datapath = csv, name = basename(csv)))
  session$setInputs(dialect_sel = "generic")
  session$setInputs(map_x = "Track1_X", map_y = "Track1_Y",
                    map_time = "Frame", map_id = "")
  session$setInputs(origin_x = origin_x, origin_y = origin_y, radius = radius)
  session$setInputs(normalize_sel = "calibrate")
  session$setInputs(go2 = 1)
}

test_that("calibrate mode builds a uniformly calibrated Tracks", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    load_calibrate(session, csv, 500, 300, 400)
    expect_equal(rv$step, 2L)
    expect_false(is.null(rv$ts))

    # normalize_xy is off in calibrate mode; the two are mutually exclusive.
    expect_false(rv$normalize_xy)
    expect_false(rv$ts@meta[["normalize_xy"]])

    # The fixed reference is recorded on both rv and the Tracks metadata.
    expect_equal(rv$origin, c(500, 300))
    expect_equal(rv$radius, 400)
    expect_equal(rv$ts@meta[["calibration"]],
                 list(origin = c(500, 300), radius = 400))

    # Value assertion (not finite-only): coords are (raw - origin) / radius,
    # applied uniformly, so the known extents land on exact unit-circle values.
    d  <- as.data.frame(rv$ts)
    xs <- d[[rv$ts@cols$x]]
    ys <- d[[rv$ts@cols$y]]
    expect_equal(min(xs), -1)
    expect_equal(max(xs),  1)
    expect_equal(min(ys), -0.5)
    expect_equal(max(ys),  0.5)
  })
})

test_that("downloaded code reproduces the calibration the app loaded with", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    load_calibrate(session, csv, 500, 300, 400)
    session$setInputs(method = "distal", go3 = 1)

    # Spec-level round-trip: origin/radius carried onto the emitted spec.
    expect_equal(current_spec()$data$origin, c(500, 300))
    expect_equal(current_spec()$data$radius, 400)

    code <- figure_code_text()
    # Emitted read_tracks() calibrates with the entered reference and, because
    # calibration and normalize_xy are exclusive, disables the shape normalizer.
    expect_true(grepl("origin = c(500, 300)", code, fixed = TRUE))
    expect_true(grepl("radius = 400", code, fixed = TRUE))
    expect_true(grepl("normalize_xy = FALSE", code, fixed = TRUE))
    expect_false(grepl("normalize_xy = TRUE", code, fixed = TRUE))
  })
})

test_that("calibrate mode with no origin/radius errors rather than loading silently", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    session$setInputs(dialect_sel = "generic")
    session$setInputs(map_x = "Track1_X", map_y = "Track1_Y",
                      map_time = "Frame", map_id = "")
    # Calibrate selected but the reference inputs were never filled in.
    session$setInputs(normalize_sel = "calibrate")
    session$setInputs(go2 = 1)

    # Must not silently fall back to an uncalibrated load: no Tracks, still step 1.
    expect_equal(rv$step, 1L)
    expect_true(is.null(rv$ts))
    expect_false(is.null(rv$error))
  })
})

test_that("the Coordinates control offers a calibrate mode with origin/radius inputs", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    html <- paste(unlist(output$normalize_box), collapse = " ")

    expect_match(html, "normalize_sel")
    expect_match(html, "Calibrate")
    # The blank fixed-reference inputs exist for the conditional calibrate panel.
    expect_match(html, "origin_x")
    expect_match(html, "origin_y")
    expect_match(html, "radius")
    # Locked naming rule: never "arena" in an identifier (prose only).
    expect_false(grepl("arena_x", html, fixed = TRUE))
    expect_false(grepl("arena_y", html, fixed = TRUE))
    # The shape-only option must still not present itself as calibration.
    expect_false(grepl("is a calibration", html, fixed = TRUE))
  })
})

test_that("a non-calibrate load carries no origin/radius", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    session$setInputs(dialect_sel = "generic")
    session$setInputs(map_x = "Track1_X", map_y = "Track1_Y",
                      map_time = "Frame", map_id = "")
    session$setInputs(normalize_sel = "FALSE")
    session$setInputs(go2 = 1)

    expect_equal(rv$step, 2L)
    expect_null(rv$origin)
    expect_null(rv$radius)
    expect_null(rv$ts@meta[["calibration"]])

    session$setInputs(method = "distal", go3 = 1)
    expect_false(grepl("origin = c(", figure_code_text(), fixed = TRUE))
  })
})

test_that("origin/radius inputs do not leak when the mode is not calibrate", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    session$setInputs(dialect_sel = "generic")
    session$setInputs(map_x = "Track1_X", map_y = "Track1_Y",
                      map_time = "Frame", map_id = "")
    # Stale values present, but the selected mode is shape-normalize, not calibrate.
    session$setInputs(origin_x = 500, origin_y = 300, radius = 400)
    session$setInputs(normalize_sel = "TRUE")
    session$setInputs(go2 = 1)

    expect_equal(rv$step, 2L)
    expect_true(rv$normalize_xy)
    expect_true(rv$ts@meta[["normalize_xy"]])
    # Gating is on the mode, not on the mere presence of typed values.
    expect_null(rv$origin)
    expect_null(rv$radius)
  })
})
