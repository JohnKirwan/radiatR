# The Coordinates control (normalize_xy) is a load-time choice: it is read once
# when the user clicks Next and recorded on rv, never re-read live.

app_dir <- function() {
  d <- system.file("app", package = "radiatR")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "app")
  d
}

# A minimal single-track generic CSV in raw pixel-like coordinates.
write_demo_csv <- function() {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(Frame = 1:30,
                              Track1_X = seq(100, 900, length.out = 30),
                              Track1_Y = seq(100, 500, length.out = 30)),
                   csv, row.names = FALSE)
  csv
}

load_generic <- function(session, csv, normalize_sel) {
  session$setInputs(file = list(datapath = csv, name = basename(csv)))
  session$setInputs(dialect_sel = "generic")
  session$setInputs(map_x = "Track1_X", map_y = "Track1_Y",
                    map_time = "Frame", map_id = "")
  session$setInputs(normalize_sel = normalize_sel)
  session$setInputs(go2 = 1)
}

test_that("normalize_sel = TRUE builds a shape-normalized Tracks", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    load_generic(session, csv, "TRUE")
    expect_equal(rv$step, 2L)
    expect_false(is.null(rv$ts))
    expect_true(rv$ts@meta[["normalize_xy"]])
    expect_true(rv$normalize_xy)
  })
})

test_that("normalize_sel = FALSE keeps the raw coordinates as supplied", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    load_generic(session, csv, "FALSE")
    expect_equal(rv$step, 2L)
    expect_false(is.null(rv$ts))
    expect_false(rv$ts@meta[["normalize_xy"]])
    expect_false(rv$normalize_xy)

    # Raw pixel values survive untouched (normalized coords would be within +-1).
    d  <- as.data.frame(rv$ts)
    xs <- d[[rv$ts@cols$x]]
    expect_equal(min(xs), 100)
    expect_equal(max(xs), 900)
  })
})

test_that("the Coordinates control renders for a trajectory upload only", {
  skip_if_not_installed("shiny")
  skip_if(!dir.exists(app_dir()), "app dir not found")
  csv <- write_demo_csv()

  shiny::testServer(app_dir(), {
    session$setInputs(file = list(datapath = csv, name = basename(csv)))
    html <- paste(unlist(output$normalize_box), collapse = " ")
    expect_match(html, "normalize_sel")
    expect_match(html, "unit circle")
    # The TODO forbids presenting this shape-only transform as calibration.
    expect_false(grepl("is a calibration", html, fixed = TRUE))

    # Headings mode is angle-only: the control must not appear.
    session$setInputs(input_type = "headings")
    expect_null(output$normalize_box)
  })
})
