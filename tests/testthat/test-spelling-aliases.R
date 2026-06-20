# a local stand-in exercising the helper exactly as real functions will
f_nulldef  <- function(colour_col = NULL, color_col = NULL) {
  radiatR:::.apply_spelling_aliases(); colour_col
}
f_realdef  <- function(colour = "black", color = NULL) {
  radiatR:::.apply_spelling_aliases(); colour
}

test_that("alias supplies the canonical value (NULL-default arg)", {
  expect_equal(f_nulldef(colour_col = "x"), "x")
  expect_equal(f_nulldef(color_col  = "x"), "x")        # American alias works
  expect_null(f_nulldef())                              # neither -> default
})

test_that("alias works for a non-NULL-default arg without false conflict", {
  expect_equal(f_realdef(), "black")                    # default untouched
  expect_equal(f_realdef(color = "red"), "red")         # alias, default not a conflict
  expect_equal(f_realdef(colour = "blue"), "blue")
})

test_that("supplying BOTH spellings is a hard error", {
  expect_error(f_nulldef(colour_col = "x", color_col = "y"), "only one")
  expect_error(f_realdef(colour = "a", color = "b"), "only one")
})

test_that(".SPELLING_PAIRS maps alias -> canonical and every value is British", {
  pairs <- radiatR:::.SPELLING_PAIRS
  expect_true(all(grepl("color", names(pairs))))        # keys are American
  expect_true(all(grepl("colour", unname(pairs))))      # values are British
  expect_equal(pairs[["color_col"]], "colour_col")
  expect_equal(pairs[["track_color"]], "track_colour")
})

test_that("every exported colour-bearing function declares the matching color* alias", {
  pairs <- radiatR:::.SPELLING_PAIRS
  brit_to_us <- stats::setNames(names(pairs), unname(pairs))
  fns <- c("add_circ","add_circ_interval","add_circ_mean","add_circular_boxplot",
           "add_circular_density","add_heading_arrow","add_heading_density",
           "add_heading_interval","add_heading_points","add_heading_vectors",
           "add_multiple_circles","add_origin_point","add_quadrant_lines",
           "add_radial_grid","add_stacked_headings","add_ticks","compute_circ_mean",
           "degree_labs")
  for (fn in fns) {
    fmls <- names(formals(getFromNamespace(fn, "radiatR")))
    canon <- intersect(fmls, names(brit_to_us))          # British colour formals
    for (b in canon)
      expect_true(brit_to_us[[b]] %in% fmls,
                  info = paste0(fn, " missing alias ", brit_to_us[[b]], " for ", b))
  }
  # radiate.default + gg_traj method
  rd <- names(formals(getS3method("radiate", "default")))
  for (b in intersect(rd, names(brit_to_us)))
    expect_true(brit_to_us[[b]] %in% rd, info = paste("radiate.default", b))
})

test_that("color aliases produce identical results to colour on real functions", {
  data(cpunctatus, package = "radiatR", envir = environment())
  hd <- derive_headings(cpunctatus, rule = "distal")

  # explicit-arg layer: same built layer data
  l_uk <- add_heading_points(hd, colour = "red")
  l_us <- add_heading_points(hd, color  = "red")
  expect_equal(l_us$aes_params$colour %||% l_us$geom_params$colour,
               l_uk$aes_params$colour %||% l_uk$geom_params$colour)

  # non-NULL default: add_ticks
  expect_equal(add_ticks(color = "red")$aes_params$colour, "red")

  # radiate.default control arg
  p_uk <- radiate(cpunctatus, show_tracks = TRUE, track_colour = "sequence")
  p_us <- radiate(cpunctatus, show_tracks = TRUE, track_color  = "sequence")
  expect_equal(class(p_us$scales$get_scales("colour")),
               class(p_uk$scales$get_scales("colour")))

  # add_circ canonical rename + both spellings reach the circle colour
  expect_silent(add_circ(circle_colour = "red"))
  expect_silent(add_circ(circle_color  = "red"))

  # both spellings -> error
  expect_error(add_heading_points(hd, colour = "a", color = "b"), "only one")
})
