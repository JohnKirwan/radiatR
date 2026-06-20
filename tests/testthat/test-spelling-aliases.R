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
