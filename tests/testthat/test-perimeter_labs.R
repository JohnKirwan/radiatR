test_that("scale_cardinal returns 4- and 8-point specs", {
  s4 <- scale_cardinal()
  expect_identical(s4$n, 4L)
  expect_identical(s4$labels, c("N", "E", "S", "W"))
  expect_length(s4$at, 4L)
  expect_equal(s4$at[1], pi / 2)
  expect_equal(s4$at[2] %% (2 * pi), 0)

  s8 <- scale_cardinal(points = 8)
  expect_identical(s8$n, 8L)
  expect_identical(s8$labels,
                   c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))
})

test_that("scale_cardinal rejects unsupported point counts", {
  expect_error(scale_cardinal(points = 16), "4 or 8")
})

test_that("scale_clock labels sparsely and auto-picks `every`", {
  s24 <- scale_clock()
  expect_identical(s24$n, 24L)
  expect_identical(s24$labels, c("0", "6", "12", "18"))

  s12 <- scale_clock(hours = 12)
  expect_identical(s12$n, 12L)
  expect_identical(s12$labels, c("0", "3", "6", "9"))

  expect_error(scale_clock(hours = 13), "12 or 24")
  expect_error(scale_clock(every = 7), "evenly divide")
})

test_that("scale_months supports three label formats", {
  expect_identical(scale_months()$labels, month.abb)
  expect_identical(scale_months("initial")$labels, substr(month.abb, 1, 1))
  expect_identical(scale_months("number")$labels, as.character(1:12))
  expect_identical(scale_months()$n, 12L)
  expect_length(scale_months()$at, 12L)
})

test_that("scale_seconds draws 60 ticks but few labels", {
  s <- scale_seconds()
  expect_identical(s$n, 60L)
  expect_identical(s$labels, c("0", "15", "30", "45"))
  expect_error(scale_seconds(every = 7), "evenly divide")
})

test_that(".check_scale rejects malformed specs", {
  expect_error(.check_scale(list(n = 12, at = 1:3)), "labels")
  expect_error(
    .check_scale(list(n = 12, at = 1:3, labels = c("a", "b"))),
    "same length")
  expect_error(
    .check_scale(list(n = 0, at = numeric(0), labels = character(0))),
    "positive integer")
})
