tiny_two_groups <- function() {
  # group "b" has only one observation -> mean direction not estimable
  data.frame(
    heading = c(0.1, 0.2, 0.3, 1.0),
    grp     = c("a", "a", "a", "b"),
    stringsAsFactors = FALSE
  )
}

test_that("test_mean_directions signals radiatR_nonestimable for too-few obs", {
  e <- tryCatch(
    test_mean_directions(tiny_two_groups(), "grp"),
    radiatR_nonestimable = function(e) e
  )
  expect_s3_class(e, "radiatR_nonestimable")
  expect_match(e$reason, "2", fixed = TRUE)
})

test_that("test_mean_directions succeeds for valid input (no condition)", {
  ok <- data.frame(
    heading = c(0.1, 0.2, 0.3, 1.0, 1.1, 1.2),
    grp     = c("a", "a", "a", "b", "b", "b"),
    stringsAsFactors = FALSE
  )
  res <- test_mean_directions(ok, "grp")
  expect_true(is.data.frame(res))
  expect_true("p_value" %in% names(res))
})

test_that("test_concentration signals radiatR_nonestimable for too-few obs", {
  e <- tryCatch(
    test_concentration(tiny_two_groups(), "grp"),
    radiatR_nonestimable = function(e) e
  )
  expect_s3_class(e, "radiatR_nonestimable")
})

test_that("test_concentration succeeds for valid input (no condition)", {
  ok <- data.frame(
    heading = c(0.1, 0.2, 0.3, 1.0, 1.1, 1.2),
    grp     = c("a", "a", "a", "b", "b", "b"),
    stringsAsFactors = FALSE
  )
  res <- test_concentration(ok, "grp")
  expect_true(is.data.frame(res))
  expect_true("p_value" %in% names(res))
})

test_that("test_distributions signals radiatR_nonestimable for too-few obs", {
  e <- tryCatch(
    test_distributions(tiny_two_groups(), "grp"),
    radiatR_nonestimable = function(e) e
  )
  expect_s3_class(e, "radiatR_nonestimable")
  expect_match(e$reason, "2", fixed = TRUE)
})

test_that("test_distributions succeeds for valid input (no condition)", {
  ok <- data.frame(
    heading = c(0.1, 0.2, 0.3, 1.0, 1.1, 1.2),
    grp     = c("a", "a", "a", "b", "b", "b"),
    stringsAsFactors = FALSE
  )
  res <- test_distributions(ok, "grp")
  expect_true(is.data.frame(res))
  expect_true("p_value" %in% names(res))
})
