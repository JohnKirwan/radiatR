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

weighted_tracks <- function(w) {
  d <- data.frame(
    id    = rep(c("a", "b"), each = 3),
    time  = rep(1:3, 2),
    angle = c(0.1, 0.2, 0.3, 1.0, 1.1, 1.2),
    w     = w
  )
  tracks(d, id = "id", time = "time", angle = "angle", angle_unit = "radians",
         weight = "w")
}

test_that("circ_summary() signals radiatR_nonestimable for an id whose weights are all zero", {
  tr <- weighted_tracks(c(1, 1, 1, 0, 0, 0))
  e <- tryCatch(circ_summary(tr), radiatR_nonestimable = function(e) e)
  expect_s3_class(e, "radiatR_nonestimable")
  expect_identical(e$group, "b")
  expect_identical(e$n, 3L)
  expect_match(e$reason, "weights sum to zero", fixed = TRUE)
})

test_that("circ_summary() signals radiatR_nonestimable when every weight is zero (global)", {
  tr <- weighted_tracks(rep(0, 6))
  expect_error(circ_summary(tr, by = "global"), class = "radiatR_nonestimable")
})

test_that("circ_summary() treats a zero weight as excluding one observation", {
  res <- circ_summary(weighted_tracks(c(1, 1, 0, 0, 2, 0)))
  expect_equal(res$mean_dir, c(0.15, 1.1))
  expect_true(all(is.finite(res$resultant_R)))
})

test_that("circ_summary() weights the mean direction, not only the resultant length", {
  res <- circ_summary(weighted_tracks(c(3, 1, 0, 1, 1, 1)), by = "id")
  expect_equal(res$mean_dir[1], atan2(3 * sin(0.1) + sin(0.2), 3 * cos(0.1) + cos(0.2)))
  expect_equal(res$mean_dir[2], 1.1)
})

test_that("circ_summary(w = ) rejects a column that is not a usable weight", {
  tr <- weighted_tracks(rep(1, 6))
  tr@data$neg <- c(1, -1, 1, 1, 1, 1)
  tr@data$gap <- c(1, NA, 1, 1, 1, 1)
  tr@data$txt <- "x"
  for (bad in c("nope", "neg", "gap", "txt")) {
    e <- tryCatch(circ_summary(tr, w = bad), radiatR_invalid_argument = function(e) e)
    expect_s3_class(e, "radiatR_invalid_argument")
    expect_identical(e$arg, "w")
  }
  expect_match(conditionMessage(tryCatch(circ_summary(tr, w = "nope"), error = identity)),
               "nope", fixed = TRUE)
})

test_that("circ_summary(w = ) uses the named column in place of the Tracks weight", {
  tr <- weighted_tracks(rep(1, 6))
  tr@data$w2 <- c(1, 1, 0, 0, 2, 0)
  expect_equal(circ_summary(tr, w = "w2")$mean_dir, c(0.15, 1.1))
})

test_that("circ_summary() does not count zero-weight observations towards kappa", {
  res <- circ_summary(weighted_tracks(c(1, 0, 0, 1, 1, 1)))
  expect_identical(res$kappa[1], NA_real_)
  expect_true(is.finite(res$kappa[2]))
})
