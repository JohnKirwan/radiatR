# kappa in the circular summaries is the von Mises maximum-likelihood estimate,
# kappa = A1inv(Rbar), as circular::mle.vonmises() computes it. It used to be NA
# in circ_summarise() and circ_summary(<Tracks>): the estimator was looked up as
# circular::est.kappa, which does not exist, so only a fallback was returned.

vm_sample <- function(n = 60, mu = 1, kappa = 5, seed = 1) {
  set.seed(seed)
  as.numeric(circular::rvonmises(n, circular::circular(mu), kappa)) %% (2 * pi)
}

test_that(".kappa_from_Rbar is the estimator circular uses", {
  r <- seq(0.01, 0.99, by = 0.01)
  expect_identical(.kappa_from_Rbar(r), circular::A1inv(r))
})

test_that("circ_summarise returns the von Mises kappa estimate", {
  x <- vm_sample()
  res <- circ_summarise(data.frame(heading = x), heading, units = "radians")
  mle <- circular::mle.vonmises(circular::circular(x, units = "radians"))

  expect_true(is.finite(res$kappa))
  expect_equal(res$kappa, as.numeric(mle$kappa))
  expect_equal(res$kappa, circular::A1inv(res$resultant_R))
  # Sanity: the sample was drawn with kappa = 5.
  expect_gt(res$kappa, 3)
  expect_lt(res$kappa, 8)
})

test_that("circ_summarise returns kappa per group", {
  df <- data.frame(
    heading = c(vm_sample(kappa = 2, seed = 1), vm_sample(kappa = 8, seed = 2)),
    g = rep(c("lo", "hi"), each = 60))
  res <- circ_summarise(df, heading, units = "radians", .by = "g")

  expect_true(all(is.finite(res$kappa)))
  expect_equal(res$kappa, circular::A1inv(res$resultant_R))
  expect_lt(res$kappa[res$g == "lo"], res$kappa[res$g == "hi"])
})

test_that("circ_summarise axial kappa is that of the doubled angles", {
  x <- vm_sample(n = 120)
  ax <- circ_summarise(data.frame(heading = x), heading, units = "radians",
                       axial = TRUE)
  expect_true(is.finite(ax$kappa))
  expect_equal(ax$kappa, circular::A1inv(ax$resultant_R))
})

test_that("circ_summary on Tracks returns kappa per id and globally", {
  ts <- simulate_tracks(n_points = 30, seed = 8, output = "trajset")
  global <- circ_summary(ts, by = "global")
  per_id <- circ_summary(ts, by = "id")

  expect_true(is.finite(global$kappa))
  expect_equal(global$kappa, circular::A1inv(global$resultant_R))
  expect_true(all(is.finite(per_id$kappa)))
  expect_equal(per_id$kappa, circular::A1inv(per_id$resultant_R))
})

test_that("circ_summary on Tracks uses the weighted resultant length for kappa", {
  ts <- simulate_tracks(n_points = 30, seed = 8, output = "trajset")
  d <- as.data.frame(ts)
  set.seed(3)
  d$wt <- stats::runif(nrow(d), 0.1, 2)
  weighted <- tracks(d, id = ts@cols$id, time = ts@cols$time,
                     angle = ts@cols$angle, angle_unit = "radians",
                     weight = "wt")
  res <- circ_summary(weighted, by = "global")
  plain <- circ_summary(ts, by = "global")

  expect_false(isTRUE(all.equal(res$resultant_R, plain$resultant_R)))
  expect_equal(res$kappa, circular::A1inv(res$resultant_R))
})

test_that("circ_summary on Tracks reports NA kappa for fewer than three angles", {
  d <- data.frame(id  = c("one", "two", "two", "three", "three", "three"),
                  t   = c(1, 1, 2, 1, 2, 3),
                  ang = c(1, 1, 1.2, 1, 1.2, 1.4))
  ts <- tracks(d, id = "id", time = "t", angle = "ang", angle_unit = "radians")
  res <- circ_summary(ts, by = "id")
  kappa <- stats::setNames(res$kappa, res$id)

  expect_true(is.na(kappa[["one"]]))
  expect_true(is.na(kappa[["two"]]))
  expect_equal(kappa[["three"]],
               circular::A1inv(res$resultant_R[res$id == "three"]))
  # The other statistics are still reported for short tracks.
  expect_false(anyNA(res$mean_dir))
  expect_false(anyNA(res$resultant_R))
})
