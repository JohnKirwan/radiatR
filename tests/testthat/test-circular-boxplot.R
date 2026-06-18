test_that("circ_boxplot_stats recovers the median and brackets ~50% in the box", {
  set.seed(1)
  ang <- as.numeric(circular::rvonmises(200, mu = circular::circular(pi/2), kappa = 4))
  s <- circ_boxplot_stats(data.frame(heading = ang))
  expect_true(s$drawable)
  dmed <- abs(((s$median - pi/2 + pi) %% (2*pi)) - pi)
  expect_lt(dmed, 0.25)
  rel <- ((ang - s$median + pi) %% (2*pi)) - pi
  h   <- ((s$hinges - s$median + pi) %% (2*pi)) - pi
  inside <- mean(rel >= min(h) & rel <= max(h))
  expect_gt(inside, 0.40); expect_lt(inside, 0.60)
})

test_that("the fence multiplier is ~1.5 when concentrated and ~0.5 near uniform", {
  set.seed(2)
  conc <- as.numeric(circular::rvonmises(300, mu = circular::circular(0), kappa = 12))
  near_u <- as.numeric(circular::rvonmises(300, mu = circular::circular(0), kappa = 0.1))
  cc <- circ_boxplot_stats(data.frame(heading = conc))$constant
  cu <- circ_boxplot_stats(data.frame(heading = near_u))$constant
  expect_gt(cc, 1.3); expect_lt(cc, 1.9)
  expect_gt(cu, 0.45); expect_lt(cu, 0.70)
})

test_that("far-out values fall outside the whiskers", {
  set.seed(3)
  ang <- c(as.numeric(circular::rvonmises(120, mu = circular::circular(0), kappa = 20)), pi)
  s <- circ_boxplot_stats(data.frame(heading = ang))
  expect_true(s$drawable)
  expect_true(length(s$far_out) >= 1)
  rel_fo <- ((s$far_out - s$median + pi) %% (2*pi)) - pi
  f <- ((s$fences - s$median + pi) %% (2*pi)) - pi
  expect_true(all(rel_fo > max(f) | rel_fo < min(f)))
})

test_that("not-drawable cases are flagged (uniform median, too few points)", {
  set.seed(4)
  uni <- runif(200, 0, 2*pi)
  su  <- circ_boxplot_stats(data.frame(heading = uni))
  expect_true(!is.na(su$reason))
  few <- circ_boxplot_stats(data.frame(heading = c(0.1, 0.2, 0.3)))
  expect_false(few$drawable)
  expect_match(few$reason, "4")
})

test_that("highly concentrated / near-identical data does not crash and gives ~1.5 constant", {
  s8 <- circ_boxplot_stats(data.frame(heading = rep(1, 8)))
  expect_true(s8$drawable)
  expect_true(is.finite(s8$constant))
  expect_gt(s8$constant, 1.3); expect_lt(s8$constant, 1.8)
  expect_true(all(is.finite(s8$fences)))
  expect_true(is.na(s8$reason))                # not flagged near-uniform
  tight <- 1 + c(-1e-3, 1e-3, rep(0, 6))
  expect_silent(circ_boxplot_stats(data.frame(heading = tight)))
})

test_that("circ_boxplot_stats matches bpDir::CircularBoxplot as an oracle", {
  skip_if_not_installed("bpDir")
  set.seed(5)
  ang <- as.numeric(circular::rvonmises(150, mu = circular::circular(1), kappa = 3))
  s <- circ_boxplot_stats(data.frame(heading = ang))
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  ref <- bpDir::CircularBoxplot(circular::circular(ang, units = "radians", modulo = "2pi"),
                                units = "radians", constant = "optimal")
  expect_equal(s$constant, as.numeric(ref$constant), tolerance = 1e-3)
})
