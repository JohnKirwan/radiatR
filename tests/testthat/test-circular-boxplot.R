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

test_that("axial boxplot recovers the median axis in [0, pi) from antipodal data", {
  set.seed(6)
  axis <- pi/3
  half <- as.numeric(circular::rvonmises(150, mu = circular::circular(2*axis), kappa = 8)) / 2
  ang  <- c(half, (half + pi) %% (2*pi))
  s <- circ_boxplot_stats(data.frame(heading = ang), axial = TRUE)
  expect_true(s$axial); expect_true(s$drawable)
  expect_true(s$median >= 0 && s$median < pi)
  daxis <- abs(((s$median - axis + pi/2) %% pi) - pi/2)
  expect_lt(daxis, 0.2)
  expect_true(all(s$far_out >= 0 & s$far_out < pi))
})

# NOTE on the bpDir::AxialBoxplot oracle: bpDir derives its "optimal" fence
# constant from A1inv(rho.circular(A)) on the UNDOUBLED axial data. For axial
# (antipodal) data that mean resultant length is ~0, so bpDir's constant
# degenerates to the near-uniform value (~0.5) for essentially any axial
# sample. radiatR instead estimates the constant on the DOUBLED (unimodal)
# data, per Buttarazzi et al. (2018) section 4.4 -- this is the statistically
# meaningful Tukey value (~1.5 when concentrated). The two therefore do NOT
# match by design; this test pins that documented divergence rather than
# asserting a (false) equality.
test_that("axial constant is taken on the doubled data, diverging from bpDir's degenerate value", {
  skip_if_not_installed("bpDir")
  if (!"AxialBoxplot" %in% getNamespaceExports("bpDir")) skip("AxialBoxplot not exported")
  set.seed(7)
  half <- as.numeric(circular::rvonmises(120, mu = circular::circular(2), kappa = 5)) / 2
  ang  <- c(half, (half + pi) %% (2*pi))
  s <- circ_boxplot_stats(data.frame(heading = ang), axial = TRUE)
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  ref <- try(bpDir::AxialBoxplot(circular::circular(ang, units = "radians", modulo = "2pi"),
                                 units = "radians"), silent = TRUE)
  if (inherits(ref, "try-error") || is.null(ref$constant)) skip("AxialBoxplot has no $constant")
  # radiatR uses the concentrated, doubled data -> a meaningful Tukey-like constant.
  expect_gt(s$constant, 1.3); expect_lt(s$constant, 1.9)
  # bpDir's undoubled estimate degenerates to the near-uniform value.
  expect_lt(as.numeric(ref$constant), 0.7)
})
