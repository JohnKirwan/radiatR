test_that(".dvm_log matches circular::dvonmises", {
  th <- seq(0, 2 * pi, length.out = 20)
  a  <- radiatR:::.dvm_log(th, mu = 1.2, kappa = 3)
  b  <- log(as.numeric(circular::dvonmises(
    circular::circular(th, units = "radians"),
    mu = circular::circular(1.2, units = "radians"), kappa = 3)))
  expect_equal(a, b, tolerance = 1e-8)
})

test_that(".fit_vm_uniform recovers a directed-plus-background mixture", {
  set.seed(1)
  n <- 600; p <- 0.7
  z <- runif(n) < p
  th <- ifelse(z,
    as.numeric(circular::rvonmises(n, mu = circular::circular(1.0), kappa = 4)),
    runif(n, 0, 2 * pi)) %% (2 * pi)
  fit <- radiatR:::.fit_vm_uniform(th)
  expect_true(fit$converged)
  expect_equal(fit$p,     0.7, tolerance = 0.15)
  expect_equal(fit$mu,    1.0, tolerance = 0.2)
  expect_true(fit$kappa > 1)
  expect_true(is.finite(fit$logLik))
})

test_that(".fit_vm_uniform logLik beats a single von Mises on mixed data", {
  set.seed(2)
  n <- 800
  th <- ifelse(runif(n) < 0.6,
    as.numeric(circular::rvonmises(n, mu = circular::circular(0.0), kappa = 6)),
    runif(n, 0, 2 * pi)) %% (2 * pi)
  vm  <- vonmises_fit(data.frame(heading = th))
  ll_vm <- sum(radiatR:::.dvm_log(th, vm$mu, vm$kappa))
  expect_gt(radiatR:::.fit_vm_uniform(th)$logLik, ll_vm)
})

test_that(".fit_two_vm recovers two non-antipodal modes", {
  set.seed(3)
  n <- 800; g <- 0.6
  z <- runif(n) < g
  th <- ifelse(z,
    as.numeric(circular::rvonmises(n, mu = circular::circular(0.5), kappa = 8)),
    as.numeric(circular::rvonmises(n, mu = circular::circular(2.5), kappa = 8))
  ) %% (2 * pi)
  fit <- radiatR:::.fit_two_vm(th)
  expect_true(fit$converged)
  expect_gte(fit$gamma, 0.5)
  # the two fitted means should match {0.5, 2.5} up to labelling
  got <- sort(c(fit$mu1, fit$mu2))
  expect_equal(got, c(0.5, 2.5), tolerance = 0.25)
})

test_that(".fit_two_vm logLik is at least the axial logLik on axial data", {
  set.seed(4)
  th <- c(as.numeric(circular::rvonmises(200, circular::circular(0), kappa = 6)),
          as.numeric(circular::rvonmises(200, circular::circular(pi), kappa = 6))) %% (2 * pi)
  ax  <- vonmises_fit(data.frame(heading = th), axial = TRUE)
  ll_ax <- sum(ax$kappa * (cos(2 * (th - ax$mu)) - 1)) -
           length(th) * (log(2 * pi) + log(besselI(ax$kappa, 0, expon.scaled = TRUE)))
  # the free 2-vM mixture generalises the constrained axial model, so its
  # maximised logLik cannot be lower (allow tiny numerical slack)
  expect_gte(radiatR:::.fit_two_vm(th)$logLik, ll_ax - 1e-3)
})

test_that(".fit_two_vm agrees with CircMLE on the best bimodal model", {
  skip_if_not_installed("CircMLE")
  set.seed(31)
  th <- c(as.numeric(circular::rvonmises(150, circular::circular(0.5), kappa = 6)),
          as.numeric(circular::rvonmises(150, circular::circular(3.0), kappa = 6))) %% (2 * pi)

  ours <- radiatR:::.fit_two_vm(th)$logLik

  x   <- circular::circular(th, units = "radians", type = "angles")
  res <- CircMLE::circ_mle(x)$results
  # circ_mle reports -LogLik per model; the free 2-component von Mises mixtures
  # are the M4/M5 family. Take the best (smallest -LogLik) mixture model as the
  # oracle for our free two-vM fit.
  mix_rows  <- grepl("^M[45]", rownames(res))
  oracle_ll <- -min(res[mix_rows, "-LogLik"], na.rm = TRUE)

  # our maximised logLik should match the best CircMLE mixture within tolerance
  # (both maximise the same family; small differences from optimiser settings).
  expect_equal(ours, oracle_ll, tolerance = 0.5)
})
