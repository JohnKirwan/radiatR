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
