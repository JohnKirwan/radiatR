test_that(".circ_model_loglik returns the three models with the uniform closed form", {
  set.seed(1)
  th <- rnorm(50, 1, 0.4) %% (2 * pi)
  ll <- radiatR:::.circ_model_loglik(th)
  expect_equal(ll$model, c("uniform", "unimodal", "axial"))
  expect_equal(ll$k, c(0L, 2L, 2L))
  expect_equal(ll$logLik[ll$model == "uniform"], -length(th) * log(2 * pi))
  expect_gt(ll$logLik[ll$model == "unimodal"], ll$logLik[ll$model == "uniform"])
})

test_that(".circ_model_loglik: antipodal data favours axial over unimodal", {
  set.seed(2)
  th <- c(rnorm(30, 0, 0.2), rnorm(30, pi, 0.2)) %% (2 * pi)
  ll <- radiatR:::.circ_model_loglik(th)
  expect_gt(ll$logLik[ll$model == "axial"], ll$logLik[ll$model == "unimodal"])
})

test_that(".circ_model_loglik: unimodal logLik matches a direct dvonmises computation", {
  set.seed(3)
  th <- rnorm(40, 2, 0.5) %% (2 * pi)
  vm <- vonmises_fit(data.frame(heading = th))
  direct <- sum(as.numeric(circular::dvonmises(
    circular::circular(th, units = "radians", type = "angles"),
    mu = circular::circular(vm$mu, units = "radians", type = "angles"),
    kappa = vm$kappa, log = TRUE)))
  ll <- radiatR:::.circ_model_loglik(th)
  expect_equal(ll$logLik[ll$model == "unimodal"], direct, tolerance = 1e-9)
})
