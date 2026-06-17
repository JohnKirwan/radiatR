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

test_that(".circ_model_criteria computes AIC/AICc/BIC, sorts by AICc, weights sum to 1", {
  ll <- data.frame(model = c("uniform", "unimodal", "axial"), k = c(0L, 2L, 2L),
                   logLik = c(-100, -60, -90), stringsAsFactors = FALSE)
  cr <- radiatR:::.circ_model_criteria(ll, n = 50)
  expect_equal(cr$AIC[cr$model == "unimodal"], -2 * -60 + 2 * 2)
  expect_equal(cr$AICc[cr$model == "unimodal"], (-2 * -60 + 4) + (2 * 2 * 3) / (50 - 2 - 1))
  expect_equal(cr$BIC[cr$model == "uniform"], -2 * -100 + 0 * log(50))
  expect_equal(cr$model[1], "unimodal")
  expect_equal(cr$dAICc[1], 0)
  expect_equal(sum(cr$weight), 1, tolerance = 1e-12)
  expect_true(all(cr$weight >= 0 & cr$weight <= 1))
})

test_that(".circ_model_criteria: NA logLik -> NA criteria, excluded from weights, NA last", {
  ll <- data.frame(model = c("uniform", "unimodal", "axial"), k = c(0L, 2L, 2L),
                   logLik = c(-100, -60, NA), stringsAsFactors = FALSE)
  cr <- radiatR:::.circ_model_criteria(ll, n = 50)
  expect_true(is.na(cr$AICc[cr$model == "axial"]))
  expect_true(is.na(cr$weight[cr$model == "axial"]))
  expect_equal(sum(cr$weight, na.rm = TRUE), 1, tolerance = 1e-12)
  expect_equal(cr$model[nrow(cr)], "axial")
})

test_that(".circ_model_criteria: small n makes 2-param AICc NA but uniform finite", {
  ll <- data.frame(model = c("uniform", "unimodal", "axial"), k = c(0L, 2L, 2L),
                   logLik = c(-5, -3, -4), stringsAsFactors = FALSE)
  cr <- radiatR:::.circ_model_criteria(ll, n = 3)
  expect_true(is.na(cr$AICc[cr$model == "unimodal"]))
  expect_false(is.na(cr$AICc[cr$model == "uniform"]))
})
