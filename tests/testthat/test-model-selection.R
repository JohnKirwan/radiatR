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

test_that("circ_model_select picks unimodal / axial / uniform on matching data", {
  set.seed(11)
  uni  <- data.frame(heading = rnorm(80, 1, 0.3) %% (2 * pi))
  ax   <- data.frame(heading = c(rnorm(40, 0, 0.2), rnorm(40, pi, 0.2)) %% (2 * pi))
  unif <- data.frame(heading = seq(0, 2 * pi, length.out = 81)[-81])

  expect_equal(circ_model_select(uni)$model[1],  "unimodal")
  expect_equal(circ_model_select(ax)$model[1],   "axial")
  expect_equal(circ_model_select(unif)$model[1], "uniform")
})

test_that("circ_model_select returns the tidy table shape and a winning weight", {
  set.seed(12)
  d <- data.frame(heading = rnorm(60, 2, 0.3) %% (2 * pi))
  r <- circ_model_select(d)
  expect_equal(nrow(r), 3L)
  expect_setequal(r$model, c("uniform", "unimodal", "axial"))
  expect_true(all(c("model", "n", "k", "logLik", "AIC", "AICc", "BIC", "dAICc", "weight")
                  %in% names(r)))
  expect_equal(sum(r$weight, na.rm = TRUE), 1, tolerance = 1e-12)
  expect_equal(r$dAICc[1], 0)
})

test_that("circ_model_select supports group_col", {
  set.seed(13)
  d <- rbind(
    data.frame(heading = rnorm(50, 1, 0.3) %% (2 * pi), grp = "uni"),
    data.frame(heading = c(rnorm(25, 0, 0.2), rnorm(25, pi, 0.2)) %% (2 * pi), grp = "ax"))
  r <- circ_model_select(d, group_col = "grp")
  expect_true("grp" %in% names(r))
  expect_equal(nrow(r), 6L)
  best <- vapply(split(r, r$grp), function(x) x$model[which.min(x$AICc)], character(1))
  expect_equal(best[["uni"]], "unimodal")
  expect_equal(best[["ax"]],  "axial")
})

test_that("circ_model_select errors on a missing angle column", {
  expect_error(circ_model_select(data.frame(x = 1:5), angle_col = "heading"),
               "not found")
})

test_that("axial closed-form density integrates to 1", {
  mu <- 0.7; kappa <- 2.3
  dens <- function(x) exp(kappa * cos(2 * (x - mu))) / (2 * pi * besselI(kappa, 0))
  area <- stats::integrate(dens, 0, 2 * pi, subdivisions = 400L)$value
  expect_equal(area, 1, tolerance = 1e-6)
})

test_that("axial does not out-score uniform on a large uniform sample", {
  set.seed(101)
  unif <- data.frame(heading = runif(500, 0, 2 * pi))
  r <- circ_model_select(unif)
  expect_equal(r$model[1], "uniform")
})
