# Simulate headings whose concentration changes with a predictor. simulate_tracks
# uses kappa = concentration_base + concentration_slope * predictor (identity).
.cr_sim <- function(slope, base = 4, n_trials = 200L, seed = 11) {
  cond <- tibble::tibble(condition = "cs", n_trials = n_trials, ref_mean = 0.0,
                         concentration_base = base, concentration_slope = slope,
                         predictor_mean = 0, predictor_sd = 1)
  s  <- simulate_tracks(n_points = 8, conditions = cond, seed = seed)
  hd <- s[!duplicated(s$trial_id), c("predictor", "final_heading")]
  names(hd)[2] <- "heading"
  hd
}

test_that("recovers a positive concentration slope (identity link)", {
  hd  <- .cr_sim(slope = 3, base = 4)
  fit <- concentration_regression(hd, heading ~ predictor, link = "identity")
  expect_true(fit$converged)
  expect_gt(fit$coefficients[["predictor"]], 0)
  expect_lt(fit$p_value[["predictor"]], 0.05)
})

test_that("recovers the trend direction under the log link", {
  hd  <- .cr_sim(slope = 3, base = 4)
  fit <- concentration_regression(hd, heading ~ predictor, link = "log")
  expect_true(fit$converged)
  expect_gt(fit$coefficients[["predictor"]], 0)
})

test_that("a zero slope is not significant", {
  hd  <- .cr_sim(slope = 0, base = 6, seed = 2)
  fit <- concentration_regression(hd, heading ~ predictor, link = "identity")
  expect_true(fit$converged)
  expect_gt(fit$p_value[["predictor"]], 0.05)
})

test_that("a factor covariate returns finite intercept + level coefficients", {
  set.seed(3)
  g  <- factor(rep(c("lo", "hi"), each = 60), levels = c("lo", "hi"))
  th <- as.numeric(c(
    circular::rvonmises(60, circular::circular(0), kappa = 1.5),
    circular::rvonmises(60, circular::circular(0), kappa = 8)))
  fit <- concentration_regression(data.frame(heading = th, g = g), heading ~ g,
                                  link = "log")
  expect_true(fit$converged)
  expect_setequal(fit$terms, c("(Intercept)", "ghi"))
  expect_true(all(is.finite(fit$coefficients)))
})

test_that("too few rows returns a converged = FALSE NA object without error", {
  fit <- concentration_regression(data.frame(heading = c(0.1, 0.2), x = c(1, 2)),
                                  heading ~ x)
  expect_false(fit$converged)
  expect_true(all(is.na(fit$coefficients)))
})

test_that("init of the wrong length errors", {
  hd <- .cr_sim(slope = 1)
  expect_error(
    concentration_regression(hd, heading ~ predictor, init = c(1)),
    "length")
})

# ---- methods -----------------------------------------------------------------

test_that("summary has the documented columns, one row per term", {
  hd  <- .cr_sim(slope = 2, base = 4)
  fit <- concentration_regression(hd, heading ~ predictor, link = "log")
  s   <- summary(fit)
  expect_named(s, c("term", "estimate", "se", "statistic", "p_value",
                    "conf.low", "conf.high"))
  expect_setequal(s$term, c("(Intercept)", "predictor"))
})

test_that("predict/fitted return positive kappa on the response scale", {
  hd  <- .cr_sim(slope = 2, base = 4)
  fit <- concentration_regression(hd, heading ~ predictor, link = "log")
  fv  <- fitted(fit)
  expect_length(fv, nrow(hd))
  expect_true(all(fv > 0))
  nd  <- data.frame(predictor = c(-1, 0, 1))
  pv  <- predict(fit, newdata = nd)
  expect_length(pv, 3)
  expect_true(all(pv > 0))
  expect_true(pv[3] > pv[1])                       # kappa rises with the predictor
})

test_that("predict returns all-NA for a non-converged fit", {
  fit <- concentration_regression(data.frame(heading = c(0.1, 0.2), x = c(1, 2)),
                                  heading ~ x)
  expect_true(all(is.na(predict(fit))))
  expect_length(predict(fit, newdata = data.frame(x = c(1, 2, 3))), 3)
})

test_that("print runs without error and returns the object invisibly", {
  hd  <- .cr_sim(slope = 2, base = 4)
  fit <- concentration_regression(hd, heading ~ predictor)
  expect_output(print(fit), "concentration")
  expect_identical(withVisible(print(fit))$visible, FALSE)
})
