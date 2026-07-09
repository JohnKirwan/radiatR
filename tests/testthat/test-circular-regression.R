test_that("circ_regression recovers the slope on exact-link data", {
  set.seed(1); n <- 250; x <- rnorm(n)
  noise <- as.numeric(circular::rvonmises(n, mu = circular::circular(0), kappa = 6))
  theta <- (2 * atan(1.2 * x) + noise) %% (2 * pi)
  fit <- circ_regression(data.frame(heading = theta, x = x), heading ~ x)
  expect_true(fit$converged)
  s <- summary(fit)
  expect_equal(s$term, "x")
  expect_gt(s$estimate, 0.9); expect_lt(s$estimate, 1.5)
  expect_true(s$conf.low < 1.2 && s$conf.high > 1.2)
  expect_lt(s$p_value, 0.01)
})

test_that("summary has the documented columns; predict/fitted are wrapped radians", {
  set.seed(2); n <- 120; x <- rnorm(n)
  theta <- (2 * atan(0.8 * x) +
            as.numeric(circular::rvonmises(n, circular::circular(0), kappa = 5))) %% (2 * pi)
  fit <- circ_regression(data.frame(heading = theta, x = x), heading ~ x)
  s <- summary(fit)
  expect_named(s, c("term", "estimate", "se", "statistic", "p_value", "conf.low", "conf.high"))
  fv <- fitted(fit)
  expect_length(fv, n); expect_true(all(fv >= 0 & fv < 2 * pi))
  pv <- predict(fit, newdata = data.frame(x = c(-1, 0, 1)))
  expect_length(pv, 3); expect_true(all(pv >= 0 & pv < 2 * pi))
})

test_that("multi-covariate formula yields one row per term", {
  set.seed(3); n <- 220; x1 <- rnorm(n); x2 <- rnorm(n)
  theta <- (2 * atan(0.9 * x1 - 0.5 * x2) +
            as.numeric(circular::rvonmises(n, circular::circular(0), kappa = 6))) %% (2 * pi)
  fit <- circ_regression(data.frame(heading = theta, x1 = x1, x2 = x2), heading ~ x1 + x2)
  s <- summary(fit)
  expect_equal(nrow(s), 2L); expect_setequal(s$term, c("x1", "x2"))
})

test_that("predict handles a factor covariate", {
  set.seed(4); n <- 160; g <- factor(rep(c("a", "b"), each = n / 2))
  shift <- ifelse(g == "b", 1.0, 0)
  theta <- (shift +
            as.numeric(circular::rvonmises(n, circular::circular(0.2), kappa = 8))) %% (2 * pi)
  fit <- circ_regression(data.frame(heading = theta, g = g), heading ~ g)
  expect_true(fit$converged)
  pv <- predict(fit, newdata = data.frame(g = factor(c("a", "b"), levels = c("a", "b"))))
  expect_length(pv, 2)
})

test_that("too few rows returns a converged = FALSE NA object without error", {
  fit <- circ_regression(data.frame(heading = c(0.1, 0.2), x = c(1, 2)), heading ~ x)
  expect_false(fit$converged)
  expect_true(is.na(summary(fit)$estimate))
  expect_true(all(is.na(predict(fit))))
})

test_that("circ_regression recovers a simulated mean_slope (sign + significance)", {
  cond <- tibble::tibble(condition = "ms", n_trials = 150L, ref_mean = 0.0,
                         concentration_base = 12, mean_slope = 0.6,
                         predictor_mean = 0, predictor_sd = 1)
  s  <- simulate_tracks(n_points = 8, conditions = cond, seed = 11)
  hd <- s[!duplicated(s$trial_id), c("predictor", "final_heading")]
  names(hd)[2] <- "heading"
  fit <- circ_regression(hd, heading ~ predictor)
  expect_true(fit$converged)
  sm <- summary(fit)
  expect_gt(sm$estimate, 0)
  expect_lt(sm$p_value, 0.05)
})

test_that("a null mean_slope yields a slope not distinguishable from zero", {
  cond <- tibble::tibble(condition = "null", n_trials = 150L, ref_mean = 0.0,
                         concentration_base = 12, mean_slope = 0,
                         predictor_mean = 0, predictor_sd = 1)
  s  <- simulate_tracks(n_points = 8, conditions = cond, seed = 12)
  hd <- s[!duplicated(s$trial_id), c("predictor", "final_heading")]
  names(hd)[2] <- "heading"
  fit <- circ_regression(hd, heading ~ predictor)
  if (fit$converged) {
    sm <- summary(fit)                       # CI brackets 0 (robust vs a tight p-threshold)
    expect_lt(sm$conf.low, 0); expect_gt(sm$conf.high, 0)
  } else succeed("non-convergence under the null is acceptable")
})
