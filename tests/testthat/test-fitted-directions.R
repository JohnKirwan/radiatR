test_that("fitted_directions shapes regression predictions for add_circ_mean", {
  set.seed(1); n <- 200; x <- rnorm(n)
  theta <- (2 * atan(1.0 * x) +
            as.numeric(circular::rvonmises(n, circular::circular(0), kappa = 6))) %% (2 * pi)
  fit <- circ_regression(data.frame(heading = theta, x = x), heading ~ x)
  fd  <- fitted_directions(fit, at = seq(-2, 2, length.out = 5))
  expect_named(fd, c("mean_dir", "resultant_R", "x"))
  expect_equal(fd$mean_dir, predict(fit, data.frame(x = seq(-2, 2, length.out = 5))))
  expect_true(all(fd$resultant_R == as.numeric(circular::A1(fit$kappa))))
  expect_s3_class(attr(fd, "display", exact = TRUE), "circ_display")
  lyr <- add_circ_mean(fd, colour_col = "x")
  expect_true(inherits(lyr, "Layer"))
  p <- ggplot2::ggplot() + ggplot2::coord_fixed() + lyr
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("fitted_directions newdata path and error cases", {
  set.seed(2); n <- 150; x1 <- rnorm(n); x2 <- rnorm(n)
  theta <- (2 * atan(0.8 * x1) +
            as.numeric(circular::rvonmises(n, circular::circular(0), kappa = 6))) %% (2 * pi)
  fit <- circ_regression(data.frame(heading = theta, x1 = x1, x2 = x2), heading ~ x1 + x2)
  fd  <- fitted_directions(fit, newdata = data.frame(x1 = c(-1, 0, 1), x2 = 0))
  expect_equal(nrow(fd), 3L)
  expect_true(all(c("x1", "x2") %in% names(fd)))
  expect_error(fitted_directions(fit, at = c(-1, 0, 1)), "multiple predictors")
  expect_error(fitted_directions(fit), "exactly one")
  expect_error(fitted_directions(fit, at = 1, newdata = data.frame(x1 = 1, x2 = 1)), "exactly one")
})

test_that("non-converged fit yields NA mean_dir without error", {
  fit <- circ_regression(data.frame(heading = c(0.1, 0.2), x = c(1, 2)), heading ~ x)  # n < p+2
  expect_false(fit$converged)
  fd <- fitted_directions(fit, at = c(0, 1))
  expect_true(all(is.na(fd$mean_dir)))
})
