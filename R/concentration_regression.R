# Von Mises regression with covariate-dependent concentration (kappa) and a
# constant mean direction. Native optim MLE. The concentration-regression
# complement to circ_regression() (which models the mean with constant kappa).

# Inverse link + a starting value for the intercept on the link scale.
.conc_reg_linkfun <- function(link) {
  switch(link,
    log = list(
      inv   = function(eta) exp(eta),
      start = function(k) log(max(k, 1e-3))),
    identity = list(
      inv   = function(eta) pmax(eta, 1e-6),
      start = function(k) max(k, 1e-3)))
}

#' Von Mises regression of concentration on covariates
#'
#' Fits a von Mises model in which the concentration \eqn{\kappa} varies with one
#' or more covariates while the mean direction \eqn{\mu} is a single constant:
#' \eqn{\theta_i \sim \mathrm{vM}(\mu, \kappa_i)} with
#' \eqn{\kappa_i = h(x_i'\gamma)} for an inverse link \eqn{h}. It is the
#' concentration-regression complement to \code{\link{circ_regression}} (which
#' models the mean with a constant concentration), and the estimator that pairs
#' with \code{\link{simulate_tracks}}'s \code{concentration_slope}. The model is
#' fitted by maximum likelihood with \code{\link[stats]{optim}}.
#'
#' @param data A data frame containing the response and predictor columns.
#' @param formula A formula \code{heading ~ x1 + x2}; the LHS is the angle column
#'   (radians), the RHS the covariate(s) modelling \eqn{\kappa}. The intercept is
#'   retained (it is the baseline concentration on the link scale).
#' @param link Link relating \eqn{\kappa} to the linear predictor: \code{"log"}
#'   (default; \eqn{\kappa = e^{x'\gamma}}, always positive) or \code{"identity"}
#'   (\eqn{\kappa = x'\gamma}, floored at a small positive value).
#' @param init Optional numeric starting values for the concentration
#'   coefficients (length = number of design-matrix columns, intercept first).
#' @return An S3 object of class \code{"concentration_regression"}. Use
#'   \code{summary()} for a tidy coefficient data frame (link scale),
#'   \code{predict()} / \code{fitted()} for fitted \eqn{\kappa} (response scale),
#'   and \code{print()} for a compact report. On non-convergence or too few rows,
#'   \code{converged} is \code{FALSE} and the coefficients are \code{NA}.
#' @references Mardia, K. V. & Jupp, P. E. (2000). \emph{Directional Statistics}.
#'   Wiley.
#' @seealso \code{\link{circ_regression}}, \code{\link{boot_kappa_ci}},
#'   \code{\link{test_concentration}}, \code{\link{simulate_tracks}}
#' @importFrom stats model.frame model.matrix model.response terms delete.response qnorm pnorm na.omit optim .getXlevels predict fitted
#' @export
concentration_regression <- function(data, formula, link = c("log", "identity"),
                                     init = NULL) {
  link <- match.arg(link)
  cl <- match.call()
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  theta <- as.numeric(stats::model.response(mf)) %% (2 * pi)
  X <- stats::model.matrix(formula, mf)            # keeps (Intercept)
  terms_nm <- colnames(X)
  p <- ncol(X)                                     # gamma length (incl. intercept)
  n <- nrow(X)
  xlev <- stats::.getXlevels(attr(mf, "terms"), mf)

  na_obj <- function() structure(list(
    coefficients = stats::setNames(rep(NA_real_, p), terms_nm),
    se = stats::setNames(rep(NA_real_, p), terms_nm),
    statistic = stats::setNames(rep(NA_real_, p), terms_nm),
    p_value = stats::setNames(rep(NA_real_, p), terms_nm),
    mu = NA_real_, se_mu = NA_real_, link = link,
    log_lik = NA_real_, n = n, converged = FALSE,
    terms = terms_nm, formula = formula, call = cl, xlevels = xlev, .X = X
  ), class = "concentration_regression")

  # parameters = mu (1) + gamma (p); need n >= p + 1 for a residual df
  if (!is.null(init) && length(init) != p)
    stop("concentration_regression: `init` must have length ", p, ".")
  if (n < p + 1L) return(na_obj())

  lk <- .conc_reg_linkfun(link)
  # log I0(kappa), stable at large kappa via the exponentially-scaled Bessel
  log_i0 <- function(kappa) log(besselI(kappa, 0, expon.scaled = TRUE)) + kappa

  negll <- function(par) {
    mu    <- par[1L]
    gamma <- par[-1L]
    kappa <- lk$inv(as.vector(X %*% gamma))
    if (any(!is.finite(kappa)) || any(kappa <= 0)) return(1e10)
    val <- sum(kappa * cos(theta - mu) - log(2 * pi) - log_i0(kappa))
    if (!is.finite(val)) return(1e10)
    -val
  }

  mu0 <- atan2(mean(sin(theta)), mean(cos(theta)))
  Rbar <- sqrt(mean(cos(theta))^2 + mean(sin(theta))^2)
  k0  <- .est_kappa_safe(
    circular::circular(theta, units = "radians", type = "angles"),
    fallback = .kappa_from_Rbar(Rbar))
  g0 <- rep(0, p)
  g0[1L] <- lk$start(if (is.finite(k0)) k0 else 1)
  if (!is.null(init)) g0 <- as.numeric(init)

  opt <- tryCatch(
    stats::optim(c(mu0, g0), negll, method = "BFGS", hessian = TRUE),
    error = function(e) NULL)
  if (is.null(opt) || opt$convergence != 0 || any(!is.finite(opt$par)))
    return(na_obj())

  mu_hat    <- opt$par[1L]
  gamma_hat <- opt$par[-1L]
  cov <- tryCatch(solve(opt$hessian), error = function(e) NULL)
  se_all <- if (is.null(cov) || any(!is.finite(diag(cov))) || any(diag(cov) < 0))
    rep(NA_real_, p + 1L) else sqrt(diag(cov))
  se_gamma <- se_all[-1L]
  stat <- gamma_hat / se_gamma
  pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)

  structure(list(
    coefficients = stats::setNames(gamma_hat, terms_nm),
    se = stats::setNames(se_gamma, terms_nm),
    statistic = stats::setNames(stat, terms_nm),
    p_value = stats::setNames(pval, terms_nm),
    mu = mu_hat %% (2 * pi), se_mu = se_all[1L], link = link,
    log_lik = -opt$value, n = n, converged = TRUE,
    terms = terms_nm, formula = formula, call = cl, xlevels = xlev, .X = X
  ), class = "concentration_regression")
}

#' @rdname concentration_regression
#' @param object A \code{concentration_regression} object.
#' @param conf.level Confidence level for the coefficient interval. Default 0.95.
#' @param ... Unused.
#' @exportS3Method summary concentration_regression
summary.concentration_regression <- function(object, conf.level = 0.95, ...) {
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  data.frame(
    term      = object$terms,
    estimate  = unname(object$coefficients),
    se        = unname(object$se),
    statistic = unname(object$statistic),
    p_value   = unname(object$p_value),
    conf.low  = unname(object$coefficients - z * object$se),
    conf.high = unname(object$coefficients + z * object$se),
    stringsAsFactors = FALSE
  )
}

#' @rdname concentration_regression
#' @param newdata Optional data frame of new covariate values. Default uses the
#'   training data.
#' @exportS3Method predict concentration_regression
predict.concentration_regression <- function(object, newdata = NULL, ...) {
  if (!isTRUE(object$converged))
    return(rep(NA_real_, if (is.null(newdata)) object$n else nrow(newdata)))
  if (is.null(newdata)) {
    X <- object$.X
  } else {
    tt <- stats::delete.response(stats::terms(object$formula))
    X  <- stats::model.matrix(tt, stats::model.frame(tt, newdata, xlev = object$xlevels))
  }
  lk <- .conc_reg_linkfun(object$link)
  lk$inv(as.vector(X %*% object$coefficients))
}

#' @rdname concentration_regression
#' @exportS3Method fitted concentration_regression
fitted.concentration_regression <- function(object, ...) predict(object)

#' @rdname concentration_regression
#' @param x A \code{concentration_regression} object.
#' @exportS3Method print concentration_regression
print.concentration_regression <- function(x, ...) {
  cat("Von Mises concentration regression (kappa ~ covariates)\n")
  cat(sprintf("  link = %s, mu = %.4g, n = %d, converged = %s\n",
              x$link, x$mu, x$n, x$converged))
  print(summary(x))
  invisible(x)
}
