# Circular-linear regression (Fisher & Lee 1992) via circular::lm.circular,
# behind a formula interface consistent with radiatR's tidy statistics.

#' Circular-linear regression of a heading on linear covariates
#'
#' Fits the Fisher-Lee circular-linear regression
#' \eqn{\theta_i \sim \mathrm{vM}(\mu_0 + 2\arctan(x_i'\beta), \kappa)} via
#' \code{\link[circular]{lm.circular}} (\code{type = "c-l"}), behind a formula
#' interface. The response is a heading in radians (unit-circle convention); the
#' right-hand side supplies one or more linear covariates (factors and
#' interactions are expanded by \code{model.matrix}).
#'
#' @param data A data frame containing the response and predictor columns.
#' @param formula A formula `heading ~ x1 + x2`; the LHS is the angle column.
#' @param init Optional numeric starting values for the slope coefficients
#'   (length = number of predictor columns). Default a vector of zeros.
#' @return An S3 object of class \code{"circ_regression"}. Use
#'   \code{summary()} for a tidy coefficient data frame, \code{predict()} /
#'   \code{fitted()} for fitted mean angles, and \code{print()} for a compact
#'   report. On non-convergence or too few rows, \code{converged} is \code{FALSE}
#'   and the coefficients are \code{NA}.
#' @references Fisher, N. I. & Lee, A. J. (1992). Regression models for an
#'   angular response. \emph{Biometrics} 48, 665-677. Mardia, K. V. & Jupp, P. E.
#'   (2000). \emph{Directional Statistics}. Wiley.
#' @seealso \code{\link{circ_cor}}, \code{\link{vonmises_fit}},
#'   \code{\link{simulate_tracks}}
#' @importFrom stats model.frame model.matrix model.response terms delete.response qnorm na.omit .getXlevels predict fitted
#' @importFrom circular circular lm.circular
#' @export
circ_regression <- function(data, formula, init = NULL) {
  cl <- match.call()
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  theta <- as.numeric(stats::model.response(mf))
  X <- stats::model.matrix(formula, mf)
  icpt <- which(colnames(X) == "(Intercept)")
  if (length(icpt)) X <- X[, -icpt, drop = FALSE]
  terms_nm <- colnames(X)
  p <- ncol(X); n <- nrow(X)
  xlev <- stats::.getXlevels(attr(mf, "terms"), mf)

  na_obj <- function() structure(list(
    coefficients = stats::setNames(rep(NA_real_, p), terms_nm),
    se = stats::setNames(rep(NA_real_, p), terms_nm),
    statistic = stats::setNames(rep(NA_real_, p), terms_nm),
    p_value = stats::setNames(rep(NA_real_, p), terms_nm),
    mu0 = NA_real_, se_mu0 = NA_real_, kappa = NA_real_, se_kappa = NA_real_,
    log_lik = NA_real_, n = n, converged = FALSE,
    terms = terms_nm, formula = formula, call = cl, xlevels = xlev, .X = X
  ), class = "circ_regression")

  if (p < 1L || n < p + 2L) return(na_obj())

  y <- circular::circular(theta %% (2 * pi), units = "radians",
                          modulo = "2pi", zero = 0, rotation = "counter")
  if (is.null(init)) init <- rep(0, p)
  fit <- tryCatch(
    suppressWarnings(circular::lm.circular(y = y, x = X, init = init, type = "c-l")),
    error = function(e) NULL)
  if (is.null(fit) || any(!is.finite(as.numeric(fit$coefficients)))) return(na_obj())

  structure(list(
    coefficients = stats::setNames(as.numeric(fit$coefficients), terms_nm),
    se = stats::setNames(as.numeric(fit$se.coef), terms_nm),
    statistic = stats::setNames(as.numeric(fit$t.values), terms_nm),
    p_value = stats::setNames(as.numeric(fit$p.values), terms_nm),
    mu0 = as.numeric(fit$mu), se_mu0 = as.numeric(fit$se.mu),
    kappa = as.numeric(fit$kappa), se_kappa = as.numeric(fit$se.kappa),
    log_lik = as.numeric(fit$log.lik), n = n, converged = TRUE,
    terms = terms_nm, formula = formula, call = cl, xlevels = xlev, .X = X
  ), class = "circ_regression")
}

#' @rdname circ_regression
#' @param object A \code{circ_regression} object.
#' @param conf.level Confidence level for the coefficient interval. Default 0.95.
#' @param ... Unused.
#' @exportS3Method summary circ_regression
summary.circ_regression <- function(object, conf.level = 0.95, ...) {
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

#' @rdname circ_regression
#' @param newdata Optional data frame of new covariate values. Default uses the
#'   training data.
#' @exportS3Method predict circ_regression
predict.circ_regression <- function(object, newdata = NULL, ...) {
  if (!isTRUE(object$converged))
    return(rep(NA_real_, if (is.null(newdata)) object$n else nrow(newdata)))
  if (is.null(newdata)) {
    X <- object$.X
  } else {
    tt <- stats::delete.response(stats::terms(object$formula))
    X  <- stats::model.matrix(tt, stats::model.frame(tt, newdata, xlev = object$xlevels))
    icpt <- which(colnames(X) == "(Intercept)")
    if (length(icpt)) X <- X[, -icpt, drop = FALSE]
  }
  (object$mu0 + 2 * atan(as.vector(X %*% object$coefficients))) %% (2 * pi)
}

#' @rdname circ_regression
#' @exportS3Method fitted circ_regression
fitted.circ_regression <- function(object, ...) predict(object)

#' @rdname circ_regression
#' @param x A \code{circ_regression} object.
#' @exportS3Method print circ_regression
print.circ_regression <- function(x, ...) {
  cat("Circular-linear regression (Fisher-Lee)\n")
  cat(sprintf("  mu0 = %.4g, kappa = %.4g, n = %d, converged = %s\n",
              x$mu0, x$kappa, x$n, x$converged))
  print(summary(x))
  invisible(x)
}

#' Fitted mean directions from a circular regression, for plotting
#'
#' Shapes the predictions of a [circ_regression()] model into the `summary_df`
#' that [add_circ_mean()] draws, so a fitted mean-direction (rho) arrow can be
#' drawn for each covariate value and colour-coded by the covariate -- showing
#' how the mean heading sweeps with the predictor. The arrow length is the
#' model's implied resultant length \code{circular::A1(kappa)}, the same for
#' every arrow, so direction and colour carry the signal.
#'
#' @param fit A `circ_regression` object.
#' @param at Numeric (or factor) values for the model's single right-hand-side
#'   variable -- a convenience for one-predictor models. Supply exactly one of
#'   `at` or `newdata`.
#' @param newdata A data frame of covariate values, one row per arrow (for
#'   multi-predictor models, or full control). Supply exactly one of `at`/`newdata`.
#' @param display A [circ_display()] object stored on the result so the arrows
#'   orient with the panel. Default [circ_display()].
#' @return A data frame with `mean_dir` (fitted heading, radians, unit-circle
#'   convention), `resultant_R` (= `circular::A1(fit$kappa)`, constant), and the
#'   covariate column(s) from `newdata`, with a `display` attribute. Pass it to
#'   `add_circ_mean(colour_col = "<predictor>")`. A non-converged fit yields `NA`
#'   `mean_dir` rows, which `add_circ_mean()` skips.
#' @seealso [circ_regression()], [add_circ_mean()], [compute_circ_mean()]
#' @importFrom circular A1
#' @export
fitted_directions <- function(fit, at = NULL, newdata = NULL, display = NULL) {
  if (!inherits(fit, "circ_regression"))
    stop("`fit` must be a circ_regression object.")
  if (is.null(at) == is.null(newdata))
    stop("supply exactly one of `at` or `newdata`.")
  if (!is.null(at)) {
    rhs <- all.vars(fit$formula[[3L]])
    if (length(rhs) != 1L)
      stop("model has multiple predictors; pass `newdata` instead of `at`.")
    newdata <- stats::setNames(data.frame(at, stringsAsFactors = FALSE), rhs)
  }
  mean_dir <- predict(fit, newdata = newdata)
  out <- cbind(
    data.frame(mean_dir = mean_dir,
               resultant_R = as.numeric(circular::A1(fit$kappa)),
               stringsAsFactors = FALSE),
    newdata
  )
  attr(out, "display") <- display %||% circ_display()
  out
}
