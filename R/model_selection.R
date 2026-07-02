# Per-model maximised log-likelihood for circ_model_select(). Returns one row per
# candidate model (uniform / unimodal von Mises / axial von Mises / unimodal +
# uniform background / asymmetric bimodal mixture) with its number of free
# parameters k and maximised logLik. The three closed-form models reuse
# vonmises_fit() for parameters and circular's densities; the two mixtures are
# fitted numerically by .fit_vm_uniform() / .fit_two_vm() (R/circ_mixtures.R). A
# fit failure yields logLik = NA for that model; uniform always computes.
.circ_model_loglik <- function(theta) {
  n    <- length(theta)
  th_c <- circular::circular(theta, units = "radians", type = "angles")
  df   <- data.frame(heading = theta)
  rad  <- function(x) circular::circular(x, units = "radians", type = "angles")

  ll_unif <- -n * log(2 * pi)

  vm <- vonmises_fit(df)
  ll_uni <- if (is.na(vm$kappa)) NA_real_ else
    sum(as.numeric(circular::dvonmises(
      th_c, mu = rad(vm$mu), kappa = vm$kappa, log = TRUE)))

  ax <- vonmises_fit(df, axial = TRUE)
  ll_ax <- if (is.na(ax$kappa)) NA_real_ else {
    # Symmetric axial (antipodal, equal) von Mises density on the FULL circle:
    #   f(theta) = exp(kappa * cos(2*(theta - mu))) / (2*pi * I0(kappa)),
    # which integrates to 1 over [0, 2*pi). mu is on [0, pi) and kappa is on the
    # doubled-angle scale, exactly as vonmises_fit(axial = TRUE) returns them.
    # Uses the exponentially-scaled Bessel I0 for numerical stability at high
    # kappa. Self-contained (no daxialvonmises), so the normalisation is explicit
    # and scored on the same full-circle support as uniform/unimodal.
    k  <- ax$kappa; mu <- ax$mu
    ll <- sum(k * (cos(2 * (theta - mu)) - 1)) -
          n * (log(2 * pi) + log(besselI(k, 0, expon.scaled = TRUE)))
    if (is.finite(ll)) ll else NA_real_
  }

  vu <- .fit_vm_uniform(theta)
  ll_vu <- if (isTRUE(vu$converged)) vu$logLik else NA_real_

  bi <- .fit_two_vm(theta)
  ll_bi <- if (isTRUE(bi$converged)) bi$logLik else NA_real_

  data.frame(
    model  = c("uniform", "unimodal", "axial", "unimodal_uniform", "bimodal"),
    k      = c(0L, 2L, 2L, 3L, 5L),
    logLik = c(ll_unif, ll_uni, ll_ax, ll_vu, ll_bi),
    stringsAsFactors = FALSE
  )
}

# Add information criteria + Akaike weights to a per-model logLik frame and sort by
# AICc (best first). AICc is NA when n - k - 1 <= 0 (undefined at small n); those
# rows get NA criteria/weight and sort last. Weights are normalised over the
# finite-AICc rows so they sum to 1.
.circ_model_criteria <- function(ll_df, n) {
  k  <- ll_df$k
  ll <- ll_df$logLik
  aic   <- -2 * ll + 2 * k
  denom <- n - k - 1
  aicc  <- ifelse(denom > 0, aic + (2 * k * (k + 1)) / denom, NA_real_)
  bic   <- -2 * ll + k * log(n)

  min_aicc <- if (all(is.na(aicc))) NA_real_ else min(aicc, na.rm = TRUE)
  d_aicc <- aicc - min_aicc
  w_raw  <- exp(-d_aicc / 2)
  weight <- w_raw / sum(w_raw, na.rm = TRUE)

  out <- data.frame(
    model = ll_df$model, n = n, k = k, logLik = ll,
    AIC = aic, AICc = aicc, BIC = bic, dAICc = d_aicc, weight = weight,
    stringsAsFactors = FALSE
  )
  out[order(out$AICc, na.last = TRUE), , drop = FALSE]
}

#' Select among candidate circular models by AICc
#'
#' Fits five candidate models to a heading sample and ranks them by the
#' small-sample-corrected Akaike information criterion (AICc):
#' \describe{
#'   \item{uniform}{no preferred direction (k = 0).}
#'   \item{unimodal}{one von Mises mode (k = 2).}
#'   \item{axial}{symmetric bimodal von Mises, two equal antipodal modes (k = 2).}
#'   \item{unimodal_uniform}{a directed von Mises mode over a uniform background,
#'     \eqn{p\,vM(\mu,\kappa) + (1-p)\,U} (k = 3).}
#'   \item{bimodal}{an asymmetric two-component von Mises mixture with free means,
#'     concentrations, and weight (k = 5).}
#' }
#' The mixture models (\code{unimodal_uniform}, \code{bimodal}) are fitted by
#' numerical maximum likelihood; the other three are closed-form. This model set
#' mirrors the Schnute-Groot family implemented by the \pkg{CircMLE} package
#' (Fitak & Johnsen 2017): \code{uniform} = M1, \code{unimodal} = single von
#' Mises, \code{axial} = symmetric bimodal, \code{unimodal_uniform} = a directed
#' component over a uniform background, and \code{bimodal} = a free two-component
#' mixture.
#'
#' Parameters for \code{uniform}, \code{unimodal}, and \code{axial} are estimated
#' with \code{\link{vonmises_fit}} (the axial model via its \code{axial = TRUE}
#' doubled-angle fit) and likelihoods with the \code{circular} package densities.
#' The table reports model comparison only; obtain the fitted parameters of a
#' chosen model from \code{\link{vonmises_fit}}.
#'
#' @param hd Data frame with a heading column in radians.
#' @param angle_col Heading column name. Default \code{"heading"}.
#' @param group_col Column to group by. \code{NULL} (default) treats the whole
#'   data frame as one sample.
#' @return Tidy data frame, one row per candidate model (five rows, or five per
#'   group when \code{group_col} is supplied), sorted by \code{AICc} ascending (best first;
#'   \code{NA} last). Columns: \code{group_col} (if supplied), \code{model},
#'   \code{n}, \code{k} (free parameters), \code{logLik}, \code{AIC}, \code{AICc},
#'   \code{BIC}, \code{dAICc} (AICc minus the group minimum), and \code{weight}
#'   (Akaike weight). \code{AICc}/\code{weight} are \code{NA} for a model whose
#'   fit failed or when \code{n - k - 1 <= 0}; remaining weights sum to 1.
#' @references Burnham, K.P. & Anderson, D.R. (2002). Model Selection and
#'   Multimodel Inference, 2nd ed. Springer.
#'
#'   Schnute, J.T. & Groot, K. (1992). Statistical analysis of animal orientation
#'   data. Animal Behaviour, 43(1), 15-33.
#'
#'   Fitak, R.R. & Johnsen, S. (2017). Bringing the analysis of animal
#'   orientation data full circle: model-based approaches with maximum likelihood.
#'   Journal of Experimental Biology, 220(21), 3878-3882.
#' @seealso \code{\link{vonmises_fit}}, \code{\link{test_uniformity}}
#' @export
circ_model_select <- function(hd, angle_col = "heading", group_col = NULL) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("circ_model_select: column '", angle_col, "' not found")

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]
    if (length(a) < 1L) return(NULL)
    .circ_model_criteria(.circ_model_loglik(a), n = length(a))
  }

  if (is.null(group_col)) return(.one(hd))
  if (!group_col %in% names(hd))
    stop("circ_model_select: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    if (is.null(r)) return(NULL)
    r[[group_col]] <- g
    r[, c(group_col, setdiff(names(r), group_col))]
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}
