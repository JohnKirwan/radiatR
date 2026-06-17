# Per-model maximised log-likelihood for circ_model_select(). Returns one row per
# candidate model (uniform / unimodal von Mises / axial von Mises) with its number
# of free parameters k and maximised logLik. Reuses vonmises_fit() for parameters
# and circular's densities for the likelihoods. A fit failure (kappa = NA) yields
# logLik = NA for that model; uniform always computes.
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
    d <- as.numeric(circular::daxialvonmises(
      th_c, mu = rad(ax$mu), kappa = ax$kappa, l = 2))
    if (any(!is.finite(d)) || any(d <= 0)) NA_real_ else sum(log(d))
  }

  data.frame(
    model  = c("uniform", "unimodal", "axial"),
    k      = c(0L, 2L, 2L),
    logLik = c(ll_unif, ll_uni, ll_ax),
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
