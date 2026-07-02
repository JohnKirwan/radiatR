# Numerical maximum-likelihood fitters for the mixture models used by
# circ_model_select(): a directed von Mises over a uniform background, and an
# asymmetric two-component von Mises mixture. Both minimise the negative
# log-likelihood with stats::optim over an unconstrained parameterisation
# (kappa = exp(.), mixing weight via logit, means wrapped to [0, 2*pi)). A fit
# that fails to converge or yields a non-finite optimum returns logLik = NA, the
# same contract as vonmises_fit() failures. Internal; not exported.

# von Mises log-density, base R, normalised over [0, 2*pi). Uses the
# exponentially-scaled Bessel I0 so high kappa does not overflow.
.dvm_log <- function(theta, mu, kappa) {
  kappa * (cos(theta - mu) - 1) - log(2 * pi) -
    log(besselI(kappa, 0, expon.scaled = TRUE))
}

# Overflow-safe elementwise log(exp(a) + exp(b)).
.logsumexp2 <- function(a, b) {
  m <- pmax(a, b)
  m + log(exp(a - m) + exp(b - m))
}

# Circular mean and mean resultant length, for start values.
.circ_start <- function(theta) {
  C <- mean(cos(theta)); S <- mean(sin(theta))
  list(mu = atan2(S, C) %% (2 * pi), rbar = sqrt(C^2 + S^2))
}

# Rough kappa start from mean resultant length (Fisher 1993 approximation).
.kappa_start <- function(rbar) {
  rbar <- min(max(rbar, 1e-3), 0.999)
  if (rbar < 0.53) 2 * rbar + rbar^3 + 5 * rbar^5 / 6
  else if (rbar < 0.85) -0.4 + 1.39 * rbar + 0.43 / (1 - rbar)
  else 1 / (rbar^3 - 4 * rbar^2 + 3 * rbar)
}

.fit_vm_uniform <- function(theta) {
  theta <- theta[is.finite(theta)]
  fail  <- list(logLik = NA_real_, p = NA_real_, mu = NA_real_,
                kappa = NA_real_, converged = FALSE)
  if (length(theta) < 3L) return(fail)
  st <- .circ_start(theta)
  log_unif <- -log(2 * pi)

  nll <- function(par) {
    p  <- plogis(par[1]); mu <- par[2]; kappa <- exp(par[3])
    lf <- .logsumexp2(log(p) + .dvm_log(theta, mu, kappa),
                      log1p(-p) + log_unif)
    if (any(!is.finite(lf))) return(1e18)
    -sum(lf)
  }

  # Start grid over the mixing weight and kappa; keep the best optimum.
  starts <- expand.grid(p = c(0.5, 0.8), k = c(0.8, .kappa_start(st$rbar)))
  best <- fail; best_nll <- Inf
  for (i in seq_len(nrow(starts))) {
    init <- c(qlogis(min(max(starts$p[i], 1e-3), 1 - 1e-3)),
              st$mu, log(max(starts$k[i], 1e-2)))
    opt <- tryCatch(stats::optim(init, nll, method = "Nelder-Mead",
                                 control = list(maxit = 500)),
                    error = function(e) NULL)
    if (is.null(opt) || !is.finite(opt$value)) next
    if (opt$value < best_nll) {
      best_nll <- opt$value
      best <- list(logLik = -opt$value, p = plogis(opt$par[1]),
                   mu = opt$par[2] %% (2 * pi), kappa = exp(opt$par[3]),
                   converged = opt$convergence == 0)
    }
  }
  best
}
