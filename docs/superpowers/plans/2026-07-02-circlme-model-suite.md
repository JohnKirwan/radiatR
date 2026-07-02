# Expanded circular model selector (circLME suite) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend `circ_model_select()` in place with two mixture models
(`unimodal_uniform`, `bimodal`), fix/verify the axial likelihood scoring, and
extend `simulate_tracks()` to generate the new distributions — improving how
radiatR diagnoses circular orientation.

**Architecture:** Native maximum-likelihood fitters (`R/circ_mixtures.R`, new)
minimise negative log-likelihood via `stats::optim`; `circ_model_select()` gains
two rows by calling them. The axial row switches from `circular::daxialvonmises`
to a self-contained closed form. `simulate_tracks()` gains two `modality` values.
CircMLE is a `Suggests` test oracle only — never in the runtime path.

**Tech Stack:** R, S4 (`Tracks`), `circular` package (densities/RNG),
`stats::optim`, `testthat` (edition 3), roxygen2 8.0.0.

## Global Constraints

- **No new hard dependency.** `CircMLE` goes in `DESCRIPTION` `Suggests` only;
  every test using it wraps with `testthat::skip_if_not_installed("CircMLE")`.
- **In-place extension.** Do not add a new exported function or change the
  `circ_model_select(hd, angle_col = "heading", group_col = NULL)` signature. The
  app (`inst/app/app.R:1509`) and emitter (`inst/app/plot_spec.R:443/446`) must
  keep working unchanged.
- **Model labels (exact):** `uniform`, `unimodal`, `axial`, `unimodal_uniform`,
  `bimodal`. Free-parameter counts `k = c(0, 2, 2, 3, 5)`.
- **A new `R/*.R` file MUST be added to `DESCRIPTION` `Collate`** or
  `R CMD check`/build fails (Collate is hand-maintained; `document()` does not
  enforce it).
- **ASCII only in R source** (non-ASCII characters in `R/*.R` trigger an
  `R CMD check` WARNING). Greek letters live in roxygen `\eqn{}`/prose, not code.
- **Never edit `NAMESPACE` or `man/*.Rd` by hand** — edit roxygen `@` tags and run
  `devtools::document()`.
- **Version:** bump `DESCRIPTION` `Version` to `0.5.0` and add a `0.5.0` NEWS
  heading (final task).
- Run a single test file with
  `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/<file>")'`.

---

## File structure

- `R/circ_mixtures.R` **(new)** — `.dvm_log()`, `.logsumexp2()`, `.fit_vm_uniform()`,
  `.fit_two_vm()`. Pure numerical MLE fitters; no S4, no export. One
  responsibility: fit the two mixture models and return maximised logLik +
  parameters.
- `R/model_selection.R` **(modify)** — `.circ_model_loglik()` gains the axial
  closed form and the two mixture rows; roxygen for `circ_model_select()` updated.
- `R/simulate_tracks.R` **(modify)** — two new `modality` values and their
  conditions columns; `.sim_principal_angle()` and `.sim_default_conditions()`
  validation extended.
- `DESCRIPTION` **(modify)** — `Collate` (+`circ_mixtures.R`), `Suggests`
  (+`CircMLE`), `Version` → `0.5.0`.
- `NEWS.md` **(modify)** — `0.5.0` heading.
- `vignettes/circular-statistics.Rmd` **(modify)** — expanded model set + millipede
  walk-through + CircMLE correspondence note.
- `tests/testthat/test-model-selection.R`, `test-circ-mixtures.R` **(new)**,
  `test-simulate-tracks.R` **(modify)** — tests.

---

## Task 1: Axial log-likelihood — closed form + audit

Replace the `circular::daxialvonmises(...)/2` computation in `.circ_model_loglik()`
with a self-contained closed form, removing dependence on that function's
`l`/normalisation parameterisation. The symmetric axial density is
\eqn{f(\theta) = e^{\kappa \cos 2(\theta-\mu)} / (2\pi I_0(\kappa))}, which
integrates to 1 over [0, 2*pi). Parameters come from the existing doubled-angle
fit `vonmises_fit(df, axial = TRUE)` (its `mu` in [0, pi), `kappa` on the
doubled-angle scale — the same kappa this density uses).

**Files:**
- Modify: `R/model_selection.R:19-28` (the `ax`/`ll_ax` block inside
  `.circ_model_loglik`)
- Test: `tests/testthat/test-model-selection.R`

**Interfaces:**
- Consumes: `vonmises_fit(df, axial = TRUE)` returning a one-row data frame with
  numeric `mu`, `kappa` (or `kappa = NA` on failure).
- Produces: unchanged `.circ_model_loglik(theta)` output shape (still the 3-row
  frame at this point); the `axial` `logLik` value is now the closed form.

- [ ] **Step 1: Write the failing normalisation test**

Add to `tests/testthat/test-model-selection.R`:

```r
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
```

- [ ] **Step 2: Run tests to verify status**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-model-selection.R")'`
Expected: the normalisation test PASSES (it only checks arithmetic); the
"large uniform sample" test is the guard — note whether it passes now. If it
already passes, keep it as a regression guard; the closed form must not break it.

- [ ] **Step 3: Replace the axial likelihood block with the closed form**

In `R/model_selection.R`, replace the current `ax`/`ll_ax` block (lines ~19-28):

```r
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
```

- [ ] **Step 4: Run the model-selection tests**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-model-selection.R")'`
Expected: PASS, including the existing `picks unimodal / axial / uniform on
matching data` test (axial still wins on genuinely axial data) and both new tests.

- [ ] **Step 5: Commit**

```bash
git add R/model_selection.R tests/testthat/test-model-selection.R
git commit -m "fix(stats): score axial model with explicit closed-form density

Replace daxialvonmises(l=2)/2 with the closed-form symmetric axial von Mises
density exp(k*cos(2*(theta-mu)))/(2*pi*I0(k)), integrating to 1 over the full
circle. Removes dependence on daxialvonmises normalisation and pins the
scoring with a normalisation test and a large-uniform-sample guard."
```

---

## Task 2: von Mises + uniform mixture fitter

Create the fitter module and the first fitter: a directed von Mises component over
a uniform background, `p * vM(mu, kappa) + (1 - p) * Uniform`.

**Files:**
- Create: `R/circ_mixtures.R`
- Modify: `DESCRIPTION` (`Collate`)
- Test: `tests/testthat/test-circ-mixtures.R` (new)

**Interfaces:**
- Produces:
  - `.dvm_log(theta, mu, kappa)` -> numeric vector of von Mises log-densities
    (base R; `2*pi*I0` normalisation via exponentially-scaled Bessel).
  - `.logsumexp2(a, b)` -> numeric vector, elementwise `log(exp(a) + exp(b))`,
    overflow-safe.
  - `.fit_vm_uniform(theta)` -> `list(logLik, p, mu, kappa, converged)`; `theta`
    is a finite numeric vector in radians. `logLik = NA_real_` and
    `converged = FALSE` on failure.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-circ-mixtures.R`:

```r
test_that(".dvm_log matches circular::dvonmises", {
  th <- seq(0, 2 * pi, length.out = 20)
  a  <- radiatR:::.dvm_log(th, mu = 1.2, kappa = 3)
  b  <- log(as.numeric(circular::dvonmises(
    circular::circular(th, units = "radians"),
    mu = circular::circular(1.2, units = "radians"), kappa = 3)))
  expect_equal(a, b, tolerance = 1e-8)
})

test_that(".fit_vm_uniform recovers a directed-plus-background mixture", {
  set.seed(1)
  n <- 600; p <- 0.7
  z <- runif(n) < p
  th <- ifelse(z,
    as.numeric(circular::rvonmises(n, mu = circular::circular(1.0), kappa = 4)),
    runif(n, 0, 2 * pi)) %% (2 * pi)
  fit <- radiatR:::.fit_vm_uniform(th)
  expect_true(fit$converged)
  expect_equal(fit$p,     0.7, tolerance = 0.15)
  expect_equal(fit$mu,    1.0, tolerance = 0.2)
  expect_true(fit$kappa > 1)
  expect_true(is.finite(fit$logLik))
})

test_that(".fit_vm_uniform logLik beats a single von Mises on mixed data", {
  set.seed(2)
  n <- 800
  th <- ifelse(runif(n) < 0.6,
    as.numeric(circular::rvonmises(n, mu = circular::circular(0.0), kappa = 6)),
    runif(n, 0, 2 * pi)) %% (2 * pi)
  vm  <- vonmises_fit(data.frame(heading = th))
  ll_vm <- sum(radiatR:::.dvm_log(th, vm$mu, vm$kappa))
  expect_gt(radiatR:::.fit_vm_uniform(th)$logLik, ll_vm)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-circ-mixtures.R")'`
Expected: FAIL — `.dvm_log` / `.fit_vm_uniform` not found.

- [ ] **Step 3: Create `R/circ_mixtures.R`**

```r
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
```

- [ ] **Step 4: Add `R/circ_mixtures.R` to `DESCRIPTION` `Collate`**

In `DESCRIPTION`, insert the line just before `'model_selection.R'`:

```
    'circ_mixtures.R'
    'model_selection.R'
```

- [ ] **Step 5: Run the tests**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-circ-mixtures.R")'`
Expected: PASS (all three tests).

- [ ] **Step 6: Commit**

```bash
git add R/circ_mixtures.R DESCRIPTION tests/testthat/test-circ-mixtures.R
git commit -m "feat(stats): von Mises + uniform-background mixture fitter

Native optim-based MLE for p*vM(mu,kappa) + (1-p)*Uniform, with base-R
von Mises log-density and overflow-safe log-sum-exp helpers. New R file
registered in DESCRIPTION Collate."
```

---

## Task 3: Asymmetric two-von-Mises mixture fitter

Add the second fitter to `R/circ_mixtures.R`: `gamma * vM(mu1, kappa1) +
(1 - gamma) * vM(mu2, kappa2)` with independent means, concentrations, and weight.
The mixture likelihood is multimodal, so use a start-value grid over candidate
mode pairs and keep the best optimum.

**Files:**
- Modify: `R/circ_mixtures.R`
- Test: `tests/testthat/test-circ-mixtures.R`

**Interfaces:**
- Consumes: `.dvm_log`, `.logsumexp2`, `.circ_start`, `.kappa_start` (Task 2).
- Produces: `.fit_two_vm(theta)` ->
  `list(logLik, gamma, mu1, kappa1, mu2, kappa2, converged)`. Components are
  reported in canonical order (larger weight first): `gamma >= 0.5`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-circ-mixtures.R`:

```r
test_that(".fit_two_vm recovers two non-antipodal modes", {
  set.seed(3)
  n <- 800; g <- 0.6
  z <- runif(n) < g
  th <- ifelse(z,
    as.numeric(circular::rvonmises(n, mu = circular::circular(0.5), kappa = 8)),
    as.numeric(circular::rvonmises(n, mu = circular::circular(2.5), kappa = 8))
  ) %% (2 * pi)
  fit <- radiatR:::.fit_two_vm(th)
  expect_true(fit$converged)
  expect_gte(fit$gamma, 0.5)
  # the two fitted means should match {0.5, 2.5} up to labelling
  got <- sort(c(fit$mu1, fit$mu2))
  expect_equal(got, c(0.5, 2.5), tolerance = 0.25)
})

test_that(".fit_two_vm logLik is at least the axial logLik on axial data", {
  set.seed(4)
  th <- c(as.numeric(circular::rvonmises(200, circular::circular(0), kappa = 6)),
          as.numeric(circular::rvonmises(200, circular::circular(pi), kappa = 6))) %% (2 * pi)
  ax  <- vonmises_fit(data.frame(heading = th), axial = TRUE)
  ll_ax <- sum(ax$kappa * (cos(2 * (th - ax$mu)) - 1)) -
           length(th) * (log(2 * pi) + log(besselI(ax$kappa, 0, expon.scaled = TRUE)))
  # the free 2-vM mixture generalises the constrained axial model, so its
  # maximised logLik cannot be lower (allow tiny numerical slack)
  expect_gte(radiatR:::.fit_two_vm(th)$logLik, ll_ax - 1e-3)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-circ-mixtures.R")'`
Expected: FAIL — `.fit_two_vm` not found.

- [ ] **Step 3: Add `.fit_two_vm` to `R/circ_mixtures.R`**

```r
.fit_two_vm <- function(theta) {
  theta <- theta[is.finite(theta)]
  fail  <- list(logLik = NA_real_, gamma = NA_real_,
                mu1 = NA_real_, kappa1 = NA_real_,
                mu2 = NA_real_, kappa2 = NA_real_, converged = FALSE)
  if (length(theta) < 5L) return(fail)
  st  <- .circ_start(theta)
  k0  <- max(.kappa_start(st$rbar), 0.5)

  nll <- function(par) {
    g  <- plogis(par[1])
    l1 <- log(g)      + .dvm_log(theta, par[2], exp(par[3]))
    l2 <- log1p(-g)   + .dvm_log(theta, par[4], exp(par[5]))
    lf <- .logsumexp2(l1, l2)
    if (any(!is.finite(lf))) return(1e18)
    -sum(lf)
  }

  # Seed the two means from candidate pairs: antipodal, and offsets of +/-2pi/3
  # around the circular mean. This spans symmetric and asymmetric configurations.
  offs <- list(c(0, pi), c(0, 2 * pi / 3), c(0, -2 * pi / 3), c(0, pi / 2))
  best <- fail; best_nll <- Inf
  for (o in offs) {
    init <- c(0, st$mu + o[1], log(k0), st$mu + o[2], log(k0))
    opt  <- tryCatch(stats::optim(init, nll, method = "Nelder-Mead",
                                  control = list(maxit = 800)),
                     error = function(e) NULL)
    if (is.null(opt) || !is.finite(opt$value)) next
    if (opt$value < best_nll) {
      g  <- plogis(opt$par[1])
      m1 <- opt$par[2] %% (2 * pi); k1 <- exp(opt$par[3])
      m2 <- opt$par[4] %% (2 * pi); k2 <- exp(opt$par[5])
      # canonical order: larger-weight component first
      if (g < 0.5) { g <- 1 - g; tmp <- c(m1, k1); m1 <- m2; k1 <- k2
                     m2 <- tmp[1]; k2 <- tmp[2] }
      best_nll <- opt$value
      best <- list(logLik = -opt$value, gamma = g,
                   mu1 = m1, kappa1 = k1, mu2 = m2, kappa2 = k2,
                   converged = opt$convergence == 0)
    }
  }
  best
}
```

- [ ] **Step 4: Run the tests**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-circ-mixtures.R")'`
Expected: PASS (all five tests in the file).

- [ ] **Step 5: Commit**

```bash
git add R/circ_mixtures.R tests/testthat/test-circ-mixtures.R
git commit -m "feat(stats): asymmetric two-von-Mises mixture fitter

Native optim MLE for gamma*vM(mu1,k1)+(1-gamma)*vM(mu2,k2) with a
start-value grid over candidate mode pairs and canonical (larger-weight-
first) ordering. Generalises the constrained axial model."
```

---

## Task 4: Wire the two mixture models into `circ_model_select`

Add the `unimodal_uniform` and `bimodal` rows to `.circ_model_loglik()`, update
the `k` vector, and refresh the roxygen documentation. Update the existing
model-selection tests that assumed exactly three models.

**Files:**
- Modify: `R/model_selection.R` (`.circ_model_loglik` body + roxygen block)
- Modify: `tests/testthat/test-model-selection.R`

**Interfaces:**
- Consumes: `.fit_vm_uniform(theta)`, `.fit_two_vm(theta)` (Tasks 2-3).
- Produces: `.circ_model_loglik(theta)` returns a **5-row** frame with
  `model = c("uniform","unimodal","axial","unimodal_uniform","bimodal")`,
  `k = c(0, 2, 2, 3, 5)`. `circ_model_select()` output gains two rows per group.

- [ ] **Step 1: Update the existing tests for five models (write failing)**

In `tests/testthat/test-model-selection.R`:

Change the tidy-shape test:
```r
test_that("circ_model_select returns the tidy table shape and a winning weight", {
  set.seed(12)
  d <- data.frame(heading = rnorm(60, 2, 0.3) %% (2 * pi))
  r <- circ_model_select(d)
  expect_equal(nrow(r), 5L)
  expect_setequal(r$model,
    c("uniform", "unimodal", "axial", "unimodal_uniform", "bimodal"))
  expect_true(all(c("model", "n", "k", "logLik", "AIC", "AICc", "BIC", "dAICc", "weight")
                  %in% names(r)))
  expect_equal(sum(r$weight, na.rm = TRUE), 1, tolerance = 1e-12)
  expect_equal(r$dAICc[1], 0)
})
```

Change the group test's row count from `6L` to `10L`:
```r
  expect_equal(nrow(r), 10L)
```

Add a new recovery test:
```r
test_that("circ_model_select recovers unimodal_uniform and bimodal", {
  set.seed(21)
  n <- 400
  du <- ifelse(runif(n) < 0.7,
    as.numeric(circular::rvonmises(n, circular::circular(1), kappa = 8)),
    runif(n, 0, 2 * pi)) %% (2 * pi)
  bi <- ifelse(runif(n) < 0.6,
    as.numeric(circular::rvonmises(n, circular::circular(0.4), kappa = 12)),
    as.numeric(circular::rvonmises(n, circular::circular(2.6), kappa = 12))) %% (2 * pi)
  expect_equal(circ_model_select(data.frame(heading = du))$model[1], "unimodal_uniform")
  expect_equal(circ_model_select(data.frame(heading = bi))$model[1], "bimodal")
})
```

- [ ] **Step 2: Run to verify failures**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-model-selection.R")'`
Expected: FAIL — `nrow` is 3/6 not 5/10, and the new models are absent.

- [ ] **Step 3: Add the two rows to `.circ_model_loglik`**

In `R/model_selection.R`, after the `ll_ax` block and before the returned
`data.frame`, add:

```r
  vu <- .fit_vm_uniform(theta)
  ll_vu <- if (isTRUE(vu$converged)) vu$logLik else NA_real_

  bi <- .fit_two_vm(theta)
  ll_bi <- if (isTRUE(bi$converged)) bi$logLik else NA_real_
```

Replace the returned `data.frame` with:

```r
  data.frame(
    model  = c("uniform", "unimodal", "axial", "unimodal_uniform", "bimodal"),
    k      = c(0L, 2L, 2L, 3L, 5L),
    logLik = c(ll_unif, ll_uni, ll_ax, ll_vu, ll_bi),
    stringsAsFactors = FALSE
  )
```

- [ ] **Step 4: Update the `circ_model_select` roxygen block**

In `R/model_selection.R`, replace the first roxygen paragraph and add a models
table. Update the `@description`/details to list all five models and add
references. Use this for the details paragraph and add the table (ASCII only):

```r
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
```

Add to the `@references`:
```r
#' @references Burnham, K.P. & Anderson, D.R. (2002). Model Selection and
#'   Multimodel Inference, 2nd ed. Springer.
#'
#'   Schnute, J.T. & Groot, K. (1992). Statistical analysis of animal orientation
#'   data. Animal Behaviour, 43(1), 15-33.
#'
#'   Fitak, R.R. & Johnsen, S. (2017). Bringing the analysis of animal
#'   orientation data full circle: model-based approaches with maximum likelihood.
#'   Journal of Experimental Biology, 220(21), 3878-3882.
```

Update the `@return` sentence to say "one row per candidate model (five per
group)".

- [ ] **Step 5: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document()'`
Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-model-selection.R")'`
Expected: docs regenerate with no manual `NAMESPACE` edits; tests PASS.

- [ ] **Step 6: Commit**

```bash
git add R/model_selection.R man/ tests/testthat/test-model-selection.R
git commit -m "feat(stats): add unimodal_uniform and bimodal to circ_model_select

Extend the selector in place from three to five candidate models, wiring
the two mixture fitters into .circ_model_loglik (k = 0,2,2,3,5). App and
code-emitter inherit the new models unchanged. Docs cite Schnute & Groot
(1992) and Fitak & Johnsen (2017)."
```

---

## Task 5: Extend `simulate_tracks()` with the two new distributions

Add `modality` values `"unimodal_uniform"` and `"bimodal"` plus their conditions
columns, so the new models are generatable end to end.

**Files:**
- Modify: `R/simulate_tracks.R` — `.sim_default_conditions()` (validation +
  defaults, ~lines 228-235), `.sim_principal_angle()` (~lines 290-312),
  `.sim_condition_trials()` (pass-through, ~lines 260-282), and the roxygen
  `@param` docs for `modality`.
- Test: `tests/testthat/test-simulate-tracks.R`

**Interfaces:**
- Consumes: existing `simulate_tracks(conditions = <data frame>)` contract.
- Produces: two new `modality` values and three new optional conditions columns:
  `mix_weight` (weight on the primary component; default `0.6`), `mode2_mean`
  (absolute mean of the 2nd von Mises, radians; default `NA` -> `ref_mean +
  2*pi/3`), `kappa2` (2nd component concentration; default `NA` ->
  `concentration_base`). Unused by the other modalities; defaults keep existing
  calls byte-identical.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-simulate-tracks.R` (near the existing `mk` helper at
lines 66-70):

```r
# Mirrors the existing `mk` helper above (rule = "net", output = "trajset"); a
# headings_frame is accepted directly by circ_model_select(). Recovery is seeded;
# if a run is marginal, raise n_trials / concentration_base rather than loosening
# the assertion.
test_that("simulate_tracks generates unimodal_uniform and bimodal recoverably", {
  mk2 <- function(modality, extra = list()) {
    cond <- tibble::tibble(condition = modality, n_trials = 200L, ref_mean = 1.0,
                           concentration_base = 12, modality = modality)
    for (nm in names(extra)) cond[[nm]] <- extra[[nm]]
    ts <- simulate_tracks(n_points = 60, conditions = cond,
                          output = "trajset", seed = 7)
    derive_headings(ts, rule = "net")
  }
  du <- mk2("unimodal_uniform", list(mix_weight = 0.7))
  bi <- mk2("bimodal", list(mix_weight = 0.6, mode2_mean = 3.5, kappa2 = 12))
  expect_equal(circ_model_select(du)$model[1], "unimodal_uniform")
  expect_equal(circ_model_select(bi)$model[1], "bimodal")
})

test_that("simulate_tracks rejects an unknown modality", {
  cond <- data.frame(condition = "c", n_trials = 5, modality = "nope")
  expect_error(simulate_tracks(conditions = cond, seed = 1), "unknown modality")
})
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-simulate-tracks.R")'`
Expected: FAIL — `unimodal_uniform`/`bimodal` rejected as unknown modality.

- [ ] **Step 3: Extend `.sim_default_conditions()` validation and defaults**

In `R/simulate_tracks.R`, change the `ok_mod` allow-list (line ~230):

```r
  ok_mod <- conditions$modality %in%
    c("unimodal", "uniform", "axial", "multimodal", "unimodal_uniform", "bimodal")
```

Add default columns just after the `n_modes` default (after line ~229):

```r
  if (!"mix_weight" %in% names(conditions))  conditions$mix_weight  <- 0.6
  if (!"mode2_mean" %in% names(conditions))  conditions$mode2_mean  <- NA_real_
  if (!"kappa2" %in% names(conditions))      conditions$kappa2      <- NA_real_
```

- [ ] **Step 4: Extend `.sim_principal_angle()` and its caller**

Replace the `.sim_principal_angle()` signature and add two `switch` branches
(keep existing branches byte-identical). New signature:

```r
.sim_principal_angle <- function(modality, ref_mean, kappa, n_modes,
                                 mix_weight = 0.6, mode2_mean = NA_real_,
                                 kappa2 = NA_real_) {
```

Add these branches to the `switch(modality, ...)` (before the terminal `stop`):

```r
    unimodal_uniform = {
      if (stats::runif(1) < mix_weight)
        list(angle = rvm(ref_mean), mode_id = 1L, mode_mean = ref_mean)
      else
        list(angle = stats::runif(1, 0, 2 * pi), mode_id = 2L, mode_mean = NA_real_)
    },
    bimodal = {
      m2 <- if (is.na(mode2_mean)) ref_mean + 2 * pi / 3 else mode2_mean
      k2 <- if (is.na(kappa2)) kappa else kappa2
      if (stats::runif(1) < mix_weight)
        list(angle = rvm(ref_mean), mode_id = 1L, mode_mean = ref_mean)
      else {
        mu_c <- circular::circular(m2, units = "radians", type = "angles",
                                   modulo = "asis", zero = 0, rotation = "counter")
        list(angle = as.numeric(circular::rvonmises(1, mu = mu_c, kappa = k2)),
             mode_id = 2L, mode_mean = m2)
      }
    },
```

Update the terminal `stop()` message to include the two new names:
```r
    stop("simulate_tracks: unknown modality '", modality,
         "'. Use unimodal, uniform, axial, multimodal, unimodal_uniform, or bimodal.")
```

In `.sim_condition_trials()` / `.sim_single_trial()`, thread the three new
condition columns through to `.sim_principal_angle()`. Where `.sim_single_trial`
calls `.sim_principal_angle(modality, ref_mean, kappa, n_modes)`, add the new
arguments from `condition_row`:

```r
      mix_weight = condition_row$mix_weight,
      mode2_mean = condition_row$mode2_mean,
      kappa2     = condition_row$kappa2,
```

(Add matching `mix_weight`/`mode2_mean`/`kappa2` parameters to `.sim_single_trial`'s
signature and pass them into the `.sim_principal_angle()` call inside it.)

- [ ] **Step 5: Update the `modality` roxygen `@param`**

In the `@param` for `modality` (line ~48), extend the allowed values:

```r
#'   or `"multimodal"`, `"unimodal_uniform"` (a directed mode over a uniform
#'   background; `mix_weight` sets the directed fraction), or `"bimodal"` (an
#'   asymmetric two-von-Mises mixture; `mix_weight`, `mode2_mean`, `kappa2` set
#'   the second mode).
```

- [ ] **Step 6: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document()'`
Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-simulate-tracks.R")'`
Expected: PASS, including the existing `mk`-based recovery tests (unchanged).

- [ ] **Step 7: Commit**

```bash
git add R/simulate_tracks.R man/ tests/testthat/test-simulate-tracks.R
git commit -m "feat(sim): simulate_tracks unimodal_uniform and bimodal modalities

Add mix_weight/mode2_mean/kappa2 conditions columns (defaulted, so existing
calls are unchanged) and the two matching draw branches, so the new
circ_model_select models can be generated and recovered end to end."
```

---

## Task 6: CircMLE oracle test (Suggests)

Add `CircMLE` as a `Suggests` dependency and a single oracle test that checks the
native fitters against `CircMLE::circ_mle` for equivalent models. Skipped when
CircMLE is not installed. Mirrors the existing Hermans-Rasson oracle pattern
(`tests/testthat/test-circular-stats.R:920`).

**Files:**
- Modify: `DESCRIPTION` (`Suggests` + `CircMLE`)
- Test: `tests/testthat/test-circ-mixtures.R`

**Interfaces:**
- Consumes: `.fit_two_vm(theta)`; `CircMLE::circ_mle(x)` where `x` is a
  `circular` object. `circ_mle()` returns a list whose `$results` is a data frame
  of candidate models with a `-LogLik` column (one row per Schnute-Groot model).

- [ ] **Step 1: Add CircMLE to Suggests**

In `DESCRIPTION`, add `CircMLE,` to the `Suggests:` list (alphabetical position is
not required; place after `chromote`).

- [ ] **Step 2: Write the oracle test**

Append to `tests/testthat/test-circ-mixtures.R`:

```r
test_that(".fit_two_vm agrees with CircMLE on the best bimodal model", {
  skip_if_not_installed("CircMLE")
  set.seed(31)
  th <- c(as.numeric(circular::rvonmises(150, circular::circular(0.5), kappa = 6)),
          as.numeric(circular::rvonmises(150, circular::circular(3.0), kappa = 6))) %% (2 * pi)

  ours <- radiatR:::.fit_two_vm(th)$logLik

  x   <- circular::circular(th, units = "radians", type = "angles")
  res <- CircMLE::circ_mle(x)$results
  # circ_mle reports -LogLik per model; the free 2-component von Mises mixtures
  # are the M4/M5 family. Take the best (smallest -LogLik) mixture model as the
  # oracle for our free two-vM fit.
  mix_rows  <- grepl("^M[45]", rownames(res))
  oracle_ll <- -min(res[mix_rows, "-LogLik"], na.rm = TRUE)

  # our maximised logLik should match the best CircMLE mixture within tolerance
  # (both maximise the same family; small differences from optimiser settings).
  expect_equal(ours, oracle_ll, tolerance = 0.5)
})
```

Note for the implementer: confirm the exact `circ_mle()` return structure against
the installed CircMLE (`?CircMLE::circ_mle`) — the column is named `-LogLik` and
rows are `M1`..`M5B`. If the model naming differs in the installed version, select
the free two-component mixture rows accordingly; the assertion (our logLik matches
the best equivalent mixture within `0.5`) stays the same.

- [ ] **Step 3: Run the test**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-circ-mixtures.R")'`
Expected: PASS if CircMLE is installed; otherwise SKIP (reported, not failed).

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION tests/testthat/test-circ-mixtures.R
git commit -m "test(stats): CircMLE oracle for the two-von-Mises mixture fit

Add CircMLE to Suggests and a skip_if_not_installed oracle comparing our
native .fit_two_vm maximised logLik to circ_mle's best mixture model."
```

---

## Task 7: Docs, vignette, NEWS, and version bump

Document the expanded selector in the statistics vignette (with the millipede
walk-through), add the `0.5.0` NEWS heading, and bump the version.

**Files:**
- Modify: `vignettes/circular-statistics.Rmd`
- Modify: `NEWS.md`
- Modify: `DESCRIPTION` (`Version`)

**Interfaces:** none (documentation).

- [ ] **Step 1: Add a model-selection section to the vignette**

In `vignettes/circular-statistics.Rmd`, after the existing `test_uniformity`
section (around line 133), add a subsection. Use the millipede data already loaded
in the vignette:

````markdown
## Model selection

`circ_model_select()` ranks five candidate distributions by AICc — `uniform`,
`unimodal`, `axial`, `unimodal_uniform` (a directed mode over a uniform
background), and `bimodal` (an asymmetric two-mode mixture) — and reports Akaike
weights. It answers whether a heading sample is best described as undirected,
singly directed, axially oriented, directed-with-scatter, or genuinely bimodal.

```{r model-select}
hd_ms <- derive_headings(cpunctatus, rule = "distal")
arc_map <- unique(cpunctatus@data[, c("trial_id", "arc")])
names(arc_map)[1] <- "id"
hd_ms <- merge(as.data.frame(hd_ms), arc_map, by = "id")
circ_model_select(hd_ms, group_col = "arc")
```

The five models mirror the Schnute-Groot family implemented by the CircMLE
package (Fitak & Johnsen 2017); radiatR fits them natively (the two mixtures by
numerical maximum likelihood) and uses human-readable labels rather than the
`M1`-`M5` codes.
````

- [ ] **Step 2: Build the vignette to verify it runs**

Run: `Rscript -e 'devtools::load_all(); rmarkdown::render("vignettes/circular-statistics.Rmd", quiet = TRUE)'`
Expected: renders without error; the model-selection chunk prints a table with
five models per `arc`. Delete any generated `.html` afterward (not committed).

- [ ] **Step 3: Add the NEWS heading**

At the top of `NEWS.md`, add:

```markdown
# radiatR 0.5.0

* `circ_model_select()` now ranks five candidate models, adding
  `unimodal_uniform` (a directed von Mises over a uniform background) and
  `bimodal` (an asymmetric two-von-Mises mixture), fitted by numerical maximum
  likelihood. The `axial` model is now scored with an explicit closed-form
  density.
* `simulate_tracks()` gains `modality = "unimodal_uniform"` and `"bimodal"` with
  `mix_weight`, `mode2_mean`, and `kappa2` condition columns.
```

- [ ] **Step 4: Bump the version**

In `DESCRIPTION`, change `Version: 0.4.1` to `Version: 0.5.0`.

- [ ] **Step 5: Commit**

```bash
git add vignettes/circular-statistics.Rmd NEWS.md DESCRIPTION
git commit -m "docs: document expanded model selector; NEWS 0.5.0; version bump"
```

---

## Final verification (leave the full suite to CI)

Do **not** run `devtools::check()` or the whole `testthat` suite locally as a gate
(project convention: CI runs it). After Task 7:

- [ ] Run the three touched test files together:
  `Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat", filter = "model-selection|circ-mixtures|simulate-tracks")'`
  Expected: all PASS (CircMLE oracle SKIPs if not installed).
- [ ] Confirm `git status` is clean and the branch holds the seven task commits.
- [ ] Push the branch and open a PR summarising the five-model selector, the axial
  closed-form fix, and the millipede-diagnosis motivation. Let GitHub Actions run
  the full `R CMD check`.

---

## Self-review notes

- **Spec coverage:** New models (Tasks 2-4), axial audit (Task 1), native/no-hard-
  dep with CircMLE Suggests oracle (Task 6), simulate_tracks extension (Task 5),
  in-place `circ_model_select` so app + emitter inherit for free (Task 4 keeps the
  signature; no app changes needed), docs + M-correspondence + references +
  0.5.0 bump (Tasks 4, 7). Collate update (Task 2). All spec sections mapped.
- **Axial finding:** Task 1 replaces the scoring with a provably-normalised
  closed form and guards against spurious axial wins on uniform data; if the old
  `daxialvonmises/2` was already equivalent, the numbers are unchanged and the
  guard still documents the behaviour — a definite, tested answer either way.
- **Type consistency:** `.fit_vm_uniform` returns `p`; `.fit_two_vm` returns
  `gamma`/`mu1`/`kappa1`/`mu2`/`kappa2`; `.circ_model_loglik` reads only
  `$logLik`/`$converged`. `.dvm_log(theta, mu, kappa)` argument order is used
  identically in Tasks 2, 3, 4-tests. `k = c(0,2,2,3,5)` and the five model labels
  are identical in Tasks 4, 5, 7.
