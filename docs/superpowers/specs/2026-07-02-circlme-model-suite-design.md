# Expanded circular model selector (circLME suite) — design

**Date:** 2026-07-02
**Status:** Design approved (pending written-spec review)
**Touches:** `R/model_selection.R`, `R/circ_mixtures.R` (new), `R/simulate_tracks.R`,
`DESCRIPTION`, `NEWS.md`, `vignettes/circular-statistics.Rmd`, `tests/testthat/`

## Motivation

`circ_model_select()` currently ranks three closed-form models — `uniform`,
`unimodal` (von Mises), and `axial` (symmetric bimodal von Mises) — by AICc/BIC
with Akaike weights. On the bundled *C. punctatus* millipede data it
**misdiagnoses orientation**:

- **arc 0 has no stimulus, so headings should be uniform**, yet the selector
  picks `axial` (weight 0.57) over `uniform` (0.24) and `unimodal` (0.19).
  Axial is beating uniform *on data that should be uniform*.
- Wherever there is any structure, `axial` out-scores `unimodal` at **identical
  k = 2** (e.g. arc 10/20: axial beats unimodal by ~4 AICc). With no parameter
  penalty to pay, the symmetric bimodal fit absorbs the return-leg / back-scatter
  of walking tracks and always wins the likelihood contest against a single mode.

This is two problems at once:

1. **Evaluation artefact.** Axial spuriously beating *uniform* on uniform data
   points at the axial likelihood being over-credited (κ mapping from the
   doubled-angle fit and/or the `/2` half-circle normalisation).
2. **Missing models.** The current set has no way to express "directed with a
   random background" or "two unequal, non-antipodal modes," so structured
   conditions get forced into the symmetric `axial` model.

The goal is **better discrimination**, not parity with a checklist. We add the
two models that directly attack the misdiagnosis and audit the axial scoring.

## Approach

Extend `circ_model_select()` **in place**: same signature, more rows. Native
implementation via `optim`; **no hard runtime dependency**. Add `CircMLE`
(Fitak & Johnsen 2017) to `Suggests` purely as a test oracle — it is unmaintained,
so it stays out of the runtime path.

Because the Shiny app (`inst/app/app.R:1509`) and the code-emitter
(`inst/app/plot_spec.R:443/446`) call `circ_model_select()` directly, both inherit
the new models for free — the only app-visible change is a longer results table.
No new wiring, no new exported entry point.

### Rejected alternatives

- **Wrap `CircMLE::circ_mle` at runtime** (Path A, full 10-model parity). Fastest
  to full parity but takes an unmaintained package into the runtime path for
  models we don't need. Rejected; retained only as a Suggests test oracle.
- **Full Schnute–Groot 10-model A/B/C suite.** More scope than the diagnostic
  problem needs. We implement the two models that fix the observed misdiagnosis
  and leave the door open (the fitters generalise).

## New models

| radiatR label      | Form                                             | k | Schnute–Groot / CircMLE family |
|--------------------|--------------------------------------------------|---|--------------------------------|
| `uniform`          | Uniform on the circle                            | 0 | uniform (M1)                   |
| `unimodal`         | `vM(μ, κ)`                                        | 2 | single von Mises               |
| `axial`            | Symmetric bimodal `vM`, antipodal equal modes     | 2 | symmetric bimodal (constrained)|
| **`unimodal_uniform`** | `p·vM(μ, κ) + (1−p)·Uniform`                  | 3 | directed + uniform background  |
| **`bimodal`**      | `γ·vM(μ1, κ1) + (1−γ)·vM(μ2, κ2)`, free modes     | 5 | asymmetric bimodal mixture     |

The exact `M1`–`M5` (and A/B/C sub-variant) codes each radiatR label corresponds
to are **confirmed against the CircMLE documentation during implementation** and
written into the docs then — the family descriptions above are the design intent,
not a pinned code mapping.

- **`unimodal_uniform`** — a directed mode over a random background. The natural
  "directed but scattered" story; the expected corrective for structured
  conditions currently forced into `axial`.
- **`bimodal`** — generalises `axial`: the two modes need not be antipodal or
  equally concentrated/weighted, so `axial` must now earn its symmetry against an
  honest alternative and pay for the three extra parameters through AICc.

radiatR keeps human-readable labels rather than the `M1..M5` codes; the
correspondence is documented (see table note) so users familiar with CircMLE can
map across.

## Numerical fitting — `R/circ_mixtures.R` (new file)

A small, single-purpose module holding the two mixture MLE fitters. Kept separate
from `model_selection.R` so the numerical machinery can be understood and tested
in isolation.

- Minimise negative log-likelihood with `stats::optim`; mixture components use
  `circular::dvonmises` densities (consistent with the existing likelihoods).
- **Unconstrained parameterisation** for stable optimisation:
  - `κ = exp(η)` (κ ≥ 0),
  - mixing weight via logit (∈ (0, 1)),
  - means optimised unconstrained, then wrapped to [0, 2π) via `.wrap_to_2pi`.
- **Start-value grid** (the mixture likelihood is multimodal): seed component
  means from the leading kernel-density peaks and/or a coarse angle grid, κ from
  the sample mean resultant length, weight from 0.5; run multiple restarts and
  keep the best maximum.
- **Convergence handling:** non-convergence or a non-finite optimum yields
  `logLik = NA` for that model — identical contract to today's `vonmises_fit`
  failures, which `.circ_model_criteria` already sorts last with `NA` criteria.
- **Identifiability:** mixture label-switching is resolved by a canonical
  ordering (larger-weight component first) for reporting only; it does not affect
  the likelihood.

The fitters return a maximised `logLik` and the fitted parameters; only `logLik`
and `k` feed the selection table (parameters are available for callers who want
them, mirroring how `vonmises_fit` exposes the chosen model's parameters).

## Axial evaluation audit

Treated as a first-class deliverable, not a side note, because arc 0 shows axial
beating *uniform* on uniform data.

- Verify the doubled-angle fit's κ (`vonmises_fit(axial = TRUE)`) maps correctly
  into the `κ` argument of `circular::daxialvonmises`.
- Verify the `/2` half-circle normalisation and the density support are scored on
  equal footing with `unimodal`/`uniform` (a proper full-circle density,
  integral 1, over [0, 2π)).
- If the current code systematically inflates the axial likelihood, fix it and
  pin the corrected behaviour with a regression test (e.g. axial must **not** out-
  score uniform on samples drawn from the uniform distribution).
- Acceptable outcome either way: the audit may confirm walking-track headings are
  genuinely bimodal and the scoring is fair. The deliverable is a definite answer
  plus a guarding test — not a presupposed bug fix.

## `simulate_tracks()` extension

Extend the generator so the new models are testable end-to-end and usable in the
vignette. Add distribution options that emit:

- **vM + uniform background** — a directed fraction `p` plus a uniform remainder.
- **asymmetric bimodal** — two von Mises components with independent means,
  concentrations, and mixing weight.

These join the existing `uniform` / `unimodal` / `axial` generators and are driven
through the same `conditions` data-frame contract (new optional columns for the
second mode / background fraction, defaulted so existing calls are unchanged).
Scope guard: only the parameters the two new models need — no general
`n`-component mixture engine.

## Testing

- **Recovery tests** (mirror `test-simulate-tracks.R:68–70`): simulate from each
  new model via `simulate_tracks()`, assert the selector ranks the generating
  model best.
- **Axial-audit regression:** on uniform samples, `axial` must not out-score
  `uniform`; pin the corrected axial logLik relationship.
- **Oracle test** (mirror the existing Hermans–Rasson oracle,
  `test-circular-stats.R:920`): compare native `unimodal_uniform` / `bimodal`
  maximised logLik against `CircMLE::circ_mle` on shared data, guarded with
  `skip_if_not_installed("CircMLE")`.
- **Millipede validation:** re-run grouped selection over `arc`; confirm arc 0's
  diagnosis moves toward `uniform` and the structured conditions gain access to
  `unimodal_uniform` / `bimodal`. (Sanity check, recorded in the vignette rather
  than asserted as a hard equality, since it depends on the heading rule.)
- `.circ_model_criteria` already handles arbitrary `k` and small-n `AICc = NA`;
  the `k` vector becomes `c(0, 2, 2, 3, 5)`.

## Housekeeping

- **DESCRIPTION `Collate`:** add `R/circ_mixtures.R` (hand-maintained Collate — a
  new source file omitted from it breaks `R CMD check`/build).
- **DESCRIPTION `Suggests`:** add `CircMLE`.
- **NEWS + DESCRIPTION `Version`:** cut a fresh `0.5.0` heading (minor bump from
  0.4.1) and bump `DESCRIPTION`. The #166–#177 block currently filed under the
  tagged 0.4.1 heading moves under 0.5.0 too.
- **Docs:** update `circ_model_select()` Roxygen — the five models, their `k`, the
  M1–M5 correspondence table, and references (Schnute & Groot 1992 for the model
  family; Fitak & Johnsen 2017 for CircMLE). Update
  `vignettes/circular-statistics.Rmd` with the expanded model set and the
  millipede walk-through. Run `devtools::document()`.

## Out of scope (deferred)

- Full Schnute–Groot 10-model A/B/C variant suite / runtime `CircMLE` wrap.
- Concentration regression / von Mises GLM (`kappa(x)`), tracked separately.
- App UI polish beyond the automatically longer results table (e.g. per-model
  parameter display) — revisit if the longer table warrants it.
