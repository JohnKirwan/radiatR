# `circ_summarise()` Design

**Date:** 2026-05-28

## Goal

Add a tidy, grouped circular summary function to radiatR that works on any data frame with a circular angle column — not just headings from `derive_headings()`. The function integrates naturally with dplyr pipelines and returns a plain tibble suitable for analysis, reporting, and downstream use with `compute_circ_interval()`.

## Background

The package already has two summary mechanisms:

- `circ_summary()` — S4 method on `TrajSet`; summarises step angles per trial or globally
- `compute_circ_mean()` — works on headings data frames; single grouping column only; plot-facing (propagates display attributes for overlay routing)

Neither covers the general case: a tibble of circular observations with arbitrary grouping variables summarised in tidyverse style. `circ_summarise()` fills that gap without touching either existing function.

## Architecture

### File

`R/circular_statistics.R` — alongside `circ_summary()` and `dwell_proportions()`. No new file.

### Dependencies

- `rlang` (already used in package) — for `ensym()` / `as_string()` to accept both bare and quoted column names
- `dplyr` (tidyverse dependency) — for `group_vars()` to read existing groups; tibble output via `tibble::as_tibble()`
- `circular` (already a dependency) — for `mean.circular()`, `rho.circular()`, `est.kappa()`; existing `.est_kappa_safe()` and `.clock_to_uc()` helpers reused

## API

```r
circ_summarise(
  data,
  col,
  .by              = NULL,
  stats            = c("n", "mean_dir", "mean_dir_deg", "resultant_R", "kappa"),
  angle_convention = NULL,
  coords           = NULL
)
```

### Parameters

| Parameter | Type | Default | Description |
|---|---|---|---|
| `data` | data frame / grouped tibble | — | Input data |
| `col` | unquoted name or string | — | Column containing angles in radians |
| `.by` | character vector or `NULL` | `NULL` | Grouping columns; overrides `group_by()` groups |
| `stats` | character vector | all cheap stats | Statistics to include; order determines column order in output |
| `angle_convention` | `"unit_circle"` / `"clock"` / `NULL` | `NULL` | Read from `attr(data, "angle_convention")` when `NULL`; defaults to `"unit_circle"` if absent |
| `coords` | `"relative"` / `"absolute"` / `NULL` | `NULL` | Read from `attr(data, "coords")` when `NULL`; defaults to `"absolute"` if absent; only used when `angle_convention = "clock"` |

### Return value

An ungrouped `tibble` with one row per group (or one row for grand summary). Columns: group columns first (in the order they appear in `.by` or `group_vars(data)`), then requested stat columns in the order listed in `stats`.

## Statistics

All computation converts angles to UC convention internally via the existing `.clock_to_uc()` helper before passing to `circular`. `mean_dir` output is returned in the same convention as the input.

| `stats` value | Output column | Computation | Notes |
|---|---|---|---|
| `"n"` | `n` | Count of non-NA angles | Integer |
| `"mean_dir"` | `mean_dir` | `circular::mean.circular()`, wrapped to [0, 2π] via `.wrap_to_2pi()` | Radians, input convention |
| `"mean_dir_deg"` | `mean_dir_deg` | `mean_dir * 180 / pi` | Degrees [0, 360), input convention |
| `"resultant_R"` | `resultant_R` | `circular::rho.circular()` | [0, 1] |
| `"kappa"` | `kappa` | `.est_kappa_safe()` | `NA` when n < 3 or estimation fails |

Unknown `stats` values raise an informative error listing valid options.

### Edge cases

- **n = 0** (all NA or empty group): `mean_dir`, `mean_dir_deg`, `resultant_R`, `kappa` all `NA`; `n = 0`
- **n = 1 or 2**: `mean_dir` and `resultant_R` computed normally; `kappa = NA`
- **Non-finite angles**: silently excluded before computation (consistent with `compute_circ_mean()`)

## Grouping mechanics

Priority order:

1. `.by` supplied → use those columns (ignores any `group_by()` groups on `data`)
2. `dplyr::group_vars(data)` non-empty → use those groups
3. Neither → single grand-summary row

Output is always **ungrouped** (same convention as `dplyr::summarise(.by = ...)`).

Group column types are preserved in output. Factor columns: row order follows factor levels. Non-factor: alphabetical order. Consistent with `compute_circ_mean()`.

**Errors:** unknown column in `.by` or `col` → immediate informative error: `".by column 'foo' not found in data"`.

## Scope: what this is NOT

- Does not compute confidence intervals — use `compute_circ_interval()` on the original headings data frame for that
- Does not replace `compute_circ_mean()` — that function's plot-facing contract (attribute propagation, overlay routing) is different and untouched
- Does not replace `circ_summary()` — that function operates on `TrajSet` step angles

## Testing

1. **Grand summary (no grouping)**: single-row output, correct `mean_dir`/`resultant_R`/`kappa`/`n` for a known set of angles
2. **`.by` grouping**: two-group input, correct per-group values, correct column order
3. **`group_by()` grouping**: same as above but via pre-grouped tibble
4. **`.by` overrides `group_by()`**: pre-grouped tibble + different `.by` → `.by` wins
5. **`stats` selection**: request subset; only those columns present in output, in requested order
6. **Unknown `stats` value**: error with informative message
7. **Clock convention**: angles in clock convention → correct `mean_dir` in clock convention on output
8. **Attribute reading**: `angle_convention`/`coords` read from data frame attributes when not supplied
9. **Edge cases**: all-NA group returns row with `n = 0`, all stats `NA`; n = 1 group returns `kappa = NA`
10. **Factor grouping**: row order follows factor levels
11. **Missing `.by` column**: informative error
12. **Missing `col` column**: informative error
