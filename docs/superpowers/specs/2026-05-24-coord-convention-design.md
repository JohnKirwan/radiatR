# Coordinate Frame and Angular Convention Design

**Date:** 2026-05-24
**Status:** Approved

## Problem

`derive_headings()` currently returns absolute headings (computed from `trans_x`/`trans_y`) with no way to request stimulus-relative headings. Output angles are in unit-circle convention (0° = East, counterclockwise) with no option for the more natural compass/clock convention (0° = North, clockwise). Downstream helpers (`circ_mean_segments()`, `circ_summary*()`) have no way to know which convention was used, so plots can silently show arrows that are biologically meaningless (averaging absolute headings from trials where the stimulus was at different azimuths).

## Approach

Approach B: new `coords` and `angle_convention` parameters on `derive_headings()`, with the output tagged via `attr()` so downstream helpers auto-detect the convention. TrajSet@cols gains optional `rel_x`/`rel_y` pointer fields. No new computation machinery — column pointers reference columns that already exist in the trajectory tibbles (produced by the existing `circular_trials.R` pipeline or supplied by the user).

## Section 1: TrajSet@cols extension

`TrajSet@cols` gains two optional character pointer fields:

- `rel_x`: name of the column in each trajectory tibble holding stimulus-relative x coordinates
- `rel_y`: name of the column in each trajectory tibble holding stimulus-relative y coordinates

Both default to `NULL`. The `TrajSet()` constructor gains matching optional arguments. The validator enforces that either both are provided or neither. `show()` prints `Relative coords: <rel_x>, <rel_y>` when non-null.

No new computation machinery is added. How `rel_x`/`rel_y` columns are produced is out of scope (the existing `circular_trials.R` pipeline already produces them for the landmark case). The plividus dataset is rebuilt with `rel_x = "rel_x"` and `rel_y = "rel_y"` registered.

## Section 2: derive_headings() API

```r
derive_headings(x, rule, ...,
                coords = c("absolute", "relative"),
                angle_convention = c("clock", "unit_circle"))
```

**`coords`**
- `"absolute"` (default): uses `x@cols$x` / `x@cols$y` — backward compatible
- `"relative"`: uses `x@cols$rel_x` / `x@cols$rel_y`; errors informatively if those pointers are `NULL`

**`angle_convention`**
- `"clock"` (default): 0° = North/top, increases clockwise
  - Absolute coords: `(π/2 − uc) mod 2π`
  - Relative coords: `(−uc) mod 2π` (stimulus at 0°/North)
- `"unit_circle"`: 0° = East, counterclockwise — current behaviour

Heading rules (`crossing`, `displacement`, etc.) compute angles in unit-circle space internally. The convention conversion is a final post-processing step.

The returned data frame is tagged:
```r
attr(out, "angle_convention") <- angle_convention  # "clock" or "unit_circle"
attr(out, "coords")           <- coords            # "absolute" or "relative"
```

These attributes travel with the data frame through pipes and `circ_summary*()`.

## Section 3: circ_summary*() and downstream auto-detection

**`circ_summary_headings()` / `circ_summary()`** gain:

```r
circ_summary_headings(headings, ...,
                      angle_convention = c("clock", "unit_circle"))
```

Resolution order:
1. Explicit `angle_convention` argument
2. `attr(headings, "angle_convention")`
3. Fall back to `"unit_circle"` silently (backward compat for raw numeric vectors)

Summary statistics (`mean_dir`, `rho`, CI bounds) are computed in unit-circle space, then converted for output. The returned summary tibble carries the same `angle_convention` attribute.

**`circ_mean_segments()`** auto-detects via `attr(df, "angle_convention")`:
- `"clock"`: converts output angle to unit-circle before the `cos`/`sin` call:
  `xend = R * cos(rad_unclock(mean_dir))`, `yend = R * sin(rad_unclock(mean_dir))`
- `"unit_circle"` or absent: current behaviour unchanged

All other geom helpers (`add_circ`, `add_ticks`, etc.) are unaffected.

## Affected files

| File | Change |
|------|--------|
| `R/TrajSet.R` | Add `rel_x`/`rel_y` to `@cols`; update constructor, validator, `show()` |
| `R/headings.R` | Add `coords` + `angle_convention` params to `derive_headings()`; conversion logic; tag output attrs |
| `R/circular_statistics.R` | Add `angle_convention` param to `circ_summary_headings()` / `circ_summary()`; attr pass-through |
| `R/circular_plotting.R` | Update `circ_mean_segments()` to auto-detect convention from attr |
| `data-raw/plividus.R` | Rebuild plividus with `rel_x`/`rel_y` registered in `@cols` |
| `vignettes/radiatR.Rmd` | Use `coords="relative", angle_convention="clock"` for per-condition plots |
| `tests/testthat/` | Update affected test files |

## Non-goals

- General landmark transformation API (future work; existing `build_unit_circle_mapping()` / `circular_trials.R` already handles the P_lividus case)
- Changing how heading rules compute angles internally
- Modifying any geom helper other than `circ_mean_segments()`
