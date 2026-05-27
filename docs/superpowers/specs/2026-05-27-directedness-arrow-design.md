# Directedness Arrow Design

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the manual `circ_mean_segments()` / `gg_add_circ_mean()` pattern with a clean three-function trio (`compute_circ_mean()` / `add_circ_mean()` / `add_heading_arrow()`) that composes with `+` like every other heading layer, fixes a coordinate-convention bug in the existing code, and simplifies the vignette dramatically.

**Architecture:** Follows the established compute / add / convenience-wrapper pattern used by `compute_circ_interval` + `add_circ_interval` + `add_heading_interval` and `compute_circular_density` + `add_circular_density` + `add_heading_density`. The compute step converts input angles to unit-circle radians using the `circular` package, stores `mean_dir` always in UC convention so the render step needs no conversion. `circ_mean_segments()` and `gg_add_circ_mean()` are deprecated with `.Deprecated()` stubs.

**Tech Stack:** R, ggplot2, circular package, existing radiatR internals (`.clock_to_uc()`, `.wrap_to_2pi()`, `.as_circ()`).

---

## The Bug Being Fixed

`circ_mean_segments()` ignores the `coords` attribute on its input. It always converts `mean_dir` from clock convention using `rad_unclock()` (`π/2 − θ`), which is correct only for `coords = "absolute"`. For `coords = "relative"` the correct inverse is `(−θ) mod 2π`. This causes per-condition arrows in multi-panel plots (where `coords = "relative"` is typical) to point in wrong directions.

---

## Section 1: Signatures

All three functions live in `R/circular_plotting.R`, consistent with `compute_circ_interval`, `add_circ_interval`, `add_heading_interval`.

```r
compute_circ_mean(
  headings_df,
  heading_col      = "heading",
  colour_col       = NULL,       # groups computation; also maps to colour in render step
  angle_convention = NULL,       # read from attr(headings_df) if NULL; "clock" or "unit_circle"
  coords           = NULL        # read from attr(headings_df) if NULL; "relative" or "absolute"
)
# Returns: data frame — columns: mean_dir (UC radians), resultant_R, colour_col (if used)

add_circ_mean(
  summary_df,
  colour_col      = NULL,
  linewidth       = 1,
  colour          = "black",
  arrow_length_cm = 0.2,
  ...                            # forwarded to geom_segment
)
# Returns: geom_segment layer

add_heading_arrow(
  headings_df,
  heading_col      = "heading",
  colour_col       = NULL,
  angle_convention = NULL,
  coords           = NULL,
  linewidth        = 1,
  colour           = "black",
  arrow_length_cm  = 0.2,
  ...
)
# Returns: geom_segment layer (calls compute_circ_mean then add_circ_mean)
```

---

## Section 2: `compute_circ_mean()` Computation

**Convention resolution (at function entry):**
- If `angle_convention` is `NULL`, read `attr(headings_df, "angle_convention")`; if still `NULL`, default to `"unit_circle"` and emit `message("angle_convention not specified and not found in headings_df attributes; defaulting to 'unit_circle'.")`.
- Same for `coords`: default `"absolute"`, same message pattern.
- Validate: `angle_convention %in% c("clock", "unit_circle")`, `coords %in% c("relative", "absolute")`.

**Per group** (split by `colour_col`; one group when `colour_col = NULL`):

1. Extract angles from `heading_col`; keep only finite values.
2. Convert to UC radians:
   - If `angle_convention == "clock"`: apply `.clock_to_uc(angles, coords)` — this uses `(−θ) mod 2π` for `"relative"` and `wrap_to_2pi(rad_unclock(θ))` for `"absolute"`.
   - If `angle_convention == "unit_circle"`: use angles as-is.
3. Construct `circular` object: `circular::circular(uc_angles, units = "radians", modulo = "2pi")` (standard UC, so `circular` package functions need no further convention parameters).
4. If fewer than 2 finite angles: return `mean_dir = NA_real_`, `resultant_R = NA_real_`.
5. `mean_dir    <- .wrap_to_2pi(as.numeric(circular::mean.circular(circ_obj, na.rm = TRUE)))`
6. `resultant_R <- as.numeric(circular::rho.circular(circ_obj, na.rm = TRUE))`

**Output:** `do.call(rbind, ...)` of per-group rows. Factor levels on `colour_col` preserved (same as `compute_circ_interval()`). `mean_dir` is **always unit-circle convention** — document this explicitly in Roxygen.

---

## Section 3: `add_circ_mean()` Rendering

`mean_dir` is always UC, so no conversion is needed before plotting:

```r
summary_df$.x    <- 0
summary_df$.y    <- 0
summary_df$.xend <- summary_df$resultant_R * cos(summary_df$mean_dir)
summary_df$.yend <- summary_df$resultant_R * sin(summary_df$mean_dir)
```

**Empty-layer guard:** if all rows have `NA` `mean_dir`, return an empty `geom_segment` (same pattern as `add_circ_interval()`).

**Colour mapping:** when `colour_col` is present in `summary_df`, add `colour = rlang::sym(colour_col)` to the `aes()` mapping and do **not** pass the fixed `colour` argument. When absent, pass `colour` as a fixed aesthetic. The `colour_col` column being present in the data is sufficient for ggplot2 faceting to work automatically — no special handling needed.

**Arrow spec:** construct `grid::arrow(length = grid::unit(arrow_length_cm, "cm"))` internally. The user can override with a custom arrow via `...` (e.g. `arrow = grid::arrow(...)`).

---

## Section 4: `add_heading_arrow()` Wrapper

Pure forwarding wrapper — identical pattern to `add_heading_interval()`:

```r
add_heading_arrow <- function(headings_df, heading_col = "heading",
                              colour_col = NULL, angle_convention = NULL,
                              coords = NULL, linewidth = 1, colour = "black",
                              arrow_length_cm = 0.2, ...) {
  sm <- compute_circ_mean(headings_df, heading_col = heading_col,
                          colour_col = colour_col,
                          angle_convention = angle_convention, coords = coords)
  add_circ_mean(sm, colour_col = colour_col, linewidth = linewidth,
                colour = colour, arrow_length_cm = arrow_length_cm, ...)
}
```

---

## Section 5: Deprecations

In `R/headings.R`, add `.Deprecated()` calls to `circ_mean_segments()` and `gg_add_circ_mean()`:

```r
circ_mean_segments <- function(...) {
  .Deprecated("compute_circ_mean")
  # existing body preserved
}

gg_add_circ_mean <- function(...) {
  .Deprecated("add_heading_arrow")
  # existing body preserved
}
```

Do **not** remove the function bodies — users on older vignette code will get a warning, not an error.

---

## Section 6: Vignette Updates (`vignettes/radiatR.Rmd`)

Every occurrence of the old pattern is replaced. Intermediate variables (`summ_all`, `seg_all`, `arc_summ`, `segs`) that exist only to feed the old pattern are removed.

| Chunk | Replacement |
|---|---|
| `headings-overlay` | Remove `summ_all`, `seg_all`; replace `gg_add_circ_mean(p_all, seg_all)` with `p_all + add_heading_arrow(hd)` |
| `arc-panels` | Remove entire 15-line `arc_summ` block + `segs`; replace `geom_segment(data = segs, ...)` with `add_heading_arrow(hd, colour_col = "arc", colour = "black")` |
| `density-overlay` | Replace `geom_segment(data = segs, ...)` with `add_heading_arrow(hd, colour_col = "arc", colour = "black")` |
| `bootstrap-ci` | Same as above |
| `build-3` | Replace `geom_segment(data = seg_all, ...)` with `add_heading_arrow(hd)` |

The `arc_summ` table displayed in the "Circular Summary Statistics" section (`arc_summ[, c("arc", "mean_dir", "resultant_R")]`) is replaced with `compute_circ_mean(hd, colour_col = "arc")[, c("arc", "mean_dir", "resultant_R")]`.

The inline R in the `headings-overlay` narrative currently displays `summ_all$mean_dir` in clock convention ("X° relative to stimulus"). Since `compute_circ_mean()` always returns UC, the replacement adds a conversion back to clock for display only: add a named chunk `summ-inline` that computes `summ_hd <- compute_circ_mean(hd)` and `summ_hd_clock <- (.uc_to_clock(summ_hd$mean_dir, attr(hd, "coords") %||% "relative") * 180 / pi)`, then the inline R references `round(summ_hd_clock, 1)` and `round(summ_hd$resultant_R, 2)`. The narrative wording is unchanged.

---

## Section 7: Testing

Tests go in `tests/testthat/test-circular-plotting.R`, three new sections at the end.

### `# ---- compute_circ_mean ----`

1. UC input: mean of angles clustered near 0 → `mean_dir` near 0, `resultant_R` > 0.
2. Clock+relative vs clock+absolute produce different `mean_dir` for the same clock angle (they use different inverse formulas — `(−θ) mod 2π` vs `rad_unclock(θ)` respectively). Verify by passing the same nonzero clock angle with both settings and confirming the UC results differ.
3. Convention roundtrip: for `angle_convention = "clock"`, the computed `mean_dir` (UC) fed through `cos`/`sin` should agree with the arrow direction produced by `add_heading_points()` on the same headings (i.e. the arrow visually points where the heading cloud is).
4. Grouped: `colour_col = "group"` with two groups → two-row output, one row per group.
5. n < 2: fewer than 2 finite values → `mean_dir = NA`, `resultant_R = NA`.
6. All-NA input → single row with `NA` values, no error.
7. Factor levels preserved: if `colour_col` is a factor, output factor levels match input.
8. Reads convention from attributes: `attr(hd, "angle_convention") <- "clock"` without explicit parameter → same result as passing `angle_convention = "clock"` explicitly.

### `# ---- add_circ_mean ----`

1. Returns `LayerInstance`.
2. All-NA summary → returns empty layer (no error, `ggplot_build` succeeds).
3. `colour_col` present → `colour` mapped as aesthetic (not fixed) in built plot data.

### `# ---- add_heading_arrow ----`

1. Returns `LayerInstance`.
2. Output matches two-step `compute_circ_mean()` + `add_circ_mean()` (compare built x/y coordinates).
3. Can be added to `radiate()` plot without error; `ggplot_build` succeeds.

---

## File Placement

| File | Change |
|---|---|
| `R/circular_plotting.R` | Add `compute_circ_mean`, `add_circ_mean`, `add_heading_arrow` after the `add_heading_interval` section |
| `R/headings.R` | Add `.Deprecated()` stubs to `circ_mean_segments` and `gg_add_circ_mean` |
| `tests/testthat/test-circular-plotting.R` | Append three new test sections |
| `vignettes/radiatR.Rmd` | Replace old pattern in five chunks (see Section 6) |
