# Headings Frame: Direct Loading and Stacked Circular Plotting

## Goal

Allow users to load pre-computed headings directly (e.g. from a CSV), without
requiring trajectory x/y data, and plot them on a circular plot with
configurable stacking to handle coincident or binned angles. `radiate()` is
extended to an S3 generic so it serves as the single plotting verb for both
trajectory and headings data.

## Motivation

Experimental headings are often recorded directly — a single compass bearing
per animal per trial, frequently binned at regular intervals (e.g. 5° bins
centred at 0°, 5°, 10°, …). Plotting coincident binned values as individual
points at the perimeter causes overplotting. Radial stacking, count shading,
and shape encoding address this. The `circular` package handles coincident
points via `stack = TRUE` / `sep`; this design brings equivalent functionality
to radiatR's ggplot2 pipeline.

---

## Architecture

Four units, each with a single responsibility:

| Unit | File | Responsibility |
|---|---|---|
| `headings_frame()` | `R/headings_frame.R` | Constructor: validate, convert, classify |
| `stack_headings()` | `R/headings_frame.R` | Preprocessing: compute stack columns |
| `radiate.headings_frame()` | `R/circular_plotting.R` | S3 method: headings plot entry point |
| `add_stacked_headings()` | `R/circular_plotting.R` | ggplot2 layer: stacked dot geometry |

`radiate()` in `circular_plotting.R` becomes an S3 generic (one-line change).
`radiate.TrajSet()` and `radiate.default()` are the renamed current
implementation — zero behaviour change.

---

## Data Container: `headings_frame()`

```r
headings_frame(data, col, units,
               angle_convention = "unit_circle",
               coords           = "absolute")
```

### Behaviour

1. Assert `col` exists in `data`; error if not.
2. `units` is required (no default); call `match.arg(units, c("radians",
   "degrees"))`. Error message hints "most behavioral data is recorded in
   degrees."
3. Call `.check_angle_units(data[[col]], units, col_name = col)` for the
   range-based mismatch warning (fires once per column name via
   `rlang::warn(.frequency = "once")`; suppressible via
   `options(radiatR.check_units = FALSE)`).
4. If `units == "degrees"`, convert the column in place: `data[[col]] <-
   data[[col]] * pi / 180`.
5. Validate `angle_convention` via `match.arg(..., c("unit_circle", "clock"))`.
6. Validate `coords` via `match.arg(..., c("absolute", "relative"))`.
7. Set attributes:
   - `attr(data, "angle_convention") <- angle_convention`
   - `attr(data, "coords") <- coords`
   - `attr(data, "heading_col") <- col`
     Downstream functions (`stack_headings()`, `add_stacked_headings()`,
     `radiate.headings_frame()`) use this attribute as the default value for
     their own `col` argument, so users do not have to repeat the column name.
8. Add class: `class(data) <- c("headings_frame", "data.frame")`.
9. Return `data`.

The heading column retains its original name. The returned object is a plain
data frame with a class prefix — all `dplyr` verbs, `split()`,
`circ_summarise()`, and standard R tools work without changes.

### Validation errors (informative messages)

| Condition | Message |
|---|---|
| `col` not in `data` | `"column '<col>' not found in data."` |
| `units` missing | `"'units' must be specified: use \"radians\" or \"degrees\".\n  Hint: most behavioral data is recorded in degrees."` |

---

## Preprocessing: `stack_headings()`

```r
stack_headings(data, col       = attr(data, "heading_col") %||% "heading",
               step            = 0.025,
               tol             = NULL,
               direction       = "inward",
               base_r          = 1,
               shade           = FALSE,
               shape           = FALSE)
```

Augments the data frame with computed columns. Returns the same data frame
(same number of rows, same grain) with new columns appended.

### Parameters

| Parameter | Default | Description |
|---|---|---|
| `col` | `"heading"` | Name of the angle column (radians) |
| `step` | `0.025` | Radial offset per stack level as a fraction of `base_r`. Matches `circular`'s `sep` default. At `base_r = 1`: 10 observations reach `r = 0.775` (inward) or `r = 1.225` (outward). |
| `tol` | `NULL` | Grouping tolerance in radians. `NULL` = exact equality (correct for binned data). `tol > 0` groups observations within `tol` radians (for continuous headings with near-coincident values). |
| `direction` | `"inward"` | `"inward"` stacks toward the centre (default, fits ggplot2 panel clipping). `"outward"` stacks away from the perimeter (matches `circular` convention). |
| `base_r` | `1` | Radius of the reference circle in data units. |
| `shade` | `FALSE` | If `TRUE`, add `shade_n` column (alias of `stack_n`) for use as an alpha aesthetic. |
| `shape` | `FALSE` | If `TRUE`, add `shape_code` integer column encoding per-observation multiplicity. |

### Output columns

| Column | Always? | Values | Purpose |
|---|---|---|---|
| `stack_r` | yes | numeric | Radial position: `base_r − step × (rank − 1)` (inward) or `base_r + step × (rank − 1)` (outward), where rank is 1-based position within the group |
| `stack_n` | yes | integer | Count of observations in this angle group |
| `shade_n` | if `shade = TRUE` | integer | Same as `stack_n`; separate column for clean `aes(alpha = shade_n)` mapping |
| `shape_code` | if `shape = TRUE` | integer 1 / 2 / 3 | 1 = hollow, 2 = filled, 3 = filled-with-ring. Encodes position in stack: outermost dot = 1 (hollow), innermost dot (when stack depth ≥ 2) = 3, middle dots = 2. Used without stacking, encodes raw count: 1 → 1 obs, 2 → 2 obs, 3 → 3+. |

### Grouping algorithm

1. Sort observations by `col`.
2. If `tol = NULL`: group by exact value. If `tol > 0`: assign each
   observation to the group of the nearest already-seen angle within `tol`
   (greedy scan on sorted values).
3. Within each group, assign ranks 1 … n in sorted order (ties broken by
   original row order).
4. Compute `stack_r` from rank and direction.

### `shape_code` logic

`shape_code` encodes position-in-stack (rank), not raw count.

```
stack depth == 1:  single obs → shape_code = 1  (hollow)
stack depth == 2:  rank 1 (outermost) = 1,  rank 2 (innermost) = 2
stack depth >= 3:  rank 1 = 1,  rank n (innermost) = 3,  all middle ranks = 2
```

A group with one observation always produces `shape_code = 1` (hollow),
distinguishing it visually from groups with two or more observations.

---

## S3 Generic: `radiate()`

### Change to existing code

```r
# Replace the current function definition header with:
radiate <- function(data, ...) UseMethod("radiate")

radiate.TrajSet <- function(data, ...) {
  # <current body of radiate() unchanged>
}

radiate.default <- function(data, ...) {
  # <current coercion block at the top of radiate() unchanged>
  # calls radiate.TrajSet() after coercion
}
```

No behaviour change for existing callers. `radiate(ts)` dispatches to
`radiate.TrajSet()`; `radiate(plain_df)` dispatches to `radiate.default()`.

### New method: `radiate.headings_frame()`

```r
radiate.headings_frame <- function(
  data,
  col       = "heading",
  step      = 0.025,
  tol       = NULL,
  direction = "inward",
  base_r    = 1,
  shade     = FALSE,
  shape     = FALSE,
  panel_by  = NULL,
  ncol      = NULL,
  ticks     = TRUE,
  degrees   = TRUE,
  title     = NULL,
  style     = c("classic", "minimal"),
  ...)
```

**Body:**

1. Build the base plot:
   ```r
   p <- ggplot2::ggplot() + ggplot2::coord_fixed() +
        add_circ(base_r) + if (ticks) add_ticks() else NULL
   ```
2. Call `add_stacked_headings(data, col, step, tol, direction, base_r,
   shade, shape, ...)` and add the result to `p`.
3. Apply `panel_by` faceting if supplied (same `facet_wrap` logic as
   `radiate.TrajSet()`).
4. Apply `style`, `title`, `degrees` label formatting.
5. Return `p`.

---

## ggplot2 Layer: `add_stacked_headings()`

```r
add_stacked_headings(data,
                     col       = "heading",
                     step      = 0.025,
                     tol       = NULL,
                     direction = "inward",
                     base_r    = 1,
                     shade     = FALSE,
                     shape     = FALSE,
                     colour    = "black",
                     size      = 2,
                     alpha     = 1,
                     ...)
```

**Body:**

1. If `stack_r` is not already a column in `data`, call `stack_headings(data,
   col, step, tol, direction, base_r, shade, shape)` to compute it.
2. Convert stacked positions to Cartesian:
   ```r
   data$.x_stk <- data$stack_r * cos(data[[col]])
   data$.y_stk <- data$stack_r * sin(data[[col]])
   ```
   (Respects `display_convention` attribute if present, via
   `.to_clock_display()` as in `add_heading_points()`.)
3. Build the `aes()` mapping on `.x_stk` / `.y_stk`.
4. If `shape_code` column is present, map it to ggplot2 shape integers:

   | `shape_code` | ggplot2 `shape` | Appearance |
   |---|---|---|
   | 1 | 1 | hollow circle |
   | 2 | 16 | filled circle |
   | 3 | 21 | filled circle with border ring |

5. Alpha precedence:
   - If `shade = TRUE` and `stack_n` is present: map `stack_n` to alpha
     (scaled 0.2–1 across the observed range). The fixed `alpha` argument
     is ignored in this case.
   - If `shade = FALSE`: use the fixed `alpha` value for all points.
6. Return `ggplot2::geom_point(...)`.

**`add_heading_points()` is kept unchanged** for backward compatibility. It
remains the zero-stacking version (dots at `r = 1`, no preprocessing).

---

## File layout

```
R/
  headings_frame.R       — headings_frame(), stack_headings()
  circular_plotting.R    — radiate() generic + radiate.TrajSet(),
                           radiate.default(), radiate.headings_frame(),
                           add_stacked_headings() (existing add_heading_points
                           unchanged)
tests/testthat/
  test-headings-frame.R  — all tests for this feature
```

---

## Error handling

| Location | Condition | Behaviour |
|---|---|---|
| `headings_frame()` | `col` not found | `stop()` with column name |
| `headings_frame()` | `units` missing | `stop()` with hint |
| `headings_frame()` | range mismatch | `rlang::warn()`, once per col |
| `stack_headings()` | `col` not found in data | `stop()` |
| `stack_headings()` | `step <= 0` | `stop("'step' must be positive.")` |
| `stack_headings()` | `tol < 0` | `stop("'tol' must be NULL or non-negative.")` |
| `add_stacked_headings()` | col not found | `stop()` with col name |

---

## Testing outline

- `headings_frame()`: valid construction, degree conversion, attribute setting,
  range warning fires once per column, explicit-units error, missing-col error.
- `stack_headings()`: exact grouping (binned), tolerance grouping, inward
  direction produces `stack_r < base_r`, outward produces `stack_r > base_r`,
  `stack_n` correct, `shade_n` / `shape_code` added only when requested,
  single-observation groups (`stack_r == base_r`), all-unique angles
  (no stacking), parameter validation errors.
- `radiate.headings_frame()`: returns a ggplot2 object, `panel_by` produces
  facets, stacking params forwarded correctly.
- `add_stacked_headings()`: auto-calls `stack_headings()` when `stack_r`
  absent, respects pre-computed `stack_r`, `shape_code` mapping correct.
- Backward compatibility: `radiate(ts)` and `radiate(plain_df)` behaviour
  unchanged.
