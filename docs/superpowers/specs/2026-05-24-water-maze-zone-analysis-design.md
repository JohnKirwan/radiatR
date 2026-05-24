# Circular Arena Zone Analysis — Design Spec

**Date:** 2026-05-24
**Branch:** to be implemented on a new feature branch off master
**Status:** Approved

---

## Overview

Two new plain functions added to `R/circular_statistics.R` for analysing spatial distributions in circular arenas — applicable to any experiment where dwell time matters more than (or alongside) heading: water mazes, open-field tests, Drosophila preference assays, place-preference arenas, etc.

1. `zone_dwell()` — dwell-time proportions across a quadrant × annular-ring grid
2. `count_goal_entries()` — number of entries into a circular zone around a defined goal location

Both follow the existing `derive_headings()` / `circ_summary_headings()` pattern: accept a `TrajSet`, return a plain `data.frame`.

---

## Function 1: `zone_dwell()`

### Signature

```r
zone_dwell(x, target_angle, target_radius = 1,
           ring_breaks = c(0, 0.5, 0.8, 1),
           coords = "absolute")
```

### Parameters

| param | type | default | description |
|---|---|---|---|
| `x` | TrajSet | — | Trajectories normalised to unit circle |
| `target_angle` | numeric | — | Radians. Centre of the target quadrant (Q1). Must match the angle convention already on `x` |
| `target_radius` | numeric | `1` | Accepted for API symmetry with `count_goal_entries()` but not used in zone assignment; quadrant splitting uses `target_angle` only |
| `ring_breaks` | numeric vector | `c(0, 0.5, 0.8, 1)` | Breakpoints for annular rings. Must start at `0`; last value should be ≥ 1. Produces `length(ring_breaks) - 1` rings |
| `coords` | character | `"absolute"` | `"absolute"` uses `@cols$x`/`@cols$y`; `"relative"` uses `@cols$rel_x`/`@cols$rel_y` |

### Zone assignment (per observation)

**Ring:**
```r
r <- sqrt(x^2 + y^2)
ring <- findInterval(r, ring_breaks)   # NA when r > max(ring_breaks)
```

**Quadrant** — Q1 is always the ±45° wedge centred on `target_angle`:
```r
rel_angle <- (atan2(y, x) - target_angle + 2 * pi) %% (2 * pi)
quadrant  <- floor((rel_angle + pi / 4) %% (2 * pi) / (pi / 2)) + 1L
```
Quadrant numbering: 1 = target, 2 = 90° CCW from target, 3 = opposite, 4 = 90° CW from target.

Points with `r > max(ring_breaks)` receive `ring = NA` and are excluded from the summary (rare after arena normalisation).

### Output

One row per `(id × quadrant × ring)` combination present in the data. Combinations with zero observations are **not** emitted (use `tidyr::complete()` downstream if needed).

| column | type | description |
|---|---|---|
| `id` | character | Trial identifier (from `@cols$id`) |
| `quadrant` | integer 1–4 | 1 = target quadrant |
| `ring` | integer 1–N | 1 = innermost ring |
| `zone` | character | Composite label, e.g. `"Q1.R3"` |
| `n_frames` | integer | Observations in this zone for this trial |
| `proportion` | numeric | `n_frames / sum(n_frames for this id)` (excludes NA-ring rows from denominator) |

### Example (water maze)

```r
# Platform at 45° (NE), 3 rings: inner / middle / thigmotaxis
dwell <- zone_dwell(ts, target_angle = pi / 4,
                    ring_breaks = c(0, 0.5, 0.8, 1))
# Q1 proportion > 0.25 indicates above-chance target preference
```

---

## Function 2: `count_goal_entries()`

### Signature

```r
count_goal_entries(x, target_angle, target_radius = 1,
                   crossing_radius = 0.15,
                   coords = "absolute")
```

### Parameters

| param | type | default | description |
|---|---|---|---|
| `x` | TrajSet | — | Trajectories normalised to unit circle |
| `target_angle` | numeric | — | Radians. Direction of the goal from the arena centre |
| `target_radius` | numeric | `1` | Distance of the goal from the centre. Together with `target_angle` gives `(gx, gy)` |
| `crossing_radius` | numeric | `0.15` | Radius of the goal zone. Default ≈ 15% of arena radius (e.g. a 10 cm platform in a 60 cm pool) |
| `coords` | character | `"absolute"` | Same as `zone_dwell()` |

### Goal location

```r
gx <- target_radius * cos(target_angle)
gy <- target_radius * sin(target_angle)
```

### Entry detection (per trial, ordered by time)

```r
d      <- sqrt((x - gx)^2 + (y - gy)^2)
inside <- d < crossing_radius
n_entries <- sum(diff(c(FALSE, inside)) == 1L)
```

Prepending `FALSE` ensures an animal starting inside the zone on frame 1 counts as one entry rather than being silently ignored.

### Output

One row per trial:

| column | type | description |
|---|---|---|
| `id` | character | Trial identifier |
| `n_entries` | integer | Number of entries into the goal zone |

### Example (water maze)

```r
# Former platform at 45° (NE), at the wall (radius = 1)
entries <- count_goal_entries(ts, target_angle = pi / 4,
                              crossing_radius = 0.15)
# n_entries > 1 in probe trial indicates spatial memory of platform location
```

---

## File Placement

| artifact | location |
|---|---|
| Implementation | `R/circular_statistics.R` (append after `circ_summary_headings`) |
| Tests | `tests/testthat/test-circular_statistics.R` |
| Documentation | Roxygen2 `@` tags inline; `man/` regenerated by `devtools::document()` |

---

## Testing Plan

### `zone_dwell()`
- Synthetic TrajSet: points placed in known quadrants and rings → assert output proportions match expected
- All-in-one-zone trial: proportion = 1.0 for that zone
- Points outside `ring_breaks` range → excluded (ring = NA), denominator unaffected
- `coords = "relative"` uses `rel_x`/`rel_y` columns

### `count_goal_entries()`
- Path that enters zone exactly once → `n_entries = 1`
- Path that enters, exits, re-enters → `n_entries = 2`
- Animal starting inside zone → counts as 1 entry
- Path that never enters zone → `n_entries = 0`
- `crossing_radius = 0` → `n_entries = 0` for all trials

---

## Out of Scope

- Duration in seconds (requires frame rate; frame counts and proportions are sufficient)
- Custom non-quadrant angular zones (e.g. 6 sectors)
- Plotting helpers for zone overlays (can be added later)
- Entry/exit timestamps (return only count, not timing)
