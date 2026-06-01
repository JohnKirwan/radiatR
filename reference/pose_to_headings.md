# Derive per-frame headings from pose data without a TrajSet

Computes a heading angle for every row in `df` from either two bodypart
keypoint columns or a pre-computed orientation angle column. Intended
for tethered or mostly stationary animals where the position trajectory
is absent or uninformative and body pose is the primary signal, but also
useful for extracting a dense heading time series from trajectory data.
The output is compatible with
[`circ_dispersion`](https://johnkirwan.github.io/radiatR/reference/circ_dispersion.md),
[`sector_summary`](https://johnkirwan.github.io/radiatR/reference/sector_summary.md),
[`add_heading_points`](https://johnkirwan.github.io/radiatR/reference/add_heading_points.md),
and
[`add_angle_rose`](https://johnkirwan.github.io/radiatR/reference/add_angle_rose.md).

## Usage

``` r
pose_to_headings(
  df,
  anterior = NULL,
  posterior = NULL,
  theta_col = NULL,
  id_col = NULL,
  time_col = NULL,
  angle_convention = c("unit_circle", "clock")
)
```

## Arguments

- df:

  Data frame with at least id, time, and keypoint or angle columns.

- anterior:

  Prefix of the anterior bodypart columns (`<anterior>_x` and
  `<anterior>_y` must exist).

- posterior:

  Prefix of the posterior bodypart columns.

- theta_col:

  Name of a pre-computed orientation angle column (alternative to
  `anterior`/`posterior`).

- id_col:

  Column identifying trials or individuals. Auto-detected from common
  names; defaults to a single group `"1"` if absent.

- time_col:

  Column of frame indices or timestamps. Defaults to row position if
  absent.

- angle_convention:

  `"unit_circle"` (default) or `"clock"`.

## Value

Data frame with columns `id`, `time`, `heading` (in radians), with an
`angle_convention` attribute for downstream compatibility.
