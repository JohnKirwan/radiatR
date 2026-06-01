# Proportion of time spent in angular sectors

Bins heading angles into sectors and returns count and proportion per
sector, optionally grouped by trial or condition. Useful for dwell-time
analysis of dense per-frame heading series (e.g. gaze direction from a
tethered animal).

## Usage

``` r
sector_summary(hd, sectors = 8L, group_col = NULL, angle_col = "heading")
```

## Arguments

- hd:

  Data frame containing headings in radians.

- sectors:

  Either a single integer (number of equal sectors spanning the full
  circle, default `8`) or a numeric vector of break points in radians.
  Break points need not include \\\pm\pi\\; they are added
  automatically.

- group_col:

  Column(s) to group by. `NULL` uses all rows.

- angle_col:

  Name of the heading column. Default `"heading"`.

## Value

Data frame with columns `group_col` (if supplied), `sector` (degree
label), `mid_angle` (sector midpoint in radians), `count`, `proportion`.
