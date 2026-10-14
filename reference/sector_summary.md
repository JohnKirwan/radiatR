# Proportion of time spent in angular sectors

Bins heading angles into sectors and returns count and proportion per
sector, optionally grouped by trial or condition. Useful for dwell-time
analysis of dense per-frame heading series (e.g. gaze direction from a
tethered subject).

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

## Examples

``` r
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc")
sector_summary(hd, sectors = 4)
#>        sector  mid_angle count proportion
#> 1 -135degrees -2.3561945    59  0.2510638
#> 2  -45degrees -0.7853982    54  0.2297872
#> 3   45degrees  0.7853982    59  0.2510638
#> 4  135degrees  2.3561945    63  0.2680851
```
