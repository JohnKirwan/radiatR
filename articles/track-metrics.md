# Quantifying path shape

``` r

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all("..", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else if (requireNamespace("radiatR", quietly = TRUE)) {
  library(radiatR)
} else {
  stop("Package 'radiatR' not installed and 'pkgload' not available.")
}
```

## Overview

Alongside the circular *heading* statistics (see
[`vignette("circular-statistics")`](https://johnkirwan.github.io/radiatR/articles/circular-statistics.md)),
radiatR provides **spatial** metrics that describe the geometry of each
trajectory: how far it travelled, how straight it was, and how tortuous.
These are per-track scalars, returned as tidy data frames keyed by the
trajectory id, computed on the recorded `x`/`y` coordinates.

There are two complementary notions of tortuosity:

- **Displacement-based** —
  [`straightness_index()`](https://johnkirwan.github.io/radiatR/reference/straightness_index.md)
  /
  [`tortuosity_ratio()`](https://johnkirwan.github.io/radiatR/reference/tortuosity_ratio.md)
  — compare the net start-to-end displacement with the total distance
  walked. They are simple and scale-invariant, but rely on net
  displacement, which is unreliable for convoluted or random-search
  paths that end up near where they started.
- **Turning-based** —
  [`sinuosity()`](https://johnkirwan.github.io/radiatR/reference/sinuosity.md)
  (Benhamou 2004) — is built from the turning angles and step lengths,
  so it stays meaningful when net displacement is small.

The per-observation **kinematics** (instantaneous speed, velocity,
turning rate,
[`plot_profile()`](https://johnkirwan.github.io/radiatR/reference/plot_profile.md))
live in the main vignette — see the kinematics section of
[`vignette("radiatR")`](https://johnkirwan.github.io/radiatR/articles/radiatR.md).

We use the bundled millipede dataset throughout.

``` r

data(cpunctatus)
cpunctatus
#> Tracks: 235 trajectories, 44331 observations
#> Columns: id='trial_id', time='frame', angle='rel_theta' (radians), x='trans_x', y='trans_y', rel_x='rel_x', rel_y='rel_y'
#> Transform steps: unit_circle_mapping 
#> # A tibble: 6 × 18
#>   trial_id frame     x     y trans_x  trans_y trans_rho abs_theta rel_theta
#>   <chr>    <int> <dbl> <dbl>   <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 10_1_1       1  456.  372. 0.00511  0         0.00511     0         6.21 
#> 2 10_1_1       2  458.  370. 0.0128   0.00770   0.0149      0.541     0.464
#> 3 10_1_1       3  456.  370. 0.00511  0.00770   0.00924     0.985     0.908
#> 4 10_1_1       4  454.  370. 0        0.00770   0.00770     1.57      1.49 
#> 5 10_1_1       5  456.  377. 0.00511 -0.0153    0.0162      5.03      4.96 
#> 6 10_1_1       6  459.  382. 0.0154  -0.0307    0.0343      5.18      5.10 
#> # ℹ 9 more variables: rel_x <dbl>, rel_y <dbl>, video <chr>, order <chr>,
#> #   vid_ord <chr>, radius <dbl>, arc <ord>, type <chr>, individual <chr>
```

## Path length

[`track_length()`](https://johnkirwan.github.io/radiatR/reference/track_length.md)
is the total distance walked along each trajectory. With the default
unit-arena coordinates it is in arena radii; attach a calibration with
[`set_distance_scale()`](https://johnkirwan.github.io/radiatR/reference/distance_scale.md)
(or
[`calibrate_distance()`](https://johnkirwan.github.io/radiatR/reference/distance_scale.md))
and it is reported in physical units.

``` r

head(track_length(cpunctatus))                         # arena radii
#>         trial_id    length
#> 10_1_1    10_1_1 1.3645581
#> 10_10_1  10_10_1 8.7387738
#> 10_11_1  10_11_1 1.2783589
#> 10_12_1  10_12_1 1.2376997
#> 10_13_1  10_13_1 1.5926105
#> 10_14_1  10_14_1 0.9365288
head(track_length(set_distance_scale(cpunctatus, 50, unit = "mm")))   # mm
#>         trial_id    length
#> 10_1_1    10_1_1  68.22790
#> 10_10_1  10_10_1 436.93869
#> 10_11_1  10_11_1  63.91794
#> 10_12_1  10_12_1  61.88499
#> 10_13_1  10_13_1  79.63053
#> 10_14_1  10_14_1  46.82644
```

The single-path helpers `path_*` operate on plain `x`/`y` vectors if you
want a metric for one trajectory outside a `Tracks`.

## Straightness and tortuosity

The straightness index is net displacement / total length, in `[0, 1]`
(1 = perfectly straight). The tortuosity ratio is its reciprocal, in
`[1, Inf)`.

``` r

si <- straightness_index(cpunctatus)
tr <- tortuosity_ratio(cpunctatus)
head(si)
#>         trial_id straightness
#> 10_1_1    10_1_1   0.71927750
#> 10_10_1  10_10_1   0.09897137
#> 10_11_1  10_11_1   0.79943750
#> 10_12_1  10_12_1   0.82488090
#> 10_13_1  10_13_1   0.63885806
#> 10_14_1  10_14_1   0.93581201
# single-path versions on bare coordinates
path_straightness(x = c(0, 1, 2), y = c(0, 0, 0))      # straight -> 1
#> [1] 1
path_tortuosity(x = c(0, 0, 1), y = c(0, 1, 1))        # L-shaped  -> sqrt(2)
#> [1] 1.414214
```

## Sinuosity

[`sinuosity()`](https://johnkirwan.github.io/radiatR/reference/sinuosity.md)
implements the Benhamou (2004) index
`S = 2 / sqrt(p (1 + c) / (1 - c) + b^2)` (`p` mean step length, `c`
mean cosine of turning angles, `b` the CV of step length). A straight
path gives `0`; more winding paths give larger values. Unlike the
straightness index it is **not** scale-invariant — it has units of
`1 / sqrt(length)`, so for the most reliable comparison across
trajectories the path should be sampled at a common step length.

``` r

sn <- sinuosity(cpunctatus)
head(sn)
#>         trial_id sinuosity
#> 10_1_1    10_1_1  3.995306
#> 10_10_1  10_10_1  2.329886
#> 10_11_1  10_11_1  3.771568
#> 10_12_1  10_12_1  3.248539
#> 10_13_1  10_13_1  3.249218
#> 10_14_1  10_14_1  1.687342
```

The three tortuosity measures can be read side by side. Here we join
them per track and average within each stimulus condition (`arc` =
target half-width in degrees, `0` = featureless control):

``` r

metrics <- Reduce(
  function(a, b) merge(a, b, by = "trial_id"),
  list(straightness_index(cpunctatus), tortuosity_ratio(cpunctatus),
       sinuosity(cpunctatus))
)
arc <- unique(as.data.frame(cpunctatus)[, c("trial_id", "arc")])
metrics <- merge(metrics, arc, by = "trial_id")

aggregate(cbind(straightness, tortuosity, sinuosity) ~ arc,
          data = metrics, FUN = function(v) round(mean(v, na.rm = TRUE), 2))
#>   arc straightness tortuosity sinuosity
#> 1   0         0.68       2.66      2.35
#> 2   5         0.52       2.51      2.16
#> 3  10         0.57       3.36      2.46
#> 4  15         0.59       2.02      1.74
#> 5  20         0.65       1.64      1.66
#> 6  30         0.72       1.67      2.13
#> 7  40         0.75       1.39      1.95
#> 8  50         0.68       1.64      2.02
```

## See also

The Shiny companion app surfaces these per-track metrics interactively
(a caption of mean straightness / sinuosity and a downloadable metrics
table) — launch it with
[`launch_app()`](https://johnkirwan.github.io/radiatR/reference/launch_app.md).
The per-observation kinematics
([`instantaneous_speed()`](https://johnkirwan.github.io/radiatR/reference/instantaneous_speed.md),
[`velocity_vector()`](https://johnkirwan.github.io/radiatR/reference/velocity_vector.md),
[`angular_velocity()`](https://johnkirwan.github.io/radiatR/reference/angular_velocity.md),
[`plot_profile()`](https://johnkirwan.github.io/radiatR/reference/plot_profile.md))
are covered in
[`vignette("radiatR")`](https://johnkirwan.github.io/radiatR/articles/radiatR.md).
