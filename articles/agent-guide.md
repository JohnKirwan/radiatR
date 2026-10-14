# Guide for AI agents

This guide is written for an AI agent asked to analyse directional data
with radiatR, and for the person checking its work. It states the
conventions the package uses, how it fails, and the mistakes that are
easy to make. Every code chunk below is run when the package is built.

## Two ways in

Start from **trajectories** when you have tracking output (x/y positions
over time) and need one heading per track, or from a **table of angles**
when the headings have already been measured.

``` r

sim <- as.data.frame(simulate_tracks(seed = 1, n_points = 50))
path <- tempfile(fileext = ".csv")
write.csv(sim, path, row.names = FALSE)

ts <- read_tracks(
  path,
  mapping   = list(id = "trial_id", time = "frame", x = "abs_x", y = "abs_y"),
  time_type = "frames",
  fps       = 30,
  keep      = "condition"
)
hd <- derive_headings(ts, rule = "distal", carry = "condition")
head(hd, 3)
#> # A tibble: 3 × 4
#>   id           time heading condition
#>   <chr>       <dbl>   <dbl> <chr>    
#> 1 control_001    50    5.90 control  
#> 2 control_002    50    5.80 control  
#> 3 control_003    49    5.84 control
```

``` r

bearings <- data.frame(bearing = c(10, 20, 350, 5), site = c("a", "a", "b", "b"))
hf <- headings_frame(bearings, col = "bearing", units = "degrees",
                     angle_convention = "clock")
hf
#> # A tibble: 4 × 2
#>   bearing site 
#>     <dbl> <chr>
#> 1    1.40 a    
#> 2    1.22 a    
#> 3    1.75 b    
#> 4    1.48 b
```

## Angle conventions

- Internally every angle is in **radians** in the **unit-circle
  convention**: 0 points East and angles increase counter-clockwise.
  Headings derived from tracks lie in \[0, 2\*pi).
- [`headings_frame()`](https://johnkirwan.github.io/radiatR/reference/headings_frame.md)
  converts on the way in. `units` has no default.
  `angle_convention = "clock"` means 0 = North, increasing clockwise
  (compass bearings); `"unit_circle"` means the data are already 0 =
  East, counter-clockwise. In the example above a bearing of 10 degrees
  is stored as 1.396 radians.
- [`headings_frame()`](https://johnkirwan.github.io/radiatR/reference/headings_frame.md)
  does not wrap values supplied in the unit-circle convention: 370
  degrees is stored as 6.46 radians and -20 degrees as -0.35. Wrap them
  yourself if a later step needs \[0, 2\*pi).
- Summary tables carry two columns. `mean_dir` is in radians,
  unit-circle convention. `mean_dir_deg` is in the **display
  convention** set by
  [`circ_display()`](https://johnkirwan.github.io/radiatR/reference/circ_display.md),
  which by default is 0 = North, clockwise. It is therefore not
  `mean_dir * 180 / pi`.

``` r

circ_summarise(hd, col = "heading", units = "radians", .by = "condition")
#> # A tibble: 3 × 6
#>   condition     n mean_dir mean_dir_deg resultant_R kappa
#>   <chr>     <int>    <dbl>        <dbl>       <dbl> <dbl>
#> 1 control      20   0.0125         89.3       0.929  7.31
#> 2 high         20   5.66          126.        0.670  1.83
#> 3 moderate     20   0.687          50.6       0.928  7.20
```

State the convention whenever you report a direction.

## Coordinates and calibration

Heading rules that use a radius (`crossing`, `exit`, `entry`,
`ring_tangent`, `distal` with `max_radius`) read radii in the units of
the coordinates the `Tracks` object holds. Decide how those coordinates
relate to the arena before choosing a rule.

- `normalize_xy = FALSE` (the default) keeps x/y as supplied.
- `origin = c(x, y)` with `radius` is **calibration**: every trajectory
  is mapped with `(xy - origin) / radius`, so the arena edge is at
  radius 1 and bearings from the arena centre are preserved. Supply both
  together.
- `normalize_xy = TRUE` rescales each trajectory on its own bounding
  box. It preserves shape only and does **not** preserve bearing from a
  fixed arena centre. It is not calibration and cannot be combined with
  `origin`/`radius`.

## Map every column yourself

- Without a dialect,
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  keeps only the mapped columns and those named in `keep` (a dialect
  keeps the columns it produces). Name every grouping column you will
  need later in `keep`, then bring it onto the headings with
  `derive_headings(carry = )`. A group comparison on a column that was
  not kept fails with “column … not found”.
- A name in `keep` or `carry` that is not in the data is dropped without
  a message. Check the result has the columns you asked for.
- A `carry` name that is already a column of the result (`id`, `time`,
  `heading`) is an error: the carried values would overwrite the derived
  ones.
- A role missing from `mapping` is guessed from the column names. An
  unmapped column called `theta`, `angle`, `phi`, `bearing`, `deg` or
  `degrees` becomes the angle, read in radians unless `angle_unit` says
  otherwise; one called `w`, `weight` or `weights` becomes the weight.
  Map `id`, `time`, `x` and `y` explicitly, and check
  [`guess_columns()`](https://johnkirwan.github.io/radiatR/reference/guess_columns.md)
  on the table before loading.

## Time

- `time_type` says how the time column is expressed: `"frames"`,
  `"seconds"` or `"posix"`. Prefer stating it to relying on `"auto"`.
- `time_type = "frames"` requires `fps`. The frame rate is stored on the
  `Tracks` object and used by
  [`elapsed_seconds()`](https://johnkirwan.github.io/radiatR/reference/elapsed_seconds.md)
  and the kinematics functions.
- radiatR never assumes a frame rate. Supply `fps`, or set it afterwards
  with
  [`set_frame_rate()`](https://johnkirwan.github.io/radiatR/reference/frame_rate.md).

## Failures are information

radiatR stops rather than guessing. Read the error; do not work around
it.

| Condition class | Meaning |
|----|----|
| `radiatR_invalid_rows` | x/y missing or non-finite; the error lists rows and ids |
| `radiatR_malformed_values` | a column holds values that are not numeric; lists rows and values |
| `radiatR_invalid_tracks` | the data break a `Tracks` invariant (see [`?tracks`](https://johnkirwan.github.io/radiatR/reference/Tracks-class.md)) |
| `radiatR_nonestimable` | a statistic cannot be estimated for a group |
| `radiatR_dropped_rows` | warning raised when `on_invalid = "drop"` removed rows |
| `radiatR_attrition` | message from the `deeplabcut`, `sleap`, `ethovision` and `anymaze` dialects: the points masked at load and the rows left without a position (kept in `@meta$attrition`; not the heading attrition of the next section) |
| `radiatR_dialect_error` | a dialect cannot read the file as that program’s output; the message says why |

- `on_invalid = "error"` is the default. `"drop"` removes rows with
  missing or non-finite x/y and warns; it is a decision about the data,
  so report it.
- When the same id appears in more than one file, the default
  `id_collision = "error"` stops. `"namespace"` prefixes each id with
  its file stem.

## Heading rules and attrition

[`derive_headings()`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md)
returns one row per track (some rules can return more). When a rule
yields no heading for a track, the row is kept with `NA` and the result
carries `n_total` and `n_missing` (headings, i.e. rows), `n_trials`
(distinct trials) and `missing_ids` (each trial with a missing heading,
once) attributes. `on_missing` controls what else happens: `"warn"` (the
default) raises a warning, `"error"` stops, `"quiet"` says nothing. The
`ransac_straight` rule draws random numbers, so call
[`set.seed()`](https://rdrr.io/r/base/Random.html) before it.

``` r

attr(hd, "n_total")
#> [1] 60
attr(hd, "n_missing")
#> [1] 0
```

Attrition is often not random: a track that never reaches a ring has no
`crossing` heading, and those animals may differ from the rest. Report
`n_missing` alongside any statistic, and do not switch rules silently to
recover the missing tracks.

## Tests

``` r

set.seed(1)
test_uniformity(hd, group_col = "condition", angle_col = "heading")
#>   condition statistic      p_value  n     test
#> 1   control 0.9289411 5.069033e-08 20 rayleigh
#> 2      high 0.6697242 4.300494e-05 20 rayleigh
#> 3  moderate 0.9277095 5.207452e-08 20 rayleigh
test_mean_directions(hd, group_col = "condition", angle_col = "heading")
#>   n_groups statistic df1 df2      p_value            test
#> 1        3  22.91549   2  57 4.976226e-08 Watson-Williams
```

- [`test_uniformity()`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
  asks whether headings are uniform around the circle; `test = "vtest"`
  tests against an expected direction `mu`, in radians.
- [`test_mean_directions()`](https://johnkirwan.github.io/radiatR/reference/test_mean_directions.md),
  [`test_concentration()`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md)
  and
  [`test_distributions()`](https://johnkirwan.github.io/radiatR/reference/test_distributions.md)
  compare groups.
- Pass `axial = TRUE` for bidirectional data (an axis, not a direction).
- Monte Carlo and permutation p-values, and the bootstrap functions, use
  R’s random number generator. Call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) first and report
  the seed.

## Pitfalls

1.  Reporting `mean_dir_deg` as if it were counter-clockwise from East.
2.  Treating `normalize_xy = TRUE` as calibration.
3.  Forgetting `keep` / `carry`, or misspelling a name in them, then
    finding the grouping column is gone.
4.  Using `on_invalid = "drop"` without saying how many rows were
    dropped.
5.  Ignoring `n_missing` after
    [`derive_headings()`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md).
6.  Giving radii to a heading rule in the wrong units.
7.  Reporting a resampling p-value without a seed.
8.  Catching an error and carrying on with a different method.
9.  Letting
    [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
    guess an angle or weight column from its name.
