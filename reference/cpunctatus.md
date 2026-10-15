# \*Cylindroiulus punctatus\* visual orientation trajectory dataset

Pre-processed \`Tracks\` of 235 millipede trajectories from a
visual-acuity experiment. \*Cylindroiulus punctatus\* individuals were
placed at the centre of a cylindrical arena under bright, downwelling
light, with a dark target of varying angular half-width on the arena
wall, and their path tracked to test whether they oriented toward the
target (object taxis). Eight stimulus conditions are represented: target
half-widths of 5, 10, 15, 20, 30, 40, and 50 degrees, plus a featureless
control (\`arc = 0\`, an angular subtense of zero).

## Usage

``` r
cpunctatus
```

## Format

A \[\`Tracks\`\] object (44,331 observations) whose data columns
include:

- trial_id:

  Character. Unique trial identifier.

- frame:

  Integer. Video frame number.

- trans_x, trans_y:

  Numeric. Unit-circle Cartesian coordinates.

- abs_theta:

  Numeric. Absolute bearing (radians).

- rel_theta:

  Numeric. Bearing relative to the target direction.

- rel_x, rel_y:

  Numeric. Target-relative unit-circle coordinates.

- arc:

  Ordered factor. Target half-width in degrees (\`0\` \< \`5\` \< \`10\`
  \< \`15\` \< \`20\` \< \`30\` \< \`40\` \< \`50\`; 0 = control).

- type:

  Character. \`"control"\` or \`"stimulus"\`.

- individual:

  Character. Animal identifier.

## Source

Behavioural experiment from Kirwan & Nilsson (2019); raw tracks produced
with dtrack and normalised with radiatR. A subset of the published
dataset.

## Details

These tracks are a \*\*subset\*\* of the full experiment; see Kirwan &
Nilsson (2019) for the complete dataset and trial counts.

Coordinates are normalised to the unit circle (radius = 1) and rotated
so the target lies in a common reference direction; \`rel_theta\`,
\`rel_x\`, and \`rel_y\` give the target-relative heading and position.
The subject identifier is retained in the \`individual\` column.

The raw dtrack landmark/track text files for every trial are shipped as
one archive, \`inst/extdata/tracks.tar.gz\` (unpack it with
\`untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
exdir = tempdir())\`), and the trial manifest in
\`inst/extdata/millipede_trials.csv\`, so the full import pipeline can
be reproduced with \[get_all_object_pos()\]. See
\`data-raw/millipede_example.R\`.

## References

Kirwan, J. D., & Nilsson, D.-E. (2019). A millipede compound eye
mediating low-resolution vision. \*Vision Research\*, 165, 36–44.
[doi:10.1016/j.visres.2019.09.003](https://doi.org/10.1016/j.visres.2019.09.003)

## Examples

``` r
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
# One heading per trial (the bearing of its furthest point from the
# origin), keeping the target half-width
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc")
table(hd$arc)
#> 
#>  0  5 10 15 20 30 40 50 
#> 35 31 29 30 27 37 20 26 
```
