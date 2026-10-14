# Tracks container for circular trajectories

Tracks container for circular trajectories

Construct a Tracks

## Usage

``` r
tracks(
  df,
  id = "id",
  time = "time",
  angle = NULL,
  x = NULL,
  y = NULL,
  rel_x = NULL,
  rel_y = NULL,
  angle_unit = NULL,
  weight = NULL,
  normalize_xy = FALSE,
  origin = NULL,
  radius = NULL,
  meta = list(),
  transform_history = NULL
)

# S4 method for class 'Tracks'
length(x)

# S4 method for class 'Tracks,ANY,missing,missing'
x[i]

# S4 method for class 'Tracks'
c(x, ..., recursive = FALSE)
```

## Arguments

- df:

  data.frame in long form

- id, time:

  Columns naming trajectory id and time

- angle:

  Optional angle column (radians or degrees, see angle_unit)

- x, y:

  Optional cartesian columns; if provided, converted to unit circle and
  angle inferred

- rel_x, rel_y:

  Optional column names for pre-transformed relative coordinates
  (centred on the origin, unit-circle scaled). Both must be supplied
  together or not at all.

- angle_unit:

  Units of provided angle ("radians" or "degrees"); stored internally as
  radians

- weight:

  Optional weight column name; weights must be finite and `>= 0`

- normalize_xy:

  If FALSE (default), (x,y) are kept as supplied. If TRUE, each
  trajectory is independently centred on its own bounding-box midpoint
  and scaled so its furthest point sits at radius 1. This is a
  shape-preserving transform ONLY: because each trajectory is translated
  by a different amount, it does NOT preserve bearing relative to a
  fixed arena origin, and must not be used as a substitute for metric
  calibration. Raw coordinates are retained in
  \`\<x\>\_raw\`/\`\<y\>\_raw\` when enabled. Prefer pre-calibrated
  input, or the landmark-relative frame (\`rel_x\`/\`rel_y\`, see
  \`coords = "relative"\` in \[derive_headings()\]) when a per-trial
  reference direction is available.

- origin:

  Optional length-2 numeric \`c(x, y)\` giving a fixed unit-circle
  centre in the raw coordinate units. Supplied together with \`radius\`,
  it calibrates every trajectory uniformly via \`(xy - origin) /
  radius\`, a similarity that preserves bearings measured from the
  origin (unlike \`normalize_xy\`, which centres each trajectory on its
  own bounding box). Mutually exclusive with \`normalize_xy = TRUE\`.
  Raw coordinates are retained in \`\<x\>\_raw\`/\`\<y\>\_raw\`. For
  example, in an experimental arena \`origin\` is the arena centre and
  \`radius\` its edge distance in pixels.

- radius:

  Optional positive finite scalar: the raw distance mapping to \`rho =
  1\`. Required together with \`origin\`.

- meta:

  Free-form list of metadata

- transform_history:

  Optional tibble describing transformation steps applied to the
  trajectories. Must contain columns \`step\`, \`order\`, \`id\`,
  \`implementation\`, \`params\`, and \`depends_on\`.

- i:

  Trajectory identifiers (character ids, numeric indices, or logical
  vector)

- ...:

  Additional Tracks objects to append. All inputs must share identical
  column mapping and angle unit, have no colliding trajectory ids, and
  agree on
  frame_rate/time_unit/distance_scale/distance_unit/normalize_xy
  metadata (including all-unset); mismatches error rather than silently
  picking one input's values. Per-id metadata (e.g. \`reference\`) and
  the attrition records of the loads are merged across all inputs. A
  subset carries the whole record of its load, so recombining subsets of
  one load repeats that record.

- recursive:

  Ignored; maintained for signature compatibility

## Value

A Tracks S4 object

## Slots

- `data`:

  data.frame of trajectory observations in long form

- `cols`:

  list mapping required column names (id/time/angle/optional x,y,weight)

- `angle_unit`:

  character describing the original angle unit supplied

- `meta`:

  list of additional metadata attached to the set

## Invariants

Every `Tracks` satisfies the following. They are checked when the object
is built and whenever
[`methods::validObject()`](https://rdrr.io/r/methods/validObject.html)
runs, which the package's own transforms do (a direct slot write is not
checked until then):

- ids are never missing, and the rows of each id are contiguous;

- time is finite and strictly increasing within each id, so no time is
  repeated within a trajectory;

- angles are finite and lie in `[0, 2*pi)`;

- coordinates are finite: `x`/`y`, and the relative and raw pairs where
  those columns are stored;

- weights, when a weight column is named, are finite and `>= 0` (a zero
  weight excludes an observation).

A `Tracks` therefore never holds a missing detection: remove those
before construction (see `on_invalid` in
[`read_tracks`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)).

`tracks()` reports violations as an error of class
`radiatR_invalid_tracks`. Its `problems` field is a data frame with one
row per rule, column and row: `rule`, `column`, `row` (the row of the
data frame supplied) and `id`; `rows` and `ids` give the union. When
called through
[`read_tracks`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md),
rows are those of the combined table passed to `tracks()` (after any
rows removed by `on_invalid = "drop"`), not line numbers in a file.

## Examples

``` r
df <- data.frame(
  id   = rep(c("a", "b"), each = 3),
  time = rep(1:3, 2),
  x    = c(0, 0.3, 0.6, 0, -0.2, -0.5),
  y    = c(0, 0.2, 0.5, 0, 0.3, 0.7)
)
ts <- tracks(df, x = "x", y = "y")
ts
#> Tracks: 2 trajectories, 6 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y'
#>   id time    x   y     angle
#> 1  a    1  0.0 0.0 0.0000000
#> 2  a    2  0.3 0.2 0.5880026
#> 3  a    3  0.6 0.5 0.6947383
#> 4  b    1  0.0 0.0 0.0000000
#> 5  b    2 -0.2 0.3 2.1587989
#> 6  b    3 -0.5 0.7 2.1910458
length(ts)
#> [1] 2
ts["b"]
#> Tracks: 1 trajectories, 3 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y'
#>   id time    x   y    angle
#> 4  b    1  0.0 0.0 0.000000
#> 5  b    2 -0.2 0.3 2.158799
#> 6  b    3 -0.5 0.7 2.191046

# combine objects with the same column mapping
more <- tracks(data.frame(id = "c", time = 1:2, x = c(0, 0.4), y = c(0, -0.4)),
               x = "x", y = "y")
length(c(ts, more))
#> [1] 3

# pixel coordinates: arena centre (320, 240), edge 200 px from it
px <- data.frame(id = "d", time = 1:3,
                 x = c(320, 380, 450), y = c(240, 280, 330))
tracks(px, x = "x", y = "y", origin = c(320, 240), radius = 200)
#> Tracks: 1 trajectories, 3 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y', raw_x='x_raw', raw_y='y_raw'
#> Transform steps: calibrate_unit_circle 
#>   id time    x    y x_raw y_raw     angle
#> 1  d    1 0.00 0.00   320   240 0.0000000
#> 2  d    2 0.30 0.20   380   280 0.5880026
#> 3  d    3 0.65 0.45   450   330 0.6055447
```
