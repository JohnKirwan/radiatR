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

  Optional weight column name

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
  agree on frame_rate/distance_scale/distance_unit/normalize_xy metadata
  (including all-unset); mismatches error rather than silently picking
  one input's values. Per-id metadata (e.g. \`reference\`) is merged
  across all inputs.

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
