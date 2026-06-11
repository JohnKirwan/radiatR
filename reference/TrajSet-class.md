# TrajSet container for circular trajectories

TrajSet container for circular trajectories

Construct a TrajSet

## Usage

``` r
TrajSet(
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
  normalize_xy = TRUE,
  meta = list(),
  transform_history = NULL
)

# S4 method for class 'TrajSet'
length(x)

# S4 method for class 'TrajSet,ANY,missing,missing'
x[i]

# S4 method for class 'TrajSet'
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
  (arena-centred, unit-circle scaled). Both must be supplied together or
  not at all.

- angle_unit:

  Units of provided angle ("radians" or "degrees"); stored internally as
  radians

- weight:

  Optional weight column name

- normalize_xy:

  If TRUE (default), (x,y) are arena-scaled per trajectory: each
  trajectory is centred on its bounding-box midpoint and scaled so its
  furthest point sits at radius 1. This preserves trajectory shape and
  places the arena centre at the origin (what the radius-based heading
  rules expect). Raw coordinates are retained in
  \`\<x\>\_raw\`/\`\<y\>\_raw\`. If FALSE, (x,y) are kept as supplied.
  (Landmark-based mapping, when available, is more accurate; this is the
  no-landmark fallback.)

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

  Additional TrajSet objects to append

- recursive:

  Ignored; maintained for signature compatibility

## Value

A TrajSet S4 object

## Slots

- `data`:

  data.frame of trajectory observations in long form

- `cols`:

  list mapping required column names (id/time/angle/optional x,y,weight)

- `angle_unit`:

  character describing the original angle unit supplied

- `meta`:

  list of additional metadata attached to the set
