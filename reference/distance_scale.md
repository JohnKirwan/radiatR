# Distance calibration for a Tracks object

Attach a physical-distance scale (and optional unit label) to a
\[Tracks\] so path lengths and speeds can be reported in real units. The
scale is physical units per unit of the recorded \`x\`/\`y\`
coordinates; it is applied on demand and the stored coordinates are
never altered. radiatR otherwise analyses in normalised (unit-circle)
space – this is an optional calibration hook.

## Usage

``` r
distance_scale(x)

# S4 method for class 'Tracks'
distance_scale(x)

distance_unit(x)

# S4 method for class 'Tracks'
distance_unit(x)

set_distance_scale(x, scale, unit = NULL)

# S4 method for class 'Tracks'
set_distance_scale(x, scale, unit = NULL)

calibrate_distance(ts, coord_distance, real_distance, unit = NULL)
```

## Arguments

- x:

  A \[Tracks\] object.

- scale:

  A single positive number: physical units per coordinate unit.

- unit:

  Optional single string, the physical unit label (e.g. \`"mm"\`).

- ts:

  A \[Tracks\] object (for \`calibrate_distance()\`).

- coord_distance, real_distance:

  For \`calibrate_distance()\`, a known separation in coordinate units
  and its real-world length; the scale is \`real_distance /
  coord_distance\`.

## Value

\`distance_scale()\`/\`distance_unit()\` the stored values (or
\`NULL\`); \`set_distance_scale()\`/\`calibrate_distance()\` the
modified \`Tracks\`.

## See also

\[frame_rate()\], \[track_length()\], \[track_speed()\]
