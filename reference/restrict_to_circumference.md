# Restrict a Tracks to within the unit circle

Removes out-of-circumference (\`rho \> 1\`) track data so that
downstream kinematics (\`track_speed()\`, \`instantaneous_speed()\`,
\`track_length()\`, the \`path\_\*\` shape metrics, ...) are computed
only on the within-circle portion of each trajectory. Distance from the
centre, \`rho\`, is taken from the relative (unit-circle) coordinates
\`rel_x\`/\`rel_y\`, where \`rho = 1\` is the circumference.

## Usage

``` r
restrict_to_circumference(ts, mode = c("truncate", "drop"), max_radius = 1)
```

## Arguments

- ts:

  A \`Tracks\` carrying relative coordinates (\`rel_x\`/\`rel_y\`).

- mode:

  How to exclude out-of-circumference data per trajectory.
  \`"truncate"\` (default) keeps the contiguous prefix up to (excluding)
  the first point beyond the circumference – the centre-to-circumference
  approach segment, with no gaps. \`"drop"\` removes individual
  beyond-circumference points only; note that dropping interior points
  lengthens the step bridging the gap, which biases speed and path
  length, so \`"truncate"\` is usually preferable for kinematics.

- max_radius:

  Circumference radius. A point is beyond the circumference when \`rho
  \> max_radius\` (with a small rim tolerance so points exactly on the
  circumference are kept). Default \`1\`.

## Value

A new \`Tracks\` (the input is unmodified) with out-of-circumference
rows removed and a \`transform_history\` entry recording the operation.
Trajectories reduced to no rows are dropped.

## Details

This is the data-filtering counterpart to the plot-only
\`radiate(clip_tracks = TRUE)\`: \`clip_tracks\` only changes what is
\*drawn\* (on a copy), whereas \`restrict_to_circumference()\` returns a
new \`Tracks\` whose data the metrics actually see.

## See also

\[radiate()\] (\`clip_tracks\`), \[track_length()\], \[track_speed()\],
\[transform_history()\]
