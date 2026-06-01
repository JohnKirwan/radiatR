# \*Paracentrotus lividus\* visual acuity trajectory dataset

Pre-processed \`TrajSet\` containing 401 baseline trajectories from a
\*Paracentrotus lividus\* (purple sea urchin) visual acuity experiment.
Urchins were placed at the centre of a ~2 m circular arena and allowed
to walk freely toward a patterned-wall stimulus. The experiment tested
six arc-angle conditions (0°, 15°, 30°, 45°, 60°, and 150°) with a
marble obstacle present; the 150° condition also includes 105 trials
without an obstacle.

## Usage

``` r
plividus
```

## Format

A \[\`TrajSet\`\] object with 134 375 rows and the following data
columns:

- trial_id:

  Character. Unique trial identifier (folder/filename).

- frame:

  Numeric. Video frame number.

- trans_x, trans_y:

  Numeric. Unit-circle Cartesian coordinates.

- abs_theta:

  Numeric. Absolute bearing (radians, −π to π).

- rel_theta:

  Numeric. Step bearing relative to arena orientation.

- rel_x, rel_y:

  Numeric. Stimulus-relative unit-circle Cartesian coordinates.
  Registered in \`@cols\` so \`derive_headings(..., coords =
  "relative")\` works directly.

- arc:

  Ordered factor. Stimulus arc angle in degrees (\`0°\` \< \`15°\` \<
  \`30°\` \< \`45°\` \< \`60°\` \< \`150°\`).

## Source

Experiment conducted at the University of Exeter. Raw tracking files
produced with a custom video-tracking workflow; coordinates normalised
with radiatR.

## Details

Coordinates are normalised to the unit circle (arena radius = 1). An
\`arc\` column (ordered factor) encodes the stimulus arc angle in
degrees.

The corresponding raw landmark and track text files for the five arc =
0° baseline trials are shipped in \`inst/extdata/tracks/\` to
demonstrate the full import pipeline. The full raw dataset is held in
the source project and was processed with \[get_all_object_pos()\].
