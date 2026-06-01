# Intersection helper for TrajSet trajectories

Extracts the first/last rows for a given id/time range and computes the
intersection with the unit circle.

## Usage

``` r
line_circle_intercept_traj(traj, id, range)
```

## Arguments

- traj:

  TrajSet

- id:

  Identifier of the trajectory

- range:

  Numeric index vector (e.g., rows within a trial)

## Value

Tibble with \`x_int\`/\`y_int\`
