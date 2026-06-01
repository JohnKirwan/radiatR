# Reconstruct calibration point matrices from a tibble

Reconstruct calibration point matrices from a tibble

## Usage

``` r
calibration_points_from_tibble(tbl)
```

## Arguments

- tbl:

  Data frame with columns \`frame\`, \`x\`, and \`y\` (and optionally
  \`corner\`).

## Value

List of 2-column matrices matching the layout expected by
\[calibration_session()\].
