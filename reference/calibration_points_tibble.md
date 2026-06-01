# Convert calibration point matrices to a tibble

Convert calibration point matrices to a tibble

## Usage

``` r
calibration_points_tibble(image_points)
```

## Arguments

- image_points:

  List of 2-column matrices containing image-space corner coordinates
  for each frame.

## Value

A tibble with columns frame, corner, x, and y.
