# Calibrate TrajSet positions using a camera model

Applies camera intrinsics and distortion removal to convert pixel
coordinates to metric space.

## Usage

``` r
calibrate_positions(x, model)

# S4 method for class 'TrajSet,CalModel'
calibrate_positions(x, model)
```

## Arguments

- x:

  TrajSet with \`x\`/\`y\` coordinates

- model:

  \`CalModel\` object containing calibration parameters

## Value

TrajSet with updated \`x\`/\`y\` and angles in metric space

## See also

Other calibration:
[`CalModel-class`](https://johnkirwan.github.io/radiatR/reference/CalModel-class.md)
