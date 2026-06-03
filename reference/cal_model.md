# Build a camera calibration model from intrinsic coefficients

Assembles a \[CalModel-class\] from the focal lengths, principal point,
and distortion coefficients reported by a camera-calibration toolbox.
This is the low-level constructor used by \[read_calibration()\]; call
it directly when you already have the numbers in hand.

## Usage

``` r
cal_model(fx, fy, cx, cy, k1 = 0, k2 = 0, p1 = 0, p2 = 0, k3 = 0, F = NULL)
```

## Arguments

- fx, fy:

  Focal lengths in pixels.

- cx, cy:

  Principal point in pixels, in the same coordinate convention as the
  track coordinates the model will be applied to (0-based for most
  tracking tools).

- k1, k2, k3:

  Radial distortion coefficients. Default to \`0\` (no radial
  distortion).

- p1, p2:

  Tangential distortion coefficients. Default to \`0\`.

- F:

  Scale factor controlling the units of the corrected output, passed to
  \[scaled_xy2mm()\]. When \`NULL\` (the default) it is set to \`c(fx,
  fy)\`, which leaves the output in undistorted pixels. Supply the
  physical focal length in millimetres to obtain a metric result, or any
  length-2 vector for custom scaling.

## Value

A \[CalModel-class\] object.

## Details

radiatR stores the intrinsic matrix in MATLAB's \*transposed\*
convention: the focal lengths sit on the diagonal (\`K\[1, 1\]\`,
\`K\[2, 2\]\`) and the principal point occupies the bottom row (\`K\[3,
1:2\]\`). \`cal_model()\` owns this layout so callers never have to
assemble \`K\` by hand. Axis skew is not used by the downstream
correction and is therefore dropped.

The distortion coefficients follow the Brown-Conrady model shared by
MATLAB (\`RadialDistortion\` + \`TangentialDistortion\`) and OpenCV
(\`\[k1, k2, p1, p2, k3\]\`): radial terms \`k1\`, \`k2\`, \`k3\` and
tangential terms \`p1\`, \`p2\`.

## See also

\[read_calibration()\], \[calibrate_positions()\]

Other calibration:
[`CalModel-class`](https://johnkirwan.github.io/radiatR/reference/CalModel-class.md),
[`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md),
[`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md)

## Examples

``` r
model <- cal_model(fx = 800, fy = 800, cx = 640, cy = 360,
                   k1 = -0.21, k2 = 0.04)
cam_cal_pt(100, 100, model@K, model@k, model@F)
#> [1] -624.1998 -300.5406
```
