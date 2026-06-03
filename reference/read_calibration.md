# Import a camera calibration from an external toolbox

Reads the intrinsic and distortion coefficients exported by a camera
calibration toolbox and returns a \[CalModel-class\] ready for
\[calibrate_positions()\]. radiatR does not estimate calibrations
itself; run the calibration in MATLAB, OpenCV, or another system and
import the result here.

## Usage

``` r
read_calibration(
  path,
  source = c("auto", "matlab", "opencv", "csv"),
  F = NULL,
  principal_base = NULL
)
```

## Arguments

- path:

  Path to the calibration file.

- source:

  File format. \`"auto"\` (default) infers it from the extension
  (\`.mat\` -\> MATLAB, \`.json\`/\`.yml\`/\`.yaml\` -\> OpenCV,
  \`.csv\`/\`.txt\` -\> CSV).

- F:

  Scale factor for the corrected output, passed to \[cal_model()\].
  \`NULL\` (default) leaves the output in undistorted pixels.

- principal_base:

  Pixel index of the first pixel in the source's convention, subtracted
  from the principal point. \`NULL\` (default) uses the per-source
  default described above.

## Value

A \[CalModel-class\] object.

## Details

Supported sources:

- \`"matlab"\`:

  A \`.mat\` file from the MATLAB Computer Vision Toolbox. Both the
  pre-R2022b \`IntrinsicMatrix\` (transposed) and the newer \`K\`
  (standard) layouts are recognised, together with \`RadialDistortion\`
  and \`TangentialDistortion\`. Requires the R.matlab package.

- \`"opencv"\`:

  A YAML (\`.yml\`/\`.yaml\`) or JSON (\`.json\`) file written by
  OpenCV's \`FileStorage\`, with a \`camera_matrix\` and
  \`distortion_coefficients\` (order \`k1, k2, p1, p2, k3\`). The
  non-standard \`%YAML:1.0\` header and \`!!opencv-matrix\` tags are
  tolerated. Requires yaml (YAML) or jsonlite (JSON).

- \`"csv"\`:

  A plain CSV, either wide (columns \`fx, fy, cx, cy\` and optional
  \`k1, k2, k3, p1, p2\`) or long (a \`parameter, value\` pair per row).

Toolboxes differ in pixel-index convention: MATLAB reports the principal
point in 1-based pixels, while OpenCV and most tracking tools use
0-based pixels. \`read_calibration()\` subtracts \`principal_base\` from
the imported principal point so it lines up with your track coordinates.
The default is 1 for \`"matlab"\` and 0 for \`"opencv"\`/\`"csv"\`;
override it if your tracker uses a different convention.

## See also

\[cal_model()\], \[calibrate_positions()\]

Other calibration:
[`CalModel-class`](https://johnkirwan.github.io/radiatR/reference/CalModel-class.md),
[`cal_model()`](https://johnkirwan.github.io/radiatR/reference/cal_model.md),
[`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
model <- read_calibration("camera_params.mat")
tracks <- calibrate_positions(tracks, model)
} # }
```
