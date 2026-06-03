# Camera calibration model for trajectory correction

Camera calibration model for trajectory correction

## Slots

- `K`:

  3x3 intrinsic calibration matrix

- `k`:

  Numeric vector or list of distortion coefficients (radial first,
  optional tangential)

- `F`:

  Numeric scalar or length-2 vector giving focal length scaling (e.g.,
  mm per pixel)

## See also

[`calibrate_positions`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md)

Other calibration:
[`cal_model()`](https://johnkirwan.github.io/radiatR/reference/cal_model.md),
[`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md),
[`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md)
