# Camera Calibration Workflow

This vignette walks through the end-to-end camera calibration utilities
available in **radiatR**. The goal is to show how to turn raw pixel
coordinates into a metric representation that removes lens distortion
and accounts for camera intrinsics, matching the pinhole model
implemented in packages such as MATLAB’s *Computer Vision Toolbox*.

## Required calibration parameters

The calibration helpers expect three key ingredients:

1.  **Intrinsic matrix (`K`)** – a 3 × 3 matrix in the usual form
    $`\left[\begin{smallmatrix}f_x & 0 & c_x \ 0 & f_y & c_y \ 0 & 0 & 1\end{smallmatrix}\right]`$
    where `c_x` / `c_y` are the principal point in pixels and `f_x` /
    `f_y` are the focal lengths, also in pixels.
2.  **Distortion coefficients (`k`)** – passed as either a numeric
    vector, a named vector (e.g.,
    `c(k1 = ..., k2 = ..., p1 = ..., p2 = ...)`), or a list with
    `radial` / `tangential` entries. Missing coefficients are assumed to
    be zero.
3.  **Metric focal length (`F`)** – the physical focal length (or
    scaling factor) in millimetres. Supply a scalar for isotropic
    scaling or a length-2 vector to differentiate between axes.

You can bundle these into a
[`CalModel`](https://johnkirwan.github.io/radiatR/reference/CalModel-class.md)
object for repeated use with trajectory sets.

``` r

K <- matrix(c(800,   0, 640,
                0, 800, 360,
                0,   0,   1),
            nrow = 3, byrow = TRUE)
distortion <- c(k1 = -0.28, k2 = 0.05, p1 = 1e-3, p2 = -5e-4)
F_mm <- 20

cal_model <- new("CalModel", K = K, k = distortion, F = rep(F_mm, 2))
cal_model
#> An object of class "CalModel"
#> Slot "K":
#>      [,1] [,2] [,3]
#> [1,]  800    0  640
#> [2,]    0  800  360
#> [3,]    0    0    1
#> 
#> Slot "k":
#>      k1      k2      p1      p2 
#> -0.2800  0.0500  0.0010 -0.0005 
#> 
#> Slot "F":
#> [1] 20 20
```

## Checkerboard control points

When calibrating from a checkerboard sequence you often need the world
coordinates for the inner corners. The helper
[`checkerboard_points()`](https://johnkirwan.github.io/radiatR/reference/checkerboard_points.md)
mirrors MATLAB’s `generateCheckerboardPoints` so you can generate them
directly in **radiatR**.

``` r

cb_pts <- checkerboard_points(c(9, 7), square_size = 5)
head(cb_pts)
#> # A tibble: 6 × 5
#>   corner   row   col     x     y
#>    <int> <int> <int> <dbl> <dbl>
#> 1      1     0     0     0     0
#> 2      2     1     0     0     5
#> 3      3     2     0     0    10
#> 4      4     3     0     0    15
#> 5      5     4     0     0    20
#> 6      6     5     0     0    25
```

Use the `square_size` argument to match your physical square width (in
millimetres, centimetres, etc.) and pass `as_tibble = FALSE` if you
prefer a simple matrix of x/y coordinates.

## Calibrating single points

Use
[`cam_cal_pt()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_pt.md)
when you need to correct a single pixel coordinate, for example when
transforming landmarks exported from a digitising tool.

``` r

raw_pt <- c(x = 650, y = 380)
cam_cal_pt(raw_pt["x"], raw_pt["y"], K, distortion, F_mm)
#> [1] 25.38876 14.87574
```

Internally the function:

1.  Recentres the point by subtracting the principal point
    ([`optically_center()`](https://johnkirwan.github.io/radiatR/reference/optically_center.md)),
2.  Normalises by the focal lengths
    ([`focalize()`](https://johnkirwan.github.io/radiatR/reference/focalize.md)),
3.  Removes radial and tangential distortion via a fixed-point iteration
    ([`radial_distort()`](https://johnkirwan.github.io/radiatR/reference/radial_distort.md)),
4.  Converts the undistorted coordinates into millimetres
    ([`scaled_xy2mm()`](https://johnkirwan.github.io/radiatR/reference/scaled_xy2mm.md)).

Both the iteration count and convergence tolerance are configurable via
the `max_iterations` and `tolerance` arguments.

## Interactive collection

For small calibration sets you can grab corners manually.
[`calibration_session()`](https://johnkirwan.github.io/radiatR/reference/calibration_session.md)
opens a graphics device per frame and records clicks in row-major order,
then solves a planar calibration:

``` r

frames <- list('frame1.png', 'frame2.png')
calib <- calibration_session(frames, pattern = 'chessboard', board_dims = c(9, 7), square_size = 5)
calib$intrinsics
```

The helper works with chessboard, ChArUco, or AprilTag grids—simply
provide `board_dims`/`square_size` matching the pattern. The output
contains the `CalModel`, per-frame extrinsics, and reprojection errors
so you can inspect the fit.

## Batch calibration

For tables or matrices of points,
[`cam_cal_many()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_many.md)
processes everything in one pass.

``` r

pts <- matrix(
  c(640, 360,
    700, 420,
    580, 320),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("x", "y"))
)
cam_cal_many(pts, K, distortion, F_mm)
#>          [,1]     [,2]
#> [1,] 24.52197 13.82316
#> [2,] 27.91428 16.79090
#> [3,] 20.00529 11.05509
```

Supplying a `CalModel` lets you calibrate entire trajectory sets with a
single call. The bundled millipede example data provides real
pixel-space tracks recorded by a camera mounted above a circular arena.
We load the first trial and apply the hypothetical calibration model
defined above.

``` r

track_dir <- system.file("extdata", "tracks", package = "radiatR")
manifest_path <- system.file("extdata", "millipede_trials.csv", package = "radiatR")

file_tbl <- import_tracks(track_dir)
manifest <- import_info(manifest_path)
file_tbl <- load_tracks(file_tbl, manifest, track_dir)
#> Warning in .augment_with_manifest(file_tbl, df, manifest_cols): Entries in
#> `file_tbl` with no matching metadata: con_19
#> Warning in .augment_with_manifest(file_tbl, df, manifest_cols): Rows in
#> `manifest` with no corresponding track: con_101, con_102, con_104, con_105,
#> con_108, con_109, con_110, con_112, con_116, con_117, con_119, con_120,
#> con_121, 5_101, 5_102, 5_103, 5_104, 5_108, 5_110, 5_117, 5_118, 5_119, 5_121,
#> 10_101, 10_102, 10_105, 10_107, 10_108, 10_109, 10_110, 10_111, 10_112, 10_113,
#> 10_114, 10_116, 10_117, 10_119, 10_121, 15_101, 15_102, 15_104, 15_105, 15_107,
#> 15_109, 15_110, 15_112, 15_116, 15_119, 15_120, 15_121, 20_101, 20_102, 20_103,
#> 20_104, 20_105, 20_106, 20_107, 20_108, 20_109, 20_110, 20_112, 20_116, 20_117,
#> 20_119, 20_121, 30_101, 30_102, 30_104, 30_105, 30_106, 30_107, 30_108, 30_109,
#> 30_113, 30_114, 30_115, 30_116, 30_117, 30_119, 30_121, 40_101, 40_102, 40_103,
#> 40_104, 40_105, 40_107, 40_108, 40_109, 40_110, 40_112, 40_118, 40_119, 40_121,
#> 50_12, 50_34, 50_101, 50_104, 50_105, 50_107, 50_108, 50_109, 50_110, 50_112,
#> 50_113, 50_119, 50_121

# Read the first track file in pixel space
track_raw <- read.delim(
  file.path(track_dir, file_tbl$track[1]),
  sep = "\t", header = FALSE
)[, 1:3]
names(track_raw) <- c("frame", "x", "y")
track_raw$id <- tools::file_path_sans_ext(basename(file_tbl$track[1]))

ts_px <- TrajSet(
  track_raw,
  id = "id",
  time = "frame", x = "x", y = "y",
  normalize_xy = FALSE
)

ts_mm <- calibrate_positions(ts_px, cal_model)
head(ts_mm@data)
#>   frame        x        y           id   radius     angle
#> 1     1 14.24001 11.64882 10_1_point02 588.4075 0.6856412
#> 2     2 14.31990 11.57890 10_1_point02 588.7537 0.6799556
#> 3     3 14.21572 11.55460 10_1_point02 586.9042 0.6824999
#> 4     4 14.14695 11.53865 10_1_point02 585.6785 0.6841978
#> 5     5 14.28932 11.83800 10_1_point02 591.4184 0.6918499
#> 6     6 14.48097 12.06383 10_1_point02 596.8850 0.6945896
```

In a real workflow, replace the hypothetical `K`, `distortion`, and
`F_mm` values with parameters estimated from a checkerboard calibration
sequence captured with the same camera and lens.

## Persisting correspondences

You can stash the clicked points for later reuse. The helpers below
write a CSV and rebuild the calibration without reopening the graphics
device.

``` r

points_tbl <- calibration_points_tibble(calib$image_points)
write_calibration_points(calib$image_points, "calibration-points.csv")
replay <- calibration_from_points(board_dims = c(9, 7), square_size = 5,
                                  image_points = points_tbl, quiet = TRUE)
replay$intrinsics
```

``` r

ts_px_cal <- calib$transform(ts_px)
head(as.data.frame(ts_px_cal))
```

[`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md)
updates the `x`/`y` columns in-place and re-computes `angle` so that the
values reflect the corrected metric geometry.

## Convergence diagnostics

The undistortion step uses a fixed-point solver similar to the
`undistortPoints` routine in OpenCV. The defaults (`max_iterations = 5`,
`tolerance = 1e-9`) are usually sufficient. If the distortion model is
extreme or the points are far from the optical axis, you can relax these
parameters:

``` r

cam_cal_pt(650, 380, K, distortion, F_mm, max_iterations = 10, tolerance = 1e-12)
```

If convergence fails, the function returns the last iterate; consider
checking your coefficients or tightening the stopping criteria.

## Validating the solver

Because radiatR calibrates from corner *coordinates* rather than images,
the solver can be checked against a fully synthetic scene with known
ground truth. The package ships such a fixture:
`calibration_corners.csv` is a noiseless projection of a planar
checkerboard through a known pinhole camera in five poses, and
`calibration_truth.csv` records the intrinsics it was built from. This
is the basis of the package’s calibration round-trip test.

``` r

corners <- read_calibration_points(
  system.file("extdata", "calibration_corners.csv", package = "radiatR")
)
truth <- read.csv(
  system.file("extdata", "calibration_truth.csv", package = "radiatR")
)

calib <- calibration_from_points(
  board_dims  = c(truth$board_rows, truth$board_cols),
  square_size = truth$square_size,
  image_points = corners,
  quiet = TRUE
)

round(calib$intrinsics, 3)
#>      [,1] [,2] [,3]
#> [1,]  800    0  320
#> [2,]    0  820  240
#> [3,]    0    0    1
```

The recovered focal lengths (800, 820) and principal point (320, 240)
match the ground truth to within numerical precision, and the mean
reprojection error is effectively zero (2.77^{-11} px). With real corner
detections the reprojection error will be on the order of the detector’s
pixel noise; it is the primary diagnostic of calibration quality
reported in `calib$reprojection`.

## Summary

- Use
  [`cam_cal_pt()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_pt.md)
  /
  [`cam_cal_many()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_many.md)
  for one-off points and tables.
- Wrap parameters in a `CalModel` to correct every trajectory in a
  `TrajSet` via
  [`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md).
- Distortion coefficients can be radial-only or include tangential
  terms; unnamed coefficients fall back to a sensible ordering.

These helpers should provide a drop-in path for applying calibration
results from common toolboxes and maintainers of large tracking
datasets.
