# Camera Calibration Workflow

Tracks recorded from an overhead camera live in *pixel* coordinates that
are distorted by the lens and offset from the arena centre. To analyse
movement in a true metric (or at least undistorted) frame you need the
camera’s calibration: its intrinsic matrix and distortion coefficients.

**radiatR does not estimate calibrations itself.** Estimating intrinsics
from a checkerboard sequence is a solved problem with mature,
well-tested implementations — MATLAB’s *Computer Vision Toolbox*,
OpenCV’s `calibrateCamera`, and others. radiatR’s job is to *import* the
coefficients those tools produce and *apply* them to your tracks. This
vignette covers that import-and-correct workflow.

## The model radiatR applies

radiatR uses the pinhole + Brown-Conrady model shared by MATLAB and
OpenCV:

- **Focal lengths** `fx`, `fy` and **principal point** `cx`, `cy`, in
  pixels.
- **Radial distortion** `k1`, `k2`, `k3` and **tangential distortion**
  `p1`, `p2`.

These are stored in a
[`CalModel`](https://johnkirwan.github.io/radiatR/reference/CalModel-class.md)
object, which
[`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md)
applies to a `TrajSet`.

A subtlety worth flagging up front: internally radiatR keeps the
intrinsic matrix in MATLAB’s *transposed* convention — focal lengths on
the diagonal, principal point in the **bottom row** (`K[3, 1:2]`), not
the right-hand column. You never have to assemble that matrix by hand:
[`cal_model()`](https://johnkirwan.github.io/radiatR/reference/cal_model.md)
and
[`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md)
own it for you.

## Building a model from known coefficients

If you already have the numbers — from a calibration report, a paper, or
a colleague —
[`cal_model()`](https://johnkirwan.github.io/radiatR/reference/cal_model.md)
assembles the `CalModel` directly:

``` r

model <- cal_model(
  fx = 800, fy = 800,   # focal lengths (px)
  cx = 640, cy = 360,   # principal point (px)
  k1 = -0.28, k2 = 0.05,
  p1 = 1e-3,  p2 = -5e-4
)
model
#> An object of class "CalModel"
#> Slot "K":
#>      [,1] [,2] [,3]
#> [1,]  800    0    0
#> [2,]    0  800    0
#> [3,]  640  360    1
#> 
#> Slot "k":
#>      k1      k2      k3      p1      p2 
#> -0.2800  0.0500  0.0000  0.0010 -0.0005 
#> 
#> Slot "F":
#> [1] 800 800
```

### What `F` controls

The `F` argument sets the units of the corrected output. When `F` is
left as `NULL` (the default) it is set to `c(fx, fy)`, which means the
output stays in **undistorted pixels** — distortion and the
principal-point offset are removed, but the scale is unchanged. To
obtain a metric result instead, pass the physical focal length in
millimetres:

``` r

model_mm <- cal_model(fx = 800, fy = 800, cx = 640, cy = 360,
                      k1 = -0.28, k2 = 0.05, F = 20)
```

## Importing a calibration file

In practice you will export the calibration from another tool and read
it with
[`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md),
which auto-detects the format from the file extension:

``` r

# MATLAB Computer Vision Toolbox (.mat)
model <- read_calibration("camera_params.mat")

# OpenCV FileStorage (.yml / .yaml / .json)
model <- read_calibration("opencv_calib.yml")

# Plain CSV (wide or parameter/value)
model <- read_calibration("calib.csv")
```

### MATLAB

Save the relevant fields of a `cameraParameters` / `cameraIntrinsics`
object to a `.mat` file. radiatR recognises both the pre-R2022b
`IntrinsicMatrix` (already in the transposed convention) and the newer
standard `K`, alongside `RadialDistortion` and `TangentialDistortion`.
Reading `.mat` files needs the **R.matlab** package.

``` matlab
% in MATLAB, after calibrating
params = cameraParams;
IntrinsicMatrix     = params.IntrinsicMatrix;
RadialDistortion    = params.RadialDistortion;
TangentialDistortion = params.TangentialDistortion;
save('camera_params.mat', 'IntrinsicMatrix', ...
     'RadialDistortion', 'TangentialDistortion');
```

MATLAB reports the principal point in **1-based** pixels.
[`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md)
subtracts one by default (`principal_base = 1`) so it lines up with the
0-based coordinates produced by most tracking tools. Override
`principal_base` if your tracker uses a different convention.

### OpenCV

`cv::FileStorage` writes a `camera_matrix` and `distortion_coefficients`
(in the order `k1, k2, p1, p2, k3`). radiatR reads both the YAML and
JSON variants and tolerates the non-standard `%YAML:1.0` header and
`!!opencv-matrix` tags. YAML needs the **yaml** package; JSON needs
**jsonlite**. OpenCV principal points are already 0-based, so no shift
is applied by default.

### Plain CSV

For coefficients typed out by hand, a CSV works in either layout — wide:

    fx,fy,cx,cy,k1,k2,k3,p1,p2
    800,800,640,360,-0.28,0.05,0,0.001,-0.0005

or long (`parameter,value` rows). Missing coefficients default to zero.
CSV principal points are treated as 0-based.

## Correcting points

For a single landmark,
[`cam_cal_pt()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_pt.md)
applies the model to one pixel coordinate;
[`cam_cal_many()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_many.md)
does the same for a table of points in one pass:

``` r

pts <- matrix(
  c(640, 360,
    700, 420,
    580, 320),
  ncol = 2, byrow = TRUE,
  dimnames = list(NULL, c("x", "y"))
)
cam_cal_many(pts, model@K, model@k, model@F)
#>           [,1]      [,2]
#> [1,]   0.00000   0.00000
#> [2,]  60.19038  60.17675
#> [3,] -60.13549 -40.09905
```

The point at the principal point `(640, 360)` maps to the origin; others
are recentred, undistorted, and scaled by `F`.

## Correcting a whole TrajSet

The usual entry point is
[`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md),
which corrects every track in a `TrajSet`, rewrites its `x`/`y` columns
in place, and re-derives `angle` from the corrected geometry. The
bundled millipede example provides real pixel-space tracks from a camera
above a circular arena.

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

ts_mm <- calibrate_positions(ts_px, model)
head(ts_mm@data)
#>   frame         x        y           id   radius    angle
#> 1     1 -187.1821 12.50332 10_1_point02 588.4075 3.074894
#> 2     2 -184.6893 10.08415 10_1_point02 588.7537 3.087046
#> 3     3 -187.1788 10.08701 10_1_point02 586.9042 3.087755
#> 4     4 -188.8331 10.08893 10_1_point02 585.6785 3.088216
#> 5     5 -187.1917 17.31612 10_1_point02 591.4184 3.049350
#> 6     6 -183.8896 22.11961 10_1_point02 596.8850 3.021880
```

In a real workflow, replace the illustrative `model` above with one
imported via
[`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md)
from a calibration captured with the same camera and lens.

## Convergence diagnostics

Removing distortion requires inverting the distortion model, which
`radiatR` does with a fixed-point solver (as OpenCV’s `undistortPoints`
does). The defaults (`max_iterations = 5`, `tolerance = 1e-9`) are
usually ample. For extreme distortion or points far from the optical
axis you can relax them:

``` r

cam_cal_pt(650, 380, model@K, model@k, model@F,
           max_iterations = 10, tolerance = 1e-12)
```

If convergence fails the function returns the last iterate; check your
coefficients or tighten the stopping criteria.

## Summary

- radiatR imports calibrations; it does not estimate them. Calibrate in
  MATLAB, OpenCV, or another tool, then bring the coefficients in.
- [`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md)
  reads MATLAB `.mat`, OpenCV YAML/JSON, and plain CSV;
  [`cal_model()`](https://johnkirwan.github.io/radiatR/reference/cal_model.md)
  builds a model from coefficients you already hold.
- Apply the resulting `CalModel` to single points
  ([`cam_cal_pt()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_pt.md)),
  tables
  ([`cam_cal_many()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_many.md)),
  or whole trajectory sets
  ([`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md)).
- Mind the conventions: principal-point pixel base (1-based MATLAB vs
  0-based OpenCV) and the `F` scale factor (undistorted pixels by
  default, metric if you pass a physical focal length).
