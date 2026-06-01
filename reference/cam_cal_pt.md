# Calibrate the camera on the given xy coordinate

Calibrate the camera on the given xy coordinate

## Usage

``` r
cam_cal_pt(x, y, K, k, F, max_iterations = 5, tolerance = 1e-09)
```

## Arguments

- x:

  x-coordinate in pixels

- y:

  y-coordinate in pixels

- K:

  intrinsic camera matrix

- k:

  distortion coefficients (see \[radial_distort()\] for supported
  layouts)

- F:

  focal length in millimeters (scalar or length-2 vector)

- max_iterations:

  Maximum number of iterations passed to \[radial_distort()\].

- tolerance:

  Convergence tolerance passed to \[radial_distort()\].

## Value

calibrated point

## Examples

``` r
K <- matrix(c(784.948340421183, 0, 0,
              0, 782.554388639436, 0,
              939.047051578744, 528.896744808718, 1), ncol = 3, byrow = TRUE)
k <- c(-0.289927776375773, 0.0392224238600441)
F <- rep(680, 2)
cam_cal_pt(100, 100, K, k, F)
#> [1] -1326.1544  -679.9658
```
