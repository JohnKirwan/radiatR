# Calibrate the camera on the given set of points

Calibrate the camera on the given set of points

## Usage

``` r
cam_cal_many(points_idx, K, k, F, max_iterations = 5, tolerance = 1e-09)
```

## Arguments

- points_idx:

  matrix of points (pixel coordinates)

- K:

  intrinsic camera matrix

- k:

  distortion coefficients (see \[radial_distort()\])

- F:

  focal length in millimeters

- max_iterations:

  Maximum number of iterations passed to \[radial_distort()\].

- tolerance:

  Convergence tolerance passed to \[radial_distort()\].

## Value

matrix of calibrated points

## Examples

``` r
K <- matrix(c(784.948340421183, 0, 0,
              0, 782.554388639436, 0,
              939.047051578744, 528.896744808718, 1), ncol = 3, byrow = TRUE)
k <- c(-0.289927776375773, 0.0392224238600441)
F <- rep(680, 2)
points <- matrix(c(100, 100, 200, 200, 300, 300), ncol = 2, byrow = TRUE)
cam_cal_many(points, K, k, F)
#>            [,1]      [,2]
#> [1,] -1326.1544 -679.9658
#> [2,] -1348.8412 -602.1086
#> [3,]  -949.2601 -341.0504
```
