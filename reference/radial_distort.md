# Remove radial/tangential distortion from a point on the image plane.

Remove radial/tangential distortion from a point on the image plane.

## Usage

``` r
radial_distort(xy, k, max_iterations = 5, tolerance = 1e-09)
```

## Arguments

- xy:

  Numeric vector of length two, or a two-column matrix of distorted
  points expressed in normalised image coordinates (after subtracting
  the principal point and dividing by the focal lengths).

- k:

  Distortion coefficients. Supports numeric vectors, named vectors
  containing entries such as \`k1\`, \`k2\`, \`k3\`, \`p1\`, \`p2\`, or
  lists with \`radial\` and \`tangential\` components. Missing
  coefficients are treated as zero.

- max_iterations:

  Maximum number of fixed-point iterations used when inverting the
  distortion model. Defaults to \`5\`.

- tolerance:

  Convergence tolerance for the iterative solve. Defaults to \`1e-9\`.

## Value

Distortion-corrected point(s) with the same shape as \`xy\`.
