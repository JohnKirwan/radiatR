# Interactive planar camera calibration from a graphics device

Opens a graphics device for each supplied frame, prompting the user to
click the calibration pattern corners in a consistent order. The
collected correspondences are used to estimate the camera intrinsics and
per-frame extrinsics via a planar calibration (Zhang, 1999).

## Usage

``` r
calibration_session(
  frames = NULL,
  pattern = c("chessboard", "charuco", "apriltag"),
  board_dims,
  square_size,
  origin = c(0, 0),
  order = c("row_major", "col_major"),
  quiet = FALSE,
  load_points = NULL,
  save_points = NULL
)
```

## Arguments

- frames:

  Optional list of image frames to annotate. Each entry may be a raster
  array (height × width × channels), a matrix, a
  \[grDevices::as.raster()\] object, or a file path readable via the
  \`png\`/\`jpeg\` packages. Frames are only required when collecting
  points interactively (i.e., when \`load_points\` is \`NULL\`).

- pattern:

  Calibration target type. Currently treats \`"chessboard"\`,
  \`"charuco"\`, and \`"apriltag"\` as planar grids requiring the same
  manual point ordering.

- board_dims:

  Integer vector of length 2 giving the number of squares in the
  checkerboard (rows, columns). Charuco and AprilTag grids use the same
  layout assumption.

- square_size:

  Physical spacing between adjacent corners in world units. A scalar
  applies to both axes; a length-two vector can be used for anisotropic
  spacing.

- origin:

  World coordinates to assign to the (row = 0, col = 0) corner.

- order:

  Expected traversal order for the clicked points. \`"row_major"\`
  (default) progresses left-to-right along each row before moving
  downward.

- quiet:

  If \`FALSE\` (default), guidance about the expected number/order of
  clicks is printed for every frame.

- load_points:

  Optional calibration point data (list, data frame, or file path) to
  use instead of interactive clicking.

- save_points:

  Optional file path at which to persist the collected point data (CSV).

## Value

A list containing: \* \`model\`: \`CalModel\` object with estimated
intrinsics (no distortion). \* \`intrinsics\`: 3 × 3 camera matrix. \*
\`extrinsics\`: list of per-frame rotation/translation matrices. \*
\`image_points\`: list of clicked image coordinates. \*
\`world_points\`: tibble of template world coordinates. \*
\`reprojection\`: tibble of per-point reprojection errors. \*
\`points\`: tibble representation of the calibration correspondences.
