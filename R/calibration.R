# Calibration utilities for camera-based coordinate correction
#

#' Normalize the given point by the focal length
#'
#' @param xy point
#' @param f focal length
#' @keywords internal
#' @return normalized point
focalize <- function(xy, f) {
  xy / f
}

#' Optically center the given point
#'
#' @param x x-coordinate
#' @param y y-coordinate
#' @param c optical center
#' @keywords internal
#' @return normalized point
optically_center <- function(x, y, c) {
  c(x, y) - c
}

#' Remove radial/tangential distortion from a point on the image plane.
#'
#' @param xy Numeric vector of length two, or a two-column matrix of distorted
#'   points expressed in normalised image coordinates (after subtracting the
#'   principal point and dividing by the focal lengths).
#' @param k Distortion coefficients. Supports numeric vectors, named vectors
#'   containing entries such as `k1`, `k2`, `k3`, `p1`, `p2`, or lists with
#'   `radial` and `tangential` components. Missing coefficients are treated as
#'   zero.
#' @param max_iterations Maximum number of fixed-point iterations used when
#'   inverting the distortion model. Defaults to `5`.
#' @param tolerance Convergence tolerance for the iterative solve. Defaults to
#'   `1e-9`.
#' @return Distortion-corrected point(s) with the same shape as `xy`.
radial_distort <- function(xy, k, max_iterations = 5, tolerance = 1e-9) {
  if (is.null(k) || (length(k) == 0)) {
    return(xy)
  }

  parsed <- .parse_distortion_coeffs(k)
  radial <- parsed$radial
  tangential <- parsed$tangential

  if ((length(radial) == 0 || all(radial == 0)) && all(tangential == 0)) {
    return(xy)
  }

  drop_vec <- is.null(dim(xy))
  xy_mat <- if (drop_vec) matrix(xy, ncol = 2) else xy

  x_d <- xy_mat[, 1]
  y_d <- xy_mat[, 2]
  x_u <- x_d
  y_u <- y_d

  for (iter in seq_len(max_iterations)) {
    r2 <- x_u^2 + y_u^2
    radial_term <- rep(1, length(r2))
    if (length(radial)) {
      for (p in seq_along(radial)) {
        radial_term <- radial_term + radial[p] * (r2^p)
      }
    }
    p1 <- tangential[1]
    p2 <- tangential[2]
    x_tang <- 2 * p1 * x_u * y_u + p2 * (r2 + 2 * x_u^2)
    y_tang <- p1 * (r2 + 2 * y_u^2) + 2 * p2 * x_u * y_u

    radial_term[!is.finite(radial_term) | abs(radial_term) < .Machine$double.eps] <- 1
    x_new <- (x_d - x_tang) / radial_term
    y_new <- (y_d - y_tang) / radial_term

    delta <- pmax(abs(x_new - x_u), abs(y_new - y_u), na.rm = TRUE)
    x_u <- x_new
    y_u <- y_new
    if (max(delta, na.rm = TRUE) < tolerance) {
      break
    }
  }

  result <- cbind(x_u, y_u)
  if (drop_vec) {
    as.numeric(result[1, ])
  } else {
    result
  }
}

.parse_distortion_coeffs <- function(k) {
  tangential <- c(0, 0)
  radial <- numeric()

  if (is.list(k)) {
    if (!is.null(k$radial)) radial <- as.numeric(k$radial)
    if (!is.null(k$tangential)) tangential <- rep_len(as.numeric(k$tangential), 2)
  } else {
    k <- as.numeric(k)
    if (!is.null(names(k))) {
      nm <- names(k)
      radial_names <- intersect(c("k1", "k2", "k3", "k4", "k5", "k6"), nm)
      tangential_names <- intersect(c("p1", "p2", "t1", "t2"), nm)
      if (length(radial_names)) radial <- k[radial_names]
      if (length(tangential_names)) tangential <- rep_len(k[tangential_names], 2)
      remaining <- k[setdiff(nm, c(radial_names, tangential_names))]
      if (!length(radial) && length(remaining)) {
        radial <- remaining[seq_len(min(3, length(remaining)))]
      }
      if (all(tangential == 0) && length(remaining) > length(radial)) {
        extra <- remaining[(length(radial) + 1):length(remaining)]
        if (length(extra)) tangential[1] <- extra[1]
        if (length(extra) > 1) tangential[2] <- extra[2]
      }
    } else {
      if (length(k)) {
        radial <- k[seq_len(min(3, length(k)))]
        remaining <- if (length(k) > length(radial)) k[(length(radial) + 1):length(k)] else numeric()
        if (length(remaining)) tangential[1] <- remaining[1]
        if (length(remaining) > 1) tangential[2] <- remaining[2]
      }
    }
  }

  radial <- as.numeric(radial)
  radial[is.na(radial)] <- 0
  tangential <- as.numeric(rep_len(tangential, 2))
  tangential[is.na(tangential)] <- 0

  list(
    radial = radial,
    tangential = tangential
  )
}

#' Generate world coordinates for checkerboard corners
#'
#' Helper mirroring MATLAB's `generateCheckerboardPoints`, returning the
#' coordinates of inner checkerboard corners in world units.
#'
#' @param board_dims Integer vector of length 2 giving the number of squares in
#'   the checkerboard along the vertical (rows) and horizontal (columns)
#'   directions. Must be >= 2 in each dimension.
#' @param square_size Numeric scalar or length-2 vector giving the spacing
#'   between adjacent corners in world units. When length-1, the same spacing is
#'   used in both directions.
#' @param origin Length-2 numeric vector specifying the world coordinate to
#'   assign to the corner at row 0, column 0. Defaults to `c(0, 0)`.
#' @param order Traversal order for the returned points. `"row_major"`
#'   increments the row index fastest (matching MATLAB), while `"col_major"`
#'   increments the column index fastest.
#' @param as_tibble Logical; if `TRUE` (default), return a tibble with columns
#'   `corner`, `row`, `col`, `x`, and `y`. If `FALSE`, return a numeric matrix of
#'   x/y coordinates.
#' @importFrom tibble tibble
#' @return Tibble or matrix describing the checkerboard corner coordinates in
#'   world units.
#' @export
checkerboard_points <- function(board_dims, square_size = 1, origin = c(0, 0),
                                order = c("row_major", "col_major"),
                                as_tibble = TRUE) {
  if (missing(board_dims) || length(board_dims) != 2) {
    stop("`board_dims` must be an integer vector of length 2 (rows, cols of squares).")
  }
  if (any(board_dims < 2)) {
    stop("`board_dims` must have at least two squares in each direction.")
  }
  if (length(square_size) == 1) {
    square_size <- rep(square_size, 2)
  }
  if (length(square_size) != 2) {
    stop("`square_size` must be a scalar or length-2 vector.")
  }
  order <- match.arg(order)
  rows <- as.integer(board_dims[1] - 1)
  cols <- as.integer(board_dims[2] - 1)

  row_idx <- seq_len(rows) - 1L
  col_idx <- seq_len(cols) - 1L

  if (order == "row_major") {
    grid <- expand.grid(row = row_idx, col = col_idx)
  } else {
    grid <- expand.grid(col = col_idx, row = row_idx)
    grid <- grid[, c("row", "col")]
  }

  x <- origin[1] + grid$col * square_size[2]
  y <- origin[2] + grid$row * square_size[1]

  if (!as.logical(as_tibble)) {
    mat <- cbind(x, y)
    dimnames(mat) <- NULL
    return(mat)
  }

  tibble::tibble(
    corner = seq_len(nrow(grid)),
    row = grid$row,
    col = grid$col,
    x = x,
    y = y
  )
}

#' Convert the given point from pixels to millimeters
#'
#' @param xy point (normalised coordinate after division by the focal lengths)
#' @param f focal length in pixels
#' @param F focal length in millimeters
#' @keywords internal
#' @return point in millimeters
scaled_xy2mm <- function(xy, f, F) {
  if (length(f) == 1) f <- rep(f, 2)
  if (length(F) == 1) F <- rep(F, 2)

  xy_pix <- if (is.null(dim(xy))) {
    xy * f
  } else {
    sweep(xy, 2, f, "*")
  }
  pixel_size <- F / f
  if (is.null(dim(xy))) {
    xy_pix * pixel_size
  } else {
    sweep(xy_pix, 2, pixel_size, "*")
  }
}

#' Switch axes of the given intrinsic camera matrix
#'
#' @param K intrinsic camera matrix
#' @keywords internal
#' @return modified intrinsic camera matrix
calibration_switch_axes <- function(K) {
  temp_matrix <- K
  temp_matrix[1, 1] <- K[2, 2]
  temp_matrix[2, 2] <- K[1, 1]
  temp_matrix[3, 1] <- K[3, 2]
  temp_matrix[3, 2] <- K[3, 1]
  temp_matrix
}

#' Calibrate the camera on the given xy coordinate
#'
#' @param x x-coordinate in pixels
#' @param y y-coordinate in pixels
#' @param K intrinsic camera matrix
#' @param k distortion coefficients (see [radial_distort()] for supported layouts)
#' @param F focal length in millimeters (scalar or length-2 vector)
#' @param max_iterations Maximum number of iterations passed to [radial_distort()].
#' @param tolerance Convergence tolerance passed to [radial_distort()].
#' @return calibrated point
#' @export
#' @examples
#' K <- matrix(c(784.948340421183, 0, 0,
#'               0, 782.554388639436, 0,
#'               939.047051578744, 528.896744808718, 1), ncol = 3, byrow = TRUE)
#' k <- c(-0.289927776375773, 0.0392224238600441)
#' F <- rep(680, 2)
#' cam_cal_pt(100, 100, K, k, F)
cam_cal_pt <- function(x, y, K, k, F, max_iterations = 5, tolerance = 1e-9) {
  if (!(length(x) == 1 && is.numeric(x) && length(y) == 1 && is.numeric(y))) {
    warning("xy not a pt!")
  }
  xy <- optically_center(x, y, K[3, 1:2])
  xy <- focalize(xy, c(K[1, 1], K[2, 2]))
  xy <- radial_distort(xy, k, max_iterations = max_iterations, tolerance = tolerance)
  scaled <- scaled_xy2mm(xy, c(K[1, 1], K[2, 2]), F)
  as.numeric(scaled)
}

#' Calibrate the camera on the given set of points
#'
#' @param points_idx matrix of points (pixel coordinates)
#' @param K intrinsic camera matrix
#' @param k distortion coefficients (see [radial_distort()])
#' @param F focal length in millimeters
#' @param max_iterations Maximum number of iterations passed to [radial_distort()].
#' @param tolerance Convergence tolerance passed to [radial_distort()].
#' @return matrix of calibrated points
#' @export
#' @examples
#' K <- matrix(c(784.948340421183, 0, 0,
#'               0, 782.554388639436, 0,
#'               939.047051578744, 528.896744808718, 1), ncol = 3, byrow = TRUE)
#' k <- c(-0.289927776375773, 0.0392224238600441)
#' F <- rep(680, 2)
#' points <- matrix(c(100, 100, 200, 200, 300, 300), ncol = 2, byrow = TRUE)
#' cam_cal_many(points, K, k, F)

cam_cal_many <- function(points_idx, K, k, F, max_iterations = 5, tolerance = 1e-9) {
  pts <- as.matrix(points_idx)
  if (ncol(pts) < 2) {
    stop("`points_idx` must supply x/y coordinates")
  }
  pts <- pts[, 1:2, drop = FALSE]
  centred <- sweep(pts, 2, K[3, 1:2], "-")
  normalised <- sweep(centred, 2, c(K[1, 1], K[2, 2]), "/")
  corrected <- radial_distort(normalised, k, max_iterations = max_iterations, tolerance = tolerance)
  out <- scaled_xy2mm(corrected, c(K[1, 1], K[2, 2]), F)
  dimnames(out) <- NULL
  out
}

#' @include TrajSet.R
NULL

#' Camera calibration model for trajectory correction
#'
#' @name CalModel-class
#' @rdname CalModel-class
#' @slot K 3x3 intrinsic calibration matrix
#' @slot k Numeric vector or list of distortion coefficients (radial first, optional tangential)
#' @slot F Numeric scalar or length-2 vector giving focal length scaling (e.g., mm per pixel)
#' @seealso \code{\link{calibrate_positions}}
#' @family calibration
#' @exportClass CalModel
setClass(
  "CalModel",
  slots = c(
    K = "matrix",
    k = "numeric",
    F = "numeric"
  )
)

#' Calibrate TrajSet positions using a camera model
#'
#' Applies camera intrinsics and distortion removal to convert pixel coordinates to metric space.
#'
#' @param x TrajSet with `x`/`y` coordinates
#' @param model `CalModel` object containing calibration parameters
#' @return TrajSet with updated `x`/`y` and angles in metric space
#' @family calibration
#' @importFrom tibble tibble
#' @export
setGeneric("calibrate_positions", function(x, model) standardGeneric("calibrate_positions"))

#' @rdname calibrate_positions
#' @export
setMethod("calibrate_positions", signature(x = "TrajSet", model = "CalModel"), function(x, model) {
  if (is.null(x@cols$x) || is.null(x@cols$y))
    stop("TrajSet has no x/y columns; supply cartesian coords to use calibration.")
  ids_vec <- ids(x)
  xc <- x@cols$x; yc <- x@cols$y
  base_x <- if (!is.null(x@cols$raw_x)) x@cols$raw_x else xc
  base_y <- if (!is.null(x@cols$raw_y)) x@cols$raw_y else yc
  K <- model@K; k <- model@k; F <- model@F

  xy <- cbind(x@data[[base_x]], x@data[[base_y]])
  centred <- sweep(xy, 2, K[3, 1:2], "-")
  normalised <- sweep(centred, 2, c(K[1, 1], K[2, 2]), "/")
  corrected <- radial_distort(normalised, k)
  xy_mm <- scaled_xy2mm(corrected, c(K[1, 1], K[2, 2]), F)

  x@data[[xc]] <- xy_mm[, 1]
  x@data[[yc]] <- xy_mm[, 2]
  if (!is.null(x@cols$raw_x)) {
    x@data[[x@cols$raw_x]] <- xy_mm[, 1]
  }
  if (!is.null(x@cols$raw_y)) {
    x@data[[x@cols$raw_y]] <- xy_mm[, 2]
  }
  x@data[[x@cols$angle]] <- (atan2(xy_mm[, 2], xy_mm[, 1]) %% (2 * pi))
  hist <- transform_history(x)
  if (nrow(hist)) {
    hist$order <- hist$order + 1L
  }
  params_list <- replicate(
    length(ids_vec),
    list(list(model = list(K = K, k = k, F = F))),
    simplify = FALSE
  )
  calibration_entry <- tibble::tibble(
    step = rep("calibration", length(ids_vec)),
    order = rep(0L, length(ids_vec)),
    id = ids_vec,
    implementation = rep("calibrate_positions", length(ids_vec)),
    params = params_list,
    depends_on = rep(list(character()), length(ids_vec))
  )
  new_history <- rbind(calibration_entry, hist)
  x <- set_transform_history(x, new_history)
  x@meta$calibration_model <- model
  methods::validObject(x)
  x
})
