# Interactive camera calibration helpers (manual point selection)
#
# Provides:
#   - calibration_session(): guide the user through clicking calibration targets
#   - Internal helpers to solve planar camera calibration (Zhang 1999) from
#     correspondences gathered via the graphics device.

#' Interactive planar camera calibration from a graphics device
#'
#' Opens a graphics device for each supplied frame, prompting the user to
#' click the calibration pattern corners in a consistent order. The collected
#' correspondences are used to estimate the camera intrinsics and per-frame
#' extrinsics via a planar calibration (Zhang, 1999).
#'
#' @param frames Optional list of image frames to annotate. Each entry may
#'   be a raster array (height × width × channels), a matrix, a
#'   [grDevices::as.raster()] object, or a file path readable via the
#'   `png`/`jpeg` packages. Frames are only required when collecting points
#'   interactively (i.e., when `load_points` is `NULL`).
#' @param pattern Calibration target type. Currently treats `"chessboard"`,
#'   `"charuco"`, and `"apriltag"` as planar grids requiring the same manual
#'   point ordering.
#' @param board_dims Integer vector of length 2 giving the number of squares in
#'   the checkerboard (rows, columns). Charuco and AprilTag grids use the same
#'   layout assumption.
#' @param square_size Physical spacing between adjacent corners in world units.
#'   A scalar applies to both axes; a length-two vector can be used for
#'   anisotropic spacing.
#' @param origin World coordinates to assign to the (row = 0, col = 0) corner.
#' @param order Expected traversal order for the clicked points. `"row_major"`
#'   (default) progresses left-to-right along each row before moving downward.
#' @param quiet If `FALSE` (default), guidance about the expected number/order
#'   of clicks is printed for every frame.
#' @param load_points Optional calibration point data (list, data frame, or file
#'   path) to use instead of interactive clicking.
#' @param save_points Optional file path at which to persist the collected point
#'   data (CSV).
#' @return A list containing:
#'   * `model`: `CalModel` object with estimated intrinsics (no distortion).
#'   * `intrinsics`: 3 × 3 camera matrix.
#'   * `extrinsics`: list of per-frame rotation/translation matrices.
#'   * `image_points`: list of clicked image coordinates.
#'   * `world_points`: tibble of template world coordinates.
#'   * `reprojection`: tibble of per-point reprojection errors.
#'   * `points`: tibble representation of the calibration correspondences.
#' @export
calibration_session <- function(frames = NULL,
                                pattern = c("chessboard", "charuco", "apriltag"),
                                board_dims,
                                square_size,
                                origin = c(0, 0),
                                order = c("row_major", "col_major"),
                                quiet = FALSE,
                                load_points = NULL,
                                save_points = NULL) {
  pattern <- match.arg(pattern)
  order <- match.arg(order)
  if (missing(board_dims) || length(board_dims) != 2) {
    stop("`board_dims` must be supplied as c(rows, cols) of squares.")
  }
  if (missing(square_size)) {
    stop("`square_size` must be supplied to define world units.")
  }
  template <- checkerboard_points(board_dims, square_size, origin, order = order, as_tibble = TRUE)
  n_pts <- nrow(template)

  using_loaded <- !is.null(load_points)
  if (!using_loaded && (is.null(frames) || !length(frames))) {
    stop("`frames` must contain at least one image when `load_points` is NULL.")
  }

  if (using_loaded) {
    image_points <- .calib_points_resolve(load_points, expected = n_pts)
  } else {
    image_points <- vector("list", length(frames))
    for (i in seq_along(frames)) {
      img <- .calib_as_raster(frames[[i]])
      dims <- attr(img, "dim")
      if (is.null(dims) || length(dims) < 2) {
        stop("Unable to determine image dimensions for frame ", i, ".")
      }
      if (!quiet) {
        message(sprintf(
          "Frame %d/%d: click %d %s corners in %s order (press ESC to abort).",
          i, length(frames), n_pts, pattern, order
        ))
      }
      pts <- .calib_collect_points(img, n_pts)
      if (nrow(pts) != n_pts) {
        stop(sprintf("Frame %d: expected %d points but received %d.", i, n_pts, nrow(pts)))
      }
      image_points[[i]] <- as.matrix(pts[, c("x", "y")])
    }
    image_points <- .calib_validate_points(image_points, expected = n_pts)
  }

  if (!is.null(save_points)) {
    write_calibration_points(image_points, save_points)
  }

  calib <- .calib_solve(template, image_points)
  model <- new("CalModel", K = calib$intrinsics, k = c(0, 0), F = rep(1, 2))
  points_tbl <- calibration_points_tibble(image_points)
  list(
    model = model,
    intrinsics = calib$intrinsics,
    extrinsics = calib$extrinsics,
    image_points = image_points,
    world_points = template,
    reprojection = calib$reprojection,
    transform = function(ts) calibrate_positions(ts, model),
    points = points_tbl
  )
}


.calib_as_raster <- function(frame) {
  if (inherits(frame, "nativeRaster") || inherits(frame, "raster")) {
    rast <- frame
  } else if (is.character(frame) && length(frame) == 1L) {
    if (!file.exists(frame)) stop("Frame file not found: ", frame)
    ext <- tolower(tools::file_ext(frame))
    if (ext %in% c("png")) {
      if (!requireNamespace("png", quietly = TRUE)) {
        stop("Reading PNG frames requires the 'png' package.")
      }
      rast <- png::readPNG(frame)
    } else if (ext %in% c("jpg", "jpeg")) {
      if (!requireNamespace("jpeg", quietly = TRUE)) {
        stop("Reading JPEG frames requires the 'jpeg' package.")
      }
      rast <- jpeg::readJPEG(frame)
    } else {
      stop("Unsupported image extension: ", ext)
    }
  } else if (is.matrix(frame)) {
    rng <- range(frame, finite = TRUE)
    if (diff(rng) > 0) frame <- (frame - rng[1]) / diff(rng)
    rast <- frame
  } else if (is.array(frame) && length(dim(frame)) %in% c(3, 4)) {
    rast <- frame
  } else {
    stop("Unsupported frame type: ", class(frame)[1])
  }
  as.raster(rast)
}

.calib_collect_points <- function(rast, n_points) {
  dims <- attr(rast, "dim")
  width <- dims[2]
  height <- dims[1]

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::plot(c(0, width), c(height, 0),
       type = "n", xlab = "", ylab = "", axes = FALSE,
       xaxs = "i", yaxs = "i", asp = 1)
  graphics::rasterImage(rast, 0, height, width, 0)
  graphics::box()

  xs <- numeric(n_points)
  ys <- numeric(n_points)
  for (idx in seq_len(n_points)) {
    loc <- graphics::locator(1)
    if (is.null(loc)) break
    xs[idx] <- loc$x
    ys[idx] <- loc$y
    graphics::points(loc$x, loc$y, pch = 21, bg = "#FF0000", col = "#000000")
    graphics::text(loc$x, loc$y, labels = idx, pos = 3, cex = 0.6, col = "#FF0000")
  }
  tibble::tibble(point = seq_len(n_points), x = xs, y = ys)
}

.calib_solve <- function(template, image_points) {
  world <- as.matrix(template[, c("x", "y")])
  Hs <- lapply(image_points, .calib_homography, world = world)
  V <- do.call(rbind, lapply(Hs, .calib_build_V))
  sv <- svd(V)
  b <- sv$v[, ncol(sv$v)]
  B <- matrix(c(b[1], b[2], b[4],
                b[2], b[3], b[5],
                b[4], b[5], b[6]), nrow = 3, byrow = TRUE)
  intr <- .calib_intrinsics_from_B(B)
  extrinsics <- lapply(Hs, .calib_extrinsics, K = intr$K)
  reprojection <- .calib_reprojection(world, image_points, intr$K, extrinsics)
  list(intrinsics = intr$K, extrinsics = extrinsics, reprojection = reprojection)
}

.calib_homography <- function(world, image) {
  if (nrow(world) != nrow(image)) {
    stop("World/image point counts differ.")
  }
  X <- world[, 1]
  Y <- world[, 2]
  u <- image[, 1]
  v <- image[, 2]
  n <- length(X)
  A <- matrix(0, nrow = 2 * n, ncol = 9)
  for (i in seq_len(n)) {
    A[(2 * i) - 1, ] <- c(-X[i], -Y[i], -1, 0, 0, 0, u[i] * X[i], u[i] * Y[i], u[i])
    A[(2 * i), ] <- c(0, 0, 0, -X[i], -Y[i], -1, v[i] * X[i], v[i] * Y[i], v[i])
  }
  sv <- svd(A)
  h <- sv$v[, ncol(sv$v)]
  H <- matrix(h, nrow = 3, byrow = TRUE)
  H / H[3, 3]
}

.calib_build_V <- function(H) {
  h1 <- H[, 1]
  h2 <- H[, 2]
  v12 <- .calib_v_ij(h1, h2)
  v11 <- .calib_v_ij(h1, h1)
  v22 <- .calib_v_ij(h2, h2)
  rbind(v12, v11 - v22)
}

.calib_v_ij <- function(hi, hj) {
  c(
    hi[1] * hj[1],
    hi[1] * hj[2] + hi[2] * hj[1],
    hi[2] * hj[2],
    hi[3] * hj[1] + hi[1] * hj[3],
    hi[3] * hj[2] + hi[2] * hj[3],
    hi[3] * hj[3]
  )
}

.calib_intrinsics_from_B <- function(B) {
  b11 <- B[1, 1]; b12 <- B[1, 2]; b13 <- B[1, 3]
  b22 <- B[2, 2]; b23 <- B[2, 3]; b33 <- B[3, 3]
  v0 <- (b12 * b13 - b11 * b23) / (b11 * b22 - b12^2)
  lambda <- b33 - (b13^2 + v0 * (b12 * b13 - b11 * b23)) / b11
  alpha <- sqrt(lambda / b11)
  beta <- sqrt(lambda * b11 / (b11 * b22 - b12^2))
  gamma <- -b12 * alpha^2 * beta / lambda
  u0 <- gamma * v0 / beta - b13 * alpha^2 / lambda
  K <- matrix(c(alpha, gamma, u0,
                0,     beta,  v0,
                0,       0,   1), nrow = 3, byrow = TRUE)
  list(K = K, parameters = list(alpha = alpha, beta = beta, gamma = gamma, u0 = u0, v0 = v0))
}

.calib_extrinsics <- function(H, K) {
  invK <- solve(K)
  h1 <- H[, 1]
  h2 <- H[, 2]
  h3 <- H[, 3]
  lambda <- 1 / sqrt(sum((invK %*% h1)^2))
  r1 <- lambda * (invK %*% h1)
  r2 <- lambda * (invK %*% h2)
  r3 <- c(
    r1[2] * r2[3] - r1[3] * r2[2],
    r1[3] * r2[1] - r1[1] * r2[3],
    r1[1] * r2[2] - r1[2] * r2[1]
  )
  R <- cbind(r1, r2, r3)
  sv <- svd(R)
  R <- sv$u %*% t(sv$v)
  tvec <- lambda * (invK %*% h3)
  list(R = R, t = as.numeric(tvec))
}

.calib_reprojection <- function(world, image_points, K, extrinsics) {
  pts <- vector("list", length(image_points))
  for (i in seq_along(image_points)) {
    img <- image_points[[i]]
    ext <- extrinsics[[i]]
    W <- cbind(world, 0)
    cam <- t(ext$R %*% t(W) + ext$t)
    proj <- t(K %*% t(cam))
    proj <- proj[, 1:2] / proj[, 3]
    d <- sqrt(rowSums((img - proj)^2))
    pts[[i]] <- tibble::tibble(
      frame = i,
      corner = seq_len(nrow(world)),
      u = img[, 1],
      v = img[, 2],
      u_hat = proj[, 1],
      v_hat = proj[, 2],
      error = d
    )
  }
  do.call(rbind, pts)
}

#' Convert calibration point matrices to a tibble
#'
#' @param image_points List of 2-column matrices containing image-space corner
#'   coordinates for each frame.
#' @return A tibble with columns frame, corner, x, and y.
#' @export
calibration_points_tibble <- function(image_points) {
  if (is.null(image_points)) return(tibble::tibble(frame = integer(), corner = integer(), x = numeric(), y = numeric()))
  if (inherits(image_points, "data.frame")) {
    image_points <- calibration_points_from_tibble(image_points)
  }
  points <- .calib_validate_points(image_points, expected = NULL)
  rows <- lapply(seq_along(points), function(i) {
    mat <- points[[i]]
    tibble::tibble(
      frame = as.integer(i),
      corner = seq_len(nrow(mat)),
      x = mat[, 1],
      y = mat[, 2]
    )
  })
  if (!length(rows)) {
    return(tibble::tibble(frame = integer(), corner = integer(), x = numeric(), y = numeric()))
  }
  do.call(rbind, rows)
}

#' Reconstruct calibration point matrices from a tibble
#'
#' @param tbl Data frame with columns `frame`, `x`, and `y` (and optionally
#'   `corner`).
#' @return List of 2-column matrices matching the layout expected by
#'   [calibration_session()].
#' @export
calibration_points_from_tibble <- function(tbl) {
  required <- c("frame", "x", "y")
  if (!all(required %in% names(tbl))) {
    stop('`tbl` must contain columns frame, x, and y.')
  }
  split_tbl <- split(tbl, tbl$frame)
  mats <- lapply(split_tbl, function(df) {
    if ("corner" %in% names(df)) {
      df <- df[order(df$corner), , drop = FALSE]
    }
    mat <- as.matrix(df[, c("x", "y")])
    dimnames(mat) <- NULL
    storage.mode(mat) <- "double"
    mat
  })
  .calib_validate_points(mats, expected = NULL)
}

#' Write calibration points to CSV
#'
#' @param image_points Calibration point data (list or tibble).
#' @param path File path to write. Directories must already exist.
#' @return Invisibly returns `path`.
#' @export
write_calibration_points <- function(image_points, path) {
  tbl <- calibration_points_tibble(image_points)
  utils::write.csv(tbl, path, row.names = FALSE)
  invisible(path)
}

#' Read calibration point CSV
#'
#' @param path File path previously written by
#'   [write_calibration_points()].
#' @return List of 2-column matrices containing image points per frame.
#' @export
read_calibration_points <- function(path) {
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE)
  calibration_points_from_tibble(tbl)
}

#' Compute a calibration from saved calibration points
#'
#' @inheritParams calibration_session
#' @param image_points Calibration points supplied as a list, tibble, or file
#'   path (as accepted by `load_points`).
#' @param quiet Suppress informational output.
#' @return Same structure as [calibration_session()] but without the interactive
#'   capture step.
#' @export
calibration_from_points <- function(board_dims, square_size, image_points,
                                    origin = c(0, 0),
                                    order = c("row_major", "col_major"),
                                    quiet = FALSE) {
  order <- match.arg(order)
  template <- checkerboard_points(board_dims, square_size, origin, order = order, as_tibble = TRUE)
  pts <- .calib_points_resolve(image_points, expected = nrow(template))
  if (!quiet) {
    message(sprintf('Loaded calibration points for %d frame(s).', length(pts)))
  }
  calib <- .calib_solve(template, pts)
  model <- new("CalModel", K = calib$intrinsics, k = c(0, 0), F = rep(1, 2))
  points_tbl <- calibration_points_tibble(pts)
  list(
    model = model,
    intrinsics = calib$intrinsics,
    extrinsics = calib$extrinsics,
    image_points = pts,
    world_points = template,
    reprojection = calib$reprojection,
    transform = function(ts) calibrate_positions(ts, model),
    points = points_tbl
  )
}

.calib_points_resolve <- function(points, expected = NULL) {
  if (is.character(points) && length(points) == 1L && file.exists(points)) {
    points <- read_calibration_points(points)
  } else if (inherits(points, "data.frame")) {
    points <- calibration_points_from_tibble(points)
  } else if (is.null(points)) {
    return(list())
  } else if (!is.list(points)) {
    stop('Calibration points must be provided as a list, data frame, or path.')
  }
  .calib_validate_points(points, expected = expected)
}

.calib_validate_points <- function(points, expected) {
  if (!is.list(points)) {
    stop('Calibration points must be a list of matrices.')
  }
  out <- vector('list', length(points))
  for (i in seq_along(points)) {
    mat <- points[[i]]
    mat <- as.matrix(mat)
    if (ncol(mat) != 2) {
      stop('Calibration point matrices must have two columns.')
    }
    if (!is.null(expected) && nrow(mat) != expected) {
      stop(sprintf('Frame %d contains %d points; expected %d.', i, nrow(mat), expected))
    }
    storage.mode(mat) <- 'double'
    dimnames(mat) <- NULL
    out[[i]] <- mat
  }
  out
}

