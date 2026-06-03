# data-raw/calibration_fixture.R
#
# Generate a synthetic, fully reproducible camera-calibration fixture for
# testing radiatR's planar (Zhang's-method) calibration solver.
#
# WHY SYNTHETIC: radiatR's calibration consumes 2-D checkerboard corner
# coordinates, not images -- there is no corner detector in the package. The
# natural, copyright-free test data is therefore a synthetic scene generated
# from KNOWN intrinsics: we project a planar checkerboard through a known
# pinhole camera in several poses, then check that the solver recovers the
# intrinsics it was built from. No external images are required or useful.
#
# Outputs (committed to inst/extdata/):
#   calibration_corners.csv  frame, corner, x, y   image points, read by
#                            read_calibration_points()  (noiseless projections)
#   calibration_truth.csv    fx, fy, cx, cy, skew, board_rows, board_cols,
#                            square_size   ground-truth parameters for the test
#
# Re-run with:  Rscript data-raw/calibration_fixture.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))

set.seed(20240603)

# ---- ground-truth intrinsics -------------------------------------------------
fx <- 800; fy <- 820; cx <- 320; cy <- 240; skew <- 0
K_true <- matrix(c(fx, skew, cx,
                   0,  fy,   cy,
                   0,  0,    1), nrow = 3, byrow = TRUE)

# ---- planar world target (checkerboard inner corners, Z = 0) -----------------
board_dims  <- c(9, 7)   # -> (9-1) * (7-1) = 48 inner corners
square_size <- 1
template <- checkerboard_points(board_dims, square_size, as_tibble = TRUE)
W <- t(cbind(as.matrix(template[, c("x", "y")]), 0))   # 3 x n world points

# ---- a handful of camera poses with genuine perspective variation -----------
rotmat <- function(rx, ry, rz) {
  Rx <- matrix(c(1, 0, 0, 0, cos(rx), -sin(rx), 0, sin(rx), cos(rx)), 3, 3, byrow = TRUE)
  Ry <- matrix(c(cos(ry), 0, sin(ry), 0, 1, 0, -sin(ry), 0, cos(ry)), 3, 3, byrow = TRUE)
  Rz <- matrix(c(cos(rz), -sin(rz), 0, sin(rz), cos(rz), 0, 0, 0, 1), 3, 3, byrow = TRUE)
  Rz %*% Ry %*% Rx
}

poses <- list(
  list(r = c( 0.30, -0.20,  0.10), t = c(-4, -3, 14)),
  list(r = c(-0.25,  0.35, -0.15), t = c(-3, -4, 16)),
  list(r = c( 0.40,  0.30,  0.20), t = c(-5, -2, 13)),
  list(r = c(-0.35, -0.30,  0.25), t = c(-2, -5, 15)),
  list(r = c( 0.15,  0.45, -0.20), t = c(-4, -4, 12))
)

project <- function(pose) {
  R    <- rotmat(pose$r[1], pose$r[2], pose$r[3])
  cam  <- R %*% W + pose$t
  proj <- K_true %*% cam
  t(proj[1:2, ] / proj[rep(3, 2), ])   # n x 2 image coordinates (u, v)
}

corners <- do.call(rbind, lapply(seq_along(poses), function(i) {
  uv <- project(poses[[i]])
  data.frame(frame  = i,
             corner = seq_len(nrow(uv)),
             x      = uv[, 1],
             y      = uv[, 2])
}))

# ---- self-check: does the solver recover K from these projections? ----------
img_pts <- calibration_points_from_tibble(corners)
calib   <- calibration_from_points(board_dims, square_size, img_pts, quiet = TRUE)
cat("max |K_hat - K_true| :", max(abs(calib$intrinsics - K_true)), "\n")
cat("mean reprojection (px):", mean(calib$reprojection$error), "\n")
stopifnot(max(abs(calib$intrinsics - K_true)) < 1e-6)

# ---- write fixtures ----------------------------------------------------------
extdir <- file.path("inst", "extdata")
utils::write.csv(corners, file.path(extdir, "calibration_corners.csv"),
                 row.names = FALSE)

truth <- data.frame(fx = fx, fy = fy, cx = cx, cy = cy, skew = skew,
                    board_rows = board_dims[1], board_cols = board_dims[2],
                    square_size = square_size)
utils::write.csv(truth, file.path(extdir, "calibration_truth.csv"),
                 row.names = FALSE)

cat("Wrote", nrow(corners), "corner rows across", length(poses), "views.\n")
