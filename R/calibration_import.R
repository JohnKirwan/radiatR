# Import camera-calibration coefficients produced by external toolboxes
# (MATLAB Computer Vision Toolbox, OpenCV) or supplied manually, and turn
# them into a `CalModel` that `calibrate_positions()` can apply to tracks.

#' @include calibration.R
NULL

#' Build a camera calibration model from intrinsic coefficients
#'
#' Assembles a [CalModel-class] from the focal lengths, principal point, and
#' distortion coefficients reported by a camera-calibration toolbox. This is the
#' low-level constructor used by [read_calibration()]; call it directly when you
#' already have the numbers in hand.
#'
#' radiatR stores the intrinsic matrix in MATLAB's *transposed* convention: the
#' focal lengths sit on the diagonal (`K[1, 1]`, `K[2, 2]`) and the principal
#' point occupies the bottom row (`K[3, 1:2]`). `cal_model()` owns this layout so
#' callers never have to assemble `K` by hand. Axis skew is not used by the
#' downstream correction and is therefore dropped.
#'
#' The distortion coefficients follow the Brown-Conrady model shared by MATLAB
#' (`RadialDistortion` + `TangentialDistortion`) and OpenCV (`[k1, k2, p1, p2,
#' k3]`): radial terms `k1`, `k2`, `k3` and tangential terms `p1`, `p2`.
#'
#' @param fx,fy Focal lengths in pixels.
#' @param cx,cy Principal point in pixels, in the same coordinate convention as
#'   the track coordinates the model will be applied to (0-based for most
#'   tracking tools).
#' @param k1,k2,k3 Radial distortion coefficients. Default to `0` (no radial
#'   distortion).
#' @param p1,p2 Tangential distortion coefficients. Default to `0`.
#' @param F Scale factor controlling the units of the corrected output, passed to
#'   [scaled_xy2mm()]. When `NULL` (the default) it is set to `c(fx, fy)`, which
#'   leaves the output in undistorted pixels. Supply the physical focal length in
#'   millimetres to obtain a metric result, or any length-2 vector for custom
#'   scaling.
#' @return A [CalModel-class] object.
#' @family calibration
#' @seealso [read_calibration()], [calibrate_positions()]
#' @export
#' @examples
#' model <- cal_model(fx = 800, fy = 800, cx = 640, cy = 360,
#'                    k1 = -0.21, k2 = 0.04)
#' cam_cal_pt(100, 100, model@K, model@k, model@F)
cal_model <- function(fx, fy, cx, cy,
                      k1 = 0, k2 = 0, p1 = 0, p2 = 0, k3 = 0,
                      F = NULL) {
  nums <- list(fx = fx, fy = fy, cx = cx, cy = cy,
               k1 = k1, k2 = k2, k3 = k3, p1 = p1, p2 = p2)
  for (nm in names(nums)) {
    v <- nums[[nm]]
    if (length(v) != 1 || !is.numeric(v) || !is.finite(v)) {
      stop("`", nm, "` must be a single finite numeric value.", call. = FALSE)
    }
  }

  K <- matrix(0, 3, 3)
  K[1, 1] <- fx
  K[2, 2] <- fy
  K[3, 1] <- cx
  K[3, 2] <- cy
  K[3, 3] <- 1

  k <- c(k1 = k1, k2 = k2, k3 = k3, p1 = p1, p2 = p2)

  if (is.null(F)) {
    F <- c(fx, fy)
  } else {
    F <- as.numeric(F)
    if (length(F) == 1) F <- rep(F, 2)
    if (length(F) != 2 || any(!is.finite(F))) {
      stop("`F` must be NULL, a finite scalar, or a finite length-2 vector.",
           call. = FALSE)
    }
  }

  methods::new("CalModel", K = K, k = k, F = F)
}

#' Import a camera calibration from an external toolbox
#'
#' Reads the intrinsic and distortion coefficients exported by a camera
#' calibration toolbox and returns a [CalModel-class] ready for
#' [calibrate_positions()]. radiatR does not estimate calibrations itself; run
#' the calibration in MATLAB, OpenCV, or another system and import the result
#' here.
#'
#' Supported sources:
#'
#' \describe{
#'   \item{`"matlab"`}{A `.mat` file from the MATLAB Computer Vision Toolbox.
#'     Both the pre-R2022b `IntrinsicMatrix` (transposed) and the newer `K`
#'     (standard) layouts are recognised, together with `RadialDistortion` and
#'     `TangentialDistortion`. Requires the \pkg{R.matlab} package.}
#'   \item{`"opencv"`}{A YAML (`.yml`/`.yaml`) or JSON (`.json`) file written by
#'     OpenCV's `FileStorage`, with a `camera_matrix` and
#'     `distortion_coefficients` (order `k1, k2, p1, p2, k3`). The non-standard
#'     `\%YAML:1.0` header and `!!opencv-matrix` tags are tolerated. Requires
#'     \pkg{yaml} (YAML) or \pkg{jsonlite} (JSON).}
#'   \item{`"csv"`}{A plain CSV, either wide (columns `fx, fy, cx, cy` and
#'     optional `k1, k2, k3, p1, p2`) or long (a `parameter, value` pair per
#'     row).}
#' }
#'
#' Toolboxes differ in pixel-index convention: MATLAB reports the principal point
#' in 1-based pixels, while OpenCV and most tracking tools use 0-based pixels.
#' `read_calibration()` subtracts `principal_base` from the imported principal
#' point so it lines up with your track coordinates. The default is 1 for
#' `"matlab"` and 0 for `"opencv"`/`"csv"`; override it if your tracker uses a
#' different convention.
#'
#' @param path Path to the calibration file.
#' @param source File format. `"auto"` (default) infers it from the extension
#'   (`.mat` -> MATLAB, `.json`/`.yml`/`.yaml` -> OpenCV, `.csv`/`.txt` -> CSV).
#' @param F Scale factor for the corrected output, passed to [cal_model()].
#'   `NULL` (default) leaves the output in undistorted pixels.
#' @param principal_base Pixel index of the first pixel in the source's
#'   convention, subtracted from the principal point. `NULL` (default) uses the
#'   per-source default described above.
#' @return A [CalModel-class] object.
#' @family calibration
#' @seealso [cal_model()], [calibrate_positions()]
#' @export
#' @examples
#' \dontrun{
#' model <- read_calibration("camera_params.mat")
#' tracks <- calibrate_positions(tracks, model)
#' }
read_calibration <- function(path,
                             source = c("auto", "matlab", "opencv", "csv"),
                             F = NULL, principal_base = NULL) {
  source <- match.arg(source)
  if (!file.exists(path)) {
    stop("Calibration file not found: ", path, call. = FALSE)
  }
  if (source == "auto") {
    source <- .detect_calib_source(path)
  }

  params <- switch(source,
    matlab = .read_calibration_matlab(path),
    opencv = .read_calibration_opencv(path),
    csv    = .read_calibration_csv(path)
  )

  base <- if (is.null(principal_base)) {
    if (is.null(params$default_base)) 0 else params$default_base
  } else {
    principal_base
  }

  cal_model(
    fx = params$fx, fy = params$fy,
    cx = params$cx - base, cy = params$cy - base,
    k1 = params$k1 %||% 0, k2 = params$k2 %||% 0, k3 = params$k3 %||% 0,
    p1 = params$p1 %||% 0, p2 = params$p2 %||% 0,
    F = F
  )
}

.detect_calib_source <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    mat  = "matlab",
    json = "opencv",
    yml  = "opencv",
    yaml = "opencv",
    csv  = "csv",
    txt  = "csv",
    stop("Cannot infer calibration source from extension '", ext,
         "'. Pass source = \"matlab\", \"opencv\", or \"csv\".", call. = FALSE)
  )
}

# ---- MATLAB ------------------------------------------------------------------

.read_calibration_matlab <- function(path) {
  if (!requireNamespace("R.matlab", quietly = TRUE)) {
    stop("Reading MATLAB .mat calibration files requires the 'R.matlab' ",
         "package. Install it with install.packages('R.matlab').",
         call. = FALSE)
  }
  .parse_matlab_calib(R.matlab::readMat(path))
}

# Recursively search a (possibly nested) named list for the first element whose
# name, normalised to lowercase with separators removed, is in `wanted`.
.calib_find <- function(x, wanted) {
  if (!is.list(x)) return(NULL)
  nm <- names(x)
  if (!is.null(nm)) {
    key <- tolower(gsub("[._ ]", "", nm))
    hit <- which(key %in% wanted)
    if (length(hit)) return(x[[hit[1]]])
  }
  for (el in x) {
    found <- .calib_find(el, wanted)
    if (!is.null(found)) return(found)
  }
  NULL
}

.parse_matlab_calib <- function(mat) {
  Kmat <- .calib_find(mat, "intrinsicmatrix")
  transposed <- TRUE
  if (is.null(Kmat)) {
    Kmat <- .calib_find(mat, c("k", "cameramatrix"))
    transposed <- FALSE
  }
  if (is.null(Kmat)) {
    stop("No 'IntrinsicMatrix' or 'K' found in MATLAB calibration file.",
         call. = FALSE)
  }
  Kmat <- matrix(as.numeric(Kmat), 3, 3)
  if (transposed) {
    fx <- Kmat[1, 1]; fy <- Kmat[2, 2]; cx <- Kmat[3, 1]; cy <- Kmat[3, 2]
  } else {
    fx <- Kmat[1, 1]; fy <- Kmat[2, 2]; cx <- Kmat[1, 3]; cy <- Kmat[2, 3]
  }

  rad <- .calib_find(mat, "radialdistortion")
  tan <- .calib_find(mat, "tangentialdistortion")
  rad <- if (is.null(rad)) numeric() else as.numeric(rad)
  tan <- if (is.null(tan)) numeric() else as.numeric(tan)

  list(
    fx = fx, fy = fy, cx = cx, cy = cy,
    k1 = .nth(rad, 1), k2 = .nth(rad, 2), k3 = .nth(rad, 3),
    p1 = .nth(tan, 1), p2 = .nth(tan, 2),
    default_base = 1
  )
}

# ---- OpenCV ------------------------------------------------------------------

.read_calibration_opencv <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Reading JSON calibration files requires the 'jsonlite' package.",
           call. = FALSE)
    }
    obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("Reading YAML calibration files requires the 'yaml' package.",
           call. = FALSE)
    }
    txt <- readLines(path, warn = FALSE)
    txt <- txt[!grepl("^%YAML", txt)]
    txt <- gsub("!!opencv-matrix", "", txt, fixed = TRUE)
    obj <- yaml::yaml.load(paste(txt, collapse = "\n"))
  }
  .parse_opencv_calib(obj)
}

.parse_opencv_calib <- function(obj) {
  get_field <- function(wanted) {
    nm <- names(obj)
    if (is.null(nm)) return(NULL)
    key <- tolower(gsub("[._ ]", "", nm))
    hit <- which(key %in% wanted)
    if (length(hit)) obj[[hit[1]]] else NULL
  }

  cm <- get_field(c("cameramatrix", "k", "intrinsic", "intrinsicmatrix"))
  if (is.null(cm)) {
    stop("No 'camera_matrix' found in OpenCV calibration file.", call. = FALSE)
  }
  cm_data <- as.numeric(if (is.list(cm)) cm[["data"]] else cm)
  if (length(cm_data) < 6) {
    stop("OpenCV camera_matrix has fewer than 9 entries.", call. = FALSE)
  }
  # row-major standard K: [fx 0 cx 0 fy cy 0 0 1]
  fx <- cm_data[1]; cx <- cm_data[3]; fy <- cm_data[5]; cy <- cm_data[6]

  dc <- get_field(c("distortioncoefficients", "distcoeffs", "distortion", "d"))
  dc_data <- if (is.null(dc)) {
    numeric()
  } else {
    as.numeric(if (is.list(dc)) dc[["data"]] else dc)
  }

  # OpenCV order: k1 k2 p1 p2 [k3]
  list(
    fx = fx, fy = fy, cx = cx, cy = cy,
    k1 = .nth(dc_data, 1), k2 = .nth(dc_data, 2),
    p1 = .nth(dc_data, 3), p2 = .nth(dc_data, 4),
    k3 = .nth(dc_data, 5),
    default_base = 0
  )
}

# ---- Plain CSV / manual ------------------------------------------------------

.read_calibration_csv <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  nm <- tolower(trimws(names(df)))

  if (all(c("fx", "fy", "cx", "cy") %in% nm)) {
    g <- function(p) {
      i <- match(p, nm)
      if (is.na(i)) 0 else as.numeric(df[[i]][1])
    }
  } else if (ncol(df) >= 2) {
    keys <- tolower(trimws(as.character(df[[1]])))
    vals <- suppressWarnings(as.numeric(df[[2]]))
    g <- function(p) {
      i <- match(p, keys)
      if (is.na(i)) 0 else vals[i]
    }
  } else {
    stop("CSV calibration must have fx/fy/cx/cy columns or 'parameter,value' ",
         "rows.", call. = FALSE)
  }

  params <- list(
    fx = g("fx"), fy = g("fy"), cx = g("cx"), cy = g("cy"),
    k1 = g("k1"), k2 = g("k2"), k3 = g("k3"), p1 = g("p1"), p2 = g("p2")
  )
  if (any(!is.finite(c(params$fx, params$fy, params$cx, params$cy)))) {
    stop("CSV calibration is missing one of the required fx/fy/cx/cy values.",
         call. = FALSE)
  }
  params$default_base <- 0
  params
}

# ---- helpers -----------------------------------------------------------------

.nth <- function(v, i) if (length(v) >= i && is.finite(v[i])) v[i] else 0
