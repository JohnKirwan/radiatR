#' *Cylindroiulus punctatus* visual orientation trajectory dataset
#'
#' Pre-processed `TrajSet` of 235 millipede trajectories from a visual-acuity
#' experiment. *Cylindroiulus punctatus* individuals were placed at the centre
#' of a cylindrical arena under bright, downwelling light, with a dark target
#' of varying angular half-width on the arena wall, and their path tracked to
#' test whether they oriented toward the target (object taxis). Eight stimulus
#' conditions are represented: target half-widths of 5, 10, 15, 20, 30, 40, and
#' 50 degrees, plus a featureless control (`arc = 0`, an angular subtense of
#' zero).
#'
#' These tracks are a **subset** of the full experiment; see Kirwan & Nilsson
#' (2019) for the complete dataset and trial counts.
#'
#' Coordinates are normalised to the unit circle (arena radius = 1) and rotated
#' so the target lies in a common reference direction; `rel_theta`, `rel_x`,
#' and `rel_y` give the target-relative heading and position. The subject
#' identifier is retained in the `individual` column.
#'
#' The raw dtrack landmark/track text files for every trial are shipped in
#' `inst/extdata/tracks/`, and the trial manifest in
#' `inst/extdata/millipede_trials.csv`, so the full import pipeline can be
#' reproduced with [get_all_object_pos()]. See `data-raw/millipede_example.R`.
#'
#' @format A [`TrajSet`] object (44,331 observations) whose data columns include:
#'   \describe{
#'     \item{trial_id}{Character. Unique trial identifier.}
#'     \item{frame}{Integer. Video frame number.}
#'     \item{trans_x, trans_y}{Numeric. Unit-circle Cartesian coordinates.}
#'     \item{abs_theta}{Numeric. Absolute bearing (radians).}
#'     \item{rel_theta}{Numeric. Bearing relative to the target direction.}
#'     \item{rel_x, rel_y}{Numeric. Target-relative unit-circle coordinates.}
#'     \item{arc}{Ordered factor. Target half-width in degrees
#'       (`0` < `5` < `10` < `15` < `20` < `30` < `40` < `50`; 0 = control).}
#'     \item{type}{Character. `"control"` or `"stimulus"`.}
#'     \item{individual}{Character. Animal identifier.}
#'   }
#'
#' @references Kirwan, J. D., & Nilsson, D.-E. (2019). A millipede compound eye
#'   mediating low-resolution vision. *Vision Research*, 165, 36--44.
#'   \doi{10.1016/j.visres.2019.09.003}
#' @source Behavioural experiment from Kirwan & Nilsson (2019); raw tracks
#'   produced with dtrack and normalised with \pkg{radiatR}. A subset of the
#'   published dataset.
"cpunctatus"

#' *Cylindroiulus punctatus* trajectory tibble
#'
#' A tidy long-form tibble (44,331 rows) holding the per-frame observations
#' from the same millipede experiment as \code{\link{cpunctatus}}, with the
#' trial condition metadata (`arc`, `type`, `individual`) joined onto every
#' frame. Convenient for analyses that prefer a plain data frame to the
#' `TrajSet` container. A subset of the published dataset.
#'
#' @format A tibble with 18 columns including \code{trial_id}, \code{frame},
#'   \code{trans_x}, \code{trans_y}, \code{rel_x}, \code{rel_y},
#'   \code{abs_theta}, \code{rel_theta}, \code{arc}, \code{type}, and
#'   \code{individual}.
#'
#' @references Kirwan, J. D., & Nilsson, D.-E. (2019). A millipede compound eye
#'   mediating low-resolution vision. *Vision Research*, 165, 36--44.
#'   \doi{10.1016/j.visres.2019.09.003}
#' @source Same experiment as \code{\link{cpunctatus}}.
"cpunctatus_tracks"
