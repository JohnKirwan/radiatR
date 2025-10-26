# Circular trial helpers (limits, track extraction, aggregation)
#

.coerce_xy_frame <- function(obj, role = c("track", "landmark")) {
  role <- match.arg(role)
  if (inherits(obj, "TrajSet")) {
    df <- as.data.frame(obj)
    time_col <- obj@cols$time
    x_col <- obj@cols$x
    y_col <- obj@cols$y
    if (is.null(time_col) || is.null(x_col) || is.null(y_col)) {
      stop("TrajSet object must define time/x/y columns for ", role, ".")
    }
    out <- df[, c(time_col, x_col, y_col)]
  } else if (is.data.frame(obj)) {
    cand_time <- intersect(c("frame", "time", "t"), names(obj))
    cand_x <- intersect(c("x", "X", "x_pos", "pos_x"), names(obj))
    cand_y <- intersect(c("y", "Y", "y_pos", "pos_y"), names(obj))
    if (!length(cand_time) || !length(cand_x) || !length(cand_y)) {
      stop("Data frame for ", role, " must contain frame/time, x, and y columns.")
    }
    out <- obj[, c(cand_time[1], cand_x[1], cand_y[1])]
  } else {
    stop("Unsupported ", role, " input: must be data frame or TrajSet.")
  }
  names(out) <- c("frame", "x", "y")
  out
}

#' Summarise per-trial metadata for a single video.
#'
#' Uses paired landmark coordinates to determine the temporal bounds of each
#' trial, the arena origin, and the stimulus heading. Additional metadata from
#' `file_tbl` is merged into the result. Accepts landmark and track data either
#' as data frames or as `TrajSet` objects.
#'
#' @param landmarks Data frame or `TrajSet` (two rows per trial) containing frame
#'   numbers and landmark coordinates.
#' @param animal_track Data frame or `TrajSet` of Cartesian coordinates for all
#'   frames in the video.
#' @param file_tbl Tibble returned by [import_tracks()] (optionally enriched by
#'   [load_tracks()] or [load_tracks2()]).
#' @param vid_num Index of the current video within `file_tbl`.
#' @param midpoint Character flag indicating whether stimulus angles should be
#'   shifted to the midpoint (`"midpoint"`, default) or left as-is.
#'
#' @return A tibble with one row per trial containing trial limits and stimulus
#'   metadata.
#'
#' @importFrom tibble as_tibble
#' @export
get_trial_limits <- function(landmarks, animal_track, file_tbl, vid_num,
                             midpoint = "midpoint") {
  landmarks_df <- .coerce_xy_frame(landmarks, "landmark")
  track_df <- .coerce_xy_frame(animal_track, "track")

  num_trials <- nrow(landmarks_df) / 2
  if (num_trials < 1 || num_trials != floor(num_trials)) {
    stop("Landmark data must contain an even number of rows (two per trial).")
  }

  tl <- tibble::as_tibble(array(
    0,
    dim = c(num_trials, 18),
    dimnames = list(NULL, c(
      "first_f", "orig_x", "orig_y", "stim_x", "stim_y",
      "last_f", "stim_x_0", "stim_y_0", "stim_theta", "r_px",
      "quadrant", "video", "order", "vid_ord", "x0", "y0", "x1", "y1"
    ))
  ), .name_repair = "minimal")

  tl$video <- as.character(tl$video)
  landmarks_df$y <- -landmarks_df$y

  for (i in seq_len(num_trials)) {
    tl$first_f[i] <- landmarks_df[(i * 2) - 1, "frame", drop = TRUE]
    tl$orig_x[i]  <- landmarks_df[(i * 2) - 1, "x", drop = TRUE]
    tl$orig_y[i]  <- landmarks_df[(i * 2) - 1, "y", drop = TRUE]
    tl$stim_x[i]  <- landmarks_df[i * 2, "x", drop = TRUE]
    tl$stim_y[i]  <- landmarks_df[i * 2, "y", drop = TRUE]
    if (i < num_trials) {
      tl$last_f[i] <- landmarks_df[((i + 1) * 2) - 1, "frame", drop = TRUE] - 1
    } else {
      tl$last_f[i] <- max(track_df$frame, na.rm = TRUE)
    }
  }

  tl$video <- file_tbl$basename[vid_num]
  for (col in c("arc", "type", "obstacle", "id")) {
    if (col %in% names(file_tbl)) {
      tl[[col]] <- file_tbl[[col]][file_tbl$basename == tl$video]
    }
  }

  tl$stim_x_0 <- tl$stim_x - tl$orig_x
  tl$stim_y_0 <- tl$stim_y - tl$orig_y
  tl$stim_theta <- atan2(tl$stim_y_0, tl$stim_x_0)
  tl$r_px <- sqrt(tl$stim_x_0^2 + tl$stim_y_0^2)

  if (identical(midpoint, "midpoint") && "type" %in% names(tl) && tl$type[1] == "Herm") {
    stim_theta <- mapply(rad2clock, tl$stim_theta)
    stim_theta <- stim_theta - tl$arc * (pi / 360)
    stim_theta <- mapply(rad_shepherd_clock, stim_theta)
    tl$stim_theta <- mapply(rad_unclock, stim_theta)
  }

  tl$stim_x_0 <- cos(tl$stim_theta)
  tl$stim_y_0 <- sin(tl$stim_theta)

  tl$quadrant <- "right"
  tl$quadrant[tl$stim_theta > .25 * pi & tl$stim_theta <= .75 * pi] <- "top"
  tl$quadrant[tl$stim_theta >= -.25 * pi & tl$stim_theta < .25 * pi] <- "left"
  tl$quadrant[tl$stim_theta >= -.75 * pi & tl$stim_theta < -.25 * pi] <- "bottom"

  tl
}

#' Derive trial-level track positions in polar coordinates.
#'
#' Using the trial limits returned by [get_trial_limits()], this helper extracts
#' the corresponding rows from a track data frame or `TrajSet`, centres and
#' scales the coordinates, and computes angles in both absolute and
#' stimulus-relative frames. The function optionally controls how inner/outer
#' radius crossings are selected.
#'
#' @param trial_limits Data frame produced by [get_trial_limits()].
#' @param animal_track Data frame or `TrajSet` of Cartesian coordinates for the
#'   entire video.
#' @param circ0 Inner radius threshold (default `0.1`).
#' @param circ1 Outer radius threshold (default `0.2`).
#' @param radius_criterion Strategy for choosing the radius landmarks. `"first_past"`
#'   selects the first point beyond each threshold, while `"closest"` chooses the
#'   closest sample to the specified radius.
#' @return A list containing the per-trial track tibbles and the updated
#'   `trial_limits` data frame.
#'
#' @importFrom purrr compact map
#' @importFrom tibble tibble add_column as_tibble
#' @export
get_tracked_object_pos <- function(
    trial_limits, animal_track,
    circ0 = 0.1, circ1 = 0.2, radius_criterion = c("first_past", "closest")) {

  radius_criterion <- match.arg(radius_criterion)
  track_df <- .coerce_xy_frame(animal_track, "track")
  track_df$y <- -track_df$y

  num_trials <- nrow(trial_limits)
  if (!"valid_track" %in% names(trial_limits)) {
    trial_limits$valid_track <- TRUE
  }

  trackz <- purrr::map(vector(length = num_trials), tibble::tibble)

  for (i in seq_len(num_trials)) {
    idx <- track_df$frame >= trial_limits$first_f[i] &
      track_df$frame <= trial_limits$last_f[i]
    XY <- tibble::as_tibble(track_df[idx, , drop = FALSE])

    XY <- XY |>
      tibble::add_column(
        trans_x = 0, trans_y = 0, abs_theta = 0,
        trans_rho = 0, video = NA_character_,
        order = NA_character_, vid_ord = NA_character_, rel_theta = 0
      )

    XY$trans_x <- (XY$x - trial_limits$orig_x[i]) / trial_limits$r_px[i]
    XY$trans_y <- (XY$y - trial_limits$orig_y[i]) / trial_limits$r_px[i]

    abs_theta <- atan2(XY$trans_y, XY$trans_x)
    XY$abs_theta <- mapply(rad2clock, abs_theta)
    XY$trans_rho <- sqrt(XY$trans_x^2 + XY$trans_y^2)
    XY$video <- trial_limits$video[i]
    XY$order <- as.character(i)
    XY$vid_ord <- paste(XY$video, XY$order, sep = "_")

    stim_theta <- rad2clock(trial_limits$stim_theta[i])
    XY$rel_theta <- XY$abs_theta - stim_theta
    XY$rel_theta <- mapply(rad_shepherd_clock, XY$rel_theta)
    XY$rel_theta <- mapply(rad_unclock, XY$rel_theta)
    XY$abs_theta <- mapply(rad_unclock, XY$abs_theta)
    XY$rel_x <- XY$trans_rho * cos(XY$rel_theta)
    XY$rel_y <- XY$trans_rho * sin(XY$rel_theta)
    trackz[[i]] <- XY

    if (all(XY$trans_rho > .4, na.rm = TRUE)) {
      trackz[[i]] <- NULL
      trial_limits$valid_track[i] <- FALSE
      warning("Track starts too far from centre: ", XY$video[1])
      next
    } else {
      trial_limits$valid_track[i] <- TRUE
    }
    if (any(XY$trans_rho > 1, na.rm = TRUE)) {
      warning("Track exceeds arena width: ", XY$video[1])
    }

    trial_limits$order[i] <- as.character(i)
    trial_limits$vid_ord[i] <- paste0(trial_limits$video[i], "_", i)

    if (radius_criterion == "first_past") {
      if (!any(XY$trans_rho > circ0, na.rm = TRUE)) {
        warning("No points beyond inner radius for ", XY$video[1])
      }
      inner_idx <- which(XY$trans_rho >= circ0)
      inner_idx <- if (length(inner_idx)) inner_idx[1] else which.max(XY$trans_rho)
      outer_idx <- which(XY$trans_rho >= circ1)
      outer_idx <- if (length(outer_idx)) outer_idx[1] else which.max(XY$trans_rho)
    } else {
      inner_idx <- which.min(abs(XY$trans_rho - circ0))
      outer_idx <- which.min(abs(XY$trans_rho - circ1))
    }

    trial_limits$x0[i] <- XY$trans_x[inner_idx]
    trial_limits$y0[i] <- XY$trans_y[inner_idx]
    trial_limits$x1[i] <- XY$trans_x[outer_idx]
    trial_limits$y1[i] <- XY$trans_y[outer_idx]
  }

  list(purrr::compact(trackz), trial_limits)
}

#' Aggregate track positions across all videos in a manifest.
#'
#' Iterates over the rows of `file_tbl`, reading the paired landmark and track
#' files for each video before computing trial limits and normalised
#' coordinates. The per-trial outputs are combined into a single tibble, and
#' the augmented trial limits are returned alongside it.
#'
#' @param landmarks Optional data frame or `TrajSet` for the first entry. Retained
#'   for backwards compatibility; values are overwritten internally.
#' @param animal_track Optional data frame or `TrajSet` for the first entry.
#' @param file_tbl Tibble produced by [import_tracks()], optionally enriched via
#'   [load_tracks()] or [load_tracks2()].
#' @param track_dir Directory containing the landmark and track text files.
#'
#' @return A list with two elements: a tibble of all valid track observations
#'   and the corresponding trial limits tibble.
#'
#' @importFrom purrr compact
#' @importFrom tibble tibble as_tibble
#' @export
get_all_object_pos <- function(landmarks = NULL, animal_track = NULL,
                               file_tbl, track_dir) {
  all_trackz <- list()
  trialz <- NULL

  for (i in seq_len(nrow(file_tbl))) {
    animal_track_df <- utils::read.delim(
      normalizePath(file.path(track_dir, file_tbl$track[i])),
      sep = "\t", header = FALSE
    )[, 1:3]
    names(animal_track_df) <- c("frame", "x", "y")
    landmarks_df <- utils::read.delim(
      normalizePath(file.path(track_dir, file_tbl$landmark[i])),
      sep = "\t", header = FALSE
    )[, 1:3]
    names(landmarks_df) <- c("frame", "x", "y")
    if (nrow(landmarks_df) %% 2 == 1) {
      warning("Odd number of landmarks: ", file_tbl$basename[i])
    }
    trial_limits <- get_trial_limits(landmarks_df, animal_track_df, file_tbl, i)
    trial_track_list <- get_tracked_object_pos(trial_limits, animal_track_df)
    all_trackz <- c(all_trackz, trial_track_list[[1]])
    if (is.null(trialz)) {
      trialz <- trial_track_list[[2]]
    } else {
      trialz <- rbind(trialz, trial_track_list[[2]])
    }
  }

  valid_tracks <- purrr::compact(all_trackz)
  if (length(valid_tracks) == 0) {
    warning("No valid tracks were produced for the supplied files.")
    trackz_tbl <- tibble::tibble()
  } else {
    trackz_tbl <- tibble::as_tibble(do.call(rbind, valid_tracks))
  }

  list(trackz_tbl, trialz)
}
