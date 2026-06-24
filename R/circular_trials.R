# Circular trial helpers (limits, track extraction, aggregation)
#

.coerce_xy_frame <- function(obj, role = c("track", "landmark")) {
  role <- match.arg(role)
  if (inherits(obj, "Tracks")) {
    df <- as.data.frame(obj)
    time_col <- obj@cols$time
    x_col <- obj@cols$x
    y_col <- obj@cols$y
    if (is.null(time_col) || is.null(x_col) || is.null(y_col)) {
      stop("Tracks object must define time/x/y columns for ", role, ".")
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
    stop("Unsupported ", role, " input: must be data frame or Tracks.")
  }
  names(out) <- c("frame", "x", "y")
  out
}

#' Summarise per-trial metadata for a single video.
#'
#' Uses paired landmark coordinates to determine the temporal bounds of each
#' trial, the origin, and the reference heading. Additional metadata from
#' `file_tbl` is merged into the result. Accepts landmark and track data either
#' as data frames or as `Tracks` objects.
#'
#' @param landmarks Data frame or `Tracks` (two rows per trial) containing frame
#'   numbers and landmark coordinates.
#' @param track Data frame or `Tracks` of Cartesian coordinates for all
#'   frames in the video.
#' @param file_tbl Tibble returned by [import_tracks()] (optionally enriched by
#'   [load_tracks()] or [load_tracks2()]).
#' @param vid_num Index of the current video within `file_tbl`.
#'
#' @return A tibble with one row per trial containing trial limits and reference
#'   metadata.
#'
#' @importFrom tibble as_tibble
#' @export
get_trial_limits <- function(landmarks, track, file_tbl, vid_num) {
  landmarks_df <- .coerce_xy_frame(landmarks, "landmark")
  track_df <- .coerce_xy_frame(track, "track")

  num_trials <- nrow(landmarks_df) / 2
  if (num_trials < 1 || num_trials != floor(num_trials)) {
    stop("Landmark data must contain an even number of rows (two per trial).")
  }

  tl <- tibble::as_tibble(array(
    0,
    dim = c(num_trials, 18),
    dimnames = list(NULL, c(
      "first_f", "orig_x", "orig_y", "ref_x", "ref_y",
      "last_f", "ref_x_0", "ref_y_0", "ref_theta", "r_px",
      "quadrant", "video", "order", "vid_ord", "x0", "y0", "x1", "y1"
    ))
  ), .name_repair = "minimal")

  tl$video <- as.character(tl$video)

  for (i in seq_len(num_trials)) {
    origin <- landmarks_df[(i * 2) - 1, c("x", "y"), drop = TRUE]
    reference <- landmarks_df[i * 2, c("x", "y"), drop = TRUE]

    tl$first_f[i] <- landmarks_df[(i * 2) - 1, "frame", drop = TRUE]
    tl$orig_x[i]  <- origin[["x"]]
    tl$orig_y[i]  <- origin[["y"]]
    tl$ref_x[i]  <- reference[["x"]]
    tl$ref_y[i]  <- reference[["y"]]
    if (i < num_trials) {
      tl$last_f[i] <- landmarks_df[((i + 1) * 2) - 1, "frame", drop = TRUE] - 1
    } else {
      tl$last_f[i] <- max(track_df$frame, na.rm = TRUE)
    }

    mapping <- build_unit_circle_mapping(origin, reference, flip_y = TRUE)
    tl$r_px[i] <- mapping$radius
    tl$ref_theta[i] <- mapping$ref_theta_unit
    tl$ref_x_0[i] <- cos(mapping$ref_theta_unit)
    tl$ref_y_0[i] <- sin(mapping$ref_theta_unit)
  }

  tl$video <- file_tbl$basename[vid_num]
  # Carry every manifest/metadata column from file_tbl onto the trial table.
  # Structural file-reference columns (basename/landmark/track) are excluded, as
  # are names already used by the trial table (so user metadata can't clobber
  # computed columns like ref_theta/quadrant).
  structural <- c("basename", "landmark", "track")
  carry_cols <- setdiff(names(file_tbl), c(structural, names(tl)))
  for (col in carry_cols) {
    tl[[col]] <- file_tbl[[col]][file_tbl$basename == tl$video]
  }

  theta <- tl$ref_theta
  quadrant <- rep(NA_character_, length(theta))
  quadrant[theta < pi / 4 | theta >= 7 * pi / 4] <- "right"
  quadrant[theta >= pi / 4 & theta < 3 * pi / 4] <- "top"
  quadrant[theta >= 5 * pi / 4 & theta < 7 * pi / 4] <- "bottom"
  quadrant[theta >= 3 * pi / 4 & theta < 5 * pi / 4] <- "left"
  tl$quadrant <- quadrant

  tl
}

# Map one trial's frame-windowed track to unit-circle coordinates and pick its
# inner/outer radius-crossing landmarks. `tl` is the one-row trial_limits slice
# for trial `i`. Returns a list with the augmented track tibble (`track`), its
# transform-history `entry`, validity, the per-trial trial_limits updates
# (`order`/`vid_ord`/`landmarks`), and the out-of-bounds point count. A track
# whose points never come near the centre (rho > 0.4 throughout) is rejected:
# `track`/`entry` are NULL, `valid` FALSE, and the order/vid_ord/landmark/OOB
# updates are omitted (a warning is emitted), matching the original loop.
.map_one_trial <- function(tl, track_df, i, circ0, circ1, radius_criterion) {
  idx <- track_df$frame >= tl$first_f & track_df$frame <= tl$last_f
  XY  <- tibble::as_tibble(track_df[idx, , drop = FALSE])
  trial_id <- paste0(tl$video, "_", i)

  mapping <- build_unit_circle_mapping(
    origin    = c(tl$orig_x, tl$orig_y),
    reference = c(tl$ref_x, tl$ref_y),
    flip_y    = TRUE
  )
  mapped <- mapping$map(XY$x, XY$y)

  XY <- XY |>
    tibble::add_column(
      trans_x   = mapped$trans_x,
      trans_y   = mapped$trans_y,
      rel_theta = mapped$rel_theta_unit,
      video     = tl$video,
      order     = as.character(i),
      vid_ord   = trial_id,
      .after    = "y"
    )

  entry <- list(
    step = "unit_circle_mapping",
    order = 1L,
    id = trial_id,
    implementation = "build_unit_circle_mapping",
    params = mapping,
    depends_on = character()
  )

  # Reject a track that never comes near the centre.
  if (all(mapped$trans_rho > .4, na.rm = TRUE)) {
    warning("Track starts too far from centre: ", tl$video)
    return(list(track = NULL, transform_entry = NULL, valid = FALSE,
                order = NULL, vid_ord = NULL, landmarks = NULL, oob_n = 0L))
  }

  if (radius_criterion == "first_past") {
    if (!any(mapped$trans_rho > circ0, na.rm = TRUE))
      warning("No points beyond inner radius for ", tl$video)
    inner_idx <- which(mapped$trans_rho >= circ0)
    inner_idx <- if (length(inner_idx)) inner_idx[1] else which.max(mapped$trans_rho)
    outer_idx <- which(mapped$trans_rho >= circ1)
    outer_idx <- if (length(outer_idx)) outer_idx[1] else which.max(mapped$trans_rho)
  } else {
    inner_idx <- which.min(abs(mapped$trans_rho - circ0))
    outer_idx <- which.min(abs(mapped$trans_rho - circ1))
  }

  list(
    track = XY,
    transform_entry = entry,
    valid = TRUE,
    order = as.character(i),
    vid_ord = trial_id,
    landmarks = list(x0 = mapped$trans_x[inner_idx], y0 = mapped$trans_y[inner_idx],
                     x1 = mapped$trans_x[outer_idx], y1 = mapped$trans_y[outer_idx]),
    oob_n = sum(mapped$trans_rho > 1, na.rm = TRUE)
  )
}

#' Derive trial-level track positions in polar coordinates.
#'
#' Using the trial limits returned by [get_trial_limits()], this helper extracts
#' the corresponding rows from a track data frame or `Tracks`, centres and
#' scales the coordinates, and computes angles in both absolute and
#' reference-relative frames. The function optionally controls how inner/outer
#' radius crossings are selected.
#'
#' @param trial_limits Data frame produced by [get_trial_limits()].
#' @param track Data frame or `Tracks` of Cartesian coordinates for the
#'   entire video.
#' @param circ0 Inner radius threshold (default `0.1`).
#' @param circ1 Outer radius threshold (default `0.2`).
#' @param radius_criterion Strategy for choosing the radius landmarks. `"first_past"`
#'   selects the first point beyond each threshold, while `"closest"` chooses the
#'   closest sample to the specified radius.
#' @return A `Tracks` containing all valid trial observations. The corresponding
#'   trial limits (including `valid_track` flags) are stored in
#'   `meta$trial_limits`.
#'
#' @importFrom purrr compact
#' @importFrom tibble tibble add_column as_tibble
#' @export
get_tracked_object_pos <- function(
    trial_limits, track,
    circ0 = 0.1, circ1 = 0.2, radius_criterion = c("first_past", "closest")) {

  radius_criterion <- match.arg(radius_criterion)
  track_df <- .coerce_xy_frame(track, "track")

  num_trials <- nrow(trial_limits)
  if (!"valid_track" %in% names(trial_limits)) {
    trial_limits$valid_track <- TRUE
  }

  trial_tracks <- vector("list", num_trials)
  transform_entries <- vector("list", num_trials)

  # Landmark-mapped coordinates are kept as-is (never rescaled); track points that
  # fall outside the circumference (radius > 1) are tallied and reported once
  # after the loop rather than warned about per trial.
  oob_points <- 0L
  oob_trials <- 0L

  for (i in seq_len(num_trials)) {
    res <- .map_one_trial(trial_limits[i, , drop = FALSE], track_df, i,
                          circ0, circ1, radius_criterion)
    # Assigning NULL via [[<-] drops the slot; `purrr::compact()` later removes
    # the rest, and each track tibble carries its own vid_ord, so list position
    # is immaterial downstream.
    trial_tracks[[i]]           <- res$track
    transform_entries[[i]]      <- res$transform_entry
    trial_limits$valid_track[i] <- res$valid
    if (res$valid) {
      trial_limits$order[i]   <- res$order
      trial_limits$vid_ord[i] <- res$vid_ord
      trial_limits$x0[i] <- res$landmarks$x0
      trial_limits$y0[i] <- res$landmarks$y0
      trial_limits$x1[i] <- res$landmarks$x1
      trial_limits$y1[i] <- res$landmarks$y1
      oob_points <- oob_points + res$oob_n
      if (res$oob_n > 0L) oob_trials <- oob_trials + 1L
    }
  }

  if (oob_points > 0L) {
    message(sprintf(
      "%d point%s across %d trial%s exceeded the unit circle boundary (radius > 1); coordinates left unscaled.",
      oob_points, if (oob_points == 1L) "" else "s",
      oob_trials, if (oob_trials == 1L) "" else "s"))
  }

  trial_tracks <- purrr::compact(trial_tracks)
  transform_entries <- purrr::compact(transform_entries)
  transform_history <- .empty_transform_history()
  if (length(transform_entries)) {
    for (entry in transform_entries) {
      transform_history <- tibble::add_row(
        transform_history,
        step = entry$step,
        order = entry$order,
        id = entry$id,
        implementation = entry$implementation,
        params = list(entry$params),
        depends_on = list(entry$depends_on)
      )
    }
  }

  if (!length(trial_tracks)) {
    return(NULL)
  }

  combined <- tibble::as_tibble(do.call(rbind, trial_tracks))
  combined <- tibble::add_column(
    combined,
    trial_id = combined$vid_ord,
    .before = "frame"
  )

  trajset <- tracks(
    combined,
    id = "trial_id",
    time = "frame",
    angle = "rel_theta",
    x = "trans_x",
    y = "trans_y",
    rel_x = "rel_x",
    rel_y = "rel_y",
    angle_unit = "radians",
    normalize_xy = FALSE,
    meta = list(source = "get_tracked_object_pos")
  )
  trajset <- set_transform_history(trajset, transform_history)
  trajset@meta$trial_limits        <- trial_limits
  present_ids <- unique(combined$trial_id)
  ref_tbl <- tibble::tibble(id = trial_limits$vid_ord,
                            ref_theta = trial_limits$ref_theta)
  trajset@meta$reference <- ref_tbl[ref_tbl$id %in% present_ids, , drop = FALSE]
  trajset@meta$display_convention  <- "clock"
  trajset@meta$plot_x_col          <- "rel_x"
  trajset@meta$plot_y_col          <- "rel_y"

  trajset
}

#' Aggregate track positions across all videos in a manifest.
#'
#' Iterates over the rows of `file_tbl`, reading the paired landmark and track
#' files for each video before computing trial limits and normalised
#' coordinates. The per-trial outputs are combined into a single tibble, and
#' the augmented trial limits are returned alongside it.
#'
#' @param landmarks Optional data frame or `Tracks` for the first entry. Retained
#'   for backwards compatibility; values are overwritten internally.
#' @param track Optional data frame or `Tracks` for the first entry.
#' @param file_tbl Tibble produced by [import_tracks()], optionally enriched via
#'   [load_tracks()] or [load_tracks2()].
#' @param track_dir Directory containing the landmark and track text files.
#'
#' @return A `Tracks` combining the normalised observations for all valid
#'   trials in the manifest. The aggregated trial limits are available via
#'   `meta$trial_limits`.
#'
#' @importFrom utils read.delim
#' @export
get_all_object_pos <- function(landmarks = NULL, track = NULL,
                               file_tbl, track_dir) {
  trajsets <- list()
  trial_limits_list <- list()

  for (i in seq_len(nrow(file_tbl))) {
    track_df <- utils::read.delim(
      normalizePath(file.path(track_dir, file_tbl$track[i])),
      sep = "\t", header = FALSE
    )[, 1:3]
    names(track_df) <- c("frame", "x", "y")
    landmarks_df <- utils::read.delim(
      normalizePath(file.path(track_dir, file_tbl$landmark[i])),
      sep = "\t", header = FALSE
    )[, 1:3]
    names(landmarks_df) <- c("frame", "x", "y")
    if (nrow(landmarks_df) %% 2 == 1) {
      warning("Odd number of landmarks: ", file_tbl$basename[i])
    }
    trial_limits <- get_trial_limits(landmarks_df, track_df, file_tbl, i)
    ts <- get_tracked_object_pos(trial_limits, track_df)
    if (!is.null(ts)) {
      trajsets <- c(trajsets, list(ts))
      trial_limits_list <- c(trial_limits_list, list(ts@meta$trial_limits))
    }
  }

  if (!length(trajsets)) {
    warning("No valid tracks were produced for the supplied files.")
    return(NULL)
  }

  combined_ts <- trajsets[[1]]
  if (length(trajsets) > 1) {
    for (j in 2:length(trajsets)) {
      combined_ts <- c(combined_ts, trajsets[[j]])
    }
  }

  if (length(trial_limits_list)) {
    combined_ts@meta$trial_limits <- do.call(rbind, trial_limits_list)
  }

  combined_ts
}
