#' Apply a bespoke transformation to a Tracks
#'
#' Applies a user-supplied function to a loaded [`Tracks`] -- for example a
#' reference-frame correction or an angular remapping -- after loading and before
#' summary or plotting, returning a modified `Tracks` and recording the step in
#' its [`transform_history`]. The function is written against the `Tracks`'s
#' column \emph{roles} (`x@cols`), not hard-coded column names.
#'
#' @param x A [`Tracks`].
#' @param fn A function. With `by = "trajectory"` (default) it is called once per
#'   trajectory as `fn(df, cols, ...)`, where `df` is that trajectory's rows (all
#'   columns, including metadata covariates) and `cols` is `x@cols`. With
#'   `by = "all"` it is called once on the whole data frame. It must return a data
#'   frame with the same rows (same `id`/`time`); a transform adjusts values, it
#'   does not add or drop trials.
#' @param ... Further arguments passed to `fn`.
#' @param by Either `"trajectory"` (default; per-trajectory) or `"all"`
#'   (whole-frame).
#' @param step Label recorded in `transform_history`. Defaults to the name of
#'   `fn`, or `"apply_transform"` for anonymous functions.
#'
#' @return A [`Tracks`] with `@data` replaced by the transformed result and one
#'   step appended to its `transform_history`.
#'
#' @seealso [transform_history()], [log_transform()]
#' @examples
#' data(cpunctatus)
#'
#' # Recipe A -- a reference-frame offset (e.g. edge-referenced -> centre-
#' # referenced stimulus) is a frame change, so use set_reference(), not
#' # apply_transform(): it re-derives rel_theta/rel_x/rel_y consistently. Because
#' # rel_theta = abs - reference, offsetting the heading by +half (half the
#' # stimulus's angular width) is a reference change of (reference - half).
#' ts     <- set_reference(cpunctatus, 0)        # ensure a reference exists
#' half   <- (20 / 2) * pi / 180                 # a 20-degree-wide stimulus
#' newref <- reference(ts)$ref_theta - half      # per-trajectory if widths differ
#' ts2    <- set_reference(ts, stats::setNames(newref, reference(ts)$id))
#'
#' # Recipe B -- polarization direction -> axis. Polarized-light e-vectors are
#' # axial (defined mod pi); doubling folds direction into orientation for axial
#' # circular statistics. (Doubling remaps the angular space, so rel_x/rel_y are
#' # not physical positions afterwards -- use it for the angle/stat path.)
#' direction_to_axis <- function(df, cols) {
#'   df[[cols$angle]] <- (2 * df[[cols$angle]]) %% (2 * pi)
#'   df
#' }
#' ts_axial <- apply_transform(cpunctatus, direction_to_axis, by = "all",
#'                             step = "direction_to_axis")
#' @export
apply_transform <- function(x, fn, ..., by = c("trajectory", "all"), step = NULL) {
  if (!methods::is(x, "Tracks")) stop("`x` must be a Tracks.")
  if (!is.function(fn))           stop("`fn` must be a function.")
  by <- match.arg(by)

  if (is.null(step)) {
    step <- tryCatch(deparse(substitute(fn))[1L], error = function(e) "apply_transform")
    if (!nzchar(step) || grepl("^function|^\\(", step)) step <- "apply_transform"
  }

  cols <- x@cols
  d    <- x@data
  idc  <- cols$id

  if (identical(by, "all")) {
    out <- fn(d, cols, ...)
    if (!is.data.frame(out))
      stop("`fn` must return a data frame.")
    if (nrow(out) != nrow(d))
      stop("`fn` must return the same number of rows it was given.")
  } else {
    idx   <- split(seq_len(nrow(d)), d[[idc]])
    parts <- lapply(idx, function(ii) fn(d[ii, , drop = FALSE], cols, ...))
    for (i in seq_along(parts)) {
      if (!is.data.frame(parts[[i]]))
        stop("`fn` must return a data frame.")
      if (nrow(parts[[i]]) != length(idx[[i]]))
        stop("`fn` must return the same number of rows it was given.")
    }
    out <- do.call(rbind, parts)
    out <- out[order(unlist(idx, use.names = FALSE)), , drop = FALSE]
    rownames(out) <- NULL
  }

  if (!identical(out[[idc]], d[[idc]]))
    stop("`fn` must not change the id column.")

  x@data <- out
  methods::validObject(x)
  dots <- list(...)
  log_transform(x, step = step, implementation = step,
                 params = if (length(dots)) list(dots) else NULL)
}

#' Restrict a Tracks to within the unit circle
#'
#' Removes out-of-circumference (`rho > 1`) track data so that downstream
#' kinematics (`track_speed()`, `instantaneous_speed()`, `track_length()`, the
#' `path_*` shape metrics, ...) are computed only on the within-circle portion of
#' each trajectory. Distance from the centre, `rho`, is taken from the relative
#' (unit-circle) coordinates `rel_x`/`rel_y`, where `rho = 1` is the
#' circumference.
#'
#' This is the data-filtering counterpart to the plot-only
#' `radiate(clip_tracks = TRUE)`: `clip_tracks` only changes what is *drawn* (on
#' a copy), whereas `restrict_to_circumference()` returns a new `Tracks` whose
#' data the metrics actually see.
#'
#' @param ts A `Tracks` carrying relative coordinates (`rel_x`/`rel_y`).
#' @param mode How to exclude out-of-circumference data per trajectory.
#'   `"truncate"` (default) keeps the contiguous prefix up to (excluding) the
#'   first point beyond the circumference -- the centre-to-circumference approach
#'   segment, with no gaps. `"drop"` removes individual beyond-circumference
#'   points only; note that dropping interior points lengthens the step bridging
#'   the gap, which biases speed and path length, so `"truncate"` is usually
#'   preferable for kinematics.
#' @param max_radius Circumference radius. A point is beyond the circumference
#'   when `rho > max_radius` (with a small rim tolerance so points exactly on the
#'   circumference are kept). Default `1`.
#' @return A new `Tracks` (the input is unmodified) with out-of-circumference
#'   rows removed and a `transform_history` entry recording the operation.
#'   Trajectories reduced to no rows are dropped.
#' @seealso [radiate()] (`clip_tracks`), [track_length()], [track_speed()],
#'   [transform_history()]
#' @export
restrict_to_circumference <- function(ts, mode = c("truncate", "drop"),
                                      max_radius = 1) {
  if (!methods::is(ts, "Tracks")) stop("`ts` must be a Tracks.")
  mode <- match.arg(mode)
  if (!is.numeric(max_radius) || length(max_radius) != 1L ||
      !is.finite(max_radius) || max_radius <= 0)
    stop("`max_radius` must be a single positive number.")

  rxc <- ts@cols$rel_x; ryc <- ts@cols$rel_y
  if (is.null(rxc) || is.null(ryc) || !all(c(rxc, ryc) %in% names(ts@data)))
    stop("restrict_to_circumference() needs relative (unit-circle) coordinates ",
         "(rel_x/rel_y); this Tracks has none.")

  d   <- ts@data
  idc <- ts@cols$id
  tc  <- ts@cols$time
  lim <- max_radius^2 + 1e-9   # rim tolerance: keep points exactly on the circumference

  n_before   <- nrow(d)
  ids_before <- unique(d[[idc]])

  keep_idx <- unlist(lapply(split(seq_len(nrow(d)), d[[idc]]), function(ii) {
    sub <- if (!is.null(tc) && tc %in% names(d)) ii[order(d[[tc]][ii])] else ii
    rho2   <- d[[rxc]][sub]^2 + d[[ryc]][sub]^2
    beyond <- is.finite(rho2) & rho2 > lim          # NA coords are kept (not "beyond")
    if (identical(mode, "truncate")) {
      first <- which(beyond)[1L]
      if (is.na(first)) sub
      else if (first == 1L) integer(0)
      else sub[seq_len(first - 1L)]
    } else {
      sub[!beyond]
    }
  }), use.names = FALSE)

  keep_idx <- sort(keep_idx)
  out_d <- d[keep_idx, , drop = FALSE]
  rownames(out_d) <- NULL

  n_rows_removed   <- n_before - nrow(out_d)
  n_tracks_removed <- length(setdiff(ids_before, unique(out_d[[idc]])))
  if (n_rows_removed > 0L)
    message(sprintf(
      "restrict_to_circumference(%s): removed %d of %d rows%s.",
      mode, n_rows_removed, n_before,
      if (n_tracks_removed > 0L)
        sprintf(" (%d track%s fully removed)", n_tracks_removed,
                if (n_tracks_removed == 1L) "" else "s") else ""))

  out <- ts
  out@data <- out_d
  p <- list(mode = mode, max_radius = max_radius,
            n_rows_removed = n_rows_removed, n_tracks_removed = n_tracks_removed)
  surviving <- ids(out)
  if (length(surviving) > 0L)
    out <- log_transform(out, step = "restrict_to_circumference",
                         traj_ids = surviving, params = list(p))
  out
}
