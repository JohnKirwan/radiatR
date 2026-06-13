#' Apply a bespoke transformation to a TrajSet
#'
#' Applies a user-supplied function to a loaded [`TrajSet`] -- for example a
#' reference-frame correction or an angular remapping -- after loading and before
#' summary or plotting, returning a modified `TrajSet` and recording the step in
#' its [`transform_history`]. The function is written against the `TrajSet`'s
#' column \emph{roles} (`x@cols`), not hard-coded column names.
#'
#' @param x A [`TrajSet`].
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
#' @return A [`TrajSet`] with `@data` replaced by the transformed result and one
#'   step appended to its `transform_history`.
#'
#' @seealso [transform_history()], [log_transform()]
#' @examples
#' data(cpunctatus)
#'
#' # Recipe A -- edge-referenced -> centre-referenced stimulus. Many experiments
#' # mark a stimulus by its (precisely digitisable) leading edge; to analyse
#' # headings relative to the stimulus *centre*, offset the reference by half the
#' # angular width, which may differ per trial. Here `fn` reads a per-trial
#' # `width` column from the data. (A constant offset -- e.g. a fixed magnetic
#' # declination -- would just skip the per-trial column.)
#' edge_to_centre <- function(df, cols, width_col, units = "degrees") {
#'   half <- (df[[width_col]] / 2) * (if (units == "degrees") pi / 180 else 1)
#'   ang  <- (df[[cols$angle]] + half) %% (2 * pi)
#'   df[[cols$angle]] <- ang
#'   rho <- df[[cols$rho]]
#'   df[[cols$rel_x]] <- rho * cos(ang)        # keep rel_x/rel_y consistent
#'   df[[cols$rel_y]] <- rho * sin(ang)
#'   df
#' }
#' ts <- cpunctatus
#' ts@data$width <- 20                          # demo: a 20-degree-wide stimulus
#' ts2 <- apply_transform(ts, edge_to_centre, width_col = "width",
#'                        units = "degrees", step = "edge_to_centre")
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
  if (!methods::is(x, "TrajSet")) stop("`x` must be a TrajSet.")
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
