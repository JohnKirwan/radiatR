# Unit of a Tracks' numeric time column: "seconds" when its metadata says so,
# otherwise a frame index (NULL or "frames"). read_tracks() records "seconds"
# for time_type = "seconds" and for duration columns.
.time_unit <- function(ts) {
  u <- ts@meta$time_unit
  if (!is.null(u) && !(identical(u, "seconds") || identical(u, "frames")))
    stop("Tracks metadata `time_unit` must be \"seconds\" or \"frames\", not ",
         paste(deparse(u), collapse = " "), ".", call. = FALSE)
  u
}

# Per-id elapsed time, in the requested units, measured from each track's
# minimum time. POSIXct and numeric seconds are used as they are (frame rate
# not needed, and not applied). Numeric frame index -> (frame - min)/fps; a
# frame rate is required.
.elapsed_time <- function(time, id, fps = NULL,
                          units = c("seconds", "minutes", "milliseconds"),
                          time_unit = NULL) {
  units <- match.arg(units)
  secs <- if (inherits(time, "POSIXct") || identical(time_unit, "seconds")) {
    as.numeric(time)
  } else {
    if (is.null(fps))
      stop("numeric (frame) time needs a frame rate; set one with ",
           "`set_frame_rate(ts, fps)`, or load with `time_type = \"seconds\"` ",
           "if the column already holds seconds.", call. = FALSE)
    as.numeric(time) / fps
  }
  el <- stats::ave(secs, id, FUN = function(s) s - min(s, na.rm = TRUE))
  switch(units, seconds = el, milliseconds = el * 1000, minutes = el / 60)
}

#' Elapsed time per observation of a Tracks object
#'
#' Real elapsed time of each point from its own track's start, computed on
#' demand from the time column and (for frame-indexed time) the [frame_rate()].
#' POSIXct time is used directly, and so is numeric time loaded as seconds
#' (`read_tracks(time_type = "seconds")`, or a duration column): a frame rate
#' is neither needed nor applied to it. Any other numeric time is a frame index
#' and requires a frame rate.
#'
#' @param ts A [Tracks] object.
#' @param units `"seconds"` (default), `"minutes"`, or `"milliseconds"`.
#' @return A numeric vector aligned to the object's observations.
#' @seealso [frame_rate()], [track_duration()]
#' @examples
#' head(elapsed_seconds(cpunctatus))
#' @export
elapsed_seconds <- function(ts, units = c("seconds", "minutes", "milliseconds")) {
  units <- match.arg(units)
  d <- as.data.frame(ts)
  .elapsed_time(d[[ts@cols$time]], d[[ts@cols$id]], frame_rate(ts), units,
                .time_unit(ts))
}

#' Duration of each track
#'
#' Total elapsed time of each trajectory (its last point minus its first),
#' computed via [elapsed_seconds()].
#'
#' @inheritParams elapsed_seconds
#' @return A data frame with columns `id` and `duration`.
#' @seealso [frame_rate()], [elapsed_seconds()]
#' @examples
#' head(track_duration(cpunctatus, units = "minutes"))
#' @export
track_duration <- function(ts, units = c("seconds", "minutes", "milliseconds")) {
  units <- match.arg(units)
  d   <- as.data.frame(ts)
  el  <- .elapsed_time(d[[ts@cols$time]], d[[ts@cols$id]], frame_rate(ts), units,
                       .time_unit(ts))
  ids <- as.character(d[[ts@cols$id]])
  dur <- tapply(el, ids, max)
  data.frame(id = names(dur), duration = as.numeric(dur), stringsAsFactors = FALSE)
}
