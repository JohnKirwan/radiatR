# Per-id elapsed time, in the requested units, measured from each track's
# minimum time. POSIXct -> difftime seconds (frame rate not needed). Numeric
# (frame index) -> (frame - min)/fps; a frame rate is required.
.elapsed_time <- function(time, id, fps = NULL,
                          units = c("seconds", "minutes", "milliseconds")) {
  units <- match.arg(units)
  secs <- if (inherits(time, "POSIXct")) {
    as.numeric(time)
  } else {
    if (is.null(fps))
      stop("numeric (frame) time needs a frame rate; set one with ",
           "`set_frame_rate(ts, fps)`.", call. = FALSE)
    as.numeric(time) / fps
  }
  el <- stats::ave(secs, id, FUN = function(s) s - min(s, na.rm = TRUE))
  switch(units, seconds = el, milliseconds = el * 1000, minutes = el / 60)
}

#' Elapsed time per observation of a Tracks object
#'
#' Real elapsed time of each point from its own track's start, computed on
#' demand from the time column and (for frame-indexed time) the [frame_rate()].
#' POSIXct time is used directly; numeric (frame) time requires a frame rate.
#'
#' @param ts A [Tracks] object.
#' @param units `"seconds"` (default), `"minutes"`, or `"milliseconds"`.
#' @return A numeric vector aligned to the object's observations.
#' @seealso [frame_rate()], [track_duration()]
#' @export
elapsed_seconds <- function(ts, units = c("seconds", "minutes", "milliseconds")) {
  units <- match.arg(units)
  d <- as.data.frame(ts)
  .elapsed_time(d[[ts@cols$time]], d[[ts@cols$id]], frame_rate(ts), units)
}

#' Duration of each track
#'
#' Total elapsed time of each trajectory (its last point minus its first),
#' computed via [elapsed_seconds()].
#'
#' @inheritParams elapsed_seconds
#' @return A data frame with columns `id` and `duration`.
#' @seealso [frame_rate()], [elapsed_seconds()]
#' @export
track_duration <- function(ts, units = c("seconds", "minutes", "milliseconds")) {
  units <- match.arg(units)
  d   <- as.data.frame(ts)
  el  <- .elapsed_time(d[[ts@cols$time]], d[[ts@cols$id]], frame_rate(ts), units)
  ids <- as.character(d[[ts@cols$id]])
  dur <- tapply(el, ids, max)
  data.frame(id = names(dur), duration = as.numeric(dur), stringsAsFactors = FALSE)
}
