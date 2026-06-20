# American/British colour-spelling aliases.
#
# radiatR's public API uses British spelling (colour, colour_col, track_colour,
# ...). To let American users pass color/color_col/track_color/..., each
# colour-bearing exported function declares a `color*=NULL` alias formal and
# calls `.apply_spelling_aliases()` as its first line. The single mapping table
# below is the source of truth.

# American alias -> canonical British name.
.SPELLING_PAIRS <- c(
  color           = "colour",
  color_col       = "colour_col",
  color_cycle     = "colour_cycle",
  track_color     = "track_colour",
  arrow_color     = "arrow_colour",
  arrow_color_col = "arrow_colour_col",
  grid_color      = "grid_colour",
  color_by_group  = "colour_by_group",
  color_minor     = "colour_minor",
  color_value     = "colour_value",
  origin_color    = "origin_colour",
  circle_color    = "circle_colour"
)

# Call as the FIRST line of any exported function that has a `colour*` argument
# (with the matching `color*=NULL` alias formal declared). Reads the caller's
# matched call to learn which names were actually supplied (so defaults never
# count), maps any American alias onto its British formal in the caller's
# environment, and errors if both spellings of a pair were supplied.
.apply_spelling_aliases <- function() {
  env  <- parent.frame()
  call <- match.call(
    definition  = sys.function(-1L),
    call        = sys.call(-1L),
    expand.dots = FALSE,
    envir       = parent.frame(2L)
  )
  supplied <- names(call)[-1L]
  supplied <- supplied[nzchar(supplied)]
  aliases  <- intersect(supplied, names(.SPELLING_PAIRS))
  for (am in aliases) {
    br <- .SPELLING_PAIRS[[am]]
    if (br %in% supplied)
      stop("Supply only one of `", br, "` / `", am, "`, not both.", call. = FALSE)
    assign(br, get(am, envir = env, inherits = FALSE), envir = env)
  }
  invisible()
}
