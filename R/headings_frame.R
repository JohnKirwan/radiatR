#' Construct a headings frame from a data frame of angles
#'
#' Validates the angle column, optionally converts degrees to radians, and
#' marks the data frame with class \code{headings_frame} and attributes that
#' downstream functions (\code{stack_headings},
#' \code{add_stacked_headings}, \code{\link{radiate}}) will use as
#' defaults.
#'
#' @param data A data frame containing the angle column.
#' @param col Unquoted or quoted name of the angle column.
#' @param units Units of the angle column: \code{"radians"} or
#'   \code{"degrees"}. No default — must be specified. Values are converted
#'   to radians in place when \code{"degrees"}.
#' @param angle_convention \code{"unit_circle"} (0 = East, CCW, default) or
#'   \code{"clock"} (0 = North, CW).
#' @param coords \code{"absolute"} (default) or \code{"relative"}.
#'
#' @return A \code{data.frame} with additional class \code{"headings_frame"}
#'   and attributes \code{heading_col}, \code{angle_convention},
#'   \code{coords}.
#'
#' @seealso \code{stack_headings}, \code{add_stacked_headings},
#'   \code{\link{radiate}}
#' @importFrom rlang ensym as_string
#' @export
headings_frame <- function(data,
                           col,
                           units,
                           angle_convention = "unit_circle",
                           coords           = "absolute") {
  col_name <- rlang::as_string(rlang::ensym(col))
  if (!col_name %in% names(data))
    stop(sprintf("column '%s' not found in data.", col_name))
  if (missing(units))
    stop("'units' must be specified: use \"radians\" or \"degrees\".\n",
         "  Hint: most behavioral data is recorded in degrees.")
  units            <- match.arg(units, c("radians", "degrees"))
  angle_convention <- match.arg(angle_convention, c("unit_circle", "clock"))
  coords           <- match.arg(coords, c("absolute", "relative"))

  .check_angle_units(data[[col_name]], units, col_name)
  if (units == "degrees")
    data[[col_name]] <- data[[col_name]] * pi / 180
  if (angle_convention == "clock")
    data[[col_name]] <- wrap_to_2pi((pi / 2) - data[[col_name]])

  # Orientation is carried once, as a circ_display object. The previous
  # display_convention/angle_convention string attributes were vestigial
  # (never read for rendering), so dropping them and defaulting display to
  # circ_display() preserves the rendered orientation.
  new_headings_frame(data, display = circ_display(), heading_col = col_name,
                     colour_col = NULL, coords = coords)
}

# The canonical attributes a headings_frame carries. Kept durable across dplyr
# verbs via dplyr_reconstruct() and base `[` below; read via the hf_* accessors.
.HF_ATTRS <- c("display", "heading_col", "colour_col", "coords")

# Set the headings_frame class vector: subclass first, always tibble-backed, and
# keep any dplyr classes a verb may have added (e.g. grouped_df from group_by).
# dplyr hands dplyr_reconstruct() a bare data.frame, so tbl_df/tbl must be
# re-ensured or the object would silently revert to a plain data.frame subclass.
.hf_set_class <- function(data) {
  cls <- setdiff(class(data), "headings_frame")
  if (!"tbl_df" %in% cls) cls <- c("tbl_df", "tbl", cls)
  class(data) <- c("headings_frame", cls)
  data
}

#' Low-level headings_frame constructor
#'
#' Wraps a data frame as a `headings_frame` (a tibble subclass) carrying the
#' canonical display/heading metadata. Most users call [headings_frame()] (which
#' validates and normalises angles) or get one from [derive_headings()].
#'
#' @param data A data frame / tibble with the heading column already in
#'   unit-circle radians.
#' @param display A [circ_display()] object (orientation convention).
#' @param heading_col Name of the heading column. Default `"heading"`.
#' @param colour_col,color_col Optional grouping/colour column name, or `NULL`.
#'   `color_col` is the American-spelling alias.
#' @param coords `"absolute"` or `"relative"`.
#' @return A `headings_frame`.
#' @export
new_headings_frame <- function(data, display = circ_display(),
                               heading_col = "heading", colour_col = NULL,
                               coords = "absolute", color_col = NULL) {
  .apply_spelling_aliases()
  data <- tibble::as_tibble(data)
  attr(data, "display")     <- display
  attr(data, "heading_col") <- heading_col
  attr(data, "colour_col")  <- colour_col
  attr(data, "coords")      <- coords
  .hf_set_class(data)
}

# Restore the headings_frame class + canonical attributes after a dplyr verb.
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.headings_frame <- function(data, template) {
  for (a in .HF_ATTRS) attr(data, a) <- attr(template, a, exact = TRUE)
  .hf_set_class(data)
}

#' @export
`[.headings_frame` <- function(x, ...) {
  out <- NextMethod()
  if (is.data.frame(out)) dplyr::dplyr_reconstruct(out, x) else out
}

#' Read the canonical attributes of a heading frame
#'
#' Accessors for the metadata a [headings_frame] carries. They also work on a
#' plain data frame, returning sensible defaults, so any function can read the
#' display convention without assuming the input is classed.
#'
#' @param x A `headings_frame` or plain data frame.
#' @return `hf_display()` a [circ_display()] object; `hf_heading_col()` /
#'   `hf_coords()` a string; `hf_colour_col()` a string or `NULL`.
#' @name hf_accessors
#' @export
hf_display <- function(x) attr(x, "display", exact = TRUE) %||% circ_display()

#' @rdname hf_accessors
#' @export
hf_heading_col <- function(x) attr(x, "heading_col", exact = TRUE) %||% "heading"

#' @rdname hf_accessors
#' @export
hf_colour_col <- function(x) attr(x, "colour_col", exact = TRUE)

#' @rdname hf_accessors
#' @usage hf_color_col(x)
#' @export
hf_color_col <- hf_colour_col

#' @rdname hf_accessors
#' @export
hf_coords <- function(x) attr(x, "coords", exact = TRUE) %||% "absolute"

#' Snap angles to fixed-width circular bin centres
#'
#' Bins angles (in radians) into fixed-width sectors and returns each angle
#' snapped to its bin's centre. Snapping coincident-binned angles to a common
#' value is the standard precursor to a stacked dot plot: feed the result to
#' \code{\link{stack_headings}} (with the default \code{tol = NULL}) to build
#' clean radial columns.
#'
#' @param angles Numeric vector of angles in radians. \code{NA} is preserved.
#' @param width Bin width in radians; must be a single positive number. For a
#'   5-degree bin use \code{pi / 36}.
#' @param phase Radian location of a bin \emph{centre}. The default \code{0}
#'   places bin centres at \code{0, width, 2 * width, ...}, so the reference
#'   direction sits on a column rather than on a bin boundary. Set
#'   \code{phase = width / 2} to reproduce the edge-aligned bins of
#'   \code{circular::plot.circular} (centres at \code{width / 2},
#'   \code{3 * width / 2}, ...). Any phase is allowed -- e.g. with
#'   \code{width = pi / 2, phase = pi / 4} the bin boundaries fall on the axes,
#'   binning by quadrant.
#'
#' @return A numeric vector the same length as \code{angles}, each value snapped
#'   to its bin centre and wrapped to \code{[0, 2 * pi)}.
#'
#' @seealso \code{\link{stack_headings}}, \code{\link{add_stacked_headings}}
#'
#' @examples
#' # 5-degree bins centred on the reference direction
#' bin_angles(c(0.01, 0.10, 0.11), width = pi / 36)
#' # circular-package style (edge-aligned) bins
#' bin_angles(c(0.01, 0.10, 0.11), width = pi / 36, phase = pi / 72)
#' @export
bin_angles <- function(angles, width, phase = 0) {
  if (!is.numeric(width) || length(width) != 1L)
    stop("'width' must be a single positive number (radians).")
  if (is.na(width) || width <= 0)
    stop("'width' must be a single positive number (radians).")
  centre <- round((angles - phase) / width) * width + phase
  wrap_to_2pi(centre)
}

#' Add stacking columns to a headings data frame
#'
#' Computes radial positions for stacked dot plots on circular plots.
#' Observations at the same angle (or within \code{tol} radians of each other)
#' are assigned successive radial positions, preventing overplotting of
#' coincident or binned headings.
#'
#' @param data A data frame with an angle column in radians.
#' @param col Name of the angle column. Defaults to the \code{heading_col}
#'   attribute when \code{data} is a \code{headings_frame}.
#' @param step Radial gap between successive dots in a stack, in data units
#'   (the analogue of \code{circular::plot.circular}'s \code{sep}). Default
#'   \code{0.025} matches that package; larger values separate the dots more.
#' @param start_sep Radial offset of the first (outermost, for \code{"inward"})
#'   dot from \code{base_r}, in data units (the analogue of
#'   \code{circular::plot.circular}'s \code{start.sep}). Default \code{0} places
#'   the first dot on the reference circle. A small positive value shifts the
#'   whole stack off the line so the dots abut rather than straddle it.
#' @param tol Grouping tolerance in radians. \code{NULL} (default) = exact
#'   equality, correct for binned data. \code{tol > 0} assigns each
#'   observation to the nearest group centre within \code{tol} radians
#'   (greedy, sorted-order scan); angles near \code{0} and \code{2*pi} are
#'   not treated as neighbours.
#' @param direction \code{"inward"} (default, stacks toward centre) or
#'   \code{"outward"} (away from perimeter, matches \code{circular} default).
#' @param base_r Radius of the reference circle in data units. Default 1.
#' @param shade If \code{TRUE}, add a \code{shade_n} column (alias of
#'   \code{stack_n}) for use as an alpha aesthetic.
#' @param shape If \code{TRUE}, add a \code{shape_code} integer column:
#'   1 = hollow (outermost / singleton), 2 = filled (middle), 3 = filled with
#'   ring (innermost in a stack of 3+).
#' @param group Optional column name; when set, stacking is computed
#'   independently within each group and the rows recombined. Default
#'   \code{NULL}.
#'
#' @return \code{data} augmented with \code{stack_r} and \code{stack_n}
#'   columns (always), plus \code{shade_n} and/or \code{shape_code} when
#'   requested. Row count is unchanged.
#'
#' @seealso \code{\link{headings_frame}}, \code{add_stacked_headings}
#' @export
stack_headings <- function(data,
                           col       = NULL,
                           step      = 0.025,
                           start_sep = 0,
                           tol       = NULL,
                           direction = "inward",
                           base_r    = 1,
                           shade     = FALSE,
                           shape     = FALSE,
                           group     = NULL) {
  if (is.null(col))
    col <- if (!is.null(attr(data, "heading_col"))) attr(data, "heading_col")
           else "heading"
  if (!col %in% names(data))
    stop(sprintf("column '%s' not found in data.", col))
  if (step <= 0)
    stop("'step' must be positive.")
  if (start_sep < 0)
    stop("'start_sep' must be non-negative.")
  if (!is.null(tol) && tol < 0)
    stop("'tol' must be NULL or non-negative.")
  direction <- match.arg(direction, c("inward", "outward"))

  # Stack within each group independently when `group` is given (e.g. one
  # stacking per facet). Recurse per group, then recombine.
  if (!is.null(group)) {
    if (!group %in% names(data))
      stop(sprintf("group column '%s' not found in data.", group))
    parts <- lapply(
      split(seq_len(nrow(data)), data[[group]], drop = TRUE),
      function(ix) stack_headings(data[ix, , drop = FALSE], col = col, step = step,
        start_sep = start_sep, tol = tol, direction = direction, base_r = base_r,
        shade = shade, shape = shape)
    )
    out <- do.call(rbind, parts)
    rownames(out) <- NULL
    return(out)
  }

  angles <- data[[col]]
  n      <- length(angles)

  # --- Assign group IDs -------------------------------------------------------
  if (is.null(tol)) {
    grp <- match(angles, unique(angles[!is.na(angles)]))
  } else {
    ord <- order(angles, na.last = TRUE)
    grp <- integer(n)
    centres <- numeric(0)
    g <- 0L
    for (i in ord) {
      a <- angles[i]
      if (is.na(a)) { grp[i] <- NA_integer_; next }
      if (length(centres)) {
        nearest <- which.min(abs(centres - a))
        if (abs(centres[nearest] - a) <= tol) { grp[i] <- nearest; next }
      }
      g <- g + 1L
      centres[g] <- a
      grp[i] <- g
    }
  }

  # --- Rank within each group (sorted angle order for ties) -------------------
  rank_in_grp <- integer(n)
  seen_g      <- if (any(!is.na(grp))) integer(max(grp, na.rm = TRUE)) else integer(0)
  for (i in order(angles, na.last = TRUE)) {
    g <- grp[i]
    if (!is.na(g)) {
      seen_g[g]      <- seen_g[g] + 1L
      rank_in_grp[i] <- seen_g[g]
    }
  }

  rank_in_grp[is.na(grp)] <- NA_integer_

  grp_tbl   <- tabulate(grp)
  grp_count <- ifelse(is.na(grp), NA_integer_, grp_tbl[grp])

  # --- Compute stack_r --------------------------------------------------------
  offset <- start_sep + (rank_in_grp - 1L) * step
  data$stack_r <- if (direction == "inward") base_r - offset else base_r + offset
  data$stack_n <- as.integer(grp_count)

  if (shade) data$shade_n <- data$stack_n

  if (shape) {
    n_rows         <- nrow(data)
    sc             <- rep(2L, n_rows)
    sc[rank_in_grp == 1L]                                    <- 1L
    sc[rank_in_grp == data$stack_n & data$stack_n >= 3L]     <- 3L
    sc[is.na(rank_in_grp) | is.na(data$stack_n)]             <- NA_integer_
    data$shape_code <- sc
  }

  data
}
