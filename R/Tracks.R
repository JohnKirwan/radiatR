# Tracks: an S4 container for sets of circular trajectories
# Stores long-form observations (id, time, angle [radians], optional x, y, weight, covariates)

#' @importFrom methods setClass setValidity setGeneric setMethod new setAs slot
#' @importFrom circular circular mean.circular rho.circular
#' @keywords internal

## ---- helpers -----------------------------------------------------------------
.wrap_to_2pi <- function(x) wrap_to_2pi(x)
.as_radians <- function(x, unit) as_radians(x, unit)

# von Mises concentration from the mean resultant length Rbar: the piecewise
# approximation to the maximum-likelihood estimate A1inv(Rbar) (Fisher 1993;
# Mardia & Jupp), identical to circular::A1inv(), which is what
# circular::mle.vonmises() uses. Rbar = 1 (all angles coincide) gives Inf.
.kappa_from_Rbar <- function(R) {
  out <- R
  low  <- R < 0.53
  mid  <- R >= 0.53 & R < 0.85
  high <- R >= 0.85
  out[low]  <- 2*R[low] + R[low]^3 + (5*R[low]^5)/6
  out[mid]  <- -0.4 + 1.39*R[mid] + 0.43/(1 - R[mid])
  out[high] <- 1/(R[high]^3 - 4*R[high]^2 + 3*R[high])
  out
}

.as_circ <- function(theta) {
  circular::circular(theta, units = "radians", type = "angles", modulo = "2pi", zero = 0)
}

.empty_transform_history <- function() {
  tibble::tibble(
    step = character(),
    order = integer(),
    id = character(),
    implementation = character(),
    params = list(),
    depends_on = list()
  )
}

.ensure_transform_history <- function(history) {
  if (is.null(history)) {
    return(.empty_transform_history())
  }
  if (inherits(history, "Tracks")) {
    history <- transform_history(history)
  } else if (is.list(history) && !inherits(history, "data.frame")) {
    history <- tibble::as_tibble(history)
  }
  required <- c("step", "order", "id", "implementation", "params", "depends_on")
  missing_cols <- setdiff(required, names(history))
  if (length(missing_cols)) {
    stop("Transform history is missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  history$step <- as.character(history$step)
  history$order <- as.integer(history$order)
  history$id <- as.character(history$id)
  history$implementation <- as.character(history$implementation)
  if (!is.list(history$params)) {
    history$params <- as.list(history$params)
  }
  if (!is.list(history$depends_on)) {
    history$depends_on <- as.list(history$depends_on)
  }
  history
}

.append_transform_history <- function(history, step, ids, implementation,
                                      params, order = NULL, depends_on = NULL) {
  history <- .ensure_transform_history(history)
  if (is.null(ids)) stop("`ids` cannot be NULL when logging a transform step.")
  ids <- as.character(ids)
  # A step over zero ids logs one row per id -> zero rows: a no-op. Return before
  # the params/depends_on length checks, which otherwise reject length-0 ids.
  if (!length(ids)) return(history)
  if (is.null(params)) {
    params <- vector("list", length(ids))
  } else if (!is.list(params)) {
    params <- rep(list(params), length(ids))
  } else if (length(params) == 1L && length(ids) > 1L) {
    params <- rep(params, length(ids))
  } else if (length(params) != length(ids)) {
    stop("Length of `params` (", length(params),
         ") must equal length of `ids` (", length(ids), ").")
  }
  if (is.null(order)) {
    current <- if (nrow(history)) max(history$order, na.rm = TRUE) else 0L
    order <- current + 1L
  }
  depends_on <- if (is.null(depends_on)) list(character()) else as.list(rep(list(depends_on), length(ids)))
  if (length(depends_on) == 1L && length(ids) > 1L) {
    depends_on <- rep(depends_on, length(ids))
  } else if (length(depends_on) != length(ids)) {
    stop("Length of `depends_on` (", length(depends_on),
         ") must equal length of `ids` (", length(ids), ").")
  }
  for (i in seq_along(ids)) {
    history <- tibble::add_row(
      history,
      step = as.character(step),
      order = as.integer(order),
      id = ids[i],
      implementation = as.character(implementation),
      params = list(params[[i]]),
      depends_on = list(depends_on[[i]])
    )
  }
  history
}

.combine_transform_histories <- function(histories) {
  out <- .empty_transform_history()
  if (!length(histories)) return(out)
  for (hist in histories) {
    hist <- .ensure_transform_history(hist)
    if (!nrow(hist)) next
    for (i in seq_len(nrow(hist))) {
      out <- tibble::add_row(
        out,
        step = hist$step[[i]],
        order = hist$order[[i]],
        id = hist$id[[i]],
        implementation = hist$implementation[[i]],
        params = list(hist$params[[i]]),
        depends_on = list(hist$depends_on[[i]])
      )
    }
  }
  out
}

## ---- class -------------------------------------------------------------------
#' Tracks container for circular trajectories
#'
#' @slot data data.frame of trajectory observations in long form
#' @slot cols list mapping required column names (id/time/angle/optional x,y,weight)
#' @slot angle_unit character describing the original angle unit supplied
#' @slot meta list of additional metadata attached to the set
#' @section Invariants:
#' Every \code{Tracks} satisfies the following. They are checked when the object
#' is built and whenever \code{methods::validObject()} runs, which the package's
#' own transforms do (a direct slot write is not checked until then):
#' \itemize{
#'   \item ids are never missing, and the rows of each id are contiguous;
#'   \item time is finite and strictly increasing within each id, so no time is
#'     repeated within a trajectory;
#'   \item angles are finite and lie in \code{[0, 2*pi)};
#'   \item coordinates are finite: \code{x}/\code{y}, and the relative and raw
#'     pairs where those columns are stored;
#'   \item weights, when a weight column is named, are finite and \code{>= 0}
#'     (a zero weight excludes an observation).
#' }
#' A \code{Tracks} therefore never holds a missing detection: remove those
#' before construction (see \code{on_invalid} in \code{\link{read_tracks}}).
#'
#' \code{tracks()} reports violations as an error of class
#' \code{radiatR_invalid_tracks}. Its \code{problems} field is a data frame with
#' one row per rule, column and row: \code{rule}, \code{column}, \code{row} (the
#' row of the data frame supplied) and \code{id}; \code{rows} and \code{ids}
#' give the union. When called through \code{\link{read_tracks}}, rows are those
#' of the combined table passed to \code{tracks()} (after any rows removed by
#' \code{on_invalid = "drop"}), not line numbers in a file.
#' @rdname Tracks-class
#' @exportClass Tracks
setClass(
  "Tracks",
  slots = c(
    data       = "data.frame",   # long: id, time, angle(rad), optional x,y,weight,covars
    cols       = "list",         # list(id=, time=, angle=, x=NULL, y=NULL, weight=NULL)
    angle_unit = "character",    # recorded input units (always stored as radians)
    meta       = "list"
  )
)

# Row-level validity. Returns a data frame (rule, column, row): one row per rule
# broken per row of `d`; zero rows means valid. Assumes .tracks_structure_error()
# returned NULL. Problems in the columns named by `skip` are not reported.
.tracks_problems <- function(d, cl, skip = character()) {
  found <- list()
  flag <- function(rule, column, bad) {
    rows <- which(bad)
    if (length(rows) && !column %in% skip)
      found[[length(found) + 1L]] <<- data.frame(
        rule = rule, column = column, row = rows, stringsAsFactors = FALSE)
  }
  n   <- nrow(d)
  id  <- d[[cl$id]]
  if (is.factor(id)) id <- as.character(id)   # an NA *level* is a missing id too
  tm  <- as.numeric(d[[cl$time]])
  ang <- d[[cl$angle]]

  flag("id_missing", cl$id, is.na(id))
  flag("time_nonfinite", cl$time, !is.finite(tm))
  if (n > 1L) {
    # A missing id is never "the same trajectory", and a non-finite step is
    # time_nonfinite's to report; both are masked so each cause is named once.
    # The later row of an offending pair is the one reported.
    same <- !is.na(id[-1L]) & !is.na(id[-n]) & id[-1L] == id[-n]
    dt   <- diff(tm)
    ok   <- same & is.finite(dt)
    flag("time_duplicate", cl$time, c(FALSE, ok & dt == 0))
    flag("time_unsorted",  cl$time, c(FALSE, ok & dt < 0))
  }
  flag("angle_nonfinite", cl$angle, !is.finite(ang))
  flag("angle_range", cl$angle, is.finite(ang) & (ang < 0 | ang >= 2 * pi))
  for (col in unique(c(cl$x, cl$y, cl$rel_x, cl$rel_y, cl$raw_x, cl$raw_y))) {
    if (col %in% names(d)) flag("coord_nonfinite", col, !is.finite(d[[col]]))
  }
  if (!is.null(cl$weight)) {
    w <- d[[cl$weight]]
    flag("weight_invalid", cl$weight, !is.finite(w) | w < 0)
  }
  if (!length(found))
    return(data.frame(rule = character(), column = character(),
                      row = integer(), stringsAsFactors = FALSE))
  do.call(rbind, found)
}

.tracks_rule_text <- c(
  id_missing      = "missing id",
  time_nonfinite  = "missing or non-finite time",
  time_duplicate  = "time repeated within an id",
  time_unsorted   = "time decreasing within an id",
  angle_nonfinite = "missing or non-finite angle",
  angle_range     = "angle outside [0, 2*pi)",
  coord_nonfinite = "missing or non-finite coordinate",
  weight_invalid  = "weight missing, non-finite or negative"
)

# One line per rule broken: the rule in words, its column(s), a count, and the
# first 10 rows and first 10 distinct ids. `ids` is parallel to `problems`.
.format_tracks_problems <- function(problems, ids) {
  first10 <- function(v) paste0(paste(utils::head(v, 10L), collapse = ", "),
                                if (length(v) > 10L) ", ..." else "")
  vapply(unique(problems$rule), function(r) {
    hit  <- problems$rule == r
    rows <- sort(unique(problems$row[hit]))
    sprintf("%s (column(s) %s): %d row(s): %s; id(s): %s",
            .tracks_rule_text[[r]],
            paste(unique(problems$column[hit]), collapse = ", "),
            length(rows), first10(rows),
            first10(unique(as.character(ids[hit]))))
  }, character(1), USE.NAMES = FALSE)
}

# Structural validity: roles named, columns present, types right. Returns NULL
# when sound, otherwise one message. Row-level rules are .tracks_problems()'s.
.tracks_structure_error <- function(d, cl) {
  req <- c("id","time","angle")
  if (!all(req %in% names(cl))) return("cols must name 'id','time','angle'")

  miss <- setdiff(c(cl$id, cl$time, cl$angle), names(d))
  if (length(miss)) return(paste("missing required column(s):", paste(miss, collapse=", ")))

  if (!is.numeric(d[[cl$angle]])) return("angle column must be numeric (radians)")

  if (!inherits(d[[cl$time]], "POSIXct") && !is.numeric(d[[cl$time]]))
    return("time must be POSIXct or numeric")

  if (!is.null(cl$x) || !is.null(cl$y)) {
    if (is.null(cl$x) || is.null(cl$y)) return("both x and y must be named in cols if either is used")
    if (!all(c(cl$x, cl$y) %in% names(d))) return("x/y columns not present in data")
    if (!is.numeric(d[[cl$x]]) || !is.numeric(d[[cl$y]])) return("x/y must be numeric")
  }
  if (!is.null(cl$raw_x) || !is.null(cl$raw_y)) {
    if (is.null(cl$raw_x) || is.null(cl$raw_y)) return("both raw_x and raw_y must be named in cols if either is used")
    if (!all(c(cl$raw_x, cl$raw_y) %in% names(d))) return("raw x/y columns not present in data")
    if (!is.numeric(d[[cl$raw_x]]) || !is.numeric(d[[cl$raw_y]])) return("raw x/y must be numeric")
  }
  if (!is.null(cl$rel_x) || !is.null(cl$rel_y)) {
    if (is.null(cl$rel_x) || is.null(cl$rel_y)) return("both rel_x and rel_y must be in cols if either is used")
    present <- c(cl$rel_x, cl$rel_y) %in% names(d)
    if (any(present) && !all(present)) return("rel_x and rel_y must both be present or both absent")
    if (all(present) && (!is.numeric(d[[cl$rel_x]]) || !is.numeric(d[[cl$rel_y]])))
      return("rel_x/rel_y must be numeric")
  }
  if (!is.null(cl$rho) && cl$rho %in% names(d) && !is.numeric(d[[cl$rho]])) {
    return("rho column must be numeric")
  }
  if (!is.null(cl$weight)) {
    if (!cl$weight %in% names(d)) return("weight column not present in data")
    if (!is.numeric(d[[cl$weight]])) return("weight must be numeric")
  }
  NULL
}

setValidity("Tracks", function(object) {
  d  <- object@data
  cl <- object@cols
  structure_error <- .tracks_structure_error(d, cl)
  if (!is.null(structure_error)) return(structure_error)

  msgs <- character()
  # Rows must be grouped by id (each id one contiguous block). A global ordering
  # across distinct id labels is not required -- and is not locale-independent.
  # match() gives NA its own code, so missing ids are compared as a value here.
  code <- match(d[[cl$id]], unique(d[[cl$id]]))
  n    <- nrow(d)
  if (n > 1L && sum(code[-1L] != code[-n]) + 1L != max(code))
    msgs <- "rows must be grouped by id (each id contiguous)"

  problems <- .tracks_problems(d, cl)
  if (nrow(problems))
    msgs <- c(msgs, .format_tracks_problems(problems, d[[cl$id]][problems$row]))

  if (length(msgs)) msgs else TRUE
})

# Per-trajectory affine map into the unit circle: centre each trajectory on its
# bounding-box midpoint and scale so the furthest point sits at radius 1. Used by
# the Tracks constructor when normalize_xy = TRUE. Preserves trajectory shape and
# places the centre at the origin (what the radius-based heading rules
# assume). Degenerate (zero-extent or all-NA) trajectories map to the origin.
.normalize_to_unit_circle <- function(x, y, id) {
  xo <- rep(NA_real_, length(x))
  yo <- rep(NA_real_, length(y))
  # Group by id; rows with a missing id are treated as one group (rather than
  # dropped, which would discard their coordinates) since `NA == NA` is NA.
  grp <- as.character(id)
  grp[is.na(grp)] <- ".__na_id__"
  for (g in unique(grp)) {
    idx <- which(grp == g)
    gx  <- x[idx]; gy <- y[idx]
    fin <- is.finite(gx) & is.finite(gy)
    if (!any(fin)) next
    cx <- (min(gx[fin]) + max(gx[fin])) / 2
    cy <- (min(gy[fin]) + max(gy[fin])) / 2
    rx <- gx - cx; ry <- gy - cy
    rmax <- max(sqrt(rx[fin]^2 + ry[fin]^2))
    s <- if (is.finite(rmax) && rmax > 0) 1 / rmax else 1
    xo[idx] <- rx * s
    yo[idx] <- ry * s
  }
  list(x = xo, y = yo)
}

# Fixed-reference affine map into the unit circle: translate every point by a
# single shared `origin` and scale by a single shared `radius`, so
# (x, y) -> ((x - origin[1]) / radius, (y - origin[2]) / radius). Unlike
# .normalize_to_unit_circle this is NOT grouped by id -- one similarity about the
# origin applied to all rows -- so bearings measured from the origin are
# preserved exactly. Inputs are validated by the tracks() constructor.
.calibrate_to_unit_circle <- function(x, y, origin, radius) {
  list(
    x = (x - origin[1]) / radius,
    y = (y - origin[2]) / radius
  )
}

#' Construct a Tracks
#'
#' @param df data.frame in long form
#' @param id,time Columns naming trajectory id and time
#' @param angle Optional angle column (radians or degrees, see angle_unit)
#' @param x,y Optional cartesian columns; if provided, converted to unit circle and angle inferred
#' @param rel_x,rel_y Optional column names for pre-transformed relative
#'   coordinates (centred on the origin, unit-circle scaled).  Both must be supplied
#'   together or not at all.
#' @param angle_unit Units of provided angle ("radians" or "degrees"); stored internally as radians
#' @param weight Optional weight column name; weights must be finite and
#'   \code{>= 0}
#' @param normalize_xy If FALSE (default), (x,y) are kept as supplied. If TRUE,
#'   each trajectory is independently centred on its own bounding-box midpoint
#'   and scaled so its furthest point sits at radius 1. This is a
#'   shape-preserving transform ONLY: because each trajectory is translated by
#'   a different amount, it does NOT preserve bearing relative to a fixed
#'   arena origin, and must not be used as a substitute for metric
#'   calibration. Raw coordinates are retained in `<x>_raw`/`<y>_raw` when
#'   enabled. Prefer pre-calibrated input, or the landmark-relative frame
#'   (`rel_x`/`rel_y`, see `coords = "relative"` in [derive_headings()]) when a
#'   per-trial reference direction is available.
#' @param origin Optional length-2 numeric `c(x, y)` giving a fixed unit-circle
#'   centre in the raw coordinate units. Supplied together with `radius`, it
#'   calibrates every trajectory uniformly via `(xy - origin) / radius`, a
#'   similarity that preserves bearings measured from the origin (unlike
#'   `normalize_xy`, which centres each trajectory on its own bounding box).
#'   Mutually exclusive with `normalize_xy = TRUE`. Raw coordinates are retained
#'   in `<x>_raw`/`<y>_raw`. For example, in an experimental arena `origin` is the
#'   arena centre and `radius` its edge distance in pixels.
#' @param radius Optional positive finite scalar: the raw distance mapping to
#'   `rho = 1`. Required together with `origin`.
#' @param meta Free-form list of metadata
#' @param transform_history Optional tibble describing transformation steps applied to the
#'   trajectories. Must contain columns `step`, `order`, `id`, `implementation`, `params`,
#'   and `depends_on`.
#' @return A Tracks S4 object
#' @examples
#' df <- data.frame(
#'   id   = rep(c("a", "b"), each = 3),
#'   time = rep(1:3, 2),
#'   x    = c(0, 0.3, 0.6, 0, -0.2, -0.5),
#'   y    = c(0, 0.2, 0.5, 0, 0.3, 0.7)
#' )
#' ts <- tracks(df, x = "x", y = "y")
#' ts
#' length(ts)
#' ts["b"]
#'
#' # combine objects with the same column mapping
#' more <- tracks(data.frame(id = "c", time = 1:2, x = c(0, 0.4), y = c(0, -0.4)),
#'                x = "x", y = "y")
#' length(c(ts, more))
#'
#' # pixel coordinates: arena centre (320, 240), edge 200 px from it
#' px <- data.frame(id = "d", time = 1:3,
#'                  x = c(320, 380, 450), y = c(240, 280, 330))
#' tracks(px, x = "x", y = "y", origin = c(320, 240), radius = 200)
#' @rdname Tracks-class
#' @export
tracks <- function(df,
                    id = "id", time = "time",
                    angle = NULL,
                    x = NULL, y = NULL,
                    rel_x = NULL, rel_y = NULL,
                    angle_unit = NULL,
                    weight = NULL,
                    normalize_xy = FALSE,
                    origin = NULL,
                    radius = NULL,
                    meta = list(),
                    transform_history = NULL) {
  stopifnot(is.data.frame(df))
  if (is.null(meta)) meta <- list()

  if (!id %in% names(df))   stop("Column '", id, "' not found")
  if (!time %in% names(df)) stop("Column '", time, "' not found")

  have_angle <- !is.null(angle) && angle %in% names(df)
  have_xy    <- !is.null(x) && !is.null(y) && all(c(x,y) %in% names(df))
  if (!have_angle && !have_xy)
    stop("Provide either an angle column or both x and y columns")

  if (have_angle) {
    if (is.null(angle_unit))
      stop("'angle_unit' must be specified when an angle column is provided: ",
           "use \"radians\" or \"degrees\".")
    angle_unit <- match.arg(angle_unit, c("radians", "degrees"))
    .check_angle_units(df[[angle]], angle_unit, col_name = angle)
  } else {
    angle_unit <- "radians"  # angles derived from x/y via atan2, always radians
  }

  # Validate rel_x/rel_y pairing. The columns are derivable, so a role may be
  # registered without the column being stored; require numeric only if present.
  have_rel <- !is.null(rel_x) || !is.null(rel_y)
  if (have_rel) {
    if (is.null(rel_x) || is.null(rel_y))
      stop("Both rel_x and rel_y must be supplied if either is provided.")
    if (rel_x %in% names(df) && !is.numeric(df[[rel_x]]))
      stop("rel_x column '", rel_x, "' must be numeric.")
    if (rel_y %in% names(df) && !is.numeric(df[[rel_y]]))
      stop("rel_y column '", rel_y, "' must be numeric.")
  }

  # Fixed-reference unit-circle calibration: translate/scale all trajectories by
  # a single shared origin and radius. Mutually exclusive with normalize_xy.
  have_calibration <- !is.null(origin) || !is.null(radius)
  if (have_calibration) {
    if (is.null(origin) || is.null(radius))
      stop("Both `origin` and `radius` are required together for calibration.")
    if (isTRUE(normalize_xy))
      stop("`normalize_xy = TRUE` and `origin`/`radius` calibration are mutually exclusive; supply only one.")
    if (!have_xy)
      stop("`origin`/`radius` calibration requires x and y columns.")
    if (!is.numeric(origin) || length(origin) != 2L || any(!is.finite(origin)))
      stop("`origin` must be a length-2 finite numeric vector c(x, y).")
    if (!is.numeric(radius) || length(radius) != 1L || !is.finite(radius) || radius <= 0)
      stop("`radius` must be a single positive finite number.")
  }

  d <- df

  # Ensure consistent polar/cartesian representation
  make_unique_name <- function(existing, proposal) {
    if (proposal %in% existing) {
      make.unique(c(existing, proposal))[length(existing) + 1]
    } else {
      proposal
    }
  }

  theta_from_xy <- NULL
  rho_col <- NULL
  raw_cols <- list(x = NULL, y = NULL)
  if (have_xy) {
    if (isTRUE(normalize_xy)) {
      raw_x_name <- make_unique_name(names(d), paste0(x, "_raw"))
      raw_y_name <- make_unique_name(names(d), paste0(y, "_raw"))
      d[[raw_x_name]] <- d[[x]]
      d[[raw_y_name]] <- d[[y]]
      raw_cols$x <- raw_x_name
      raw_cols$y <- raw_y_name
      norm <- .normalize_to_unit_circle(d[[x]], d[[y]], d[[id]])
      xy_x <- norm$x; xy_y <- norm$y
    } else if (have_calibration) {
      raw_x_name <- make_unique_name(names(d), paste0(x, "_raw"))
      raw_y_name <- make_unique_name(names(d), paste0(y, "_raw"))
      d[[raw_x_name]] <- d[[x]]
      d[[raw_y_name]] <- d[[y]]
      raw_cols$x <- raw_x_name
      raw_cols$y <- raw_y_name
      cal <- .calibrate_to_unit_circle(d[[x]], d[[y]], origin, radius)
      xy_x <- cal$x; xy_y <- cal$y
    } else {
      xy_x <- d[[x]]; xy_y <- d[[y]]
    }

    conv <- cartesian_to_polar(xy_x, xy_y, normalize = FALSE)
    d[[x]] <- conv$x
    d[[y]] <- conv$y
    theta_from_xy <- conv$theta
    # Register a radius role when radius information is available. The column is
    # derivable from the position, so it is materialized on demand by
    # as.data.frame() rather than stored.
    if (!all(is.na(conv$rho))) {
      rho_col <- make_unique_name(names(d), if (normalize_xy) "rho" else "radius")
    }
  }

  theta_from_ang <- NULL
  if (have_angle) {
    theta_from_ang <- .as_radians(d[[angle]], angle_unit)
  }

  if (have_angle) {
    d$..theta_tmp <- theta_from_ang
  } else {
    d$..theta_tmp <- theta_from_xy
  }

  if (is.null(d$..theta_tmp)) {
    stop("Unable to derive angles from the supplied inputs.")
  }

  # Optional weights sanity
  if (!is.null(weight)) {
    if (!weight %in% names(d)) stop("weight column '", weight, "' not found")
    if (!is.numeric(d[[weight]])) stop("weight must be numeric")
  }

  # Sort by (id,time)
  ord <- order(d[[id]], d[[time]])
  d <- d[ord, , drop = FALSE]

  # Finalize angle column
  angle_col <- if (have_angle) angle else "angle"
  if (!have_angle) {
    angle_col <- make_unique_name(names(d), angle_col)
    d[[angle_col]] <- d$..theta_tmp
  } else {
    d[[angle_col]] <- d$..theta_tmp
  }
  d$..theta_tmp <- NULL

  if (!have_xy) {
    cart_out <- polar_to_cartesian(d[[angle_col]])
    existing <- names(d)
    x <- make_unique_name(existing, if (is.null(x)) "x" else x)
    d[[x]] <- cart_out$x
    existing <- names(d)
    y <- make_unique_name(existing, if (is.null(y)) "y" else y)
    d[[y]] <- cart_out$y
  }

  meta[["normalize_xy"]] <- isTRUE(normalize_xy)
  meta[["raw_xy_cols"]] <- raw_cols
  if (have_calibration) {
    meta[["calibration"]] <- list(origin = as.numeric(origin),
                                  radius = as.numeric(radius))
  }
  if (is.null(transform_history) && !is.null(meta$transform_history)) {
    transform_history <- meta$transform_history
  }
  transform_history <- .ensure_transform_history(transform_history)
  if (have_calibration) {
    transform_history <- .append_transform_history(
      transform_history,
      step = "calibrate_unit_circle",
      ids = unique(as.character(d[[id]])),
      implementation = ".calibrate_to_unit_circle",
      params = list(list(origin = as.numeric(origin), radius = as.numeric(radius)))
    )
  }
  meta$transform_history <- transform_history

  cl <- list(id = id, time = time, angle = angle_col,
             x = if (!is.null(x)) x else NULL,
             y = if (!is.null(y)) y else NULL,
             rel_x = if (have_rel) rel_x else NULL,
             rel_y = if (have_rel) rel_y else NULL,
             raw_x = raw_cols$x,
             raw_y = raw_cols$y,
             rho = rho_col,
             weight = weight)

  # Report broken invariants as one classed condition, against the rows of the
  # data frame supplied (`ord` undoes the sort). Columns derived or copied above
  # are skipped so a cause is not reported again under a derived column name;
  # new() below still validates every column. Structural faults are left to
  # new(), which names them.
  if (is.null(.tracks_structure_error(d, cl))) {
    derived <- c(if (!have_angle) angle_col, if (!have_xy) c(x, y),
                 raw_cols$x, raw_cols$y)
    problems <- .tracks_problems(d, cl, skip = derived)
    if (nrow(problems)) {
      problems$id  <- as.character(d[[id]][problems$row])
      problems$row <- ord[problems$row]
      radiatR_abort_invalid_tracks(problems)
    }
  }

  new("Tracks", data = d, cols = cl, angle_unit = angle_unit, meta = meta)
}

## ---- accessors & show --------------------------------------------------------
#' Trajectory identifiers of a Tracks
#'
#' @param x A [Tracks-class] object.
#' @return The unique trajectory ids, in first-appearance order.
#' @examples
#' ids(cpunctatus)
#' @name ids
#' @export
setGeneric("ids", function(x) standardGeneric("ids"))

#' @rdname ids
#' @export
setMethod("ids", "Tracks", function(x) unique(x@data[[x@cols$id]]))

#' @rdname Tracks-class
#' @export
setMethod("length", "Tracks", function(x) length(ids(x)))

setGeneric("angles", function(x, as = c("numeric","circular"), unit = c("radians","degrees")) standardGeneric("angles"))
setMethod("angles", "Tracks", function(x, as = c("numeric","circular"), unit = c("radians","degrees")) {
  as <- match.arg(as); unit <- match.arg(unit)
  th <- x@data[[x@cols$angle]]
  if (unit == "degrees") th <- th * 180/pi
  if (as == "circular") return(.as_circ(if (unit=="degrees") th * pi/180 else th))
  th
})

setMethod("show", "Tracks", function(object) {
  id <- object@cols$id; tm <- object@cols$time; th <- object@cols$angle
  xy <- if (!is.null(object@cols$x)) paste0(", x='", object@cols$x, "', y='", object@cols$y, "'") else ""
  rel_xy <- if (!is.null(object@cols$rel_x)) paste0(", rel_x='", object@cols$rel_x, "', rel_y='", object@cols$rel_y, "'") else ""
  raw_xy <- if (!is.null(object@cols$raw_x)) paste0(", raw_x='", object@cols$raw_x, "', raw_y='", object@cols$raw_y, "'") else ""
  cat(sprintf("Tracks: %d trajectories, %d observations\n", length(ids(object)), nrow(object@data)))
  cat(sprintf("Columns: id='%s', time='%s', angle='%s' (radians)%s%s%s\n", id, tm, th, xy, rel_xy, raw_xy))
  hist <- transform_history(object)
  if (nrow(hist)) {
    steps <- unique(hist$step[order(hist$order, hist$step)])
    cat("Transform steps:", paste(steps, collapse = " -> "), "\n")
  }
  print(utils::head(object@data, 6))
})

## ---- transform history helpers ----------------------------------------------
#' Transform history helpers for Tracks objects
#'
#' @param x A `Tracks` object.
#' @param step Character identifier for the transform step.
#' @param traj_ids Character vector of trajectory identifiers affected by the step.
#'   Defaults to all trajectories in `x` when `NULL`.
#' @param implementation Character label for the implementation used to apply
#'   the step. Defaults to `step`.
#' @param params List-column of per-trajectory parameter sets (recycled when a
#'   single entry is provided).
#' @param order Optional integer giving the execution order. When omitted the
#'   step is appended to the end of the log.
#' @param depends_on Optional character vector naming prerequisite step(s).
#' @param history Tibble or list describing the full transform history to
#'   replace.
#'
#' @return For `transform_history`, a tibble describing the recorded steps. For
#'   `log_transform` and `set_transform_history`, the updated `Tracks` object.
#' @examples
#' df <- data.frame(id = "a", time = 1:3, x = c(0, 0.3, 0.6), y = c(0, 0.2, 0.5))
#' ts <- tracks(df, x = "x", y = "y")
#' ts <- log_transform(ts, "smooth", params = list(list(window = 3)))
#' transform_history(ts)
#' # replace the whole log, here with an empty one
#' ts <- set_transform_history(ts, transform_history(ts)[0, ])
#' nrow(transform_history(ts))
#' @name transform_history
NULL

#' @rdname transform_history
#' @export
setGeneric("transform_history", function(x) standardGeneric("transform_history"))

#' @rdname transform_history
#' @export
setMethod("transform_history", "Tracks", function(x) {
  .ensure_transform_history(x@meta$transform_history)
})

#' @rdname transform_history
#' @export
setGeneric("log_transform", function(x, step, traj_ids = NULL,
                                     implementation = step, params = NULL,
                                     order = NULL, depends_on = NULL)
  standardGeneric("log_transform"))

#' @rdname transform_history
#' @export
setMethod("log_transform", "Tracks", function(x, step, traj_ids = NULL,
                                               implementation = step, params = NULL,
                                               order = NULL, depends_on = NULL) {
  if (is.null(traj_ids)) {
    traj_ids <- ids(x)
  }
  history <- .append_transform_history(
    transform_history(x),
    step = step,
    ids = traj_ids,
    implementation = implementation,
    params = params,
    order = order,
    depends_on = depends_on
  )
  x@meta$transform_history <- history
  methods::validObject(x)
  x
})

#' @rdname transform_history
#' @export
setGeneric("set_transform_history", function(x, history) standardGeneric("set_transform_history"))

#' @rdname transform_history
#' @export
setMethod("set_transform_history", "Tracks", function(x, history) {
  x@meta$transform_history <- .ensure_transform_history(history)
  methods::validObject(x)
  x
})

## ---- subsetting & extraction -------------------------------------------------
#' @param i Trajectory identifiers (character ids, numeric indices, or logical vector)
#' @rdname Tracks-class
#' @export
setMethod(
  f = "[",
  signature = c(x="Tracks", i="ANY", j="missing", drop="missing"),
  definition = function(x, i) {
    idcol <- x@cols$id
    all_ids <- ids(x)
    if (missing(i)) return(x)

    if (is.logical(i)) i <- which(i)
    if (is.numeric(i)) {
      if (any(i < 1 | i > length(all_ids))) stop("id index out of bounds")
      i <- all_ids[i]
    }
    if (is.character(i)) {
      miss <- setdiff(i, all_ids)
      if (length(miss)) stop("unknown id(s): ", paste(miss, collapse=", "))
      keep <- x@data[[idcol]] %in% i
    } else {
      stop("unsupported index type for Tracks")
    }

    new("Tracks", data = x@data[keep, , drop=FALSE],
        cols = x@cols, angle_unit = x@angle_unit, meta = x@meta)
  }
)

setGeneric("trajectory", function(x, id) standardGeneric("trajectory"))
setMethod("trajectory", "Tracks", function(x, id) {
  idcol <- x@cols$id
  d <- x@data[x@data[[idcol]] == id, , drop = FALSE]
  if (!nrow(d)) stop("id not found: ", id)
  d
})

## ---- coercions ---------------------------------------------------------------

#' Coerce a Tracks to a data frame
#'
#' Returns the long-form trajectory table held in the Tracks's `data` slot.
#'
#' Registered as an S3 method so that `as.data.frame(ts)` dispatches correctly
#' from any caller. (A bare S4 method on the base S3 generic is only reached
#' from within the package's own namespace, not from user code using the
#' installed package.)
#'
#' @param x A [Tracks-class] object.
#' @param row.names,optional Accepted for S3 generic consistency; ignored.
#' @param ... Ignored.
#' @return A data frame: the Tracks's `data` slot.
#' @examples
#' head(as.data.frame(cpunctatus))
#' @exportS3Method base::as.data.frame
as.data.frame.Tracks <- function(x, row.names = NULL, optional = FALSE, ...) {
  d    <- x@data
  cols <- x@cols
  if (is.null(cols$x) || is.null(cols$y)) return(d)   # no position -> cannot derive

  # Stored derived column name -> field of derive_coords() output.
  want <- list()
  if (!is.null(cols$angle)) want[[cols$angle]] <- "rel_theta_unit"
  if (!is.null(cols$rel_x)) want[[cols$rel_x]] <- "rel_x"
  if (!is.null(cols$rel_y)) want[[cols$rel_y]] <- "rel_y"
  if (!is.null(cols$rho))   want[[cols$rho]]   <- "trans_rho"
  want[["trans_rho"]] <- "trans_rho"
  want[["abs_theta"]] <- "abs_theta_unit"

  missing_cols <- names(want)[!names(want) %in% names(d)]
  if (!length(missing_cols)) return(d)               # all present -> no-op

  lut     <- .reference_lookup(x)
  ids_chr <- as.character(d[[cols$id]])
  for (cname in missing_cols) d[[cname]] <- if (nrow(d) == 0L) numeric(0) else NA_real_
  for (id in unique(ids_chr)) {
    rows <- which(ids_chr %in% id)
    idx  <- match(id, names(lut))
    ref  <- if (!is.na(idx)) lut[[idx]] else 0
    dc <- derive_coords(d[[cols$x]][rows], d[[cols$y]][rows], reference = ref)
    for (cname in missing_cols) d[[cname]][rows] <- dc[[ want[[cname]] ]]
  }
  d
}

setAs("data.frame", "Tracks", function(from) {
  guess <- function(nms, candidates) {
    hit <- intersect(candidates, nms)
    if (length(hit)) hit[1] else NULL
  }
  id <- guess(names(from), c("id","ID","track","trajectory","animal","subject"))
  tm <- guess(names(from), c("time","t","timestamp","datetime","frame"))
  th <- guess(names(from), c("theta","angle","phi","bearing"))
  xx <- guess(names(from), c("x","X","x_pos","xcoord"))
  yy <- guess(names(from), c("y","Y","y_pos","ycoord"))

  if (is.null(id) || is.null(tm)) stop("Could not guess 'id' and 'time' columns")
  if (is.null(th) && (is.null(xx) || is.null(yy)))
    stop("Provide angle OR both x and y in the data frame")

  # Explicit: this coercion has no calibration context, so it must not
  # silently re-centre (x,y) on a per-object bounding box.
  tracks(from, id = id, time = tm, angle = th, x = xx, y = yy, angle_unit = "radians",
         normalize_xy = FALSE)
})

## ---- combine -----------------------------------------------------------------
#' @param ... Additional Tracks objects to append. All inputs must share
#'   identical column mapping and angle unit, have no colliding trajectory
#'   ids, and agree on frame_rate/time_unit/distance_scale/distance_unit/normalize_xy
#'   metadata (including all-unset); mismatches error rather than silently
#'   picking one input's values. Per-id metadata (e.g. `reference`) and the
#'   attrition records of the loads are merged across all inputs. A subset
#'   carries the whole record of its load, so recombining subsets of one load
#'   repeats that record.
#' @param recursive Ignored; maintained for signature compatibility
#' @rdname Tracks-class
#' @export
setMethod("c", signature(x="Tracks"), function(x, ..., recursive = FALSE) {
  xs <- list(x, ...)
  same_map <- vapply(xs, function(z) identical(z@cols, x@cols) && identical(z@angle_unit, x@angle_unit), logical(1))
  if (!all(same_map)) stop("All Tracks objects must share identical column mapping")

  id_lists <- lapply(xs, function(z) unique(as.character(z@data[[z@cols$id]])))
  id_counts <- table(unlist(id_lists))
  colliding <- names(id_counts)[id_counts > 1L]
  if (length(colliding)) {
    stop("c(): trajectory id(s) collide across combined Tracks objects: ",
         paste(sprintf("'%s'", colliding), collapse = ", "),
         call. = FALSE)
  }

  for (field in c("frame_rate", "time_unit", "distance_scale", "distance_unit", "normalize_xy")) {
    # an unstated time unit is a frame index
    vals <- lapply(xs, function(z)
      if (field == "time_unit") .time_unit(z) %||% "frames" else z@meta[[field]])
    if (length(unique(vals)) > 1L) {
      stop("c(): Tracks objects have conflicting '", field, "' metadata: ",
           paste(vapply(vals, function(v) if (is.null(v)) "NULL" else format(v), character(1)),
                 collapse = ", "),
           call. = FALSE)
    }
  }

  df <- do.call(rbind, lapply(xs, slot, "data"))
  # Tracks requires rows ordered by (id, time); inputs may be concatenated in
  # any order, so sort the combined data before validation.
  df <- df[order(df[[x@cols$id]], df[[x@cols$time]]), , drop = FALSE]
  histories <- lapply(xs, transform_history)
  meta <- x@meta
  meta$transform_history <- .combine_transform_histories(histories)

  ref_tbls <- Filter(Negate(is.null), lapply(xs, function(z) z@meta$reference))
  if (length(ref_tbls)) meta$reference <- do.call(rbind, ref_tbls)

  tl_tbls <- Filter(Negate(is.null), lapply(xs, function(z) z@meta$trial_limits))
  if (length(tl_tbls)) meta$trial_limits <- do.call(rbind, tl_tbls)

  sim_tbls <- Filter(Negate(is.null), lapply(xs, function(z) z@meta$sim_conditions))
  if (length(sim_tbls)) meta$sim_conditions <- do.call(rbind, sim_tbls)

  oob_points <- Filter(Negate(is.null), lapply(xs, function(z) z@meta$oob_points))
  if (length(oob_points)) meta$oob_points <- Reduce(`+`, oob_points)
  oob_trials <- Filter(Negate(is.null), lapply(xs, function(z) z@meta$oob_trials))
  if (length(oob_trials)) meta$oob_trials <- Reduce(`+`, oob_trials)
  # load records: every input's, not just the first's
  attrition <- Filter(Negate(is.null), lapply(xs, function(z) z@meta$attrition))
  if (length(attrition)) meta$attrition <- do.call(rbind, attrition)
  source_meta <- do.call(c, lapply(xs, function(z) z@meta$source_meta))
  if (length(source_meta)) meta$source_meta <- source_meta
  new("Tracks",
      data = df,
      cols = x@cols,
      angle_unit = x@angle_unit,
      meta = meta)
})
