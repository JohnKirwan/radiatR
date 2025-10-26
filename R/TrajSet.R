# TrajSet: an S4 container for sets of circular trajectories
# Stores long-form observations (id, time, angle [radians], optional x, y, weight, covariates)

#' @importFrom methods setClass setValidity setGeneric setMethod new setAs slot
#' @importFrom circular circular mean.circular rho.circular
#' @keywords internal

## ---- helpers -----------------------------------------------------------------
.wrap_to_2pi <- function(x) x %% (2*pi)

.as_radians <- function(x, unit = c("radians","degrees")) {
  unit <- match.arg(unit)
  if (unit == "degrees") x <- x * pi/180
  .wrap_to_2pi(as.numeric(x))
}

# Fallback approximation for kappa from resultant length Rbar (Mardia & Jupp)
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

.est_kappa_safe <- function(tc, fallback = NA_real_, ...) {
  if (exists("est.kappa", envir = asNamespace("circular"), inherits = FALSE)) {
    fun <- get("est.kappa", envir = asNamespace("circular"), inherits = FALSE)
    res <- tryCatch(fun(tc, ...), error = function(e) NA_real_)
    if (is.numeric(res) && length(res)) {
      res <- as.numeric(res)[1]
      if (is.finite(res)) return(res)
    }
  }
  fallback
}

## ---- class -------------------------------------------------------------------
#' TrajSet container for circular trajectories
#'
#' @slot data data.frame of trajectory observations in long form
#' @slot cols list mapping required column names (id/time/angle/optional x,y,weight)
#' @slot angle_unit character describing the original angle unit supplied
#' @slot meta list of additional metadata attached to the set
#' @rdname TrajSet-class
#' @exportClass TrajSet
setClass(
  "TrajSet",
  slots = c(
    data       = "data.frame",   # long: id, time, angle(rad), optional x,y,weight,covars
    cols       = "list",         # list(id=, time=, angle=, x=NULL, y=NULL, weight=NULL)
    angle_unit = "character",    # recorded input units (always stored as radians)
    meta       = "list"
  )
)

setValidity("TrajSet", function(object) {
  d  <- object@data
  cl <- object@cols
  req <- c("id","time","angle")
  if (!all(req %in% names(cl))) return("cols must name 'id','time','angle'")

  miss <- setdiff(c(cl$id, cl$time, cl$angle), names(d))
  if (length(miss)) return(paste("missing required column(s):", paste(miss, collapse=", ")))

  if (!is.numeric(d[[cl$angle]])) return("angle column must be numeric (radians)")
  bad <- which(is.na(d[[cl$angle]]) | d[[cl$angle]] < 0 | d[[cl$angle]] >= 2*pi)
  if (length(bad)) return("angle values must be in [0, 2*pi) or NA only")

  if (!inherits(d[[cl$time]], "POSIXct") && !is.numeric(d[[cl$time]]))
    return("time must be POSIXct or numeric")

  if (!is.null(cl$x) || !is.null(cl$y)) {
    if (is.null(cl$x) || is.null(cl$y)) return("both x and y must be named in cols if either is used")
    if (!all(c(cl$x, cl$y) %in% names(d))) return("x/y columns not present in data")
    if (!is.numeric(d[[cl$x]]) || !is.numeric(d[[cl$y]])) return("x/y must be numeric")
  }

  # enforce (id,time) sorted order
  o <- order(d[[cl$id]], d[[cl$time]])
  if (!identical(o, seq_len(nrow(d)))) return("rows must be sorted by id, then time")

  TRUE
})

#' Construct a TrajSet
#'
#' @param df data.frame in long form
#' @param id,time Columns naming trajectory id and time
#' @param angle Optional angle column (radians or degrees, see angle_unit)
#' @param x,y Optional cartesian columns; if provided, converted to unit circle and angle inferred
#' @param angle_unit Units of provided angle ("radians" or "degrees"); stored internally as radians
#' @param weight Optional weight column name
#' @param normalize_xy If TRUE, (x,y) are normalized to unit vectors (zero-length -> NA)
#' @param meta Free-form list of metadata
#' @return A TrajSet S4 object
#' @rdname TrajSet-class
#' @export
TrajSet <- function(df,
                    id = "id", time = "time",
                    angle = NULL,
                    x = NULL, y = NULL,
                    angle_unit = c("radians","degrees"),
                    weight = NULL,
                    normalize_xy = TRUE,
                    meta = list()) {
  stopifnot(is.data.frame(df))
  angle_unit <- match.arg(angle_unit)

  if (!id %in% names(df))   stop("Column '", id, "' not found")
  if (!time %in% names(df)) stop("Column '", time, "' not found")

  have_angle <- !is.null(angle) && angle %in% names(df)
  have_xy    <- !is.null(x) && !is.null(y) && all(c(x,y) %in% names(df))
  if (!have_angle && !have_xy)
    stop("Provide either an angle column or both x and y columns")

  d <- df

  # If cartesian present, (optionally) normalize to unit circle and compute angle
  if (have_xy) {
    r <- sqrt(d[[x]]^2 + d[[y]]^2)
    zero <- r == 0 | is.na(r)
    if (normalize_xy) {
      d[[x]][!zero] <- d[[x]][!zero] / r[!zero]
      d[[y]][!zero] <- d[[y]][!zero] / r[!zero]
    }
    theta_from_xy <- atan2(d[[y]], d[[x]])
    theta_from_xy <- .wrap_to_2pi(theta_from_xy)
  }

  if (have_angle) {
    theta_from_ang <- .as_radians(d[[angle]], angle_unit)
  }

  # Choose authoritative angle; prefer explicit angle if provided
  if (have_angle && have_xy) {
    d$..theta_tmp <- theta_from_ang
  } else if (have_angle) {
    d$..theta_tmp <- theta_from_ang
  } else {
    d$..theta_tmp <- theta_from_xy
  }

  # Optional weights sanity
  if (!is.null(weight)) {
    if (!weight %in% names(d)) stop("weight column '", weight, "' not found")
    if (!is.numeric(d[[weight]])) stop("weight must be numeric")
  }

  # Sort by (id,time)
  d <- d[order(d[[id]], d[[time]]), , drop = FALSE]

  # Finalize angle column
  angle_col <- if (have_angle) angle else "angle"
  if (!have_angle) {
    angle_col <- "angle"
    d[[angle_col]] <- d$..theta_tmp
  } else {
    d[[angle]] <- d$..theta_tmp
  }
  d$..theta_tmp <- NULL

  new("TrajSet",
      data = d,
      cols = list(id = id, time = time, angle = angle_col,
                  x = if (have_xy) x else NULL,
                  y = if (have_xy) y else NULL,
                  weight = weight),
      angle_unit = "radians",
      meta = meta)
}

## ---- accessors & show --------------------------------------------------------
setGeneric("ids", function(x) standardGeneric("ids"))
setMethod("ids", "TrajSet", function(x) unique(x@data[[x@cols$id]]))

#' @rdname TrajSet-class
#' @export
setMethod("length", "TrajSet", function(x) length(ids(x)))

setGeneric("angles", function(x, as = c("numeric","circular"), unit = c("radians","degrees")) standardGeneric("angles"))
setMethod("angles", "TrajSet", function(x, as = c("numeric","circular"), unit = c("radians","degrees")) {
  as <- match.arg(as); unit <- match.arg(unit)
  th <- x@data[[x@cols$angle]]
  if (unit == "degrees") th <- th * 180/pi
  if (as == "circular") return(.as_circ(if (unit=="degrees") th * pi/180 else th))
  th
})

setMethod("show", "TrajSet", function(object) {
  id <- object@cols$id; tm <- object@cols$time; th <- object@cols$angle
  xy <- if (!is.null(object@cols$x)) paste0(", x='", object@cols$x, "', y='", object@cols$y, "'") else ""
  cat(sprintf("TrajSet: %d trajectories, %d observations\n", length(ids(object)), nrow(object@data)))
  cat(sprintf("Columns: id='%s', time='%s', angle='%s' (radians)%s\n", id, tm, th, xy))
  print(utils::head(object@data, 6))
})

## ---- subsetting & extraction -------------------------------------------------
#' @param i Trajectory identifiers (character ids, numeric indices, or logical vector)
#' @rdname TrajSet-class
#' @export
setMethod(
  f = "[",
  signature = c(x="TrajSet", i="ANY", j="missing", drop="missing"),
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
      stop("unsupported index type for TrajSet")
    }

    new("TrajSet", data = x@data[keep, , drop=FALSE],
        cols = x@cols, angle_unit = x@angle_unit, meta = x@meta)
  }
)

setGeneric("trajectory", function(x, id) standardGeneric("trajectory"))
setMethod("trajectory", "TrajSet", function(x, id) {
  idcol <- x@cols$id
  d <- x@data[x@data[[idcol]] == id, , drop = FALSE]
  if (!nrow(d)) stop("id not found: ", id)
  d
})

## ---- summaries ---------------------------------------------------------------
#' Circular summaries per trajectory
#' @param x TrajSet
#' @param w Optional weight column name (defaults to x@cols$weight if present)
#' @return data.frame(id, n, t_start, t_end, mean_dir, resultant_R, kappa)
#' @export
setGeneric("circ_summary", function(x, w = NULL) standardGeneric("circ_summary"))

setMethod("circ_summary", "TrajSet", function(x, w = NULL) {
  id <- x@cols$id; tm <- x@cols$time; th <- x@cols$angle
  wcol <- if (is.null(w)) x@cols$weight else w
  if (!is.null(wcol) && !wcol %in% names(x@data)) stop("weight column '", wcol, "' not found")

  d <- x@data
  idx <- split(seq_len(nrow(d)), d[[id]])

  rows <- lapply(names(idx), function(k) {
    ii <- idx[[k]]
    theta <- d[[th]][ii]
    wts <- if (!is.null(wcol)) d[[wcol]][ii] else NULL

    tc <- .as_circ(theta)
    mu <- circular::mean.circular(tc, na.rm = TRUE, weights = wts)
    R  <- circular::rho.circular(tc,  na.rm = TRUE, weights = wts)
    kap <- .est_kappa_safe(tc, fallback = .kappa_from_Rbar(as.numeric(R)), w = wts)

    data.frame(
      id          = k,
      n           = sum(!is.na(theta)),
      t_start     = d[[tm]][ii[1]],
      t_end       = d[[tm]][ii[length(ii)]],
      mean_dir    = .wrap_to_2pi(as.numeric(mu)),  # radians
      resultant_R = as.numeric(R),
      kappa       = as.numeric(kap),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
})

## ---- coercions ---------------------------------------------------------------
setMethod("as.data.frame", "TrajSet", function(x, row.names = NULL, optional = FALSE, ...) x@data)

setAs("data.frame", "TrajSet", function(from) {
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

  TrajSet(from, id = id, time = tm, angle = th, x = xx, y = yy, angle_unit = "radians")
})

## ---- combine -----------------------------------------------------------------
#' @param ... Additional TrajSet objects to append
#' @param recursive Ignored; maintained for signature compatibility
#' @rdname TrajSet-class
#' @export
setMethod("c", signature(x="TrajSet"), function(x, ..., recursive = FALSE) {
  xs <- list(x, ...)
  same_map <- vapply(xs, function(z) identical(z@cols, x@cols) && identical(z@angle_unit, x@angle_unit), logical(1))
  if (!all(same_map)) stop("All TrajSet objects must share identical column mapping")
  df <- do.call(rbind, lapply(xs, slot, "data"))
  TrajSet(df,
          id = x@cols$id, time = x@cols$time, angle = x@cols$angle,
          x = x@cols$x, y = x@cols$y, angle_unit = "radians",
          weight = x@cols$weight, meta = x@meta)
})
