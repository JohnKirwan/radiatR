# TrajSet: an S4 container for sets of circular trajectories
# Stores long-form observations (id, time, angle [radians], optional x, y, weight, covariates)

#' @importFrom methods setClass setValidity setGeneric setMethod new setAs slot
#' @importFrom circular circular mean.circular rho.circular
#' @keywords internal

## ---- helpers -----------------------------------------------------------------
.wrap_to_2pi <- function(x) wrap_to_2pi(x)
.as_radians <- function(x, unit = c("radians","degrees")) as_radians(x, unit)

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
  if (inherits(history, "TrajSet")) {
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
  if (!is.null(cl$raw_x) || !is.null(cl$raw_y)) {
    if (is.null(cl$raw_x) || is.null(cl$raw_y)) return("both raw_x and raw_y must be named in cols if either is used")
    if (!all(c(cl$raw_x, cl$raw_y) %in% names(d))) return("raw x/y columns not present in data")
    if (!is.numeric(d[[cl$raw_x]]) || !is.numeric(d[[cl$raw_y]])) return("raw x/y must be numeric")
  }
  if (!is.null(cl$rho)) {
    if (!cl$rho %in% names(d)) return("rho column not present in data")
    if (!is.numeric(d[[cl$rho]])) return("rho column must be numeric")
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
#' @param transform_history Optional tibble describing transformation steps applied to the
#'   trajectories. Must contain columns `step`, `order`, `id`, `implementation`, `params`,
#'   and `depends_on`.
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
                    meta = list(),
                    transform_history = NULL) {
  stopifnot(is.data.frame(df))
  angle_unit <- match.arg(angle_unit)
  if (is.null(meta)) meta <- list()

  if (!id %in% names(df))   stop("Column '", id, "' not found")
  if (!time %in% names(df)) stop("Column '", time, "' not found")

  have_angle <- !is.null(angle) && angle %in% names(df)
  have_xy    <- !is.null(x) && !is.null(y) && all(c(x,y) %in% names(df))
  if (!have_angle && !have_xy)
    stop("Provide either an angle column or both x and y columns")

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
    }

    conv <- cartesian_to_polar(d[[x]], d[[y]], normalize = normalize_xy)
    d[[x]] <- conv$x
    d[[y]] <- conv$y
    theta_from_xy <- conv$theta
    # Store radius information when available
    if (!all(is.na(conv$rho))) {
      rho_name <- make_unique_name(names(d), if (normalize_xy) "rho" else "radius")
      d[[rho_name]] <- conv$rho
      rho_col <- rho_name
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
  d <- d[order(d[[id]], d[[time]]), , drop = FALSE]

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
  if (is.null(transform_history) && !is.null(meta$transform_history)) {
    transform_history <- meta$transform_history
  }
  meta$transform_history <- .ensure_transform_history(transform_history)

  new("TrajSet",
      data = d,
      cols = list(id = id, time = time, angle = angle_col,
                  x = if (!is.null(x)) x else NULL,
                  y = if (!is.null(y)) y else NULL,
                  raw_x = raw_cols$x,
                  raw_y = raw_cols$y,
                  rho = rho_col,
                  weight = weight),
      angle_unit = angle_unit,
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
  raw_xy <- if (!is.null(object@cols$raw_x)) paste0(", raw_x='", object@cols$raw_x, "', raw_y='", object@cols$raw_y, "'") else ""
  cat(sprintf("TrajSet: %d trajectories, %d observations\n", length(ids(object)), nrow(object@data)))
  cat(sprintf("Columns: id='%s', time='%s', angle='%s' (radians)%s%s\n", id, tm, th, xy, raw_xy))
  hist <- transform_history(object)
  if (nrow(hist)) {
    steps <- unique(hist$step[order(hist$order, hist$step)])
    cat("Transform steps:", paste(steps, collapse = " -> "), "\n")
  }
  print(utils::head(object@data, 6))
})

## ---- transform history helpers ----------------------------------------------
#' Transform history helpers for TrajSet objects
#'
#' @param x A `TrajSet` object.
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
#'   `log_transform` and `set_transform_history`, the updated `TrajSet` object.
#' @name transform_history
NULL

#' @rdname transform_history
#' @export
setGeneric("transform_history", function(x) standardGeneric("transform_history"))

#' @rdname transform_history
#' @export
setMethod("transform_history", "TrajSet", function(x) {
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
setMethod("log_transform", "TrajSet", function(x, step, traj_ids = NULL,
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
setMethod("set_transform_history", "TrajSet", function(x, history) {
  x@meta$transform_history <- .ensure_transform_history(history)
  methods::validObject(x)
  x
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
  histories <- lapply(xs, transform_history)
  meta <- x@meta
  meta$transform_history <- .combine_transform_histories(histories)
  new("TrajSet",
      data = df,
      cols = x@cols,
      angle_unit = x@angle_unit,
      meta = meta)
})
