# Loader utilities for radiatR: manifest importers, track discovery, and generic TrajSet readers
#

# import_info
#
#' Import landmark coordinates from text files
#'
#' @param filename Path to the CSV file describing each track/landmark pair.
#' @param cond_cols Optional character vector of column names whose values should
#'   be concatenated to create a `cond` column.
#' @param file_tbl Optional tibble produced by [import_tracks()]. When supplied,
#'   the function verifies that the listed files are present in both sources.
#' @return A data frame containing the parsed metadata (and optional `cond`
#'   column).
#' @examples
#' \dontrun{
#' manifest <- import_info("trials_list.csv", cond_cols = c("type", "arc"))
#' }
#' @importFrom utils read.csv
#' @export
#
import_info <- function(filename, cond_cols = NULL, file_tbl = NULL){
  if (missing(filename) || is.null(filename)) {
    stop("`filename` must be provided.")
  }
  df <- read.csv(file = filename,header = TRUE)

  if (!is.null(cond_cols)) {
    missing_cols <- setdiff(cond_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Columns ", paste(missing_cols, collapse = ", "),
           " requested in `cond_cols` were not found in the manifest.")
    }
    df$cond <- do.call(paste, c(df[cond_cols], sep = "_"))
  }

  if (!is.null(file_tbl)) {
    if (!"basename" %in% names(file_tbl)) {
      stop("`file_tbl` must contain a `basename` column.")
    }
    if (!"file" %in% names(df)) {
      stop("The metadata file must contain a `file` column.")
    }
    if (any(!file_tbl$basename %in% df$file)) {
      warning("Missing metadata rows for: ",
              paste(setdiff(file_tbl$basename, df$file), collapse = ", "))
    }
    if (any(!df$file %in% file_tbl$basename)) {
      warning("Manifest rows without matching files: ",
              paste(setdiff(df$file, file_tbl$basename), collapse = ", "))
    }
  }

  return(df)
}


# import_tracks
#
#' Discover dtrack (or compatible) landmark/track file pairs in a directory
#'
#' Scans \code{dir} for paired files matching \code{landmark_suffix} and
#' \code{track_suffix} and returns a tibble of basenames and paths.
#'
#' @param dir Directory to scan. Defaults to the current working directory.
#' @param landmark_suffix Suffix identifying landmark files. Default
#'   \code{"_point01.txt"}.
#' @param track_suffix Suffix identifying trajectory files. Default
#'   \code{"_point02.txt"}.
#' @return A tibble with columns \code{basename}, \code{landmark}, and
#'   \code{track}.
#' @details
#'   The default suffixes match the export naming convention used by dtrack
#'   (\url{https://bitbucket.org/jochensmolka/dtrack}). In the bundled
#'   millipede example data, \code{_point01} files contain two
#'   landmark rows per trial (the origin and stimulus edge on the circumference)
#'   and \code{_point02} files contain the per-frame animal trajectory. This
#'   two-file role split is specific to that experiment and is not a general
#'   dtrack convention. Use [dtrack_read()] to read an individual trajectory
#'   file.
#' @export
#' @importFrom tibble tibble
#
import_tracks <- function(dir, landmark_suffix = NULL, track_suffix = NULL){
  if (is.null(dir)) {dir = getwd()}
  if (is.null(landmark_suffix)) {landmark_suffix = "_point01.txt"}
  if (is.null(track_suffix)) {track_suffix = "_point02.txt"}

  dir      <-  normalizePath(dir)
  files    <- list.files(dir,recursive = TRUE)

  landmark_pattern <- paste0(escape_specials(landmark_suffix), "$")
  track_pattern <- paste0(escape_specials(track_suffix), "$")

  landmark_files <- files[grep(landmark_pattern, files)]
  if (length(landmark_files) == 0) {
    warning("No landmark files found with suffix ", landmark_suffix)
  }

  basenames <- sub(landmark_pattern, "", landmark_files)

  file_tbl <- tibble::tibble(basename = basenames)
  file_tbl$landmark <- paste0(file_tbl$basename, landmark_suffix)
  file_tbl$track <- paste0(file_tbl$basename, track_suffix)

  return(file_tbl)
}

#' Read a dtrack trajectory file into a TrajSet
#'
#' Reads a tab-separated, headerless file produced by dtrack
#' (\url{https://bitbucket.org/jochensmolka/dtrack}). The file is expected to
#' have at least three columns: frame number, x coordinate, y coordinate. A
#' fourth confidence/flag column (always \code{1} in practice) is silently
#' dropped.
#'
#' @param path Path to a dtrack \code{_point02.txt} trajectory file.
#' @param normalize_xy Logical; passed to [TrajSet_read()]. Default \code{FALSE}
#'   because dtrack files are in pixel space.
#' @param ... Additional arguments passed to [TrajSet_read()].
#' @return A \code{TrajSet}.
#' @seealso [import_tracks()] for discovering dtrack file pairs in a directory.
#' @export
dtrack_read <- function(path, normalize_xy = FALSE, ...) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  df <- utils::read.delim(path, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
  if (ncol(df) < 3L) stop("dtrack: expected at least 3 columns (frame, x, y)")
  df <- df[, 1:3, drop = FALSE]
  names(df) <- c("frame", "x", "y")
  df$id <- tools::file_path_sans_ext(basename(path))
  TrajSet_read(
    df,
    mapping = list(id = "id", time = "frame", x = "x", y = "y"),
    normalize_xy = normalize_xy,
    ...
  )
}

escape_specials <- function(x) {
  gsub("([.|()\\^{}+$*?]|\\[|\\]|\\\\)", "\\\\\\1", x)
}
# Generic loaders for TrajSet (no video-specific assumptions)
#
# Provides:
#   - TrajSet_read(x, mapping=..., angle_unit=..., ...): construct TrajSet from data.frame or file path(s)
#   - TrajSet_read_dir(dir, pattern, ...): bind multiple files
#   - register_loader_dialect(name, fun), list_loader_dialects(): pluggable dialects for known exporters
#
# Dependencies kept soft: base R by default; uses readr/data.table/arrow if installed.

#' @importFrom methods is validObject
#' @importFrom utils modifyList
#' @keywords internal

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- dialect registry ---------------------------------------------------------
.loader_registry <- new.env(parent = emptyenv())

# ---- declarative format registry ---------------------------------------------
.loader_format_registry <- new.env(parent = emptyenv())

#' Register a declarative loader *format* (list or YAML/JSON file)
#' The spec maps cleanly onto TrajSet_read() args and supports regex-based column finding.
#' @param name A unique name
#' @param spec A named list, or a path to a YAML/JSON file defining the spec
#' @param overwrite Overwrite an existing format of the same name
#' @export
register_loader_format <- function(name, spec, overwrite = FALSE) {
  stopifnot(is.character(name), length(name) == 1L)
  if (is.character(spec) && length(spec) == 1L && file.exists(spec)) {
    if (grepl("\\.ya?ml$", spec, ignore.case = TRUE)) {
      if (!.is_installed("yaml")) stop("Please install the 'yaml' package to read YAML specs")
      spec <- yaml::yaml.load_file(spec)
    } else if (grepl("\\.json$", spec, ignore.case = TRUE)) {
      if (!.is_installed("jsonlite")) stop("Please install the 'jsonlite' package to read JSON specs")
      spec <- jsonlite::fromJSON(spec, simplifyVector = TRUE)
    } else {
      stop("Unsupported spec file extension (use .yaml/.yml or .json)")
    }
  }
  if (!is.list(spec)) stop("spec must be a list or a YAML/JSON file path")
  if (exists(name, envir = .loader_format_registry, inherits = FALSE) && !overwrite)
    stop("A format named '", name, "' already exists; set overwrite = TRUE to replace")
  assign(name, spec, envir = .loader_format_registry)
  invisible(name)
}

#' List registered declarative formats
#' @export
list_loader_formats <- function() sort(ls(envir = .loader_format_registry))

.loader_registry <- new.env(parent = emptyenv())

#' Register a custom loader dialect
#' The function must accept (x, ...) and return a data.frame in long form with columns at least id,time and one of (angle) or (x,y)
#' @param name Unique dialect name
#' @param fun Function that accepts (x, ...) and returns a data.frame with id/time/angle or id/time/x/y
#' @param overwrite Replace an existing dialect registered with the same name
#' @export
register_loader_dialect <- function(name, fun, overwrite = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, is.function(fun))
  if (exists(name, envir = .loader_registry, inherits = FALSE) && !overwrite)
    stop("A loader named '", name, "' already exists; set overwrite = TRUE to replace")
  assign(name, fun, envir = .loader_registry)
  invisible(name)
}

#' List registered loader dialects
#' @export
list_loader_dialects <- function() sort(ls(envir = .loader_registry))

# ---- helpers -----------------------------------------------------------------
.is_installed <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

# Guess columns by conventional names
.guess_col <- function(nms, candidates) {
  ln <- tolower(nms)
  for (cand in candidates) {
    i <- which(ln == tolower(cand))
    if (length(i)) return(nms[i[1L]])
  }
  NULL
}

# Match a coordinate column whose name ends in the axis letter after a start or
# separator (e.g. "Track1_X" -> x). Used when no exact x/y candidate matches.
.guess_xy_suffix <- function(nms, axis) {
  re  <- sprintf("(^|[._-])%s$", axis)
  hit <- nms[grepl(re, nms, ignore.case = TRUE)]
  if (length(hit)) hit[1L] else NULL
}

#' Guess the role of each column in a track table
#'
#' Inspects a data frame's column names and returns the best guess for each
#' `TrajSet` role (`id`, `time`, `x`, `y`, `angle`, `weight`), honouring any
#' explicit `mapping` overrides. Matching is case-insensitive; `x`/`y` also match
#' separator-suffixed names such as `Track1_X`. A role with no match is `NULL`.
#' This is the same logic [TrajSet_read()] uses internally; call it to see or
#' pre-fill a column mapping. It does not synthesize missing `id`/`time` columns
#' (a `NULL` signals that `TrajSet_read()` will apply its single-track / row-order
#' fallback).
#'
#' @param data A data frame (or anything with `names()`).
#' @param mapping Optional named list of explicit role -> column overrides.
#' @return A named list with elements `id`, `time`, `x`, `y`, `angle`, `weight`
#'   (each a column name or `NULL`).
#' @examples
#' guess_columns(data.frame(Frame = 1:2, Track1_X = 0:1, Track1_Y = 0:1))
#' @export
guess_columns <- function(data, mapping = list()) {
  nms <- names(data)
  pick <- function(role, cands) mapping[[role]] %||% .guess_col(nms, cands)
  list(
    id     = pick("id",     c("id", "track", "trajectory", "animal", "subject")),
    time   = pick("time",   c("time", "t", "timestamp", "datetime", "frame", "sec", "seconds")),
    angle  = pick("angle",  c("theta", "angle", "phi", "bearing", "deg", "degrees")),
    x      = mapping$x %||% .guess_col(nms, c("x", "x_pos", "xcoord", "pos_x")) %||% .guess_xy_suffix(nms, "x"),
    y      = mapping$y %||% .guess_col(nms, c("y", "y_pos", "ycoord", "pos_y")) %||% .guess_xy_suffix(nms, "y"),
    weight = pick("weight", c("w", "weight", "weights"))
  )
}

# Drop a leading pandas-style row-index column.
# pandas `df.to_csv()` writes the DataFrame index as an unnamed first column,
# which R's CSV readers name "X" (or "" / "Unnamed: 0"). Left in place it is
# mistaken for an "x" position column by .guess_col. We only remove it when it
# is genuinely an index: the first column, an unnamed/auto-named header, and a
# consecutive integer run starting at 0 or 1.
.drop_index_col <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) return(df)
  nm1 <- names(df)[1L]
  looks_unnamed <- is.null(nm1) || is.na(nm1) ||
    grepl("^(x|v1|unnamed[._ ]*0?)$", tolower(trimws(nm1 %||% "")))
  if (!looks_unnamed) return(df)
  col <- df[[1L]]
  v <- suppressWarnings(as.numeric(col))
  if (anyNA(v) || length(v) < 2L) return(df)
  if (any(v != floor(v))) return(df)
  start <- v[1L]
  if (!(start %in% c(0, 1)) || !all(v == seq.int(start, length.out = length(v))))
    return(df)
  df[, -1L, drop = FALSE]
}

# Heuristic angle unit detection
# returns "degrees" if most values are > 2*pi or abs(values) > 2*pi by margin
.guess_angle_unit <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return("radians")
  frac_deg <- mean(abs(x) > 2*pi)
  if (is.na(frac_deg)) return("radians")
  if (frac_deg > 0.5) "degrees" else "radians"
}

# Coerce time column to POSIXct or numeric seconds
.coerce_time <- function(v, time_type = c("auto","posix","seconds","frames"),
                         tz = "UTC", fps = NULL, origin = "1970-01-01") {
  time_type <- match.arg(time_type)
  if (time_type == "auto") {
    # try POSIX
    if (inherits(v, "POSIXt")) return(as.POSIXct(v, tz = tz))
    if (is.numeric(v) && !is.null(fps)) return(as.numeric(v)/fps)
    # try character timestamp
    if (is.character(v)) {
      suppressWarnings({
        t <- as.POSIXct(v, tz = tz)
      })
      if (any(!is.na(t))) return(t)
    }
    # numeric seconds since origin
    if (is.numeric(v)) return(as.numeric(v))
    # fallback: integer-ish 
    return(as.numeric(v))
  }
  if (time_type == "posix") return(as.POSIXct(v, tz = tz, origin = origin))
  if (time_type == "seconds") return(as.numeric(v))
  if (time_type == "frames") {
    if (is.null(fps)) stop("time_type='frames' requires fps")
    return(as.numeric(v)/fps)
  }
}

# read file with soft dependencies
.read_any <- function(path, ...) {
  ext <- tolower(sub(".*\\.", "", path))
  if (.is_installed("readr") && ext %in% c("csv","tsv","txt")) {
    if (ext == "tsv") return(readr::read_tsv(path, show_col_types = FALSE, progress = FALSE, ...))
    return(readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...))
  }
  if (.is_installed("data.table") && ext %in% c("csv","tsv","txt")) {
    sep <- if (ext == "tsv") "\t" else ","
    return(data.table::fread(path, sep = sep, showProgress = FALSE, ...))
  }
  if (.is_installed("arrow") && ext %in% c("parquet","feather")) {
    if (ext == "parquet") return(arrow::read_parquet(path, ...))
    return(arrow::read_feather(path, ...))
  }
  # base fallback
  if (ext == "tsv") return(utils::read.table(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, ...))
  utils::read.table(path, header = TRUE, sep = if (ext=="csv") "," else " ", stringsAsFactors = FALSE, ...)
}

# ---- core: TrajSet_read ------------------------------------------------------
#' Construct a TrajSet from a *format* spec (registered name or inline list)
#' @param x data.frame or path(s)
#' @param format registered format name or list spec
#' @param ... extra args override spec fields
#' @export
TrajSet_read_format <- function(x, format, ...) {
  spec <- if (is.character(format) && exists(format, envir = .loader_format_registry, inherits = FALSE)) {
    get(format, envir = .loader_format_registry, inherits = FALSE)
  } else if (is.list(format)) {
    format
  } else stop("format must be a registered name or a list spec")

  # Extract/merge fields with defaults; allow ... to override
  dots <- list(...)
  mapping <- spec$mapping %||% list()

  # regex mapping (resolve against first file or df headers)
  if (!is.null(spec$regex_mapping)) {
    nm <- NULL
    if (is.data.frame(x)) nm <- names(x)
    else if (is.character(x) && length(x) >= 1L && file.exists(x[1])) {
      peek <- .read_any(x[1])
      nm <- names(peek)
    }
    if (!is.null(nm)) {
      pick <- function(pats) {
        if (is.null(pats)) return(NULL)
        pats <- if (is.character(pats)) pats else unlist(pats)
        hits <- lapply(pats, function(p) grep(p, nm, value = TRUE, ignore.case = TRUE))
        unique(unlist(hits))[1]
      }
      rm <- spec$regex_mapping
      mapping <- modifyList(mapping, list(
        id     = mapping$id     %||% pick(rm$id),
        time   = mapping$time   %||% pick(rm$time),
        x      = mapping$x      %||% pick(rm$x),
        y      = mapping$y      %||% pick(rm$y),
        angle  = mapping$angle  %||% pick(rm$angle),
        weight = mapping$weight %||% pick(rm$weight)
      ))
    }
  }

  # Optional pre-processing via a dialect from spec
  dialect <- spec$dialect %||% NULL
  if (!is.null(dialect) && !is.null(spec$dialect_args)) {
    dots$dialect <- dialect
    # dial args handled by TrajSet_read via dialect=... and will pass along ...
    for (nm in names(spec$dialect_args)) if (is.null(dots[[nm]])) dots[[nm]] <- spec$dialect_args[[nm]]
  } else if (!is.null(dialect)) {
    dots$dialect <- dialect
  }

  args <- list(
    mapping        = mapping,
    angle_unit     = spec$angle$unit %||% spec$angle_unit %||% "auto",
    time_type      = spec$time$type  %||% spec$time_type %||% "auto",
    tz             = spec$time$tz    %||% spec$tz %||% "UTC",
    fps            = spec$time$fps   %||% spec$fps %||% NULL,
    normalize_xy   = if (!is.null(spec$normalize_xy)) isTRUE(spec$normalize_xy) else TRUE,
    mutate         = spec$mutate %||% NULL,
    keep           = spec$keep %||% NULL,
    drop           = spec$drop %||% NULL,
    id_from_filename = if (!is.null(spec$id_from_filename)) isTRUE(spec$id_from_filename) else TRUE,
    validate       = if (!is.null(spec$validate)) isTRUE(spec$validate) else TRUE
  )

  # Allow explicit overrides via ...
  args <- modifyList(args, dots)
  do.call(TrajSet_read, c(list(x = x), args))
}

#' Construct a TrajSet from a data.frame or file(s)
#'
#' @param x data.frame, file path, or character vector of file paths
#' @param mapping named list for column mapping: id, time, x, y, angle, weight. Any missing will be guessed when possible.
#' @param angle_unit "radians","degrees", or "auto" to guess from values
#' @param time_type one of "auto","posix","seconds","frames"
#' @param tz timezone for POSIX times (default "UTC")
#' @param fps frames-per-second when time_type = "frames"
#' @param normalize_xy TRUE to normalize (x,y) to unit circle when both provided
#' @param dialect optional registered dialect name to pre-process raw input
#' @param dialect_args named list of extra arguments forwarded to the dialect
#'   function (e.g. \code{list(bodypart = c("head","thorax"))})
#' @param mutate list of transformations applied after reading (named functions or formulas)
#' @param keep only keep these columns (NULL = keep all)
#' @param drop drop these columns after mapping
#' @param id_from_filename if TRUE and id missing, derive id from file stem when reading multiple files
#' @param validate if TRUE run S4 validity checks
#' @param format Optional loader format name or list spec registered via [register_loader_format()]
#' @return TrajSet
#' @export
TrajSet_read <- function(x,
                         mapping = list(id=NULL, time=NULL, x=NULL, y=NULL, angle=NULL, weight=NULL),
                         angle_unit = c("radians","degrees","auto"),
                         time_type = c("auto","posix","seconds","frames"), tz = "UTC", fps = NULL,
                         normalize_xy = TRUE,
                         dialect = NULL,
                         dialect_args = list(),
                         mutate = NULL,
                         keep = NULL, drop = NULL,
                         id_from_filename = TRUE,
                         validate = TRUE,
                         format = NULL) {
  angle_unit <- match.arg(angle_unit)
  time_type  <- match.arg(time_type)

  if (!is.null(format)) {
    return(TrajSet_read_format(
      x,
      format,
      mapping = mapping,
      angle_unit = angle_unit,
      time_type = time_type,
      tz = tz,
      fps = fps,
      normalize_xy = normalize_xy,
      dialect = dialect,
      mutate = mutate,
      keep = keep,
      drop = drop,
      id_from_filename = id_from_filename,
      validate = validate
    ))
  }

  # read
  if (is.data.frame(x)) {
    df <- x
    src <- NULL
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    df <- .read_any(x)
    src <- basename(x)
  } else if (is.character(x) && length(x) > 1L) {
    dfl <- lapply(x, function(p) {
      d <- .read_any(p)
      if (id_from_filename && !is.null(mapping$id) && !(mapping$id %in% names(d))) {
        d[[mapping$id]] <- tools::file_path_sans_ext(basename(p))
      }
      d$..source_file <- basename(p)
      d
    })
    df <- do.call(rbind, dfl)
    src <- "(multiple)"
  } else if (is.character(x) && length(x) == 1L && !file.exists(x) && exists(x, envir = .loader_registry, inherits = FALSE)) {
    dialect <- x
    df <- NULL
  } else {
    stop("TrajSet_read: 'x' must be a data.frame or path(s) to files")
  }

  # dialect preprocessing if requested
  if (!is.null(dialect)) {
    if (!exists(dialect, envir = .loader_registry, inherits = FALSE)) stop("Unknown dialect '", dialect, "'")
    prep <- get(dialect, envir = .loader_registry, inherits = FALSE)
    df <- do.call(prep, c(list(x), dialect_args))
  }
  # Extra columns emitted by a dialect are kept unless explicitly dropped.
  dialect_extra <- if (!is.null(dialect)) names(df) else character(0)

  stopifnot(is.data.frame(df))

  nms <- names(df)
  # Guess columns if not mapped
  id <- mapping$id %||% .guess_col(nms, c("id","ID","track","trajectory","animal","subject"))
  time <- mapping$time %||% .guess_col(nms, c("time","t","timestamp","datetime","frame","sec","seconds"))
  angle <- mapping$angle %||% .guess_col(nms, c("theta","angle","phi","bearing","deg","degrees"))
  xcol <- mapping$x %||% .guess_col(nms, c("x","X","x_pos","xcoord","pos_x"))
  ycol <- mapping$y %||% .guess_col(nms, c("y","Y","y_pos","ycoord","pos_y"))
  wcol <- mapping$weight %||% .guess_col(nms, c("w","weight","weights"))

  if (is.null(id) || is.null(time)) stop("Could not identify 'id' and 'time' columns; specify mapping=")
  if (is.null(angle) && (is.null(xcol) || is.null(ycol)))
    stop("Need either an angle column or both x and y columns; specify mapping=")

  # coarse type fixes
  if (!is.numeric(df[[id]])) df[[id]] <- as.character(df[[id]])
  # time coercion
  df[[time]] <- .coerce_time(df[[time]], time_type = time_type, tz = tz, fps = fps)

  # angle unit
  ang_unit <- if (angle_unit == "auto" && !is.null(angle)) .guess_angle_unit(df[[angle]]) else angle_unit

  # cleanup names and select
  keep_cols <- unique(c(id, time, angle, xcol, ycol, wcol, keep, dialect_extra, "..source_file"))
  keep_cols <- keep_cols[keep_cols %in% names(df)]
  if (!is.null(drop)) keep_cols <- setdiff(keep_cols, drop)
  df <- df[, keep_cols, drop = FALSE]

  # user mutations
  if (!is.null(mutate)) {
    for (nm in names(mutate)) {
      fn <- mutate[[nm]]
      if (is.function(fn)) {
        df[[nm]] <- fn(df)
      } else if (inherits(fn, "formula")) {
        env <- environment(fn) %||% parent.frame()
        df[[nm]] <- with(df, eval(attr(terms(fn), "variables")[[2]], envir = env))
      }
    }
  }

  # Hand off to TrajSet constructor
  ts <- TrajSet(df,
                id = id, time = time, angle = angle,
                x = xcol, y = ycol,
                angle_unit = ang_unit,
                weight = wcol,
                normalize_xy = normalize_xy,
                meta = list(source = src))

  if (isTRUE(validate)) validObject(ts)
  ts
}

#' Construct a TrajSet from a data.frame or file(s)
#'
#' @param x data.frame, file path, or character vector of file paths
#' @param mapping named list for column mapping: id, time, x, y, angle, weight. Any missing will be guessed when possible.
#' @param angle_unit "radians","degrees", or "auto" to guess from values
#' @param time_type one of "auto","posix","seconds","frames"
#' @param tz timezone for POSIX times (default "UTC")
#' @param fps frames-per-second when time_type = "frames"
#' @param normalize_xy TRUE to normalize (x,y) to unit circle when both provided
#' @param dialect optional registered dialect name to pre-process raw input
#' @param dialect_args named list of extra arguments forwarded to the dialect
#'   function (e.g. \code{list(bodypart = c("head","thorax"))})
#' @param mutate list of transformations applied after reading (named functions or formulas)
#' @param keep only keep these columns (NULL = keep all)
#' @param drop drop these columns after mapping
#' @param id_from_filename if TRUE and id missing, derive id from file stem when reading multiple files
#' @param validate if TRUE run S4 validity checks
#' @param format Optional loader format name or list spec registered via [register_loader_format()]
#' @return TrajSet
#' @export
TrajSet_read <- function(x,
                         mapping = list(id=NULL, time=NULL, x=NULL, y=NULL, angle=NULL, weight=NULL),
                         angle_unit = c("radians","degrees","auto"),
                         time_type = c("auto","posix","seconds","frames"), tz = "UTC", fps = NULL,
                         normalize_xy = TRUE,
                         dialect = NULL,
                         dialect_args = list(),
                         mutate = NULL,
                         keep = NULL, drop = NULL,
                         id_from_filename = TRUE,
                         validate = TRUE,
                         format = NULL) {
  angle_unit <- match.arg(angle_unit)
  time_type  <- match.arg(time_type)

  if (!is.null(format)) {
    return(TrajSet_read_format(
      x,
      format,
      mapping = mapping,
      angle_unit = angle_unit,
      time_type = time_type,
      tz = tz,
      fps = fps,
      normalize_xy = normalize_xy,
      dialect = dialect,
      mutate = mutate,
      keep = keep,
      drop = drop,
      id_from_filename = id_from_filename,
      validate = validate
    ))
  }

  # read
  if (is.data.frame(x)) {
    df <- x
    src <- NULL
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    df <- .read_any(x)
    src <- basename(x)
  } else if (is.character(x) && length(x) > 1L) {
    dfl <- lapply(x, function(p) {
      d <- .read_any(p)
      if (id_from_filename && !is.null(mapping$id) && !(mapping$id %in% names(d))) {
        d[[mapping$id]] <- tools::file_path_sans_ext(basename(p))
      }
      d$..source_file <- basename(p)
      d
    })
    df <- do.call(rbind, dfl)
    src <- "(multiple)"
  } else if (is.character(x) && length(x) == 1L && !file.exists(x) && exists(x, envir = .loader_registry, inherits = FALSE)) {
    dialect <- x
    df <- NULL
  } else {
    stop("TrajSet_read: 'x' must be a data.frame or path(s) to files")
  }

  # dialect preprocessing if requested
  if (!is.null(dialect)) {
    if (!exists(dialect, envir = .loader_registry, inherits = FALSE)) stop("Unknown dialect '", dialect, "'")
    prep <- get(dialect, envir = .loader_registry, inherits = FALSE)
    df <- do.call(prep, c(list(x), dialect_args))
  }
  # Extra columns emitted by a dialect are kept unless explicitly dropped.
  dialect_extra <- if (!is.null(dialect)) names(df) else character(0)

  stopifnot(is.data.frame(df))

  guessed <- guess_columns(df, mapping)
  id <- guessed$id; time <- guessed$time; angle <- guessed$angle
  xcol <- guessed$x; ycol <- guessed$y; wcol <- guessed$weight

  if (is.null(angle) && (is.null(xcol) || is.null(ycol)))
    stop("Need either an angle column or both x and y columns; specify mapping=")

  # Single-track id fallback: no id column -> treat the file as one trajectory.
  if (is.null(id)) {
    message("No id column found; treating the file as a single trajectory.")
    df[["..id"]] <- if (!is.null(src) && isTRUE(id_from_filename))
      tools::file_path_sans_ext(src) else "1"
    id <- "..id"
  }
  # Row-order time fallback: no time/frame column -> use row order.
  if (is.null(time)) {
    message("No time/frame column found; using row order as time.")
    df[["..time"]] <- seq_len(nrow(df))
    time <- "..time"
  }
  # Drop rows with non-finite coordinates (position-based load).
  if (!is.null(xcol) && !is.null(ycol)) {
    finite <- is.finite(df[[xcol]]) & is.finite(df[[ycol]])
    n_bad <- sum(!finite)
    if (n_bad > 0L) {
      message(sprintf("Dropped %d row(s) with non-finite coordinates.", n_bad))
      df <- df[finite, , drop = FALSE]
    }
  }

  # coarse type fixes
  if (!is.numeric(df[[id]])) df[[id]] <- as.character(df[[id]])
  # time coercion
  df[[time]] <- .coerce_time(df[[time]], time_type = time_type, tz = tz, fps = fps)

  # angle unit
  ang_unit <- if (angle_unit == "auto" && !is.null(angle)) .guess_angle_unit(df[[angle]]) else angle_unit

  # cleanup names and select
  keep_cols <- unique(c(id, time, angle, xcol, ycol, wcol, keep, dialect_extra, "..source_file"))
  keep_cols <- keep_cols[keep_cols %in% names(df)]
  if (!is.null(drop)) keep_cols <- setdiff(keep_cols, drop)
  df <- df[, keep_cols, drop = FALSE]

  # user mutations
  if (!is.null(mutate)) {
    for (nm in names(mutate)) {
      fn <- mutate[[nm]]
      if (is.function(fn)) {
        df[[nm]] <- fn(df)
      } else if (inherits(fn, "formula")) {
        # very small formula interface: ~ with(df, expr)
        env <- environment(fn) %||% parent.frame()
        df[[nm]] <- with(df, eval(attr(terms(fn), "variables")[[2]], envir = env))
      }
    }
  }

  # Hand off to TrajSet constructor
  ts <- TrajSet(df,
                id = id, time = time, angle = angle,
                x = xcol, y = ycol,
                angle_unit = ang_unit,
                weight = wcol,
                normalize_xy = normalize_xy,
                meta = list(source = src))

  if (isTRUE(validate)) validObject(ts)
  ts
}

#' Read all matching files from a directory and bind into a TrajSet
#' @param dir Directory to scan for files
#' @param pattern Regex passed to `list.files()` to select files
#' @param recursive Recurse into subdirectories when TRUE
#' @param ... Additional arguments passed to `TrajSet_read()`
#' @export
TrajSet_read_dir <- function(dir, pattern = "\\.(csv|tsv|txt|parquet|feather)$", recursive = FALSE, ...) {
  stopifnot(dir.exists(dir))
  files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (!length(files)) stop("No files matched in ", dir)
  TrajSet_read(files, ...)
}

.is_absolute_path <- function(path) {
  grepl("^([A-Za-z]:|\\\\\\\\|/|~)", path)
}

.resolve_track_paths <- function(track_dir, tracks) {
  if (missing(track_dir) || is.null(track_dir)) stop("`track_dir` must be provided.")
  base_dir <- normalizePath(track_dir, winslash = "/", mustWork = TRUE)
  vapply(tracks, function(trk) {
    if (is.na(trk) || identical(trk, "")) {
      stop("`file_tbl$track` contains missing entries.")
    }
    target <- if (.is_absolute_path(trk)) trk else file.path(base_dir, trk)
    normalizePath(target, winslash = "/", mustWork = FALSE)
  }, character(1))
}

.validate_track_paths <- function(resolved_paths) {
  missing <- resolved_paths[!file.exists(resolved_paths)]
  if (length(missing)) {
    stop("Missing track files: ", paste(unique(missing), collapse = ", "))
  }
  normalizePath(resolved_paths, winslash = "/", mustWork = TRUE)
}

.augment_with_manifest <- function(file_tbl, manifest, manifest_cols) {
  if (is.null(manifest)) return(file_tbl)
  if (!"file" %in% names(manifest)) {
    stop("`manifest` must contain a `file` column matching `file_tbl$basename`.")
  }

  if (is.null(manifest_cols)) {
    manifest_cols <- setdiff(names(manifest), "file")
    manifest_cols <- setNames(manifest_cols, manifest_cols)
  } else if (is.list(manifest_cols)) {
    manifest_cols <- unlist(manifest_cols)
  }

  if (!is.character(manifest_cols) || is.null(names(manifest_cols))) {
    stop("`manifest_cols` must be a named character vector or list.")
  }

  missing_cols <- setdiff(unname(manifest_cols), names(manifest))
  if (length(missing_cols)) {
    stop("Columns ", paste(missing_cols, collapse = ", "),
         " specified in `manifest_cols` were not found in `manifest`.")
  }

  match_idx <- match(file_tbl$basename, manifest$file)
  unmatched_file_tbl <- file_tbl$basename[is.na(match_idx)]
  if (length(unmatched_file_tbl)) {
    warning("Entries in `file_tbl` with no matching metadata: ",
            paste(unique(unmatched_file_tbl), collapse = ", "))
  }
  unmatched_manifest <- setdiff(manifest$file, file_tbl$basename)
  if (length(unmatched_manifest)) {
    warning("Rows in `manifest` with no corresponding track: ",
            paste(unique(unmatched_manifest), collapse = ", "))
  }

  out <- file_tbl
  for (new_col in names(manifest_cols)) {
    src <- manifest_cols[[new_col]]
    out[[new_col]] <- manifest[[src]][match_idx]
  }
  out
}

#' Load trajectories listed in a file table into a TrajSet
#'
#' This high-level helper combines the file discovery tibble returned by
#' [import_tracks()] with optional metadata from an experiment manifest, reads
#' each track file using [TrajSet_read()], and merges the results into a single
#' `TrajSet`.
#'
#' @param file_tbl Tibble returned by [import_tracks()], containing at least the
#'   columns `basename` and `track`.
#' @param track_dir Directory containing the track files on disk.
#' @param manifest Optional data frame with a `file` column that matches
#'   `file_tbl$basename`.
#' @param manifest_cols Optional named character vector (or list) mapping new
#'   column names in `file_tbl` to column names present in `manifest`. When
#'   `NULL`, all columns aside from `file` are carried over with the same names.
#' @param mapping Optional explicit column mapping passed to [TrajSet_read()].
#' @param angle_unit,time_type,tz,fps Passed to [TrajSet_read()].
#' @param normalize_xy Logical; normalise x/y to unit circle. Default \code{TRUE}.
#' @param dialect Optional dialect name; passed to [TrajSet_read()].
#' @param keep,drop Column selection vectors passed to [TrajSet_read()].
#' @param ... Additional arguments forwarded to [TrajSet_read()].
#'
#' @return A `TrajSet` with metadata columns replicated for each observation.
#' @export
TrajSet_load_manifest <- function(file_tbl, track_dir, manifest = NULL,
                                  manifest_cols = NULL,
                                  mapping = NULL,
                                  angle_unit = c("radians","degrees","auto"),
                                  time_type = c("auto","posix","seconds","frames"),
                                  tz = "UTC", fps = NULL,
                                  normalize_xy = TRUE,
                                  dialect = NULL,
                                  keep = NULL, drop = NULL, ...) {
  angle_unit <- match.arg(angle_unit)
  time_type <- match.arg(time_type)
  required_cols <- c("basename", "track")
  missing_required <- setdiff(required_cols, names(file_tbl))
  if (length(missing_required)) {
    stop("`file_tbl` is missing required column(s): ",
         paste(missing_required, collapse = ", "))
  }

  augmented_tbl <- .augment_with_manifest(file_tbl, manifest, manifest_cols)
  resolved_paths <- .resolve_track_paths(track_dir, augmented_tbl$track)
  resolved_paths <- .validate_track_paths(resolved_paths)

  if (!nrow(augmented_tbl)) {
    stop("`file_tbl` has no rows to load.")
  }

  metadata_cols <- setdiff(names(augmented_tbl), character(0))
  traj_list <- vector("list", nrow(augmented_tbl))

  reader_args <- list(mapping = mapping, angle_unit = angle_unit,
                      time_type = time_type, tz = tz, fps = fps,
                      normalize_xy = normalize_xy, dialect = dialect,
                      keep = keep, drop = drop)

  for (i in seq_len(nrow(augmented_tbl))) {
    path <- resolved_paths[i]
    args <- c(list(x = path), reader_args, list(...))
    ts <- do.call(TrajSet_read, args)

    n_rows <- nrow(ts@data)
    row_vals <- augmented_tbl[i, metadata_cols, drop = FALSE]
    for (col in names(row_vals)) {
      value <- row_vals[[col]]
      if (is.factor(value)) value <- as.character(value)
      ts@data[[col]] <- rep(value, n_rows)
    }
    if (!"source_file" %in% names(ts@data)) {
      ts@data$source_file <- rep(path, n_rows)
    }
    traj_list[[i]] <- ts
  }

  combined <- traj_list[[1]]
  if (length(traj_list) > 1) {
    for (i in 2:length(traj_list)) {
      combined <- c(combined, traj_list[[i]])
    }
  }
  combined
}

#' Legacy helper to merge manifest metadata with a track table
#'
#' @param file_tbl Tibble returned by [import_tracks()].
#' @param df Data frame containing at least a `file` column and, optionally,
#'   `arc`, `type`, `obstacle`, and `id`.
#' @param track_dir Directory containing the track files (used for validation).
#' @return `file_tbl` with additional metadata columns bound in.
#' @export
load_tracks <- function(file_tbl, df, track_dir) {
  if (missing(df)) stop("`df` must be supplied.")
  # Carry every manifest column (all non-`file` columns); `.augment_with_manifest`
  # does this when `manifest_cols = NULL`. Use load_tracks2() to rename/restrict.
  out <- .augment_with_manifest(file_tbl, df, NULL)
  .validate_track_paths(.resolve_track_paths(track_dir, out$track))
  out
}

#' Flexible metadata join for track tables
#'
#' @param file_tbl Tibble returned by [import_tracks()].
#' @param df Manifest data frame containing a `file` column.
#' @param track_dir Directory containing the track files (used for validation).
#' @param colnames Named character vector (or list) mapping the desired column
#'   names in the output to columns in `df`.
#' @return `file_tbl` with additional columns appended.
#' @export
load_tracks2 <- function(file_tbl, df, track_dir, colnames) {
  if (missing(colnames) || is.null(colnames) || !length(colnames)) {
    stop("`colnames` must be a non-empty named character vector or list.")
  }
  if (is.list(colnames)) colnames <- unlist(colnames)
  if (is.null(names(colnames))) {
    stop("`colnames` must be named so that new column names can be assigned.")
  }

  out <- .augment_with_manifest(file_tbl, df, colnames)
  .validate_track_paths(.resolve_track_paths(track_dir, out$track))
  out
}

# ---- example dialects (opt-in) -----------------------------------------------
# Declarative *format* examples (YAML/JSON). Users can register a spec and then call TrajSet_read_format(...).
# YAML example:
# ---
# mapping:
#   id: subject
#   time: frame
# regex_mapping:
#   x: ["^x$", "^pos_x$"]
#   y: ["^y$", "^pos_y$"]
# time:
#   type: frames
#   fps: 60
# angle:
#   unit: auto
# normalize_xy: true
# keep: ["treatment", "session"]
# drop: ["junk"]
# dialect: null
#
# R usage:
#   register_loader_format("my_frames_spec", "/path/to/spec.yaml")
#   ts <- TrajSet_read_format("/data/dir/run1.csv", format = "my_frames_spec")

# Users can register lightweight mappers here without adding heavy deps.
# Example: a common "wide" layout with prefixed columns per animal: x_<id>, y_<id>
register_loader_dialect("wide_prefix_xy", function(x, id_prefixes = NULL, time_col = NULL) {
  stopifnot(is.data.frame(x))
  if (is.null(id_prefixes)) {
    # infer prefixes based on x_*/y_* pattern
    xp <- grep("^x_", names(x), value = TRUE)
    if (!length(xp)) stop("No columns starting with 'x_' found")
    id_prefixes <- sub("^x_", "", xp)
  }
  if (is.null(time_col)) time_col <- .guess_col(names(x), c("time","frame","t","timestamp"))
  if (is.null(time_col)) stop("time column not found; specify time_col")
  rows <- lapply(id_prefixes, function(idv) {
    xc <- paste0("x_", idv); yc <- paste0("y_", idv)
    if (!all(c(xc,yc) %in% names(x))) return(NULL)
    data.frame(id = idv, time = x[[time_col]], x = x[[xc]], y = x[[yc]], stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)
  df
})

# ---- built-in dialects -------------------------------------------------------
# DeepLabCut: per-bodypart *_x / *_y columns (post-multiheader collapse or plain CSV).
#
# bodypart = NULL       -> centroid of ALL detected bodyparts (likelihood-weighted)
# bodypart = "head"     -> single bodypart, original row-filter behaviour
# bodypart = c("h","t") -> centroid of the named subset
#
# Per-bodypart <name>_x / <name>_y columns are always appended to the output so
# downstream heading rules (e.g. bodypart_axis) can access individual points.
register_loader_dialect("deeplabcut", function(x, bodypart = NULL, likelihood_min = NULL,
                                                id_col = NULL, time_col = NULL, fps = NULL) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  nms <- names(df)

  # Discover bodypart names from <name>_x / <name>_y pairs
  bpx     <- sub("_x$", "", grep("_x$", nms, value = TRUE))
  bpy     <- sub("_y$", "", grep("_y$", nms, value = TRUE))
  bps_all <- intersect(bpx, bpy)
  if (!length(bps_all)) stop("deeplabcut: no *_x/*_y column pairs found")

  sel <- if (is.null(bodypart)) bps_all else bodypart
  bad <- setdiff(sel, bps_all)
  if (length(bad))
    stop("deeplabcut: bodypart(s) not found: ", paste(bad, collapse = ", "),
         "; available: ", paste(bps_all, collapse = ", "))

  id   <- id_col %||% .guess_col(nms, c("id","individual","animal","subject")) %||% "1"
  time <- time_col %||% .guess_col(nms, c("time","timestamp","t","frame","frames")) %||% "..row"
  if (time == "..row") df[[time]] <- seq_len(nrow(df))

  if (length(sel) == 1L) {
    # single bodypart: row-level likelihood filter, x/y from that bodypart
    xc <- paste0(sel, "_x"); yc <- paste0(sel, "_y"); lc <- paste0(sel, "_likelihood")
    if (!is.null(likelihood_min) && lc %in% nms)
      df <- df[!is.na(df[[lc]]) & df[[lc]] >= as.numeric(likelihood_min), , drop = FALSE]
    cx <- as.numeric(df[[xc]]); cy <- as.numeric(df[[yc]])
  } else {
    # multi-bodypart: per-row likelihood-weighted centroid
    # bodyparts below likelihood_min contribute zero weight for that row
    xs <- vapply(sel, function(bp) as.numeric(df[[paste0(bp, "_x")]]), numeric(nrow(df)))
    ys <- vapply(sel, function(bp) as.numeric(df[[paste0(bp, "_y")]]), numeric(nrow(df)))
    dim(xs) <- c(nrow(df), length(sel)); dim(ys) <- c(nrow(df), length(sel))
    ws <- vapply(sel, function(bp) {
      lc <- paste0(bp, "_likelihood")
      w  <- if (lc %in% nms) as.numeric(df[[lc]]) else rep(1, nrow(df))
      # NA likelihood with no threshold -> include at equal weight;
      # NA likelihood with a threshold -> exclude (can't verify it passes)
      if (is.null(likelihood_min)) w[is.na(w)] <- 1 else {
        w[is.na(w)] <- 0; w[!is.na(w) & w < as.numeric(likelihood_min)] <- 0
      }
      w
    }, numeric(nrow(df)))
    dim(ws) <- c(nrow(df), length(sel))
    wsum <- rowSums(ws)
    cx <- ifelse(wsum > 0, rowSums(xs * ws) / wsum, NA_real_)
    cy <- ifelse(wsum > 0, rowSums(ys * ws) / wsum, NA_real_)
  }

  out <- data.frame(
    id   = if (length(id) == 1L && id == "1") rep("1", nrow(df)) else df[[id]],
    time = .coerce_time(df[[time]],
                        time_type = if (!is.null(fps) && grepl("frame", time, ignore.case = TRUE)) "frames" else "auto",
                        fps = fps),
    x = cx, y = cy,
    stringsAsFactors = FALSE
  )
  # Always append per-bodypart columns for use with bodypart_axis heading rule
  for (bp in sel) {
    out[[paste0(bp, "_x")]] <- as.numeric(df[[paste0(bp, "_x")]])
    out[[paste0(bp, "_y")]] <- as.numeric(df[[paste0(bp, "_y")]])
  }
  out
})

# DeepLabCut three-row multiheader CSV: collapses scorer/bodypart/coord rows into
# <bodypart>_<coord> columns, then delegates to the deeplabcut dialect.
# Supports the same bodypart selection and multi-bodypart centroid logic.
register_loader_dialect("deeplabcut_multiheader", function(path, bodypart = NULL, ...) {
  stopifnot(is.character(path) && file.exists(path))
  lines <- readLines(path, n = 3)
  # if it's not a three-row header, fall back to standard reader
  if (length(lines) < 3) return(get("deeplabcut", envir = .loader_registry)(path, bodypart = bodypart, ...))
  # rebuild colnames by pasting row2_row3
  raw <- utils::read.csv(path, header = FALSE, skip = 3, check.names = FALSE, stringsAsFactors = FALSE)
  hdr1 <- strsplit(lines[1], ",", fixed = TRUE)[[1]]
  hdr2 <- strsplit(lines[2], ",", fixed = TRUE)[[1]]
  hdr3 <- strsplit(lines[3], ",", fixed = TRUE)[[1]]
  len <- min(length(hdr2), length(hdr3))
  cn  <- paste0(hdr2[seq_len(len)], "_", hdr3[seq_len(len)])
  names(raw)[seq_along(cn)] <- cn
  get("deeplabcut", envir = .loader_registry)(raw, bodypart = bodypart, ...)
})

# idtracker.ai wide format: x_1,y_1,x_2,y_2,... (+ optional frame/time)
register_loader_dialect("idtrackerai_wide", function(x, time_col = NULL, fps = NULL, ids = NULL) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  nms <- names(df)
  xs <- grep("^x_[0-9]+$", nms, value = TRUE)
  ys <- sub("^x_", "y_", xs)
  keep <- ys %in% nms
  xs <- xs[keep]; ys <- ys[keep]
  if (!length(xs)) stop("idtrackerai_wide: no x_<id>/y_<id> pairs found")
  if (is.null(time_col)) time_col <- .guess_col(nms, c("time","t","frame","frames")) %||% "..row"
  if (time_col == "..row") df[[time_col]] <- seq_len(nrow(df))
  ids_found <- sub("^x_", "", xs)
  if (!is.null(ids)) ids_found <- intersect(ids_found, as.character(ids))
  rows <- lapply(ids_found, function(idv) {
    xc <- paste0("x_", idv); yc <- paste0("y_", idv)
    data.frame(id = idv, time = df[[time_col]], x = df[[xc]], y = df[[yc]], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$time <- .coerce_time(out$time, time_type = if (!is.null(fps) && grepl("frame", time_col, ignore.case = TRUE)) "frames" else "auto", fps = fps)
  out
})

# EthoVision XT: single- or multi-body-point exports.
# Standard export: "X Center" / "Y Center" -> x_center / y_center (prefix style).
# Multiple Body Point Tracking export adds "X Nose", "Y Nose", "X Tail", "Y Tail"
# (also prefix style) or "<Zone> point X" / "<Zone> point Y" (suffix style).
# zone = NULL  -> use the centre/primary position (backward-compatible default).
# zone = "nose"               -> position from that single zone.
# zone = c("nose", "tail")    -> equal-weight centroid of those zones.
# All detected zone columns are always appended for use with bodypart_axis.
register_loader_dialect("ethovision", function(x, id_col = NULL, time_col = NULL,
                                               zone = NULL) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  nms  <- norm(names(df)); names(df) <- nms

  # Detect zones from prefix style (x_<zone>, y_<zone>) and suffix style (<zone>_x, <zone>_y).
  # Normalise prefix zones to suffix style so access is uniform.
  zones_a <- intersect(sub("^x_", "", grep("^x_.+", nms, value = TRUE)),
                       sub("^y_", "", grep("^y_.+", nms, value = TRUE)))
  zones_b <- intersect(sub("_x$",  "", grep(".+_x$",  nms, value = TRUE)),
                       sub("_y$",  "", grep(".+_y$",  nms, value = TRUE)))
  for (z in setdiff(zones_a, zones_b)) {
    df[[paste0(z, "_x")]] <- df[[paste0("x_", z)]]
    df[[paste0(z, "_y")]] <- df[[paste0("y_", z)]]
  }
  nms      <- names(df)
  zones_all <- setdiff(union(zones_a, zones_b), c("x", "y", ""))

  id   <- id_col %||% .guess_col(nms, c("id","animal","subject","trial","track_id")) %||% "1"
  time <- time_col %||% .guess_col(nms, c("time","time_s","position_t","t","frame")) %||% "..row"
  if (time == "..row") { df[["..row"]] <- seq_len(nrow(df)); time <- "..row" }
  xc   <- .guess_col(nms, c("x_center","x_centre","position_x","x"))
  yc   <- .guess_col(nms, c("y_center","y_centre","position_y","y"))
  if (is.null(xc) || is.null(yc)) stop("ethovision: could not find x/y columns")

  if (!is.null(zone)) {
    sel <- zone
    bad <- setdiff(sel, zones_all)
    if (length(bad))
      stop("ethovision: zone(s) not found: ", paste(bad, collapse = ", "),
           "; available: ", paste(zones_all, collapse = ", "))
    if (length(sel) == 1L) {
      cx <- as.numeric(df[[paste0(sel, "_x")]])
      cy <- as.numeric(df[[paste0(sel, "_y")]])
    } else {
      xs   <- vapply(sel, function(z) as.numeric(df[[paste0(z, "_x")]]), numeric(nrow(df)))
      ys   <- vapply(sel, function(z) as.numeric(df[[paste0(z, "_y")]]), numeric(nrow(df)))
      dim(xs) <- c(nrow(df), length(sel)); dim(ys) <- c(nrow(df), length(sel))
      wsum <- rowSums(!is.na(xs))
      cx   <- ifelse(wsum > 0L, rowSums(xs, na.rm = TRUE) / wsum, NA_real_)
      cy   <- ifelse(wsum > 0L, rowSums(ys, na.rm = TRUE) / wsum, NA_real_)
    }
  } else {
    cx <- as.numeric(df[[xc]]); cy <- as.numeric(df[[yc]])
  }

  out <- data.frame(id   = if (id == "1") rep("1", nrow(df)) else df[[id]],
                    time = df[[time]], x = cx, y = cy, stringsAsFactors = FALSE)
  for (z in zones_all) {
    out[[paste0(z, "_x")]] <- as.numeric(df[[paste0(z, "_x")]])
    out[[paste0(z, "_y")]] <- as.numeric(df[[paste0(z, "_y")]])
  }
  out
})

# TrackMate (Fiji): columns like TRACK_ID/TRAJECTORY_ID, FRAME/POSITION_T, POSITION_X, POSITION_Y
register_loader_dialect("trackmate", function(x, id_col = NULL, time_col = NULL, fps = NULL) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  nms <- norm(names(df)); names(df) <- nms
  id  <- id_col %||% .guess_col(nms, c("track_id","trajectory_id","id"))
  time<- time_col %||% .guess_col(nms, c("position_t","time","frame"))
  xc  <- .guess_col(nms, c("position_x","x"))
  yc  <- .guess_col(nms, c("position_y","y"))
  if (is.null(id) || is.null(time) || is.null(xc) || is.null(yc)) stop("trackmate: required columns not found")
  out <- data.frame(id = df[[id]], time = df[[time]], x = df[[xc]], y = df[[yc]], stringsAsFactors = FALSE)
  # convert frame timestamps to seconds if needed
  out$time <- .coerce_time(out$time, time_type = if (!is.null(fps) && grepl("frame", time, ignore.case = TRUE)) "frames" else "auto", fps = fps)
  out
})

# ToxTrac: typical columns TrackID, Frame, X, Y or Time(s)
register_loader_dialect("toxtrac", function(x, fps = NULL) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  nms <- norm(names(df)); names(df) <- nms
  id   <- .guess_col(nms, c("trackid","id","animal","subject"))
  time <- .guess_col(nms, c("time_s","time","frame"))
  xc   <- .guess_col(nms, c("x","x_mm","x_px"))
  yc   <- .guess_col(nms, c("y","y_mm","y_px"))
  if (is.null(id) || is.null(time) || is.null(xc) || is.null(yc)) stop("toxtrac: required columns not found")
  out <- data.frame(id = df[[id]], time = df[[time]], x = df[[xc]], y = df[[yc]], stringsAsFactors = FALSE)
  out$time <- .coerce_time(out$time, time_type = if (!is.null(fps) && grepl("frame", time, ignore.case = TRUE)) "frames" else "auto", fps = fps)
  out
})

# BORIS (if XY logged): expect Subject, Time, X, Y
register_loader_dialect("boris_xy", function(x) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  nms <- norm(names(df)); names(df) <- nms
  id   <- .guess_col(nms, c("subject","id","animal")) %||% "1"
  time <- .guess_col(nms, c("time","timestamp","t")) %||% "..row"
  if (time == "..row") df[[time]] <- seq_len(nrow(df))
  xc   <- .guess_col(nms, c("x","x_pos","position_x"))
  yc   <- .guess_col(nms, c("y","y_pos","position_y"))
  if (is.null(xc) || is.null(yc)) stop("boris_xy: no x/y columns found")
  data.frame(id = if (id == "1") rep("1", nrow(df)) else df[[id]],
             time = df[[time]], x = df[[xc]], y = df[[yc]], stringsAsFactors = FALSE)
})



# dtrack (https://bitbucket.org/jochensmolka/dtrack)
# Accepts a pre-parsed data frame with >=3 columns: frame, x, y[, confidence].
# The confidence column is dropped silently. id is set to "1" because no
# filename is available; use dtrack_read() when reading from a file path --
# it derives id from the filename stem instead.
register_loader_dialect("dtrack", function(x) {
  stopifnot(is.data.frame(x))
  if (ncol(x) < 3L) stop("dtrack: expected at least 3 columns (frame, x, y)")
  df <- x[, 1:3, drop = FALSE]
  names(df) <- c("frame", "x", "y")
  df$id <- "1"
  df
})

# TRex (https://trex.run): per-individual CSV export
# Each file typically covers one individual; the individual index is inferred
# from the numeric suffix of the filename stem (e.g. "run_0.csv" -> id "0").
# Aggregated exports that include an id/individual column are also supported.
# Position columns: TRex exports plain X / Y, or hashtag-tagged centroid
# variants -- X#wcentroid (weighted, TRex's recommended best estimate),
# X#centroid (geometric), and X#pcentroid (posture-based). These normalise to
# x_wcentroid / x_centroid / x_pcentroid. By default the dialect prefers plain
# X/Y, then wcentroid, then centroid, then pcentroid; set `centroid` to force
# one source ("wcentroid", "centroid", or "pcentroid").
register_loader_dialect("trex", function(x, id_col = NULL, time_col = NULL,
                                          fps = NULL, centroid = NULL) {
  stem <- if (is.character(x) && length(x) == 1L && file.exists(x))
            tools::file_path_sans_ext(basename(x)) else NULL
  df <- if (!is.null(stem)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  # TRex annotates headers with a trailing unit in parentheses, e.g.
  # "X#wcentroid (cm)" or "SPEED#wcentroid (cm/s)"; strip it before normalising.
  strip_unit <- function(v) sub("\\s*\\([^)]*\\)\\s*$", "", v)
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(strip_unit(v)))
  nms  <- norm(names(df)); names(df) <- nms

  id <- id_col %||% .guess_col(nms, c("id","individual","individual_id","fish","animal","subject"))
  if (is.null(id)) {
    m <- if (!is.null(stem)) regmatches(stem, regexpr("[0-9]+$", stem)) else character(0)
    file_id <- if (length(m)) m else (stem %||% "1")
    df[["..file_id"]] <- rep(file_id, nrow(df))
    id <- "..file_id"
  }
  time <- time_col %||% .guess_col(nms, c("frame","frames","time","t","timestamp")) %||% "..row"
  if (time == "..row") { df[["..row"]] <- seq_len(nrow(df)); time <- "..row" }

  if (!is.null(centroid)) {
    centroid <- match.arg(centroid, c("wcentroid", "centroid", "pcentroid"))
    xc <- .guess_col(nms, paste0("x_", centroid))
    yc <- .guess_col(nms, paste0("y_", centroid))
    if (is.null(xc) || is.null(yc))
      stop("trex: centroid = '", centroid, "' requested but X#", centroid,
           " / Y#", centroid, " columns not found")
  } else {
    # priority: plain X/Y, then weighted, geometric, posture centroid
    xc <- .guess_col(nms, c("x","x_wcentroid","x_centroid","x_pcentroid",
                            "pos_x","position_x","x_px","cx","center_x"))
    yc <- .guess_col(nms, c("y","y_wcentroid","y_centroid","y_pcentroid",
                            "pos_y","position_y","y_px","cy","center_y"))
  }
  if (is.null(xc) || is.null(yc)) stop("trex: could not find x/y position columns")
  data.frame(
    id   = df[[id]],
    time = .coerce_time(df[[time]],
                        time_type = if (!is.null(fps) && grepl("frame", time, ignore.case = TRUE)) "frames" else "auto",
                        fps = fps),
    x    = as.numeric(df[[xc]]),
    y    = as.numeric(df[[yc]]),
    stringsAsFactors = FALSE
  )
})

# ANY-maze (Stoelting): CSV export.  Standard columns: "Trial time", "X Centre",
# "Y Centre".  Newer versions with nose/tail tracking add "Nose X Centre",
# "Nose Y Centre", "Tail X Centre", "Tail Y Centre" (or _Center, American).
# zone = NULL  -> primary centre position (default, backward-compatible).
# zone = "nose" / zone = c("nose","tail") -> single-zone or equal-weight centroid.
# skip_units_row strips the optional units row ANY-maze inserts after the header.
register_loader_dialect("anymaze", function(x, id_col = NULL, time_col = NULL,
                                            zone = NULL, skip_units_row = TRUE) {
  df <- if (is.character(x) && length(x) == 1L && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(trimws(v)))
  nms  <- norm(names(df)); names(df) <- nms

  # Detect multi-point zones from "<zone> X Centre" -> <zone>_x_centre pattern,
  # then normalise to plain <zone>_x / <zone>_y for uniform access.
  pat_x <- grep("_x_centr[ei]$", nms, value = TRUE)
  pat_y <- grep("_y_centr[ei]$", nms, value = TRUE)
  zones_centre <- intersect(sub("_x_centr[ei]$", "", pat_x),
                            sub("_y_centr[ei]$", "", pat_y))
  for (z in zones_centre) {
    xz <- grep(paste0("^", z, "_x_centr"), nms, value = TRUE)[1L]
    yz <- grep(paste0("^", z, "_y_centr"), nms, value = TRUE)[1L]
    df[[paste0(z, "_x")]] <- suppressWarnings(as.numeric(df[[xz]]))
    df[[paste0(z, "_y")]] <- suppressWarnings(as.numeric(df[[yz]]))
  }
  nms <- names(df)
  zones_plain <- intersect(sub("_x$", "", grep(".+_x$", nms, value = TRUE)),
                           sub("_y$", "", grep(".+_y$", nms, value = TRUE)))
  zones_all <- setdiff(union(zones_centre, zones_plain), c("x", "y", "..row"))

  # Strip optional units row before any numeric operations
  xc_peek <- .guess_col(nms, c("x_centre","x_center","x_pos","x_position","x"))
  if (skip_units_row && !is.null(xc_peek) && nrow(df) > 0 &&
      is.na(suppressWarnings(as.numeric(df[[xc_peek]][1L]))))
    df <- df[-1L, , drop = FALSE]

  id   <- id_col %||% .guess_col(nms, c("id","animal","subject","trial","track")) %||% "1"
  time <- time_col %||% .guess_col(nms, c("trial_time","time_s","time","t")) %||% "..row"
  if (time == "..row") { df[["..row"]] <- seq_len(nrow(df)); time <- "..row" }
  xc <- .guess_col(nms, c("x_centre","x_center","x_pos","x_position","x"))
  yc <- .guess_col(nms, c("y_centre","y_center","y_pos","y_position","y"))
  if (is.null(xc) || is.null(yc)) stop("anymaze: could not find x/y position columns")

  if (!is.null(zone)) {
    sel <- zone
    bad <- setdiff(sel, zones_all)
    if (length(bad))
      stop("anymaze: zone(s) not found: ", paste(bad, collapse = ", "),
           "; available: ", paste(zones_all, collapse = ", "))
    if (length(sel) == 1L) {
      cx <- suppressWarnings(as.numeric(df[[paste0(sel, "_x")]]))
      cy <- suppressWarnings(as.numeric(df[[paste0(sel, "_y")]]))
    } else {
      xs   <- vapply(sel, function(z) suppressWarnings(as.numeric(df[[paste0(z,"_x")]])), numeric(nrow(df)))
      ys   <- vapply(sel, function(z) suppressWarnings(as.numeric(df[[paste0(z,"_y")]])), numeric(nrow(df)))
      dim(xs) <- c(nrow(df), length(sel)); dim(ys) <- c(nrow(df), length(sel))
      wsum <- rowSums(!is.na(xs))
      cx   <- ifelse(wsum > 0L, rowSums(xs, na.rm = TRUE) / wsum, NA_real_)
      cy   <- ifelse(wsum > 0L, rowSums(ys, na.rm = TRUE) / wsum, NA_real_)
    }
  } else {
    cx <- suppressWarnings(as.numeric(df[[xc]]))
    cy <- suppressWarnings(as.numeric(df[[yc]]))
  }

  out <- data.frame(
    id   = if (id == "1") rep("1", nrow(df)) else df[[id]],
    time = suppressWarnings(as.numeric(df[[time]])),
    x = cx, y = cy,
    stringsAsFactors = FALSE
  )
  for (z in zones_all) {
    out[[paste0(z, "_x")]] <- suppressWarnings(as.numeric(df[[paste0(z, "_x")]]))
    out[[paste0(z, "_y")]] <- suppressWarnings(as.numeric(df[[paste0(z, "_y")]]))
  }
  out
})

# SLEAP (https://sleap.ai): CSV analysis export.
# Column naming: <node>.x, <node>.y, <node>.score (dot -> underscore after norm).
# bodypart = NULL  -> centroid of all nodes, score-weighted.
# bodypart = c("head","thorax") -> centroid of named subset.
# Per-node <name>_x / <name>_y columns always appended for bodypart_axis use.
# score_min: minimum per-node score to include in centroid (analogous to likelihood_min).
register_loader_dialect("sleap", function(x, bodypart = NULL, score_min = NULL,
                                          id_col = NULL, time_col = NULL, fps = NULL) {
  df <- if (is.character(x) && length(x) == 1L && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  nms  <- norm(names(df)); names(df) <- nms

  # Detect nodes from <node>_x / <node>_y pairs; alias <node>_score -> <node>_likelihood
  bpx     <- sub("_x$", "", grep("_x$", nms, value = TRUE))
  bpy     <- sub("_y$", "", grep("_y$", nms, value = TRUE))
  bps_all <- intersect(bpx, bpy)
  if (!length(bps_all)) stop("sleap: no *_x/*_y column pairs found")
  for (bp in bps_all) {
    sc <- paste0(bp, "_score")
    lc <- paste0(bp, "_likelihood")
    if (sc %in% nms && !lc %in% nms) df[[lc]] <- df[[sc]]
  }
  nms <- names(df)

  sel <- if (is.null(bodypart)) bps_all else bodypart
  bad <- setdiff(sel, bps_all)
  if (length(bad))
    stop("sleap: node(s) not found: ", paste(bad, collapse = ", "),
         "; available: ", paste(bps_all, collapse = ", "))

  id   <- id_col %||% .guess_col(nms, c("track","id","individual","animal","subject")) %||% "1"
  time <- time_col %||% .guess_col(nms, c("frame_idx","frame","frames","time","t")) %||% "..row"
  if (time == "..row") { df[["..row"]] <- seq_len(nrow(df)); time <- "..row" }

  if (length(sel) == 1L) {
    xc <- paste0(sel, "_x"); yc <- paste0(sel, "_y"); lc <- paste0(sel, "_likelihood")
    if (!is.null(score_min) && lc %in% nms)
      df <- df[!is.na(df[[lc]]) & df[[lc]] >= as.numeric(score_min), , drop = FALSE]
    cx <- as.numeric(df[[xc]]); cy <- as.numeric(df[[yc]])
  } else {
    xs <- vapply(sel, function(bp) as.numeric(df[[paste0(bp, "_x")]]), numeric(nrow(df)))
    ys <- vapply(sel, function(bp) as.numeric(df[[paste0(bp, "_y")]]), numeric(nrow(df)))
    dim(xs) <- c(nrow(df), length(sel)); dim(ys) <- c(nrow(df), length(sel))
    ws <- vapply(sel, function(bp) {
      lc <- paste0(bp, "_likelihood")
      w  <- if (lc %in% nms) as.numeric(df[[lc]]) else rep(1, nrow(df))
      if (is.null(score_min)) w[is.na(w)] <- 1 else {
        w[is.na(w)] <- 0; w[!is.na(w) & w < as.numeric(score_min)] <- 0
      }
      w
    }, numeric(nrow(df)))
    dim(ws) <- c(nrow(df), length(sel))
    wsum <- rowSums(ws)
    cx <- ifelse(wsum > 0, rowSums(xs * ws) / wsum, NA_real_)
    cy <- ifelse(wsum > 0, rowSums(ys * ws) / wsum, NA_real_)
  }

  out <- data.frame(
    id   = if (length(id) == 1L && id == "1") rep("1", nrow(df)) else df[[id]],
    time = .coerce_time(df[[time]],
                        time_type = if (!is.null(fps) && grepl("frame", time, ignore.case = TRUE)) "frames" else "auto",
                        fps = fps),
    x = cx, y = cy,
    stringsAsFactors = FALSE
  )
  for (bp in sel) {
    out[[paste0(bp, "_x")]] <- as.numeric(df[[paste0(bp, "_x")]])
    out[[paste0(bp, "_y")]] <- as.numeric(df[[paste0(bp, "_y")]])
  }
  out
})

# Tracktor (https://github.com/jgraving/tracktor): tidy CSV with frame, x, y [, id]
# Tracktor outputs one row per frame per individual; the id column (called
# "id" or "identity") distinguishes individuals in multi-animal exports.
register_loader_dialect("tracktor", function(x, id_col = NULL, time_col = NULL, fps = NULL) {
  df <- if (is.character(x) && length(x) == 1L && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  df   <- .drop_index_col(df)
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  nms  <- norm(names(df)); names(df) <- nms
  id   <- id_col %||% .guess_col(nms, c("id","identity","individual","animal","subject","track_id")) %||% "1"
  time <- time_col %||% .guess_col(nms, c("frame","frames","time","t","timestamp")) %||% "..row"
  if (time == "..row") { df[["..row"]] <- seq_len(nrow(df)); time <- "..row" }
  xc <- .guess_col(nms, c("x","pos_x","position_x","cx","x_pos","x_coord"))
  yc <- .guess_col(nms, c("y","pos_y","position_y","cy","y_pos","y_coord"))
  if (is.null(xc) || is.null(yc)) stop("tracktor: could not find x/y position columns")
  data.frame(
    id   = if (id == "1") rep("1", nrow(df)) else df[[id]],
    time = .coerce_time(df[[time]],
                        time_type = if (!is.null(fps) && grepl("frame", time, ignore.case = TRUE)) "frames" else "auto",
                        fps = fps),
    x    = as.numeric(df[[xc]]),
    y    = as.numeric(df[[yc]]),
    stringsAsFactors = FALSE
  )
})

# Ctrax (http://ctrax.sourceforge.net): multi-animal MATLAB tracker.
# Primary output is a .mat file whose 'trx' field is a 1xN struct array --
# one element per tracked individual -- with fields x, y, theta, a, b,
# firstframe, nframes.  Requires the 'R.matlab' package.
# ids: optional integer vector selecting a subset of individuals (1-indexed).
register_loader_dialect("ctrax", function(x, ids = NULL) {
  if (!.is_installed("R.matlab")) stop("Please install 'R.matlab' to read Ctrax .mat files")
  if (!is.character(x) || !file.exists(x)) stop("ctrax: provide a path to a Ctrax .mat file")
  mat <- R.matlab::readMat(x)
  trx <- mat[["trx"]]
  if (is.null(trx)) stop("ctrax: no 'trx' field found in .mat file")
  d <- dim(trx)
  if (length(d) < 3L) stop("ctrax: unexpected trx structure -- expected a 3-D struct array from R.matlab")
  n_ind <- d[3L]
  fly_ids <- seq_len(n_ind)
  if (!is.null(ids)) fly_ids <- intersect(fly_ids, as.integer(ids))
  rows <- lapply(fly_ids, function(i) {
    traj <- trx[, , i]
    xs <- as.numeric(traj$x)
    ys <- as.numeric(traj$y)
    if (!length(xs) || !length(ys)) return(NULL)
    ff <- if (!is.null(traj$firstframe)) as.integer(traj$firstframe[[1L]]) else 1L
    frames <- seq(ff, by = 1L, length.out = length(xs))
    data.frame(
      id    = as.character(i),
      time  = frames,
      x     = xs,
      y     = ys,
      theta = if (!is.null(traj$theta)) as.numeric(traj$theta) else rep(NA_real_, length(xs)),
      a     = if (!is.null(traj$a))     as.numeric(traj$a)     else rep(NA_real_, length(xs)),
      b     = if (!is.null(traj$b))     as.numeric(traj$b)     else rep(NA_real_, length(xs)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
})

# ---- built-in dialects: general (non-animal) ---------------------------------
# 1) MOTChallenge / SORT / DeepSORT-like track files
#    Default spec: frame,id,x,y,w,h,conf,x3,y3,z3 (headerless by convention)
#    We'll read headerless CSV by default and compute bbox center (x+w/2, y+h/2)
register_loader_dialect("motchallenge", function(x, fps = NULL, headerless = TRUE, center = TRUE) {
  df <- if (is.character(x) && file.exists(x)) {
    if (headerless) utils::read.csv(x, header = FALSE, stringsAsFactors = FALSE)
    else .read_any(x)
  } else x
  stopifnot(is.data.frame(df))
  if (is.null(colnames(df)) || headerless) {
    nm <- c("frame","id","x","y","w","h","conf","x3","y3","z3")
    names(df)[seq_along(nm)[seq_len(min(length(nm), ncol(df)))]] <- nm[seq_len(min(length(nm), ncol(df)))]
  }
  if (!all(c("frame","id","x","y") %in% names(df))) stop("motchallenge: need columns frame,id,x,y (and optional w,h)")
  x0 <- df$x; y0 <- df$y
  if (center) {
    if ("w" %in% names(df)) x0 <- x0 + df$w/2
    if ("h" %in% names(df)) y0 <- y0 + df$h/2
  }
  out <- data.frame(id = df$id, time = df$frame, x = x0, y = y0, stringsAsFactors = FALSE)
  # NOTE: call TrajSet_read(..., time_type="frames", fps=...) afterwards to convert to seconds
  out
})

# 2) GeoJSON LineString / MultiLineString
register_loader_dialect("geojson_linestring", function(x, id_prop = "id", time_prop = NULL, feature_index = NULL) {
  if (!.is_installed("jsonlite")) stop("Please install 'jsonlite' to read GeoJSON")
  gj <- if (is.character(x) && file.exists(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  get_feats <- function(g) {
    if (is.list(g) && !is.null(g$type) && g$type == "FeatureCollection") return(g$features)
    if (is.list(g) && !is.null(g$type) && g$type == "Feature") return(list(g))
    stop("geojson_linestring: expected a Feature or FeatureCollection")
  }
  feats <- get_feats(gj)
  if (!is.null(feature_index)) feats <- feats[feature_index]
  rows <- list()
  k <- 1L
  for (f in feats) {
    props <- f$properties %||% list()
    gid <- as.character(props[[id_prop]] %||% k)
    g <- f$geometry
    if (is.null(g) || is.null(g$type)) next
    add_coords <- function(coords) {
      for (i in seq_along(coords)) {
        crd <- coords[[i]]
        if (length(crd) < 2) next
        lon <- as.numeric(crd[[1]]); lat <- as.numeric(crd[[2]])
        tm  <- if (!is.null(time_prop) && !is.null(props[[time_prop]]) && length(props[[time_prop]]) >= i) props[[time_prop]][[i]] else i
        rows[[length(rows)+1L]] <<- data.frame(id = gid, time = tm, x = lon, y = lat, stringsAsFactors = FALSE)
      }
    }
    if (g$type == "LineString") add_coords(g$coordinates)
    else if (g$type == "MultiLineString") for (seg in g$coordinates) add_coords(seg)
    k <- k + 1L
  }
  if (!length(rows)) stop("geojson_linestring: no coordinates found")
  do.call(rbind, rows)
})

# 3) GPX tracks (trk/trkseg/trkpt)
register_loader_dialect("gpx", function(x, track_index = NULL) {
  if (!.is_installed("xml2")) stop("Please install 'xml2' to read GPX")
  doc <- if (is.character(x) && file.exists(x)) xml2::read_xml(x) else stop("gpx: provide a .gpx file path")
  trks <- xml2::xml_find_all(doc, "//trk")
  if (!length(trks)) stop("gpx: no <trk> found")
  if (!is.null(track_index)) trks <- trks[track_index]
  rows <- list(); k <- 1L
  for (trk in trks) {
    name <- xml2::xml_text(xml2::xml_find_first(trk, "name"))
    segs <- xml2::xml_find_all(trk, "./trkseg")
    for (sg in segs) {
      pts <- xml2::xml_find_all(sg, "./trkpt")
      for (pt in pts) {
        lat <- as.numeric(xml2::xml_attr(pt, "lat"))
        lon <- as.numeric(xml2::xml_attr(pt, "lon"))
        t   <- xml2::xml_text(xml2::xml_find_first(pt, "time"))
        rows[[length(rows)+1L]] <- data.frame(id = name %||% as.character(k), time = t %||% NA, x = lon, y = lat, stringsAsFactors = FALSE)
      }
      k <- k + 1L
    }
  }
  do.call(rbind, rows)
})

# 4) NMEA GPRMC (basic): $GPRMC,hhmmss,A,lat,N,lon,E,speed,trackangle,date,...
register_loader_dialect("nmea_gprmc", function(x, tz = "UTC") {
  lines <- if (is.character(x) && file.exists(x)) readLines(x) else as.character(x)
  stopifnot(length(lines) > 0)
  .nmea_deg <- function(v, hemi) {
    v <- as.numeric(v); if (!is.finite(v)) return(NA_real_)
    deg <- floor(v/100); min <- v - deg*100
    d <- deg + min/60
    if (hemi %in% c("S","W")) d <- -d
    d
  }
  .parse_time <- function(hms, dmy) {
    if (is.null(hms) || is.null(dmy) || hms == "" || dmy == "") return(NA)
    hh <- substr(hms,1,2); mm <- substr(hms,3,4); ss <- substr(hms,5,6)
    dd <- substr(dmy,1,2); mo <- substr(dmy,3,4); yy <- substr(dmy,5,6)
    iso <- sprintf("20%s-%s-%sT%s:%s:%sZ", yy, mo, dd, hh, mm, ss)
    as.POSIXct(iso, tz = tz)
  }
  out <- list()
  for (ln in lines) {
    if (!startsWith(ln, "$GPRMC")) next
    f <- strsplit(ln, ",", fixed = TRUE)[[1]]
    if (length(f) < 10) next
    tm  <- f[2]; stat <- f[3]; lat <- f[4]; nss <- f[5]; lon <- f[6]; ews <- f[7]; dmy <- f[10]
    if (stat != "A") next
    out[[length(out)+1L]] <- data.frame(
      id = "1", time = .parse_time(tm, dmy),
      x = .nmea_deg(lon, ews), y = .nmea_deg(lat, nss), stringsAsFactors = FALSE)
  }
  if (!length(out)) stop("nmea_gprmc: no valid $GPRMC sentences found")
  do.call(rbind, out)
})

# 5) TUM RGB-D trajectory format: timestamp tx ty tz qx qy qz qw
register_loader_dialect("tum_traj", function(x) {
  df <- if (is.character(x) && file.exists(x)) utils::read.table(x, header = FALSE, stringsAsFactors = FALSE) else x
  stopifnot(is.data.frame(df))
  if (ncol(df) < 8) stop("tum_traj: expected 8 columns: t tx ty tz qx qy qz qw")
  names(df)[1:8] <- c("t","tx","ty","tz","qx","qy","qz","qw")
  data.frame(id = "1", time = df$t, x = df$tx, y = df$ty, stringsAsFactors = FALSE)
})

# 6) EuRoC MAV CSV: usually has time(us), p_RS_R_x [m], p_RS_R_y [m], p_RS_R_z [m]
register_loader_dialect("euroc_mav", function(x) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  nms <- tolower(gsub("[^a-z0-9]+","_", names(df)))
  names(df) <- nms
  tcol <- .guess_col(nms, c("time","timestamp","t","time_us","time_ns"))
  xc   <- .guess_col(nms, c("p_rs_r_x","position_x","x"))
  yc   <- .guess_col(nms, c("p_rs_r_y","position_y","y"))
  if (is.null(tcol) || is.null(xc) || is.null(yc)) stop("euroc_mav: required columns not found")
  # Convert microseconds/nanoseconds to seconds if necessary
  tt <- as.numeric(df[[tcol]])
  if (all(tt > 1e12)) tt <- tt/1e9 else if (all(tt > 1e9)) tt <- tt/1e6
  data.frame(id = "1", time = tt, x = df[[xc]], y = df[[yc]], stringsAsFactors = FALSE)
})

# 7) Generic bbox CSV: columns (xmin,ymin,xmax,ymax) or (x,y,w,h) -> center (x,y)
register_loader_dialect("csv_bbox", function(x, schema = c("xywh","xyxy"), time_col = NULL, id_col = NULL) {
  schema <- match.arg(schema)
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  nms <- tolower(gsub("[^a-z0-9]+","_", names(df)))
  names(df) <- nms
  if (schema == "xywh") {
    xc <- .guess_col(nms, c("x","left")); yc <- .guess_col(nms, c("y","top"))
    wc <- .guess_col(nms, c("w","width")); hc <- .guess_col(nms, c("h","height"))
    if (is.null(xc) || is.null(yc) || is.null(wc) || is.null(hc)) stop("csv_bbox xywh: need x/y/w/h columns")
    x0 <- df[[xc]] + df[[wc]]/2; y0 <- df[[yc]] + df[[hc]]/2
  } else {
    xmin <- .guess_col(nms, c("xmin","left")); ymin <- .guess_col(nms, c("ymin","top"))
    xmax <- .guess_col(nms, c("xmax","right")); ymax <- .guess_col(nms, c("ymax","bottom"))
    if (is.null(xmin) || is.null(ymin) || is.null(xmax) || is.null(ymax)) stop("csv_bbox xyxy: need xmin/ymin/xmax/ymax columns")
    x0 <- (df[[xmin]] + df[[xmax]])/2; y0 <- (df[[ymin]] + df[[ymax]])/2
  }
  idc <- id_col %||% .guess_col(nms, c("id","track_id","trajectory_id","object_id")) %||% "1"
  tc  <- time_col %||% .guess_col(nms, c("frame","time","t","timestamp")) %||% "..row"
  if (tc == "..row") df[[tc]] <- seq_len(nrow(df))
  data.frame(id = if (idc == "1") rep("1", nrow(df)) else df[[idc]],
             time = df[[tc]], x = x0, y = y0, stringsAsFactors = FALSE)
})
