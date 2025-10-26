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
#' Import landmark coordinates from text files
#'
#' @param dir The directory in which to look for the landmark files. Defaults to the current working directory.
#' @param landmark_suffix The suffix of files containing landmark coordinates.
#' @param track_suffix The suffix of files containing track coordinates.
#' @return A dataframe of file names.
#' #examples
#' #import_tracks(dir=data)
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
  hit <- intersect(candidates, nms)
  if (length(hit)) hit[1] else NULL
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
    return(arrow::read_table(path, ...))
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
#' @param mutate list of transformations applied after reading (named functions or formulas)
#' @param keep only keep these columns (NULL = keep all)
#' @param drop drop these columns after mapping
#' @param id_from_filename if TRUE and id missing, derive id from file stem when reading multiple files
#' @param validate if TRUE run S4 validity checks
#' @return TrajSet
#' @export
TrajSet_read <- function(x,
                         mapping = list(id=NULL, time=NULL, x=NULL, y=NULL, angle=NULL, weight=NULL),
                         angle_unit = c("radians","degrees","auto"),
                         time_type = c("auto","posix","seconds","frames"), tz = "UTC", fps = NULL,
                         normalize_xy = TRUE,
                         dialect = NULL,
                         mutate = NULL,
                         keep = NULL, drop = NULL,
                         id_from_filename = TRUE,
                         validate = TRUE) {
  angle_unit <- match.arg(angle_unit)
  time_type  <- match.arg(time_type)

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
    df <- prep(df %||% x)
  }

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
  keep_cols <- unique(c(id, time, angle, xcol, ycol, wcol, keep, "..source_file"))
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
#' @param mutate list of transformations applied after reading (named functions or formulas)
#' @param keep only keep these columns (NULL = keep all)
#' @param drop drop these columns after mapping
#' @param id_from_filename if TRUE and id missing, derive id from file stem when reading multiple files
#' @param validate if TRUE run S4 validity checks
#' @return TrajSet
#' @export
TrajSet_read <- function(x,
                         mapping = list(id=NULL, time=NULL, x=NULL, y=NULL, angle=NULL, weight=NULL),
                         angle_unit = c("radians","degrees","auto"),
                         time_type = c("auto","posix","seconds","frames"), tz = "UTC", fps = NULL,
                         normalize_xy = TRUE,
                         dialect = NULL,
                         mutate = NULL,
                         keep = NULL, drop = NULL,
                         id_from_filename = TRUE,
                         validate = TRUE) {
  angle_unit <- match.arg(angle_unit)
  time_type  <- match.arg(time_type)

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
    df <- prep(df %||% x)
  }

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
  ang_unit <- if (angle_unit == "auto") .guess_angle_unit(df[[angle]]) else angle_unit

  # cleanup names and select
  keep_cols <- unique(c(id, time, angle, xcol, ycol, wcol, keep, "..source_file"))
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
#' @param angle_unit, time_type, tz, fps, normalize_xy, dialect, keep, drop, ...
#'   Additional parameters forwarded to [TrajSet_read()].
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
  defaults <- intersect(c("arc", "type", "obstacle", "id"), names(df))
  manifest_cols <- NULL
  if (length(defaults)) {
    manifest_cols <- setNames(defaults, defaults)
  }
  out <- .augment_with_manifest(file_tbl, df, manifest_cols)
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
# DeepLabCut (three-row multiheader csv) -> collapse to <bodypart>_<coord>
register_loader_dialect("deeplabcut", function(x, bodypart = NULL, likelihood_min = NULL,
                                                id_col = NULL, time_col = NULL, fps = NULL) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  nms <- names(df)
  # discover candidate bodyparts by suffix _x / _y
  bpx <- sub("_x$", "", grep("_x$", nms, value = TRUE))
  bpy <- sub("_y$", "", grep("_y$", nms, value = TRUE))
  bps <- intersect(bpx, bpy)
  if (is.null(bodypart)) {
    if (!length(bps)) stop("deeplabcut: no *_x/*_y columns found")
    bodypart <- bps[1]
  } else if (!(bodypart %in% bps)) stop("deeplabcut: bodypart '", bodypart, "' not found; candidates: ", paste(bps, collapse=", "))

  xc <- paste0(bodypart, "_x"); yc <- paste0(bodypart, "_y")
  lc <- paste0(bodypart, "_likelihood")

  # choose id and time
  id <- id_col %||% .guess_col(nms, c("id","individual","animal","subject")) %||% "1"
  time <- time_col %||% .guess_col(nms, c("time","timestamp","t","frame","frames")) %||% "..row"
  if (time == "..row") df[[time]] <- seq_len(nrow(df))

  # optionally filter on likelihood
  if (!is.null(likelihood_min) && lc %in% nms) {
    df <- df[df[[lc]] >= as.numeric(likelihood_min), , drop = FALSE]
  }

  out <- data.frame(
    id   = if (length(id) == 1L && id == "1") rep("1", nrow(df)) else df[[id]],
    time = .coerce_time(df[[time]], time_type = if (!is.null(fps) && grepl("frame", time, ignore.case = TRUE)) "frames" else "auto", fps = fps),
    x    = df[[xc]],
    y    = df[[yc]],
    stringsAsFactors = FALSE
  )
  out
})

# DeepLabCut (three-row multiheader csv) -> collapse to <bodypart>_<coord>
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

# EthoVision XT: typically columns like "Time","X Center","Y Center","Arena" etc.
register_loader_dialect("ethovision", function(x, id_col = NULL, time_col = NULL) {
  df <- if (is.character(x) && file.exists(x)) .read_any(x) else x
  stopifnot(is.data.frame(df))
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  nms <- norm(names(df))
  names(df) <- nms
  id  <- id_col %||% .guess_col(nms, c("id","animal","subject","trial","track_id")) %||% "1"
  time<- time_col %||% .guess_col(nms, c("time","time_s","position_t","t","frame")) %||% "..row"
  if (time == "..row") df[[time]] <- seq_len(nrow(df))
  xc  <- .guess_col(nms, c("x_center","position_x","x"))
  yc  <- .guess_col(nms, c("y_center","position_y","y"))
  if (is.null(xc) || is.null(yc)) stop("ethovision: could not find x/y columns")
  data.frame(id = if (id == "1") rep("1", nrow(df)) else df[[id]],
             time = df[[time]], x = df[[xc]], y = df[[yc]], stringsAsFactors = FALSE)
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
