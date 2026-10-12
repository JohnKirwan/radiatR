# Loader utilities for radiatR: manifest importers, track discovery, and generic Tracks readers
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
#' manifest_path <- system.file("extdata", "millipede_trials.csv",
#'                              package = "radiatR")
#' manifest <- import_info(manifest_path, cond_cols = c("type", "arc"))
#' head(manifest)
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
#'   dtrack convention. `vignette("loaders")` registers a custom loader that
#'   reads an individual trajectory file.
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
# Generic loaders for Tracks (no video-specific assumptions)
#
# Provides:
#   - read_tracks(x, mapping=..., angle_unit=..., ...): construct Tracks from data.frame or file path(s)
#   - read_tracks_dir(dir, pattern, ...): bind multiple files
#   - register_loader_dialect(name, fun), list_loader_dialects(): pluggable dialects for known exporters
#
# Dependencies kept soft: base R by default; uses readr/data.table/arrow if installed.

#' @importFrom methods is validObject
#' @importFrom utils modifyList
#' @keywords internal

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- dialect registry ---------------------------------------------------------
.loader_registry <- new.env(parent = emptyenv())

# Built-in dialects dropped in 0.1.3: their software is little used or no
# longer developed, or they read a general file format. read_tracks() names
# the removal unless the caller has registered a loader under the same name.
.removed_dialects <- c("boris_xy", "csv_bbox", "dtrack", "euroc_mav",
                       "geojson_linestring", "gpx", "motchallenge",
                       "nmea_gprmc", "tracktor", "tum_traj", "wide_prefix_xy")

# ---- declarative format registry ---------------------------------------------
.loader_format_registry <- new.env(parent = emptyenv())

#' Register a declarative loader *format* (list or YAML/JSON file)
#'
#' The spec maps cleanly onto read_tracks() args and supports regex-based column finding.
#' @param name A unique name
#' @param spec A named list, or a path to a YAML/JSON file defining the spec
#' @param overwrite Overwrite an existing format of the same name
#' @return `name`, invisibly. Called for its side effect of registering the
#'   format for [read_tracks_format()].
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
#' @return A sorted character vector of registered format names.
#' @export
list_loader_formats <- function() sort(ls(envir = .loader_format_registry))

.loader_registry <- new.env(parent = emptyenv())

#' Register a custom loader dialect
#'
#' The function must accept (x, ...) and return a data.frame in long form with columns at least id,time and one of (angle) or (x,y)
#' @param name Unique dialect name
#' @param fun Function that accepts (x, ...) and returns a data.frame with id/time/angle or id/time/x/y
#' @param overwrite Replace an existing dialect registered with the same name
#' @return `name`, invisibly. Called for its side effect of registering the
#'   dialect for `read_tracks(dialect = )`.
#' @export
register_loader_dialect <- function(name, fun, overwrite = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, is.function(fun))
  if (exists(name, envir = .loader_registry, inherits = FALSE) && !overwrite)
    stop("A loader named '", name, "' already exists; set overwrite = TRUE to replace")
  assign(name, fun, envir = .loader_registry)
  invisible(name)
}

#' List registered loader dialects
#' @return A sorted character vector of registered dialect names.
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
#' `Tracks` role (`id`, `time`, `x`, `y`, `angle`, `weight`), honouring any
#' explicit `mapping` overrides. Matching is case-insensitive; `x`/`y` also match
#' separator-suffixed names such as `Track1_X`. A role with no match is `NULL`.
#' This is the same logic [read_tracks()] uses internally; call it to see or
#' pre-fill a column mapping. It does not synthesize missing `id`/`time` columns
#' (a `NULL` signals that `read_tracks()` will apply its single-track / row-order
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

# Heuristic angle unit detection
# returns "degrees" if most values are > 2*pi or abs(values) > 2*pi by margin
.guess_angle_unit <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return("radians")
  frac_deg <- mean(abs(x) > 2*pi)
  if (is.na(frac_deg)) return("radians")
  if (frac_deg > 0.5) "degrees" else "radians"
}

.assert_valid_fps <- function(fps) {
  if (is.null(fps) || !is.numeric(fps) || length(fps) != 1L ||
      !is.finite(fps) || fps <= 0)
    stop("`fps` must be a single positive, finite number (frames per second).",
         call. = FALSE)
}

# Cells that count as missing in a text column: blank, or NA in any case.
.is_missing_chr <- function(s) is.na(s) | toupper(s) %in% c("", "NA")

# as.numeric() on text read with decimal mark `decimal`: a column left as text
# by one bad cell still holds "1,5" where the file's decimal mark is a comma.
.parse_number <- function(s, decimal = ".") {
  if (identical(decimal, ","))
    s <- sub("^([-+]?[0-9]*),([0-9]+)$", "\\1.\\2", s)
  suppressWarnings(as.numeric(s))
}

# Numeric coercion that tells missing from malformed. Numeric input is returned
# unchanged. Otherwise missing cells become NA, anything as.numeric() parses is
# kept (including "nan" and "inf"), and any other value aborts with
# radiatR_malformed_values naming the column, its data rows and the offending
# strings. `decimal` is the mark the source file was read with.
.as_numeric_strict <- function(v, column, source = NULL, dialect = NULL,
                               decimal = ".") {
  if (is.numeric(v)) return(v)
  s <- trimws(as.character(v))
  missing <- .is_missing_chr(s)
  num <- .parse_number(s, decimal)
  num[missing] <- NA_real_
  bad <- !missing & is.na(num) & !is.nan(num)
  if (any(bad))
    radiatR_abort_malformed_values(
      column, which(bad), s[bad], source = source, dialect = dialect,
      # read_opts only helps a file read_tracks() itself read, with a decimal
      # point; a dialect reads its own file and is not passed read_opts
      decimal_hint = !is.null(source) && is.null(dialect) &&
        !identical(decimal, ","))
  num
}

# Timestamp layouts accepted for text time, in order of preference. A value
# must match its layout in full: base as.POSIXct() accepts a prefix match,
# which silently cuts "2024-01-01T10:00:00Z" and "2024-01-01 garbage" to
# midnight. A trailing "Z" is read as UTC.
.time_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                   "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ",
                   "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d")

.time_format_regex <- function(fmt) {
  subs <- list(c("%OS", "[0-9]{1,2}([.][0-9]+)?"), c("%Y", "[0-9]{4}"),
               c("%[mdHM]", "[0-9]{1,2}"))
  for (p in subs) fmt <- gsub(p[1], p[2], fmt)
  paste0("^", fmt, "$")
}

# Text time: numbers throughout give numeric time, timestamps in one layout
# throughout give POSIXct. Otherwise the values that break the majority
# reading abort with radiatR_malformed_values. Beside a date-time layout a
# bare date is midnight (as.character() writes midnight that way).
.parse_time_chr <- function(v, tz, column, source, dialect, decimal = ".",
                            allow_numeric = TRUE) {
  s <- trimws(as.character(v))
  num <- .parse_number(s, decimal)
  missing <- .is_missing_chr(s) | is.nan(num)
  num_ok <- allow_numeric & !missing & !is.na(num)
  if (allow_numeric && all(num_ok | missing)) {
    num[missing] <- NA_real_
    return(num)
  }
  hit <- lapply(.time_formats, function(f) grepl(.time_format_regex(f), s))
  best <- which.max(vapply(hit, sum, integer(1)))
  fmt <- .time_formats[best]
  fmt_tz <- if (endsWith(fmt, "Z")) "UTC" else tz
  out <- as.POSIXct(strptime(s, fmt, tz = fmt_tz))
  date_fmt <- sub("[ T]%H.*$", "", fmt)
  bare <- fmt != date_fmt & grepl(.time_format_regex(date_fmt), s)
  out[bare] <- as.POSIXct(strptime(s[bare], date_fmt, tz = fmt_tz))
  attr(out, "tzone") <- tz
  ok <- (hit[[best]] | bare) & !is.na(out)
  if (all(ok | missing)) return(out)
  as_num <- sum(num_ok) >= sum(ok)
  bad <- !missing & !(if (as_num) num_ok else ok)
  expected <- if (!any(num_ok | ok)) "a number or date-time"
              else if (as_num) "numeric" else "a date-time"
  radiatR_abort_malformed_values(column, which(bad), s[bad], expected = expected,
                                 source = source, dialect = dialect,
                                 decimal_hint = FALSE)
}

# Coerce time column to POSIXct or numeric seconds. Numeric time under
# time_type = "frames" (or "auto" with fps supplied) is kept as a raw frame
# index: read_tracks() stores `fps` as the Tracks frame_rate, and
# elapsed_seconds()/.elapsed_time() perform the single frame -> seconds
# division on demand. Converting here as well as there double-divides by fps.
# Malformed values abort (radiatR_malformed_values); they are never turned
# into NA.
.coerce_time <- function(v, time_type = c("auto","posix","seconds","frames"),
                         tz = "UTC", fps = NULL, origin = "1970-01-01",
                         column = "time", source = NULL, dialect = NULL,
                         decimal = ".") {
  time_type <- match.arg(time_type)
  if (time_type == "frames") .assert_valid_fps(fps)
  if (time_type == "frames" && inherits(v, c("POSIXt", "Date", "difftime")))
    rlang::abort(sprintf(
      "Column '%s' holds %s values, which are not frame indices; use time_type = \"auto\".",
      column, class(v)[1L]), class = "radiatR_time_type_mismatch")
  duration <- inherits(v, "difftime")
  if (duration) v <- as.numeric(v, units = "secs")
  if (inherits(v, "Date")) v <- as.POSIXct(format(v), tz = tz)
  if (time_type %in% c("seconds", "frames")) {
    if (inherits(v, "POSIXt")) return(as.numeric(v))
    return(as.numeric(.as_numeric_strict(v, column, source, dialect, decimal)))
  }
  if (inherits(v, "POSIXt")) return(as.POSIXct(v, tz = tz))
  if (time_type == "posix" && is.numeric(v))
    return(as.POSIXct(v, tz = tz, origin = origin))
  out <- if (is.numeric(v)) as.numeric(v) else
    .parse_time_chr(v, tz, column, source, dialect, decimal,
                    allow_numeric = time_type == "auto")
  # Numeric time with an fps is a frame index (see above), so fps must be valid.
  if (is.numeric(out) && !duration && !is.null(fps)) .assert_valid_fps(fps)
  out
}

# Sniff the field separator (and, for ';', the decimal mark) from a delimited
# file's first non-empty lines. Returns list(delim, decimal). Chooses the
# candidate that appears most often AND consistently across lines; ties or no
# signal fall back to comma + dot. ';' implies the European decimal comma.
.guess_delim <- function(path) {
  default <- list(delim = ",", decimal = ".")
  lines <- tryCatch(
    readLines(path, n = 20L, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0))
  lines <- lines[nzchar(trimws(lines))]
  lines <- utils::head(lines, 5L)
  if (!length(lines)) return(default)

  cands <- c(",", ";", "\t", "|")
  counts <- vapply(cands, function(d) {
    vapply(lines, function(ln) lengths(regmatches(ln, gregexpr(d, ln, fixed = TRUE))),
           integer(1))
  }, integer(length(lines)))
  if (is.null(dim(counts))) counts <- matrix(counts, nrow = length(lines))

  # a separator is viable only if it appears at least once and the same number
  # of times on every line we looked at (consistency guard)
  viable <- apply(counts, 2L, function(col) col[1L] > 0 && length(unique(col)) == 1L)
  if (!any(viable)) return(default)
  best <- cands[which(viable)][which.max(counts[1L, viable])]
  list(delim = best, decimal = if (best == ";") "," else ".")
}

# read file with soft dependencies. Delimited formats are content-sniffed
# (separator + decimal mark) unless delim/decimal are supplied; Excel and
# Arrow formats dispatch by extension. `sheet` selects an Excel worksheet.
.read_any <- function(path, ext = NULL, delim = NULL, decimal = NULL, sheet = NULL, ...) {
  ext <- if (!is.null(ext)) tolower(sub("^\\.", "", ext)) else tolower(sub(".*\\.", "", path))

  if (ext %in% c("xlsx", "xls")) {
    if (!.is_installed("readxl"))
      stop("Reading Excel files requires the 'readxl' package; ",
           "install it with install.packages(\"readxl\").")
    return(readxl::read_excel(path, sheet = sheet %||% 1L, ...))
  }

  if (.is_installed("arrow") && ext %in% c("parquet", "feather")) {
    if (ext == "parquet") return(arrow::read_parquet(path, ...))
    return(arrow::read_feather(path, ...))
  }

  # delimited: sniff separator + decimal mark unless overridden
  if (is.null(delim) || is.null(decimal)) {
    g <- .guess_delim(path)
    if (is.null(delim))   delim   <- g$delim
    if (is.null(decimal)) decimal <- g$decimal
  }

  # Column types are guessed from every row, and no grouping mark is assumed:
  # readr's defaults turn a malformed cell past row 1000 into NA and read
  # "1,5" as 15, which hides both from the strict coercion downstream.
  df <- if (.is_installed("readr"))
    readr::read_delim(
      path, delim = delim,
      locale = readr::locale(decimal_mark = decimal, grouping_mark = ""),
      guess_max = Inf, show_col_types = FALSE, progress = FALSE, ...)
  else if (.is_installed("data.table"))
    data.table::fread(
      path, sep = delim, dec = decimal, showProgress = FALSE, ...)
  else utils::read.table(
    path, header = TRUE, sep = delim, dec = decimal,
    stringsAsFactors = FALSE, ...)
  # Kept for the strict coercion of any column the reader left as text.
  attr(df, "decimal") <- decimal
  df
}

# A dialect must hand back a data frame; anything else is a broken loader.
.assert_dialect_table <- function(out, dialect, source = NULL) {
  if (!is.data.frame(out))
    rlang::abort(sprintf(
      "read_tracks: dialect '%s' returned %s%s, not a data frame.", dialect,
      class(out)[1L], if (!is.null(source)) sprintf(" for file '%s'", source) else ""),
      class = "radiatR_dialect_error", source = source)
  out
}

# Read and combine multiple track files into one long data frame, preserving
# per-file trajectory identity. Schema is detected from the FIRST file: a batch
# is assumed to share one schema (standard for batch imports; mixed-schema
# batches are out of scope). Returns the combined data frame, the resolved id
# column name, and whether that id was synthesized from file names.
#
#   - Real id column? Resolved with the same order guess_columns() uses:
#     mapping$id if supplied, else .guess_col(names(first), id candidates).
#   - No real id + id_from_filename = FALSE -> stop (was the silent
#     "(multiple)" collapse bug; now a deliberate, informative failure).
#   - No real id + id_from_filename = TRUE -> synthesize id_col from each
#     file's basename stem.
#   - Every row is tagged with ..source_file = basename(path).
#   - Bound with vctrs::vec_rbind() (NA-tolerant across differing columns).
#
# With a dialect, `reader` (the dialect, called on one path) replaces the
# generic read, and everything above applies to the tables it returns. A
# radiatR condition from the dialect is passed on as it is; any other failure
# is re-raised naming the file.
.combine_track_files <- function(paths, mapping, id_from_filename, read_opts,
                                 time_args = list(), reader = NULL,
                                 dialect = NULL) {
  read_opts <- read_opts %||% list()
  dfl <- lapply(paths, function(p) {
    if (is.null(reader))
      return(.read_any(p, ext = read_opts$ext, delim = read_opts$delim,
                       decimal = read_opts$decimal, sheet = read_opts$sheet))
    out <- tryCatch(reader(p), error = function(e) {
      if (any(startsWith(class(e), "radiatR_"))) rlang::cnd_signal(e)
      rlang::abort(sprintf("read_tracks: dialect '%s' failed on file '%s'.",
                           dialect, basename(p)),
                   class = "radiatR_dialect_error", parent = e, source = basename(p))
    })
    .assert_dialect_table(out, dialect, basename(p))
  })

  first_names <- names(dfl[[1L]])
  real_id <- mapping$id %||%
    .guess_col(first_names, c("id", "track", "trajectory", "animal", "subject"))
  synth <- is.null(real_id)

  if (synth && !isTRUE(id_from_filename)) {
    stop("read_tracks: no id column was found in the first file and ",
         "id_from_filename = FALSE, so no per-file trajectory id can be ",
         "assigned. Supply mapping = list(id = <column>) or set ",
         "id_from_filename = TRUE.", call. = FALSE)
  }

  id_col <- if (synth) (mapping$id %||% "..id") else real_id

  # The numeric role columns and text time are coerced file by file, so a
  # malformed cell is reported against its own file and row rather than as a
  # type clash on bind. Typed time is left for read_tracks(), which needs to
  # see whether it was a duration.
  roles <- guess_columns(dfl[[1L]], mapping)
  num_cols <- unlist(roles[c("x", "y", "angle", "weight")], use.names = FALSE)

  dfl <- Map(function(d, p) {
    dec <- attr(d, "decimal") %||% "."
    for (cc in intersect(num_cols, names(d)))
      d[[cc]] <- .as_numeric_strict(d[[cc]], cc, source = basename(p),
                                    dialect = dialect, decimal = dec)
    tc <- roles$time
    if (!is.null(tc) && (is.character(d[[tc]]) || is.factor(d[[tc]])))
      d[[tc]] <- do.call(.coerce_time, c(
        list(d[[tc]], column = tc, source = basename(p), dialect = dialect,
             decimal = dec), time_args))
    if (synth) d[[id_col]] <- tools::file_path_sans_ext(basename(p))
    d[["..source_file"]] <- basename(p)
    # Internal only: full/relative path, used to distinguish same-named files
    # in different directories for collision detection. Dropped before the
    # final Tracks data is built (not in keep_cols selection below).
    d[["..source_path"]] <- p
    d
  }, dfl, paths)

  df <- vctrs::vec_rbind(!!!dfl)
  list(df = df, id_col = id_col, synth = synth)
}

# ---- core: read_tracks ------------------------------------------------------
#' Construct a Tracks from a *format* spec (registered name or inline list)
#' @param x data.frame or path(s)
#' @param format registered format name or list spec
#' @param ... extra args override spec fields
#' @return A `Tracks` object, as returned by [read_tracks()].
#' @export
read_tracks_format <- function(x, format, ...) {
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
    # dial args handled by read_tracks via dialect=... and will pass along ...
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
    normalize_xy   = if (!is.null(spec$normalize_xy)) isTRUE(spec$normalize_xy) else FALSE,
    mutate         = spec$mutate %||% NULL,
    keep           = spec$keep %||% NULL,
    drop           = spec$drop %||% NULL,
    id_from_filename = if (!is.null(spec$id_from_filename)) isTRUE(spec$id_from_filename) else TRUE,
    validate       = if (!is.null(spec$validate)) isTRUE(spec$validate) else TRUE,
    id_collision   = spec$id_collision %||% "error"
  )

  # Allow explicit overrides via ...
  args <- modifyList(args, dots)
  do.call(read_tracks, c(list(x = x), args))
}

#' Construct a Tracks from a data.frame or file(s)
#'
#' @param x data.frame, file path, or character vector of file paths
#' @param mapping named list for column mapping: id, time, x, y, angle, weight. Any missing will be guessed when possible.
#' @param angle_unit "radians","degrees", or "auto" to guess from values
#' @param time_type one of "auto","posix","seconds","frames"
#' @param tz timezone for POSIX times (default "UTC")
#' @param fps Frames-per-second. Required when time_type = "frames" (the time
#'   column is kept as a raw frame index). When a single finite positive value
#'   is supplied and the time column is frame-indexed, it is stored as the
#'   Tracks capture frame rate (see [frame_rate()]); [elapsed_seconds()] then
#'   performs the frame -> seconds conversion once, on demand, so kinematics
#'   and the app adopt it automatically without converting twice.
#' @param normalize_xy FALSE (default) keeps (x,y) as supplied. TRUE
#'   independently centres each trajectory on its own bounding-box midpoint
#'   and scales it to the unit circle -- shape-preserving only, NOT
#'   calibration (it does not preserve bearing from a fixed arena origin).
#'   See [tracks()] for the full explanation.
#' @param origin,radius Optional fixed unit-circle calibration forwarded to
#'   [tracks()]: `origin = c(x, y)` centre and positive scalar `radius` in raw
#'   coordinate units. Supplied together, they map every trajectory uniformly via
#'   `(xy - origin) / radius`, preserving bearings from the origin. Mutually
#'   exclusive with `normalize_xy = TRUE`.
#' @param read_opts list of file-reading overrides: `delim` (field separator),
#'   `decimal` (decimal mark), and `sheet` (Excel worksheet name or number).
#'   Any `NULL` element is auto-detected. Defaults to all-auto.
#' @param dialect optional registered dialect name. The dialect reads `x`
#'   itself (`read_tracks()` does not parse the file first) and is called once
#'   per file when `x` holds several paths. See [register_loader_dialect()].
#' @param dialect_args named list of extra arguments forwarded to the dialect
#'   function (e.g. \code{list(bodypart = c("head","thorax"))})
#' @param mutate list of transformations applied after reading (named functions or formulas)
#' @param keep only keep these columns (NULL = keep all)
#' @param drop drop these columns after mapping
#' @param id_from_filename if TRUE and id missing, derive id from file stem when reading multiple files
#' @param validate Deprecated and ignored: `Tracks` invariants are always
#'   checked. Any value but `TRUE` warns.
#' @param id_collision how to handle a trajectory id that appears under more
#'   than one source file during a multi-file import: `"error"` (default) stops
#'   with a message naming the colliding id(s) and files; `"namespace"` rewrites
#'   every id to `paste0(file_stem, "::", id)` so ids are unique per file. Has no
#'   effect on single-file or data.frame input.
#' @param on_invalid How to handle rows whose x/y coordinates are missing or
#'   non-finite: "error" (default) aborts with a radiatR_invalid_rows condition
#'   listing the affected rows and ids; "drop" removes them and signals a
#'   radiatR_dropped_rows warning with the same payload. It covers nothing
#'   else: a missing time, id or weight is an error either way, and so is a
#'   malformed value (see "Malformed values").
#' @section Malformed values:
#' A cell in an x, y, angle, weight or time column that is present but does not
#' parse (`"oops"`, `"1,5"`, a stray header or units row) stops the load with a
#' `radiatR_malformed_values` error, a subclass of `radiatR_invalid_rows`. Its
#' `columns`, `rows`, `values`, `source` and `dialect` fields name the column,
#' the data rows (counted from the first row after the header, within its own
#' file in a multi-file import), the offending strings, the file and the
#' dialect. Such a value is never dropped or turned into `NA`. Blank cells and
#' `NA` are missing values; `nan` and `inf` are read as such. Files are read
#' with no digit-grouping mark, so `1,5` is malformed unless
#' `read_opts = list(decimal = ",")` says the comma is the decimal mark.
#'
#' An error raised inside a dialect does not yet name the file or the source
#' column, and counts rows after the dialect's own filtering and reshaping.
#' Coordinates that a dialect converts itself (ANY-maze, EthoVision,
#' DeepLabCut, SLEAP, TRex) are not yet checked this way.
#'
#' A time column given as text must hold either numbers throughout or
#' timestamps in one layout throughout: `YYYY-MM-DD HH:MM:SS`,
#' `YYYY-MM-DD HH:MM` or `YYYY-MM-DD` (with `-` or `/`), or ISO 8601
#' `YYYY-MM-DDTHH:MM:SS` with an optional `Z` for UTC. A bare date beside
#' date-times is midnight.
#' @param format Optional loader format name or list spec registered via [register_loader_format()]
#' @return Tracks
#' @export
read_tracks <- function(x,
                         mapping = list(id=NULL, time=NULL, x=NULL, y=NULL, angle=NULL, weight=NULL),
                         angle_unit = c("radians","degrees","auto"),
                         time_type = c("auto","posix","seconds","frames"), tz = "UTC", fps = NULL,
                         normalize_xy = FALSE,
                         origin = NULL,
                         radius = NULL,
                         dialect = NULL,
                         dialect_args = list(),
                         read_opts = list(delim = NULL, decimal = NULL, sheet = NULL),
                         mutate = NULL,
                         keep = NULL, drop = NULL,
                         id_from_filename = TRUE,
                         validate = TRUE,
                         format = NULL,
                         id_collision = c("error", "namespace"),
                         on_invalid = c("error", "drop")) {
  angle_unit <- match.arg(angle_unit)
  time_type  <- match.arg(time_type)
  id_collision <- match.arg(id_collision)
  on_invalid <- match.arg(on_invalid)

  if (!is.null(format)) {
    return(read_tracks_format(
      x,
      format,
      mapping = mapping,
      angle_unit = angle_unit,
      time_type = time_type,
      tz = tz,
      fps = fps,
      normalize_xy = normalize_xy,
      origin = origin,
      radius = radius,
      dialect = dialect,
      mutate = mutate,
      keep = keep,
      drop = drop,
      id_from_filename = id_from_filename,
      validate = validate,
      id_collision = id_collision,
      on_invalid = on_invalid
    ))
  }

  if (!isTRUE(validate))
    .Deprecated(msg = paste("`validate` is deprecated and has no effect:",
                            "Tracks invariants are always checked."))

  # A dialect is the only reader of its input: read_tracks() does not parse
  # the file itself first, so a loader for a layout the generic reader cannot
  # make sense of (headerless, several header rows, not a table) still works.
  prep <- NULL
  if (!is.null(dialect)) {
    if (!exists(dialect, envir = .loader_registry, inherits = FALSE)) {
      if (dialect %in% .removed_dialects)
        rlang::abort(sprintf(paste0(
          "The '%s' dialect was removed in radiatR 0.1.3. Register your own ",
          "loader with register_loader_dialect(\"%s\", <function>); see ",
          "vignette(\"loaders\")."), dialect, dialect),
          class = "radiatR_removed_dialect", dialect = dialect)
      stop("Unknown dialect '", dialect, "'")
    }
    fun <- get(dialect, envir = .loader_registry, inherits = FALSE)
    prep <- function(input) do.call(fun, c(list(input), dialect_args))
    # Dialects no longer convert frame indices to seconds themselves (that
    # single conversion now happens once, on demand, via elapsed_seconds());
    # adopt a dialect-only fps so it still becomes the Tracks frame_rate.
    if (is.null(fps) && !is.null(dialect_args$fps)) fps <- dialect_args$fps
  }

  # read
  dec <- "."
  if (is.character(x) && any(!file.exists(x)))
    stop("read_tracks: file(s) not found: ",
         paste(x[!file.exists(x)], collapse = ", "), call. = FALSE)
  if (is.data.frame(x)) {
    df <- if (is.null(prep)) x else .assert_dialect_table(prep(x), dialect)
    src <- NULL
  } else if (is.character(x) && length(x) == 1L) {
    df <- if (!is.null(prep)) .assert_dialect_table(prep(x), dialect, basename(x))
          else .read_any(x, ext = read_opts$ext, delim = read_opts$delim,
                         decimal = read_opts$decimal, sheet = read_opts$sheet)
    dec <- attr(df, "decimal") %||% "."
    src <- basename(x)
  } else if (is.character(x) && length(x) > 1L) {
    combined <- .combine_track_files(x, mapping, id_from_filename, read_opts,
                                     list(time_type = time_type, tz = tz, fps = fps),
                                     reader = prep, dialect = dialect)
    df <- combined$df
    # Force the synthesized id column to be picked up by guess_columns() below,
    # so multi-file input never falls into the single-track "(multiple)" path.
    if (isTRUE(combined$synth)) mapping$id <- combined$id_col
    src <- "(multiple)"
  } else {
    stop("read_tracks: 'x' must be a data.frame or path(s) to files")
  }
  # Extra columns emitted by a dialect are kept unless explicitly dropped.
  dialect_extra <- if (!is.null(dialect)) setdiff(names(df), "..source_path") else character(0)

  guessed <- guess_columns(df, mapping)
  id <- guessed$id; time <- guessed$time; angle <- guessed$angle
  xcol <- guessed$x; ycol <- guessed$y; wcol <- guessed$weight
  roles <- c(id = id, time = time, angle = angle, x = xcol, y = ycol, weight = wcol)
  absent <- roles[!roles %in% names(df)]
  if (length(absent))
    rlang::abort(sprintf(
      "read_tracks: mapped column(s) not found: %s. Available: %s.",
      paste0(names(absent), " = '", absent, "'", collapse = ", "),
      paste(names(df), collapse = ", ")),
      class = "radiatR_missing_columns", columns = unname(absent))
  # A guessed id column with no values is no id column (SLEAP writes an empty
  # `track` for single-animal exports). A column named in `mapping` is taken at
  # the caller's word, and multi-file imports keep their own id handling.
  # The next id-like column, if any, is tried in its place.
  empty_ids <- character(0)
  while (nrow(df) > 0L && !is.null(id) && is.null(mapping$id) &&
         !("..source_file" %in% names(df)) &&
         all(.is_missing_chr(trimws(as.character(df[[id]]))))) {
    message("Id column '", id, "' is empty; ignoring it.")
    empty_ids <- c(empty_ids, id)
    id <- guess_columns(df[setdiff(names(df), empty_ids)], mapping)$id
  }

  # Multi-file id-collision policy. Only runs for multi-file imports (the combine
  # path tags rows with ..source_file); single-file / data.frame input is
  # untouched. For synthesized ids this handles same-stem files in different
  # subdirectories (read_tracks_dir(recursive = TRUE)); for real ids it handles
  # the same id value reused across files.
  if (!is.null(id) && "..source_file" %in% names(df)) {
    id_vals   <- as.character(df[[id]])
    src_vals  <- as.character(df[["..source_file"]])
    # Collision existence is determined by distinct source PATHS (so same-
    # named files in different directories are correctly seen as distinct
    # sources); the user-facing message still lists basenames for readability.
    path_vals <- as.character(df[["..source_path"]])
    n_src    <- tapply(path_vals, id_vals, function(s) length(unique(s)))
    colliding <- names(n_src)[n_src > 1L]
    if (length(colliding)) {
      if (id_collision == "error") {
        details <- vapply(colliding, function(idv) {
          files <- unique(src_vals[id_vals == idv])
          sprintf("  id '%s' appears in: %s", idv, paste(files, collapse = ", "))
        }, character(1))
        stop("read_tracks: trajectory id(s) collide across source files:\n",
             paste(details, collapse = "\n"),
             "\nPass id_collision = \"namespace\" to prefix ids with the file ",
             "stem, or supply a mapping that yields file-unique ids.",
             call. = FALSE)
      } else {
        # "namespace": prefix ALL ids (not just colliding ones) so id "1" means
        # the same thing everywhere it appears.
        df[[id]] <- paste0(tools::file_path_sans_ext(src_vals), "::", id_vals)
      }
    }
  }

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
  # Duplicate (id, time) check for multi-file imports, kept for its file-level
  # message (it names the files that collide). Uses raw pre-coercion time.
  # tracks() rejects any remaining duplicate key as radiatR_invalid_tracks.
  if ("..source_file" %in% names(df)) {
    key <- paste(as.character(df[[id]]), as.character(df[[time]]), sep = "\r")
    dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(dup)) {
      d_id   <- as.character(df[[id]])[dup]
      d_time <- as.character(df[[time]])[dup]
      d_src  <- as.character(df[["..source_file"]])[dup]
      d_key  <- key[dup]
      first  <- !duplicated(d_key)
      details <- vapply(which(first), function(i) {
        files <- unique(d_src[d_key == d_key[i]])
        sprintf("  id '%s' at time '%s' in: %s",
                d_id[i], d_time[i], paste(files, collapse = ", "))
      }, character(1))
      stop("read_tracks: duplicate (id, time) key(s) after combining files:\n",
           paste(details, collapse = "\n"),
           "\nEach (id, time) pair must be unique across the combined import.",
           call. = FALSE)
    }
  }
  # Malformed cells abort here whatever `on_invalid` says, before any row is
  # dropped, so the rows they name are rows of the table as read. Only missing
  # and non-finite coordinates reach the report / drop step below.
  for (cc in c(if (!is.null(xcol) && !is.null(ycol)) c(xcol, ycol), angle, wcol))
    df[[cc]] <- .as_numeric_strict(df[[cc]], cc, source = src, dialect = dialect,
                                   decimal = dec)
  time_is_duration <- inherits(df[[time]], "difftime")
  df[[time]] <- .coerce_time(df[[time]], time_type = time_type, tz = tz, fps = fps,
                             column = time, source = src, dialect = dialect,
                             decimal = dec)
  # Report / drop rows with non-finite coordinates (position-based load).
  if (!is.null(xcol) && !is.null(ycol)) {
    finite <- is.finite(df[[xcol]]) & is.finite(df[[ycol]])
    if (any(!finite)) {
      bad  <- which(!finite)
      ids  <- as.character(df[[id]])[bad]
      vals <- paste(df[[xcol]][bad], df[[ycol]][bad], sep = "/")
      if (identical(on_invalid, "error"))
        radiatR_abort_invalid_rows(bad, ids, c(xcol, ycol), vals, call = NULL)
      radiatR_warn_dropped_rows(bad, ids, c(xcol, ycol), vals)
      df <- df[finite, , drop = FALSE]
    }
  }

  # coarse type fixes
  if (!is.numeric(df[[id]])) df[[id]] <- as.character(df[[id]])
  # `time_is_frames` records whether the *stored* time column
  # is still a raw frame index needing a frame_rate-driven division (done
  # once, on demand, by elapsed_seconds()) rather than already being real
  # seconds/POSIXct -- storing frame_rate for the latter would double-convert.
  # Read off the coerced column, so numeric strings count as frames too; a
  # duration (difftime) is seconds already.
  time_is_frames <- time_type == "frames" ||
    (time_type == "auto" && is.numeric(df[[time]]) && !time_is_duration && !is.null(fps))

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

  # Hand off to Tracks constructor. A valid capture rate supplied via `fps`
  # is stored durably so kinematics and the app adopt it without re-entry --
  # but only when the time column is still frame-indexed; otherwise the time
  # column already carries real seconds/POSIXct and no further division
  # should happen downstream.
  meta <- list(source = src)
  if (time_is_frames && is.numeric(fps) && length(fps) == 1L && is.finite(fps) && fps > 0)
    meta$frame_rate <- as.numeric(fps)
  ts <- tracks(df,
                id = id, time = time, angle = angle,
                x = xcol, y = ycol,
                angle_unit = ang_unit,
                weight = wcol,
                normalize_xy = normalize_xy,
                origin = origin,
                radius = radius,
                meta = meta)
}

#' Read all matching files from a directory and bind into a Tracks
#' @param dir Directory to scan for files
#' @param pattern Regex passed to `list.files()` to select files
#' @param recursive Recurse into subdirectories when TRUE
#' @param ... Additional arguments passed to `read_tracks()`
#' @return A single `Tracks` object combining every matching file.
#' @export
read_tracks_dir <- function(dir, pattern = "\\.(csv|tsv|txt|parquet|feather)$", recursive = FALSE, ...) {
  stopifnot(dir.exists(dir))
  files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (!length(files)) stop("No files matched in ", dir)
  read_tracks(files, ...)
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

#' Load trajectories listed in a file table into a Tracks
#'
#' This high-level helper combines the file discovery tibble returned by
#' [import_tracks()] with optional metadata from an experiment manifest, reads
#' each track file using [read_tracks()], and merges the results into a single
#' `Tracks`.
#'
#' @param file_tbl Tibble returned by [import_tracks()], containing at least the
#'   columns `basename` and `track`.
#' @param track_dir Directory containing the track files on disk.
#' @param manifest Optional data frame with a `file` column that matches
#'   `file_tbl$basename`.
#' @param manifest_cols Optional named character vector (or list) mapping new
#'   column names in `file_tbl` to column names present in `manifest`. When
#'   `NULL`, all columns aside from `file` are carried over with the same names.
#' @param mapping Optional explicit column mapping passed to [read_tracks()].
#' @param angle_unit,time_type,tz,fps Passed to [read_tracks()].
#' @param normalize_xy Logical; normalise x/y to unit circle. Default
#'   \code{FALSE}. See [tracks()] for why \code{TRUE} is shape-preserving
#'   only, not calibration.
#' @param dialect Optional dialect name; passed to [read_tracks()].
#' @param keep,drop Column selection vectors passed to [read_tracks()].
#' @param ... Additional arguments forwarded to [read_tracks()].
#'
#' @return A `Tracks` with metadata columns replicated for each observation.
#' @export
load_manifest <- function(file_tbl, track_dir, manifest = NULL,
                                  manifest_cols = NULL,
                                  mapping = NULL,
                                  angle_unit = c("radians","degrees","auto"),
                                  time_type = c("auto","posix","seconds","frames"),
                                  tz = "UTC", fps = NULL,
                                  normalize_xy = FALSE,
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
    ts <- do.call(read_tracks, args)

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

# ---- example dialects (opt-in) -----------------------------------------------
# Declarative *format* examples (YAML/JSON). Users can register a spec and then call read_tracks_format(...).
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
#   ts <- read_tracks_format("/data/dir/run1.csv", format = "my_frames_spec")

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
  # SLEAP writes an empty track column for untracked (single-animal) exports.
  if (id != "1" && id %in% names(df) && all(is.na(df[[id]]))) id <- "1"
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
