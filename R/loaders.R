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
#' @examples
#' untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
#'       exdir = tempdir())
#' track_dir <- file.path(tempdir(), "tracks")
#' file_tbl <- import_tracks(track_dir)
#' head(file_tbl)
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

# Removed dialects whose files another dialect now reads.
.replaced_dialects <- c(deeplabcut_multiheader = "deeplabcut")

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
#' @examples
#' # A tracker whose columns are found by pattern; time is in seconds
#' spec <- list(
#'   regex_mapping = list(id = "^animal", time = "^sec",
#'                        x = "^pos_?x", y = "^pos_?y"),
#'   time = list(type = "seconds")
#' )
#' register_loader_format("my_tracker", spec, overwrite = TRUE)
#' list_loader_formats()
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
#' @examples
#' # Empty until a format is registered with register_loader_format()
#' list_loader_formats()
#' @export
list_loader_formats <- function() sort(ls(envir = .loader_format_registry))

.loader_registry <- new.env(parent = emptyenv())

#' Register a custom loader dialect
#'
#' The function must accept (x, ...) and return a data.frame in long form with columns at least id,time and one of (angle) or (x,y)
#'
#' `x` is whatever was passed to [read_tracks()]: a data frame, or one file
#' path. The dialect is the only reader of that file, and is called once per
#' file when `read_tracks()` is given several paths. A table returned without
#' an id column is one trajectory, named after its file in a multi-file
#' import. Numeric columns returned as text are coerced strictly (see
#' "Malformed values" in [read_tracks()]) using the decimal mark sniffed from
#' the file; a dialect that knows better can set `attr(out, "decimal")`.
#'
#' Numeric time is read as a frame index unless the dialect says its time
#' column is in seconds, with `attr(out, "time_unit") <- "seconds"` (or
#' `"frames"`, to rule out `time_type = "seconds"`). See "Time units from a
#' dialect" in [read_tracks()].
#'
#' A dialect that derives each position from several points can report what
#' it left out with `attr(out, "attrition")`: a data frame with columns
#' `source`, `point`, `masked`, `frames`, `frames_no_position`, `score_min`
#' and `partial`, one row per point, with whole non-negative counts and one
#' `frames` and one `frames_no_position` value (the per-file totals, repeated
#' on each row). See "Masked points" in [read_tracks()].
#'
#' A dialect can keep facts about the file it read (a header block, the units
#' of its columns) with `attr(out, "source_meta")`: a list with distinct names
#' other than `source` and `path`. [read_tracks()] stores one record per file
#' in `Tracks@meta$source_meta`, each the dialect's list with the file's name
#' (`source`) and path (`path`) in front. The records are a plain list in file
#' order, not named by file, because files in different folders share names.
#'
#' A loader that reads files can take `read_tracks(read_opts = )` by
#' declaring a `read_opts` argument: it receives the non-`NULL` entries
#' (`decimal`, `delim`, `sheet`, `ext`) and must honour them or stop. A table
#' it returns without `attr(, "decimal")` is coerced with `read_opts$decimal`,
#' else the file's sniffed mark. Without that argument, any `read_opts` stop
#' the load.
#' @param name Unique dialect name
#' @param fun Function that accepts (x, ...) and returns a data.frame with id/time/angle or id/time/x/y
#' @param overwrite Replace an existing dialect registered with the same name
#' @return `name`, invisibly. Called for its side effect of registering the
#'   dialect for `read_tracks(dialect = )`.
#' @examples
#' # dtrack writes headerless, tab-separated frame, x, y (and a flag) columns
#' dtrack_loader <- function(x) {
#'   d <- utils::read.delim(x, header = FALSE, colClasses = "numeric")
#'   data.frame(id = tools::file_path_sans_ext(basename(x)),
#'              frame = d[[1]], x = d[[2]], y = d[[3]])
#' }
#' register_loader_dialect("dtrack", dtrack_loader, overwrite = TRUE)
#' list_loader_dialects()
#'
#' untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
#'       exdir = tempdir())
#' track_dir <- file.path(tempdir(), "tracks")
#' read_tracks(file.path(track_dir, "10_10_point02.txt"), dialect = "dtrack")
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
#' @examples
#' # Built-in dialects plus any registered this session
#' list_loader_dialects()
#' @export
list_loader_dialects <- function() sort(ls(envir = .loader_registry))

# ---- helpers -----------------------------------------------------------------
.is_installed <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

.drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

# The compression of a file, told by its first bytes: "zip", "gzip", "bzip2",
# "xz" or "none".
.compression <- function(path) {
  head <- readBin(path, "raw", 6L)
  sigs <- list(zip = c(0x50, 0x4b, 0x03, 0x04), gzip = c(0x1f, 0x8b),
               bzip2 = c(0x42, 0x5a, 0x68), xz = c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00))
  for (k in names(sigs))
    if (identical(head[seq_along(sigs[[k]])], as.raw(sigs[[k]]))) return(k)
  "none"
}

# A line break quoted in a name or cell of a CRLF file is "\r\n" to readr and
# fread() and "\n" to read.table(): make it "\n" whichever reader read it.
.lf_breaks <- function(df) {
  # by bytes, as a cell may not be valid in the locale; keep each encoding mark
  lf <- function(v) {
    out <- gsub("\r\n", "\n", v, fixed = TRUE, useBytes = TRUE)
    Encoding(out) <- Encoding(v)
    out
  }
  names(df) <- lf(names(df))
  crlf <- vapply(df, function(v)
    is.character(v) && any(grepl("\r\n", v, fixed = TRUE, useBytes = TRUE)), logical(1))
  for (cc in which(crlf)) df[[cc]] <- lf(df[[cc]])
  df
}

# Write the gzip, bzip2 or xz file `path` decompressed to `to`.
.decompress <- function(path, to) {
  from <- gzfile(path, "rb")
  on.exit(close(from))
  out <- file(to, "wb")
  on.exit(close(out), add = TRUE)
  while (length(b <- readBin(from, "raw", 1e6L))) writeBin(b, out)
}

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
    id     = pick("id",     .id_candidates),
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
# There a "." is not a decimal mark ("1.234" may as well be 1234), so a value
# holding one is not a number unless `dot_ok`.
.parse_number <- function(s, decimal = ".", dot_ok = FALSE) {
  if (!identical(decimal, ",")) return(suppressWarnings(as.numeric(s)))
  num <- suppressWarnings(as.numeric(sub("^([-+]?[0-9]*),([0-9]+)$", "\\1.\\2", s)))
  if (!dot_ok) num[grepl(".", s, fixed = TRUE)] <- NA_real_
  num
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
      # read_opts help only where they reach the reader (see .takes_read_opts())
      decimal_hint = .takes_read_opts(dialect), decimal = decimal)
  num
}

# Timestamp layouts accepted for text time, in order of preference. A value
# must match its layout in full: base as.POSIXct() accepts a prefix match,
# which silently cuts "2024-01-01T10:00:00Z" and "2024-01-01 garbage" to
# midnight. A zone designator after the time ("Z", "+02:00", "-0130", "+02")
# is read apart from the layout (see .split_zone()).
.time_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                   "%Y-%m-%dT%H:%M:%OS",
                   "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M", "%Y-%m-%dT%H%M",
                   "%Y%m%dT%H%M%OS",
                   "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d")

# The zone designator that ends a date-time: "Z" (UTC) or an offset from UTC
# of at most 14 hours, +hh:mm, +hhmm or +hh. Returns the value without it
# (`core`), whether it had one (`zoned`) and the offset in seconds (`offset`,
# NA where it had none or one out of range: `zoned` & is.na(offset) is no
# date-time).
.split_zone <- function(s) {
  zre <- "(Z|[+-][0-9]{2}(:?[0-9]{2})?)$"
  # after hh:mm(:ss), or the ISO 8601 basic Thhmm(ss)
  zoned <- !is.na(s) & grepl(paste0(
    "([0-9]:[0-9]{2}(:[0-9]{2}([.][0-9]+)?)?|T[0-9]{4}([0-9]{2}([.][0-9]+)?)?)", zre), s)
  core <- s
  core[zoned] <- sub(zre, "", s[zoned])
  z <- substring(s[zoned], nchar(core[zoned]) + 1L)
  digits <- gsub("[^0-9]", "", z)
  hh <- as.integer(substr(digits, 1L, 2L))
  mm <- ifelse(nchar(digits) >= 4L, as.integer(substr(digits, 3L, 4L)), 0L)
  secs <- ifelse(z == "Z", 0, ifelse(startsWith(z, "-"), -1, 1) * (hh * 3600 + mm * 60))
  secs[z != "Z" & (mm > 59L | abs(secs) > 14 * 3600)] <- NA_real_
  offset <- rep(NA_real_, length(s))
  offset[zoned] <- secs
  list(core = core, zoned = zoned, offset = offset)
}

# A field between separators may drop its leading zero; one run into the
# field before or after it (ISO 8601 basic, "%Y%m%dT%H%M%OS") has its full
# two digits, or it could not be told from its neighbour.
.time_format_regex <- function(fmt) {
  tok <- regmatches(fmt, gregexpr("%OS|%[YmdHM]|[^%]+", fmt))[[1L]]
  field <- startsWith(tok, "%")
  run <- field & (c(FALSE, utils::head(field, -1L)) | c(field[-1L], FALSE))
  re <- ifelse(!field, tok, ifelse(tok == "%Y", "[0-9]{4}", paste0(
    ifelse(run, "[0-9]{2}", "[0-9]{1,2}"), ifelse(tok == "%OS", "([.][0-9]+)?", ""))))
  paste0("^", paste(re, collapse = ""), "$")
}

# The layout of .time_formats that text date-times `core` (zone designators
# split off) are read in: the one most of them match, a bare date counting
# toward a date-time layout that has values of its own. Returns its index
# (`best`) and which values match each layout (`hit`).
.time_layout <- function(core) {
  hit <- lapply(.time_formats, function(f) grepl(.time_format_regex(f), core))
  n_hit <- vapply(hit, sum, integer(1))
  n_bare <- n_hit[match(sub("[ T]%H.*$", "", .time_formats), .time_formats)]
  n_bare[is.na(n_bare)] <- 0L     # the bare date of a basic layout is no layout
  has_time <- grepl("%H", .time_formats, fixed = TRUE) & n_hit > 0L
  list(best = which.max(n_hit + ifelse(has_time, n_bare, 0L)), hit = hit)
}

# Whether text `v` is date-times: every value that is not missing, less its
# zone designator, in the one layout .parse_time_chr() would read it in, or
# the bare date of that layout.
.looks_like_time <- function(v) {
  if (!is.character(v)) return(FALSE)
  s <- trimws(v)
  s <- s[!.is_missing_chr(s)]
  if (!length(s)) return(FALSE)
  lay <- .time_layout(.split_zone(s)$core)
  fmt <- .time_formats[lay$best]
  date_fmt <- sub("[ T]%H.*$", "", fmt)
  all(lay$hit[[lay$best]] | (fmt != date_fmt & grepl(.time_format_regex(date_fmt), s)))
}

# Wall-clock fields `lt` (POSIXlt) as instants in `tz`. A time that does not
# exist there (the hour skipped when clocks go forward, or a whole day) is NA:
# base R moves it on, or not, by platform. Built with as.POSIXct.POSIXlt (mktime),
# several times faster than ISOdatetime().
.wall_clock <- function(lt, tz) {
  lt <- as.POSIXlt(lt)
  want <- lt
  # mktime() in tz, with DST left for it to find; a time that does not exist
  # there comes back moved, which the check below turns into NA
  attr(lt, "tzone") <- tz
  lt$isdst[] <- -1L
  lt$zone <- NULL
  lt$gmtoff <- NULL
  out <- as.POSIXct(lt, tz = tz)
  back <- as.POSIXlt(out, tz = tz)
  moved <- back$hour != want$hour | back$min != want$min |
    back$mday != want$mday | back$mon != want$mon | back$year != want$year
  out[which(moved)] <- NA
  out
}

# The first instant of each date in tz: midnight, or, where a DST change skips
# midnight (Cairo, Sao Paulo), the end of the skipped time. A bare date names a
# day, so it is that day's start. A date tz skips altogether is malformed;
# `rows` are the positions to report for `dates` (default: their own). With
# `skipped = "na"` such a date is NA instead, for the caller to report.
.day_start <- function(dates, tz, column = "time", source = NULL, dialect = NULL,
                       rows = seq_along(dates), skipped = c("abort", "na")) {
  skipped <- match.arg(skipped)
  dates <- as.Date(dates)     # also fread's IDate
  out <- .wall_clock(as.POSIXlt(dates), tz)
  # .wall_clock() gives NA for a midnight tz skips (Cairo) and for a day it
  # skips altogether (Apia): look for the day's first instant
  need <- !is.na(dates) & is.na(out)
  for (day in unique(format(dates[need]))) {
    # from the previous noon (a real time) in 15-minute steps to the first
    # instant whose date in tz is `day`
    t <- as.POSIXct(paste(as.Date(day) - 1, "12:00:00"), tz = tz) +
      seq(0, 36 * 3600, by = 900)
    hit <- t[match(day, format(t, "%Y-%m-%d", tz = tz))]
    if (is.na(hit) && skipped == "abort") {
      at <- which(format(dates) %in% day)
      radiatR_abort_malformed_values(
        column, rows[at], format(dates[at]),
        expected = sprintf("dates that exist in time zone '%s'", tz),
        source = source, dialect = dialect)
    }
    out[which(format(dates) %in% day)] <- hit
  }
  attr(out, "tzone") <- tz
  out
}

# Text time: numbers throughout give numeric time, timestamps in one layout
# throughout give POSIXct. Otherwise the values that break the majority
# reading abort with radiatR_malformed_values. Beside a date-time layout a
# bare date is the first instant of its day (as.character() writes midnight
# that way).
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
  zn <- .split_zone(s)
  core <- zn$core
  lay <- .time_layout(core)
  hit <- lay$hit
  best <- lay$best
  fmt <- .time_formats[best]
  # a zoned value is the instant its own zone says, wherever tz is
  z <- zn$zoned
  out <- .POSIXct(rep(NA_real_, length(core)), tz = tz)
  out[!z] <- if (grepl("%H", fmt, fixed = TRUE))
    .wall_clock(strptime(core[!z], fmt, tz = tz), tz)
  else .day_start(as.Date(strptime(core[!z], fmt, tz = "UTC")), tz, column, source, dialect,
                  rows = which(!z))
  out[z] <-.wall_clock(strptime(core[z], fmt, tz = "UTC"), "UTC") - zn$offset[z]
  date_fmt <- sub("[ T]%H.*$", "", fmt)
  bare <- fmt != date_fmt & grepl(.time_format_regex(date_fmt), s)
  # a bare date carries no zone, whatever the layout beside it says
  out[bare] <- .day_start(as.Date(strptime(s[bare], date_fmt, tz = "UTC")), tz,
                          column, source, dialect, rows = which(bare))
  attr(out, "tzone") <- tz
  ok <- (hit[[best]] | bare) & !is.na(out)
  # Zoned and unzoned date-times in one column are two readings: the fewer
  # are malformed, as a layout in the minority is.
  dt <- hit[[best]] & !bare
  mix <- dt & (if (sum(dt & z) >= sum(dt & !z)) !z else z) & any(dt & z) & any(dt & !z)
  ok[mix] <- FALSE
  if (all(ok | missing)) return(out)
  as_num <- sum(num_ok) >= sum(ok)
  bad <- !missing & !(if (as_num) num_ok else ok)
  if (!as_num) {
    # name the cause where it is known: a leap second, a time skipped in tz,
    # then a zone mix. %OS ends every layout it is in, and its field there is
    # two digits after ':' or (ISO 8601 basic) after the hour and minute.
    layout <- hit[[best]] & !missing & !bare
    leap <- layout & grepl("%OS", fmt, fixed = TRUE) &
      grepl("(:|T[0-9]{4})60([.][0-9]+)?$", core)
    real <- !is.na(strptime(core, fmt, tz = "UTC"))
    gapv <- layout & !leap & !z & real & is.na(out)
    for (cause in list(list(leap, "a date-time (a leap second, :60, cannot be represented)"),
                       list(gapv, sprintf("times that exist in time zone '%s'", tz)),
                       list(mix & !missing, "date-times that all carry a zone, or none")))
      if (any(cause[[1]]))
        radiatR_abort_malformed_values(column, which(cause[[1]]), s[cause[[1]]],
                                       expected = cause[[2]], source = source,
                                       dialect = dialect, decimal_hint = FALSE,
                                       decimal = decimal)
  }
  expected <- if (!any(num_ok | ok)) "a number or date-time"
              else if (as_num) "numeric" else "a date-time"
  radiatR_abort_malformed_values(column, which(bad), s[bad], expected = expected,
                                 source = source, dialect = dialect,
                                 decimal_hint = .takes_read_opts(dialect),
                                 decimal = decimal)
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
  if (inherits(v, "Date")) v <- .day_start(v, tz, column, source, dialect)
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

# A time column of durations written H:MM:SS(.fff), as ANY-maze writes them,
# as seconds. Hours run to any length; minutes and seconds are two digits
# below 60. A difftime (hms included) is converted. Missing cells are NA.
# Anything else is malformed, plain numbers included: their unit (seconds,
# milliseconds, frames) cannot be told, and a caller who knows it converts
# them with as.difftime().
.parse_duration <- function(v, column, source = NULL, dialect = NULL, decimal = ".") {
  if (inherits(v, "difftime")) return(as.numeric(v, units = "secs"))
  s <- trimws(as.character(v))
  missing <- .is_missing_chr(s)
  mark <- if (identical(decimal, ",")) "," else "[.]"
  pat <- paste0("^([0-9]+):([0-5][0-9]):([0-5][0-9](", mark, "[0-9]+)?)$")
  dur <- !missing & grepl(pat, s)
  part <- function(i) as.numeric(sub(",", ".", sub(pat, paste0("\\", i), s[dur]), fixed = TRUE))
  bad <- !missing & !dur
  if (any(bad))
    radiatR_abort_malformed_values(
      column, which(bad), s[bad], source = source, dialect = dialect,
      decimal_hint = .takes_read_opts(dialect), decimal = decimal,
      expected = paste0("a duration H:MM:SS.fff (a column of seconds can be given as ",
                        "as.difftime(<seconds>, units = \"secs\"))"))
  out <- rep(NA_real_, length(s))
  out[dur] <- part(1L) * 3600 + part(2L) * 60 + part(3L)
  out[missing] <- NA_real_
  out
}

# Sniff the field separator (and, for ';', the decimal mark) from a delimited
# file's first non-empty lines. Returns list(delim, decimal). Chooses the
# candidate that appears most often AND consistently across lines; fields are
# counted as .check_fields() counts them (a separator inside quotes is not
# one); a header that alone differs is left to .check_fields(), which names
# it; no signal falls back to comma + dot (read_tracks() names a file that
# then reads as one column). ';' implies the European decimal comma,
# unless the lines read show decimal points and no decimal comma: a dotted
# number that cannot be digit grouping ("1.5", "0.125"; not "1.234").
# `lines` are the file's lines when the caller has read (and decoded) them.
.guess_delim <- function(path, lines = NULL) {
  default <- list(delim = ",", decimal = ".")
  if (is.null(lines)) {
    lines <- readLines(path, n = 20L, warn = FALSE, encoding = "UTF-8")
    # The sniff looks only at separators and digits, which are ASCII: read a
    # line that is not UTF-8 (latin-1 from a Windows program) as latin-1 so the
    # pattern matching below can run on it.
    bad <- !validUTF8(lines)
    lines[bad] <- iconv(lines[bad], "latin1", "UTF-8")
  }
  lines <- utils::head(lines, 20L)
  lines <- lines[nzchar(trimws(lines))]
  sample <- lines
  lines <- utils::head(lines, 5L)
  if (!length(lines)) return(default)

  cands <- c(",", ";", "\t", "|")
  # Times each candidate separates fields on each line, counted as
  # .check_fields() counts them: a separator inside quotes is part of a field,
  # and a line that continues a quoted field gives NA (dropped). Up to the 20
  # kept lines are counted so that continuations still leave 5 counts.
  per <- lapply(cands, function(d) {
    con <- textConnection(sample)
    on.exit(close(con))
    n <- utils::count.fields(con, sep = d, quote = "\"", comment.char = "",
                             blank.lines.skip = TRUE)
    n[!is.na(n)] - 1L
  })
  k <- min(5L, lengths(per))
  if (k == 0L) return(default)   # an unclosed quote: leave it to .check_fields()
  counts <- matrix(vapply(per, function(n) n[seq_len(k)], integer(k)), nrow = k)

  # a separator is viable only if it appears at least once and the same number
  # of times on every line we looked at (consistency guard)
  consistent <- function(m) apply(m, 2L, function(col) col[1L] > 0 && length(unique(col)) == 1L)
  first <- 1L
  viable <- consistent(counts)
  # A header that alone differs (a table written with row names): take the
  # rows' separator and leave the header to .check_fields(), which names it.
  # It must separate the header too, or a ';' file's decimal commas could win.
  if (!any(viable) && nrow(counts) > 1L) {
    first <- 2L
    viable <- consistent(counts[-1L, , drop = FALSE]) & counts[1L, ] > 0L
  }
  if (!any(viable)) return(default)
  best <- cands[which(viable)][which.max(counts[first, viable])]
  if (best != ";") return(list(delim = best, decimal = "."))
  cells <- gsub("^\"|\"$", "", trimws(unlist(strsplit(sample, ";", fixed = TRUE))))
  m <- .mark_cells(cells)
  if (!any(m$point) || any(m$comma)) return(list(delim = best, decimal = ","))
  # A point: keep each column's cells, from the data records of the sample, so
  # that read_tracks() can check the point is shown by a column it reads
  # (.check_decimal_evidence()). Fields are split as the readers split them
  # ('"' quotes). The tally ends at the last line where no quoted field is
  # open; a record whose quoted field runs past the sample is left out.
  # `cells` is list() when no data record is whole in the sample (the first
  # one's quoted field runs past it): the reader stops on that, as the point
  # cannot be checked.
  quotes <- lengths(regmatches(sample, gregexpr("\"", sample, fixed = TRUE)))
  closed <- which(cumsum(quotes) %% 2L == 0L)
  rows <- if (length(closed) >= 2L) sample[(closed[1L] + 1L):closed[length(closed)]]
  tally <- if (length(rows))
    utils::read.table(text = rows, sep = ";", quote = "\"", header = FALSE,
                      colClasses = "character", fill = TRUE, comment.char = "",
                      na.strings = character(0), strip.white = TRUE,
                      blank.lines.skip = TRUE)
  list(delim = best, decimal = ".", cells = if (is.null(tally)) list() else unname(as.list(tally)))
}

# What each cell (trimmed text) shows about the decimal mark: a decimal comma
# ("1,5"); a decimal point that cannot be digit grouping ("1.5", "0.125", or a
# duration's fraction "0:00:00.320", as ANY-maze writes time); or a dotted
# number that may as well be digit grouping ("1.234", "1.234.567").
.mark_cells <- function(cells) {
  # A grouping shape has a dot ("1.234.567" is not `dotted`), a comma or
  # duration cell is not `dotted`: each pattern is tried only on the cells it
  # can still match.
  dotted <- grepl("^[-+]?[0-9]*[.][0-9]+([eE][-+]?[0-9]+)?$", cells)
  grouping <- comma <- duration <- logical(length(cells))
  dot <- grepl(".", cells, fixed = TRUE)
  grouping[dot] <- grepl("^[-+]?[1-9][0-9]{0,2}([.][0-9]{3})+$", cells[dot])
  comma[!dotted] <- grepl("^[-+]?[0-9]*,[0-9]+$", cells[!dotted])
  duration[!dotted] <- grepl("^[0-9]+:[0-5][0-9]:[0-5][0-9][.][0-9]+$", cells[!dotted])
  list(comma = comma, point = (dotted & !grouping) | duration,
       grouping = grouping, dotted = dotted)
}

# Each column's distinct non-missing cells, trimmed.
.distinct_cells <- function(cells) lapply(cells, function(v) unique(trimws(unique(v[!is.na(v)]))))

# The decimal-point evidence of a table's columns (`cells`, a named list of
# character vectors): `point`, the columns with a decimal point that cannot be
# digit grouping; `grouping`, the columns whose dotted numbers all could be,
# each with up to three of them (for the error). `marks` are the columns'
# .mark_cells() when the caller has them (of `vals`, the distinct trimmed cells).
.decimal_evidence <- function(cells, vals = .distinct_cells(cells),
                              marks = lapply(vals, .mark_cells)) {
  point <- vapply(marks, function(m) any(m$point), logical(1))
  # a column with a decimal comma is not grouping-only: EthoVision assumes a
  # point beside commas, and the comma cell is then malformed, which says more
  grouping <- !point & vapply(marks, function(m) any(m$grouping) && !any(m$comma),
                              logical(1))
  list(point = names(vals)[point],
       grouping = Map(function(v, m) utils::head(v[m$grouping], 3L),
                      vals[grouping], marks[grouping]))
}

# A decimal point sniffed from a file stands when a column read for a role
# (`cols`: x, y, time, angle, weight) shows one, or when no such column holds
# only dotted numbers that could be digit grouping. Otherwise "1.234" there is
# 1.234 or 1234 on the strength of some other column (a label "1.5"), and the
# load stops. `evidence` is attr(, "decimal_evidence"); NULL (the mark was
# given, or not sniffed) is no check.
.check_decimal_evidence <- function(evidence, cols, source, dialect = NULL) {
  if (is.null(evidence)) return(invisible(NULL))
  cols <- unique(as.character(cols))
  hit <- intersect(cols, names(evidence$grouping))
  if (!length(hit) || length(intersect(cols, evidence$point))) return(invisible(NULL))
  radiatR_abort_ambiguous_decimal(source, hit,
                                  utils::head(unlist(evidence$grouping[hit], use.names = FALSE), 3L),
                                  evidence$point, dialect)
}

# The column count of the sniff's cells must be the table's; `cells` empty
# means no whole data record was sniffed, so a sniffed point cannot be checked.
.sniffed_evidence <- function(cells, names, source, dialect = NULL) {
  if (!length(cells))
    radiatR_abort_ambiguous_decimal(source, character(0), character(0), character(0), dialect)
  if (length(cells) != length(names))
    radiatR_abort_malformed_file(source, "columns", detail = sprintf(
      "the decimal-mark sniff saw %d columns, the reader %d.", length(cells), length(names)))
  .decimal_evidence(stats::setNames(cells, names))
}

# Decimal mark .read_any() sniffs for `path`: what a dialect that reads its own
# file saw. "." for a format .read_any() does not sniff and for a file that is
# not UTF-8 text (a custom loader's binary format), which cannot be sniffed.
.file_decimal <- function(path) {
  if (tolower(tools::file_ext(path)) %in% c("xlsx", "xls", "parquet", "feather"))
    return(".")
  lines <- tryCatch(readLines(path, n = 20L, warn = FALSE),
                    error = function(e) character(0))
  if (!all(validUTF8(lines))) return(".")
  .guess_delim(path, lines)$decimal
}

# Replace the date-time columns a reader typed (POSIXct) by the text in the
# file, for .parse_time_chr() to read: one parser for every reader, so DST
# gaps, leap seconds, zones and fractions of a second come out the same.
# `read_text(pos)` reads the columns at positions `pos` as text. The names of
# the replaced columns are kept in attr(df, "datetime_text").
.datetime_as_text <- function(df, read_text, source) {
  pos <- which(vapply(df, inherits, logical(1), "POSIXct"))
  if (!length(pos)) return(df)
  text <- read_text(unname(pos))
  if (nrow(text) != nrow(df) || ncol(text) != length(pos))
    radiatR_abort_malformed_file(source, "rows", detail = sprintf(
      "its date-time column(s) %s were not read the same way twice.",
      paste0("'", names(df)[pos], "'", collapse = ", ")))
  for (i in seq_along(pos)) df[[pos[i]]] <- as.character(text[[i]])
  attr(df, "datetime_text") <- names(df)[pos]
  df
}

# read file with soft dependencies. Delimited formats are content-sniffed
# (separator + decimal mark) unless delim/decimal are supplied; Excel and
# Arrow formats dispatch by extension. `sheet` selects an Excel worksheet.
# `tz` is the time zone of timestamps that carry none: an Excel date-time is
# typed in `tz` here; a delimited file's are handed back as text (named in
# attr(df, "datetime_text")), which .coerce_time() reads in `tz`.
# `id` names the mapped id column, a label column like those an id is guessed
# from (see .keep_labels()).
.read_any <- function(path, ext = NULL, delim = NULL, decimal = NULL, sheet = NULL,
                      tz = "UTC", id = NULL, id_text = FALSE, unique_names = FALSE, ...) {
  ext <- if (!is.null(ext)) tolower(sub("^\\.", "", ext)) else tolower(sub(".*\\.", "", path))

  if (ext %in% c("xlsx", "xls")) {
    if (!.is_installed("readxl"))
      stop("Reading Excel files requires the 'readxl' package; ",
           "install it with install.packages(\"readxl\").")
    # With `id_text` the id column is read as the text of its cells (a number
    # cell 100000 as "100000"), to be compared with the ids of a text file.
    types <- NULL
    if (id_text && !is.null(id)) {
      nm <- names(readxl::read_excel(path, sheet = sheet %||% 1L, n_max = 0L,
                                     .name_repair = "minimal"))
      types <- ifelse(trimws(nm) %in% id, "text", "guess")
    }
    df <- .check_names(readxl::read_excel(path, sheet = sheet %||% 1L, col_types = types,
                                          .name_repair = "minimal", ...),
                       basename(path), unique_names)
    # A date-time cell has no zone: readxl hands back its wall-clock time as UTC.
    # A time that does not exist in `tz` is NA and is noted in `tz_gaps` (rows
    # and values by column): read_tracks() stops on one in
    # a column it uses (see .abort_tz_gaps()), so that it never blanks a kept
    # column unseen.
    if (!identical(tz, "UTC")) {
      gaps <- list()
      for (cc in names(df)[vapply(df, inherits, logical(1), "POSIXct")]) {
        out <- .wall_clock(as.POSIXlt(df[[cc]], tz = "UTC"), tz)
        gap <- which(is.na(out) & !is.na(df[[cc]]))
        # a date cell at a midnight that is skipped is its day's first instant;
        # one on a day tz skips altogether stays a gap
        mid <- gap[format(df[[cc]][gap], "%H:%M:%S", tz = "UTC") == "00:00:00"]
        if (length(mid)) {
          out[mid] <- .day_start(as.Date(df[[cc]][mid], tz = "UTC"), tz, skipped = "na")
          gap <- setdiff(gap, mid[!is.na(out[mid])])
        }
        if (length(gap))
          gaps[[cc]] <- list(rows = gap,
                             values = format(df[[cc]][gap], "%Y-%m-%d %H:%M:%S", tz = "UTC"))
        df[[cc]] <- out
      }
      if (length(gaps)) attr(df, "tz_gaps") <- gaps
    }
    return(df)
  }

  if (.is_installed("arrow") && ext %in% c("parquet", "feather")) {
    if (ext == "parquet") return(arrow::read_parquet(path, ...))
    return(arrow::read_feather(path, ...))
  }

  # delimited: sniff separator + decimal mark unless overridden
  cells <- NULL
  if (is.null(delim) || is.null(decimal)) {
    g <- .guess_delim(path)
    if (is.null(delim))   delim   <- g$delim
    # a sniffed point is checked against the role columns
    # (.check_decimal_evidence()); the sniff's cells are fields of its own
    # delimiter, so they count only when that is the one read with
    if (is.null(decimal)) {
      decimal <- g$decimal
      if (identical(delim, g$delim)) cells <- g$cells
    }
  }

  # Column types are guessed from every row, and no grouping mark is assumed:
  # readr's defaults turn a malformed cell past row 1000 into NA and read
  # "1,5" as 15, which hides both from the strict coercion downstream.
  # Every reader reads the same cells: '"' is the only quote, nothing is a
  # comment, blank lines are skipped, an empty cell or NA is missing, a cell
  # keeps its surrounding whitespace and column names are kept as written. The fields are counted first, and
  # every row must be read: each reader drops, wraps, fills or shifts rows of
  # a ragged file in its own way, some with only a warning (see .check_fields()).
  src <- basename(path)
  records <- .check_fields(path, delim, src)
  use_readr <- .is_installed("readr")
  use_fread <- !use_readr && .is_installed("data.table")
  # A latin-1 file is decoded by every reader (see .is_latin1()), and its
  # text handed on as UTF-8.
  latin1 <- .is_latin1(path)
  decode <- if (latin1) .as_utf8 else identity
  # fread() decompresses gzip and bzip2 only with R.utils, and parses xz as
  # it stands: it is handed the file decompressed (it reads a zip archive itself).
  input <- path
  # A date-time readr or fread() types is read again as its text (see
  # .datetime_as_text()), so each types it in UTC: in `tz`, readr would turn a
  # time that does not exist there into NA with a warning that names nothing,
  # and fread() takes no zone but UTC and the local one.
  readr_read <- function(...)
    decode(readr::read_delim(
      path, delim = delim, quote = "\"", na = c("", "NA"), trim_ws = FALSE,
      locale = readr::locale(decimal_mark = decimal, grouping_mark = "", tz = "UTC",
                             encoding = if (latin1) "latin1" else "UTF-8"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, ...))
  fread_read <- function(...)
    decode(data.table::fread(input, sep = delim, showProgress = FALSE, blank.lines.skip = TRUE,
                               strip.white = FALSE, na.strings = c("", "NA"),
                               encoding = if (latin1) "Latin-1" else "unknown", ...))
  # read.table() reads a zip archive's first member through a connection. A
  # latin-1 file is re-encoded when read from a path (fileEncoding), but its
  # strings are only marked latin-1 when read through a connection (encoding).
  table_read <- function(...) {
    src <- .first_member(path)
    zip <- !is.character(src)
    decode(utils::read.table(src, header = TRUE, sep = delim, quote = "\"",
                             comment.char = "", check.names = FALSE, stringsAsFactors = FALSE,
                             fileEncoding = if (latin1 && !zip) "latin1" else "",
                             encoding = if (latin1 && zip) "latin1" else "unknown", ...))
  }
  # The columns at positions `pos` read again, as the text in the file.
  read_text <- function(pos) {
    if (use_readr)
      readr_read(col_types = paste(ifelse(seq_along(df) %in% pos, "c", "_"), collapse = ""), ...)
    else if (use_fread) fread_read(select = pos, colClasses = "character", ...)
    else table_read(colClasses = "character", ...)[pos]
  }
  df <- if (use_readr)
    readr_read(guess_max = Inf, ...)
  else if (use_fread) {
    if (.compression(path) %in% c("gzip", "bzip2", "xz")) {
      input <- tempfile()
      on.exit(unlink(input), add = TRUE)
      .decompress(path, input)
    }
    fread_read(dec = decimal, tz = "UTC", ...)
  }
  else table_read(dec = decimal, na.strings = c("", "NA"), ...)
  df <- .lf_breaks(df)
  # fread() skips first lines it takes for a preamble, readr a cut-off last line
  if (nrow(df) != records)
    radiatR_abort_malformed_file(src, "rows", detail = sprintf(paste0(
      "%d row(s) were read of the %d in the file (a first line taken for a ",
      "preamble, or a last line cut off?)."), nrow(df), records))
  df <- .check_names(df, src, unique_names)
  # Kept for the strict coercion of any column the reader left as text.
  attr(df, "decimal") <- decimal
  df <- .datetime_as_text(df, read_text, src)
  text <- attr(df, "datetime_text")
  df <- .keep_labels(df, id, id_text = id_text, read_text = read_text)
  attr(df, "datetime_text") <- text
  # the sniff's cells by column; the sniff split them as the reader did, so
  # their count is the table's (.check_fields() has stopped a ragged file)
  if (!is.null(cells))
    attr(df, "decimal_evidence") <- .sniffed_evidence(cells, names(df), src)
  df
}

# What to open to read the text of `path`: a connection to the first member of
# a zip archive, else the path (file() decompresses gzip, bzip2 and xz itself).
.first_member <- function(path)
  if (.compression(path) == "zip") unz(path, utils::unzip(path, list = TRUE)$Name[1L]) else path

# `df` with its names and text columns as UTF-8.
.as_utf8 <- function(df) {
  names(df) <- enc2utf8(names(df))
  for (j in which(vapply(df, is.character, logical(1)))) df[[j]] <- enc2utf8(df[[j]])
  df
}

# Whether the text of delimited file `path` is latin-1, by the rule of
# .read_text_lines(): a line that is not valid UTF-8 makes the whole file
# latin-1 (as a line break is never part of a UTF-8 character, that is the
# file's bytes not being valid UTF-8). A file of ASCII bytes alone is UTF-8,
# and one that starts with a UTF-16 byte-order mark is left as it is read.
.is_latin1 <- function(path) {
  # gzfile() reads gzip, bzip2, xz and plain files as their bytes
  con <- .first_member(path)
  if (is.character(con)) con <- gzfile(con, "rb") else open(con, "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", n = 1e7)
  if (length(bytes) >= 2L && (identical(bytes[1:2], as.raw(c(0xff, 0xfe))) ||
                              identical(bytes[1:2], as.raw(c(0xfe, 0xff)))))
    return(FALSE)
  while (length(bytes) && !any(bytes > as.raw(0x7f))) bytes <- readBin(con, "raw", n = 1e7)
  if (!length(bytes)) return(FALSE)
  # the rest of the file is checked whole, as a chunk boundary can split a
  # character; a NUL byte, which rawToChar() refuses, is no part of one
  rest <- list(bytes)
  while (length(bytes <- readBin(con, "raw", n = 1e7))) rest[[length(rest) + 1L]] <- bytes
  bytes <- unlist(rest)
  nul <- bytes == as.raw(0)
  if (any(nul)) bytes <- bytes[!nul]
  !validUTF8(rawToChar(bytes))
}

# The column names .read_any() gives `path`, read from its first line that is
# not blank (of the first 20) or, for Excel and Arrow, its schema.
.read_names <- function(path, ext = NULL, delim = NULL, sheet = NULL) {
  ext <- if (!is.null(ext)) tolower(sub("^\\.", "", ext)) else tolower(sub(".*\\.", "", path))
  src <- basename(path)
  if (ext %in% c("xlsx", "xls")) {
    if (!.is_installed("readxl"))
      stop("Reading Excel files requires the 'readxl' package; ",
           "install it with install.packages(\"readxl\").")
    nm <- names(readxl::read_excel(path, sheet = sheet %||% 1L, n_max = 0L,
                                   .name_repair = "minimal"))
  } else if (.is_installed("arrow") && ext == "parquet") {
    nm <- arrow::open_dataset(path)$schema$names
  } else if (.is_installed("arrow") && ext == "feather") {
    nm <- names(arrow::read_feather(path, as_data_frame = FALSE))
  } else {
    delim <- delim %||% .guess_delim(path)$delim
    lines <- readLines(.first_member(path), n = 20L, warn = FALSE, encoding = "UTF-8")
    if (.is_latin1(path)) lines <- iconv(lines, "latin1", "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) return(character(0))
    line <- sub(paste0("^", intToUtf8(0xfeff)), "", lines[1L])
    nm <- unname(unlist(utils::read.table(
      text = line, sep = delim, quote = "\"", header = FALSE, colClasses = "character",
      comment.char = "", check.names = FALSE, na.strings = character(0))[1L, ]))
  }
  names(.check_names(stats::setNames(rep(list(NULL), length(nm)), nm), src))
}

# Count the fields of each record of a delimited file, as .read_any() reads
# it ('"' quotes, no comment character), and stop on any record whose count
# is not the header's. Returns the number of records after the header, which
# the reader must return as rows. A record quoted over several lines is
# counted on its last line; it is reported by the line it starts on. A line
# of spaces is not blank: it has one field.
.check_fields <- function(path, delim, source) {
  # readr and fread() read the first file of a zip archive; count its fields
  # (gzip, bzip2 and xz are decompressed by file() itself)
  n <- utils::count.fields(.first_member(path), sep = delim, quote = "\"", comment.char = "",
                           blank.lines.skip = FALSE)
  run <- 0L
  start <- integer(length(n))
  for (i in seq_along(n)) {
    start[i] <- i - run
    run <- if (is.na(n[i])) run + 1L else 0L
  }
  if (run > 0L)
    radiatR_abort_malformed_file(source, "ragged", lines = start[length(n)], detail = sprintf(
      "a quote opened on line %d is not closed.", start[length(n)]))
  rec <- which(!is.na(n) & n > 0L)
  if (!length(rec)) return(0L)
  h <- rec[1L]
  rows <- rec[-1L]
  bad <- rows[n[rows] != n[h]]
  if (length(bad))
    .abort_ragged(source, start[bad], n[h], every = if (length(bad) == length(rows) &&
                                                          length(unique(n[bad])) == 1L) n[bad[1L]])
  length(rows)
}

# A row of `source` with more or fewer fields than the `n` of its `first`
# line: at file `lines` (first line = 1). `every`: the number of fields every
# data row has, when they all differ from the header alike; one more than the
# header is the layout of a table written with row names, or of rows that end
# in a separator.
.abort_ragged <- function(source, lines, n, every = NULL, first = "the header",
                          caller = "read_tracks") {
  radiatR_abort_malformed_file(
    source, "ragged", lines = lines, caller = caller, detail = if (!is.null(every)) paste0(
      sprintf("every row has %d fields where its header has %d names", every, n),
      if (every == n + 1L) paste0(
        " (a table written with row names, or a separator at the end of every row?). ",
        "Give the first column a name, or remove the last separator"), ".")
    else sprintf("line(s) %s do not have the %d fields of %s.",
                 paste(utils::head(lines, 20L), collapse = ", "), n, first))
}

# Column names are taken as written less surrounding whitespace (readr keeps
# " id" where the other readers trim it), and a name used twice is kept, not
# renamed (readers rename the second differently, or not at all). With
# `unique_names` a name used twice stops the read: which column a role is read
# from would otherwise depend on the reader. A dialect, which knows its
# format's repeated columns, sorts them out itself. Unnamed columns (a
# trailing separator) are not checked.
.check_names <- function(df, source, unique_names = FALSE) {
  names(df) <- trimws(names(df))
  nm <- names(df)[nzchar(names(df))]
  dup <- unique(nm[duplicated(nm)])
  if (unique_names && length(dup))
    radiatR_abort_malformed_file(
      source, "duplicate_columns", columns = dup, detail = sprintf(paste0(
        "column name(s) %s appear more than once. A tracker's export is read with its ",
        "dialect (see list_loader_dialects())."),
        paste0("'", dup, "'", collapse = ", ")))
  df
}

# Names an id column is guessed from, in order of preference.
.id_candidates <- c("id", "track", "trajectory", "animal", "subject")

# The mapped id column `id` and any column an id is guessed from hold labels,
# not measurements. A reader types such a column as numbers or logicals when
# it can, which merges labels that differ as text: "1.1" and "1.10", "1" and
# "1.0", "01" and "1", "T" and "TRUE". Where that happened the column is kept
# as the text in the file; otherwise it is left as the reader typed it. With
# `id_text` the column `id` is text whatever it holds (see
# .combine_track_files()). `read_text(pos)` reads the columns at positions
# `pos` as text; text that does not read as the values the reader typed is
# from other cells, and stops the load rather than be attached to the rows.
.keep_labels <- function(df, id, read_text, id_text = FALSE) {
  # readr keeps the whitespace around a cell, so " 1" is text to it and the
  # number 1 to the other readers. Type such an id-like column as they do,
  # unless that would merge labels (" 1.1" and " 1.10"), which stay text under
  # every reader.
  dec <- attr(df, "decimal") %||% "."
  for (j in which((names(df) %in% id | tolower(names(df)) %in% .id_candidates) &
                  vapply(df, is.character, logical(1)))) {
    if (id_text && names(df)[j] %in% id) next
    v <- df[[j]]
    seen <- !is.na(v)
    if (!any(seen) || !any(v[seen] != trimws(v[seen]))) next
    num <- .parse_number(trimws(v[seen]), dec, dot_ok = !identical(dec, ","))
    if (anyNA(num) || length(unique(num)) != length(unique(v[seen]))) next
    out <- rep(NA_real_, length(v)); out[seen] <- num
    df[[j]] <- out
  }
  is_id <- names(df) %in% id
  # dates and times too: "2024-01-01 00:00" and "2024-01-01 00:00:00" are one
  # instant but two labels
  typed <- vapply(df, function(v) is.numeric(v) || is.logical(v) ||
                    inherits(v, c("Date", "POSIXt", "difftime")), logical(1))
  pos <- unname(which(
    ((is_id | tolower(names(df)) %in% .id_candidates) & typed) |
      (id_text & is_id & !vapply(df, is.character, logical(1)))))
  if (!length(pos) || !nrow(df)) return(df)
  text <- read_text(pos)
  misread <- function(col)
    rlang::abort(sprintf(paste0(
      "read_tracks: the text of column '%s' is not the values read from it; ",
      "the file was not read the same way twice."), col),
      class = "radiatR_label_misread", columns = col, call = NULL)
  if (ncol(text) != length(pos) || nrow(text) != nrow(df)) misread(names(df)[pos[1L]])
  for (i in seq_along(pos)) {
    was <- df[[pos[i]]]
    label <- as.character(text[[i]])
    seen <- !is.na(was)
    if (typed[pos[i]]) {
      # near enough is the same cells; an integer64 loses digits as a double
      num <- suppressWarnings(as.numeric(was[seen]))
      same <- if (is.numeric(was) || is.logical(was)) {
        now <- if (is.logical(was)) as.logical(trimws(label[seen]))
               else .parse_number(trimws(label[seen]), attr(df, "decimal") %||% ".", dot_ok = TRUE)
        all(now == num | abs(now - num) <= 1e-8 * abs(num))
      } else {
        # a date or time: one text is one value wherever it is written
        all(tapply(num, label[seen], function(v) length(unique(v)) == 1L))
      }
      if (!isTRUE(same)) misread(names(df)[pos[i]])
    }
    label[!seen] <- NA_character_
    if ((id_text && is_id[pos[i]]) || length(unique(label)) > length(unique(was)))
      df[[pos[i]]] <- label
  }
  df
}

# The columns `cols` of `d`, which the caller keeps, read as date-times in
# `tz` where they hold them: a Date or POSIXct (from Excel, or a reader's
# dates) and text whose every value is in one date-time layout (see
# .looks_like_time()), whichever reader read it. A delimited file's
# date-times are text here (see .datetime_as_text()), so a column that is
# not all in one layout is left as read under every reader. A value of that
# layout that is no date-time (a time tz skips) stops the load, as in the
# time column.
.parse_kept_times <- function(d, cols, tz, source, decimal = ".") {
  for (cc in cols) {
    v <- d[[cc]]
    if (inherits(v, c("Date", "POSIXct")) || .looks_like_time(v))
      d[[cc]] <- .coerce_time(v, time_type = "posix", tz = tz, column = cc,
                              source = source, decimal = decimal)
  }
  d
}

# `gaps` holds, for each file read, list(source, cols): its name and what
# .read_any() noted about times that do not exist in `tz`. One in a column
# that is `used` stops the load. Files are told apart by position: two of one
# import may share a name.
.abort_tz_gaps <- function(gaps, used, tz) {
  for (g in gaps) for (cc in intersect(names(g$cols), used))
    radiatR_abort_malformed_values(
      cc, g$cols[[cc]]$rows, g$cols[[cc]]$values, source = g$source,
      expected = sprintf("times that exist in time zone '%s'", tz))
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

# What a dialect wants kept about the file it read (attr "source_meta" of its
# result, a named list), as a record that says which file: `source` (its
# name) and `path` (as given to read_tracks()). NULL when the dialect set
# none. read_tracks() keeps the records in a list, one per file, and never
# keyed by file name: files in different folders share names.
.source_meta_record <- function(out, path, dialect) {
  sm <- attr(out, "source_meta")
  if (is.null(sm)) return(NULL)
  if (!is.list(sm) || is.data.frame(sm) || is.null(names(sm)) || !all(nzchar(names(sm))) ||
      anyDuplicated(names(sm)) || any(names(sm) %in% c("source", "path")))
    rlang::abort(sprintf(paste0(
      "read_tracks: dialect '%s' returned a `source_meta` that is not a list with distinct ",
      "names other than `source` and `path`."), dialect),
      class = "radiatR_dialect_error",
      source = if (!is.null(path)) basename(path))
  c(list(source = if (is.null(path)) NA_character_ else basename(path),
         path = if (is.null(path)) NA_character_ else path), sm)
}

# What a dialect reports about the points it masked (see .dialect_position()):
# a data frame with these columns, one row per source file and point.
.attrition_cols <- c("source", "point", "masked", "frames", "frames_no_position",
                     "score_min", "partial")

# `recs` is a list of attrition records, one per file read. Returns them with
# those columns only, so that the records of several files and loads row-bind.
.check_attrition <- function(recs, dialect) {
  ok <- function(a)
    is.data.frame(a) && nrow(a) > 0L && all(.attrition_cols %in% names(a)) &&
      all(vapply(a[c("masked", "frames", "frames_no_position")],
                 function(v) is.numeric(v) && !anyNA(v) && all(v >= 0 & v == round(v)),
                 logical(1))) &&
      length(unique(a$frames)) == 1L && length(unique(a$frames_no_position)) == 1L &&
      is.character(a$point) && !anyNA(a$point) && all(a$partial %in% c("na", "mean"))
  if (!all(vapply(recs, ok, logical(1))))
    rlang::abort(sprintf(paste0(
      "read_tracks: dialect '%s' returned an attrition record that is not a data frame with ",
      "columns %s: one row per point (a name, no NA), counts as whole numbers >= 0, `partial` ",
      "\"na\" or \"mean\", and one `frames` and one `frames_no_position` value."),
      dialect, paste(.attrition_cols, collapse = ", ")),
      class = "radiatR_dialect_error")
  lapply(recs, function(a) as.data.frame(a)[.attrition_cols])
}

# One message for the load when any point was masked. The frame counts are
# per file (repeated on each of its rows), so they are summed over files, not
# over rows; files are not told apart by name, which need not be unique.
.inform_attrition <- function(recs, dialect, on_invalid = "error") {
  masked <- do.call(rbind, lapply(recs, function(a) a[c("point", "masked")]))
  if (is.null(masked) || !sum(masked$masked)) return(invisible())
  by_point <- tapply(masked$masked, factor(masked$point, unique(masked$point)), sum)
  by_point <- by_point[by_point > 0]
  frames <- sum(vapply(recs, function(a) a$frames[1L], numeric(1)))
  lost <- sum(vapply(recs, function(a) a$frames_no_position[1L], numeric(1)))
  score_min <- recs[[1L]]$score_min[1L]
  partial <- recs[[1L]]$partial[1L]
  msg <- sprintf(
    "%s: %d of %d row(s) have no position (%s, partial = \"%s\"). Masked points: %s.",
    dialect, lost, frames,
    if (is.na(score_min)) "no threshold" else paste("threshold", format(score_min)),
    partial, paste(sprintf("%s %d", names(by_point), by_point), collapse = ", "))
  if (lost > 0 && on_invalid == "error")
    msg <- paste0(
      msg, "\n  Pass on_invalid = \"drop\" to leave those rows out",
      # with one point there is nothing left to average (and a dialect that
      # reads one point has no `partial`)
      if (partial == "na" && length(unique(masked$point)) > 1L)
        ", or dialect_args = list(partial = \"mean\") to average the points that remain",
      ".")
  rlang::inform(msg, class = "radiatR_attrition")
}

# The prefix id_collision = "namespace" gives each file of an import: its name
# less extension, or, where two files share that (n1.csv and n1.tsv), its full
# name, or, where two still share it (a/n1.csv and b/n1.csv), the fewest
# trailing parts of its path that tell them apart.
.source_labels <- function(paths) {
  up <- unique(paths)
  parts <- strsplit(gsub("\\\\", "/", up), "/", fixed = TRUE)
  lab <- tools::file_path_sans_ext(basename(up))
  shared <- function(l) l %in% l[duplicated(l)]
  s <- shared(lab); lab[s] <- basename(up[s])
  for (k in seq(2L, max(2L, lengths(parts)))) {
    s <- shared(lab)
    if (!any(s)) break
    lab[s] <- vapply(parts[s], function(p) paste(utils::tail(p, k), collapse = "/"),
                     character(1))
  }
  stopifnot("two source paths normalise to one namespace label" = !anyDuplicated(lab))
  stats::setNames(lab, up)
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
                                 dialect = NULL, keep = NULL, drop = NULL) {
  read_opts <- read_opts %||% list()
  read <- function(p, id, id_text = FALSE)
    .read_any(p, ext = read_opts$ext, delim = read_opts$delim,
              decimal = read_opts$decimal, sheet = read_opts$sheet,
              tz = time_args$tz %||% "UTC", id = id, id_text = id_text,
              unique_names = TRUE)
  dfl <- lapply(paths, function(p) {
    if (is.null(reader)) return(read(p, mapping$id))
    out <- tryCatch(reader(p), error = function(e) {
      if (any(startsWith(class(e), "radiatR_"))) rlang::cnd_signal(e)
      rlang::abort(sprintf("read_tracks: dialect '%s' failed on file '%s'.",
                           dialect, basename(p)),
                   class = "radiatR_dialect_error", parent = e, source = basename(p))
    })
    .assert_dialect_table(out, dialect, basename(p))
  })

  first_names <- names(dfl[[1L]])
  real_id <- mapping$id %||% .guess_col(first_names, .id_candidates)
  synth <- is.null(real_id)

  if (synth && !isTRUE(id_from_filename)) {
    stop("read_tracks: no id column was found in the first file and ",
         "id_from_filename = FALSE, so no per-file trajectory id can be ",
         "assigned. Supply mapping = list(id = <column>) or set ",
         "id_from_filename = TRUE.", call. = FALSE)
  }

  id_col <- if (synth) (mapping$id %||% "..id") else real_id

  # Ids are labels: where the files this function read do not agree on the
  # type of the id column (numbers in one, text in another), or two files hold
  # one typed value (written 1.1 in one and 1.10 in the other, or alike), a
  # file that was not read as text is read again for the ids as they are
  # written in it. A table that cannot be read again (Parquet, Feather, a
  # dialect's) where the types still disagree stops the load: its ids written
  # as R prints them (1e+05) would not match the same ids written in a file.
  id_type <- function(dfl) vapply(dfl, function(d) {
    v <- d[[id_col]]
    if (!id_col %in% names(d)) NA_character_
    else if (is.character(v) || is.factor(v)) "text"
    # fread() reads a large whole number as integer64, which does not bind
    # with the doubles of another reader: read both again as text
    else if (inherits(v, "integer64")) "integer64"
    else if (is.numeric(v)) "number"
    else class(v)[1L]
  }, character(1))
  types <- id_type(dfl)
  if (is.null(reader)) {
    again <- which(!is.na(types) & types != "text")
    shared <- anyDuplicated(unlist(lapply(dfl[again], function(d)
      unique(as.character(d[[id_col]][!is.na(d[[id_col]])])))))
    if (length(unique(stats::na.omit(types))) > 1L || shared)
      dfl[again] <- lapply(paths[again], read, id = id_col, id_text = TRUE)
    types <- id_type(dfl)
  }
  if (length(unique(stats::na.omit(types))) > 1L)
    radiatR_abort_id_type_mismatch(id_col, basename(paths), types, dialect)
  tz_gaps <- Map(function(d, p) list(source = basename(p), cols = attr(d, "tz_gaps")),
                 dfl, paths)

  # The numeric role columns and text or Date time are coerced file by file,
  # so a malformed cell is reported against its own file and row rather than
  # as a type clash on bind, and a Date is its day's first instant in tz (cast
  # on bind it would be read some other way). Other typed time is left for
  # read_tracks(), which needs to see whether it was a duration.
  roles <- guess_columns(dfl[[1L]], mapping)
  num_cols <- unlist(roles[c("x", "y", "angle", "weight")], use.names = FALSE)
  role_cols <- unlist(roles, use.names = FALSE)
  kept <- setdiff(keep, c(drop, role_cols))
  used <- c(role_cols, kept, "..source_file", "..source_path")

  dfl <- Map(function(d, p) {
    .check_decimal_evidence(attr(d, "decimal_evidence"), c(num_cols, roles$time),
                            basename(p), dialect)
    dec <- attr(d, "decimal") %||% "."
    for (cc in intersect(num_cols, names(d)))
      d[[cc]] <- .as_numeric_strict(d[[cc]], cc, source = basename(p),
                                    dialect = dialect, decimal = dec)
    tc <- roles$time
    if (!is.null(tc) && (is.character(d[[tc]]) || is.factor(d[[tc]]) ||
                         inherits(d[[tc]], "Date")))
      d[[tc]] <- do.call(.coerce_time, c(
        list(d[[tc]], column = tc, source = basename(p), dialect = dialect,
             decimal = dec), time_args))
    d <- .parse_kept_times(d, intersect(kept, names(d)), time_args$tz %||% "UTC",
                           basename(p), dec)
    # A date-time column that is not used is dropped with the others after
    # binding; its text beside another file's typed dates would stop the bind.
    if (is.null(reader))
      for (cc in setdiff(attr(d, "datetime_text"), used)) d[[cc]] <- NULL
    if (synth) d[[id_col]] <- tools::file_path_sans_ext(basename(p))
    d[["..source_file"]] <- basename(p)
    # Internal only: full/relative path, used to distinguish same-named files
    # in different directories for collision detection. Dropped before the
    # final Tracks data is built (not in keep_cols selection below).
    d[["..source_path"]] <- p
    attr(d, "decimal_evidence") <- NULL
    d
  }, dfl, paths)

  # A dialect says what unit its time column is in; the files must agree.
  units <- unique(lapply(dfl, attr, "time_unit"))
  if (length(units) > 1L)
    rlang::abort(sprintf(
      "read_tracks: the files do not share a time unit under dialect '%s' (%s).", dialect,
      paste(vapply(units, function(u) u %||% "unknown", character(1)), collapse = ", ")),
      class = "radiatR_time_type_mismatch")

  # An unused column that holds dates or date-times in one file and values of
  # another type in another (an Excel date-time beside the text of a file
  # whose reader typed none, a Date beside a POSIXct) would stop the bind too:
  # it is dropped from every file.
  if (is.null(reader))
    for (cc in setdiff(unique(unlist(lapply(dfl, names))), used)) {
      cls <- Filter(Negate(is.null), lapply(dfl, function(d) if (cc %in% names(d)) class(d[[cc]])))
      dated <- vapply(cls, function(k) any(k %in% c("Date", "POSIXct")), logical(1))
      if (any(dated) && length(unique(cls)) > 1L)
        dfl <- lapply(dfl, function(d) { d[[cc]] <- NULL; d })
    }

  attrition <- Filter(Negate(is.null), lapply(dfl, attr, "attrition"))
  source_meta <- Filter(Negate(is.null),
                        Map(.source_meta_record, dfl, paths, MoreArgs = list(dialect = dialect)))
  df <- vctrs::vec_rbind(!!!dfl)
  list(df = df, id_col = id_col, synth = synth, time_unit = units[[1L]],
       attrition = attrition, source_meta = unname(source_meta), tz_gaps = tz_gaps)
}

# ---- core: read_tracks ------------------------------------------------------
#' Construct a Tracks from a *format* spec (registered name or inline list)
#' @param x data.frame or path(s)
#' @param format registered format name or list spec
#' @param ... extra args override spec fields
#' @return A `Tracks` object, as returned by [read_tracks()].
#' @examples
#' raw <- data.frame(animal = "m1", seconds = c(0, 0.5, 1),
#'                   pos_x = c(0.1, 0.2, 0.3), pos_y = c(0, 0.05, 0.1))
#' # An inline spec: columns found by pattern, time in seconds
#' spec <- list(
#'   regex_mapping = list(id = "^animal", time = "^sec",
#'                        x = "^pos_?x", y = "^pos_?y"),
#'   time = list(type = "seconds")
#' )
#' read_tracks_format(raw, spec)
#'
#' # The same spec registered under a name
#' register_loader_format("my_tracker", spec, overwrite = TRUE)
#' read_tracks_format(raw, "my_tracker")
#' @export
read_tracks_format <- function(x, format, ...)
  .read_tracks_format(x, format, list(...))

# `dots` are the caller's overrides of the format. `angle_default` is the angle
# unit of a format that sets none: guessed here, but read_tracks(format = )
# keeps its own default.
.read_tracks_format <- function(x, format, dots, angle_default = "auto") {
  spec <- if (is.character(format) && exists(format, envir = .loader_format_registry, inherits = FALSE)) {
    get(format, envir = .loader_format_registry, inherits = FALSE)
  } else if (is.list(format)) {
    format
  } else stop("format must be a registered name or a list spec")

  # A NULL argument is one the caller did not set: it does not delete the
  # format's setting.
  dots <- .drop_null(dots)

  # Extract/merge fields with defaults; allow `dots` to override
  # The caller's mapping goes in first, so that a role it names is not looked
  # up by pattern. A NULL role is no mapping: it does not delete the format's.
  mapping <- modifyList(spec$mapping %||% list(), .drop_null(dots$mapping %||% list()))
  dots$mapping <- NULL

  # regex mapping (resolve against first file or df headers)
  if (!is.null(spec$regex_mapping)) {
    nm <- NULL
    if (is.data.frame(x)) nm <- names(x)
    else if (is.character(x) && length(x) >= 1L && file.exists(x[1])) {
      ro <- dots[["read_opts"]] %||% list()
      nm <- .read_names(x[1], ext = ro[["ext"]], delim = ro[["delim"]], sheet = ro[["sheet"]])
    }
    if (!is.null(nm)) {
      # A pattern that matches nothing is an error: left unset, the role would
      # be guessed from the default column names instead.
      pick <- function(role) {
        pats <- unlist(spec$regex_mapping[[role]])
        if (!is.null(mapping[[role]]) || !length(pats)) return(mapping[[role]])
        hit <- unlist(lapply(pats, grep, x = nm, value = TRUE, ignore.case = TRUE))
        if (!length(hit))
          rlang::abort(sprintf(
            "read_tracks_format: regex_mapping matched no column: %s = '%s'. Available: %s.",
            role, paste(pats, collapse = "', '"), paste(nm, collapse = ", ")),
            class = "radiatR_missing_columns", columns = pats)
        hit[1L]
      }
      roles <- c("id", "time", "x", "y", "angle", "weight")
      mapping <- modifyList(mapping, sapply(roles, pick, simplify = FALSE))
    }
  }

  # Optional pre-processing via the format's dialect, unless the caller names
  # another (the format's dialect_args are then not its to take). `[[`: `$`
  # would take the caller's `dialect_args` for a `dialect`.
  if (is.null(dots[["dialect"]]) && !is.null(spec[["dialect"]])) {
    dots[["dialect"]] <- spec[["dialect"]]
    dots$dialect_args <- modifyList(spec$dialect_args %||% list(),
                                    dots$dialect_args %||% list())
  }

  args <- list(
    mapping        = mapping,
    angle_unit     = spec$angle$unit %||% spec$angle_unit %||% angle_default,
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
#' @param time_type one of "auto","posix","seconds","frames". Numeric time is
#'   taken to be a frame index unless `time_type = "seconds"` (or the column is
#'   a duration): seconds are recorded as such in the Tracks metadata
#'   (`time_unit`), so [elapsed_seconds()] and the kinematics use them directly,
#'   with no frame rate.
#' @param tz Time zone of timestamps that carry none (default "UTC"): in the
#'   time column and the kept columns (see `keep`) of a file, in an Excel
#'   date-time cell, and in a text time column of a data frame. A dialect is
#'   not given `tz`: it applies to the dates and text date-times of the table
#'   the dialect returns. A bare date is the first instant of that day in `tz`
#'   (midnight, or the end of a DST change that skips midnight; so is an Excel
#'   date-time cell at such a midnight). A wall-clock time that does not exist
#'   in `tz` (the hour skipped when clocks go forward, or a day skipped
#'   altogether) is an error. A timestamp that ends in a zone designator (`Z`,
#'   `+02:00`, `-0130`, `+02`) is that instant whatever `tz` is, and a
#'   date-time column of a data frame keeps its instants.
#' @section Time units from a dialect:
#' A dialect that knows the unit of its time column says so, and under the
#' default `time_type = "auto"` that decides how the column is read: TrackMate
#' `POSITION_T` (when the version 7 units row gives seconds), ToxTrac
#' `Time (sec)`, idtracker.ai `seconds` and TRex `time` are seconds, and need
#' no `fps`; `FRAME`/`frame` columns, the DeepLabCut frame index and SLEAP
#' `frame_idx` are frames. `ethovision` (`Trial time`, or `Recording time`
#' through `time_col`) and `anymaze` (`Time`, a duration) declare seconds; an
#' `ethovision` data frame's time is taken as seconds (EthoVision writes no
#' other unit), and a file's units row is checked. A contrary explicit
#' `time_type` stops with a `radiatR_time_type_mismatch` error, as do files of
#' one import that disagree.
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
#' @param read_opts list of file-reading overrides: `delim` (field separator)
#'   and `decimal` (decimal mark) for a text file, `sheet` (Excel worksheet
#'   name or number) for an Excel file, and `ext` (file type, for a path
#'   without its extension). Any `NULL` element is auto-detected. Defaults to all-auto. With a
#'   `dialect`, the non-`NULL` entries are handed to the dialect: every
#'   built-in takes `decimal`; `trackmate`, `trex`, `sleap` and
#'   `idtrackerai_wide` also take `delim` and `ext` for a text file and `sheet`
#'   for an Excel file (`decimal` and `delim` are refused for Excel, `sheet`
#'   for a text file); `ethovision` and
#'   `anymaze` take `delim` (for EthoVision it must be the export's own
#'   separator). An entry a dialect cannot use stops with a
#'   `radiatR_invalid_argument` error, as does any entry for a registered
#'   dialect whose function has no `read_opts` argument. Without a dialect,
#'   an entry that does not fit the file's type (`delim` or `decimal` for an
#'   Excel or Arrow file, `sheet` for a text or Arrow file) stops the same
#'   way. A data frame `x` takes only `decimal`, the mark of any numeric
#'   column it holds as text.
#' @section Masked points:
#' The `deeplabcut`, `sleap`, `ethovision` and `anymaze` dialects take each
#' position from one or more points (bodyparts, nodes, body points; ANY-maze's
#' centre point alone). EthoVision writes `-` and ANY-maze an empty cell for a
#' sample with no position; neither has scores, thresholds, or for `anymaze` a
#' `partial` argument. A point is masked in a frame when its
#' coordinates are missing or not finite, when its score (likelihood) is 0, or when a
#' threshold is given (`dialect_args = list(likelihood_min = )` for
#' DeepLabCut, `score_min` for SLEAP) and its score is missing or below it.
#' A frame with a masked point has no position, unless
#' `dialect_args = list(partial = "mean")` asks for the score-weighted mean of
#' the points that remain. The dialect never removes a row: frames without a
#' position meet `on_invalid` like any other missing coordinate.
#'
#' What was masked is reported in one message (class `radiatR_attrition`) and
#' kept in the `attrition` element of the Tracks metadata: a data frame with
#' one row per source file and point, giving the rows in which the point was
#' masked (`masked`), the rows read from that file (`frames`), the rows left
#' without a position (`frames_no_position`), and the threshold and `partial`
#' setting used. `c()` row-binds these records; a subset keeps the whole
#' record of its load, so recombining subsets of one load repeats it.
#'
#' @param dialect optional registered dialect name. The dialect reads `x`
#'   itself (`read_tracks()` does not parse the file first) and is called once
#'   per file when `x` holds several paths. See [register_loader_dialect()].
#' @param dialect_args named list of extra arguments forwarded to the dialect
#'   function (e.g. \code{list(bodypart = c("head","thorax"))})
#' @param mutate list of transformations applied after reading (named functions or formulas)
#' @param keep extra columns to retain besides the mapped ones and those a
#'   dialect produces (NULL = none). All other columns are dropped; a name
#'   that is not in the data is ignored without a message. A kept column of a
#'   file is read as date-times by content, under every reader: when its
#'   reader typed it as dates or date-times, or every value that is not
#'   missing is a date-time in one layout, it is parsed in `tz` as the time
#'   column is (a value that is no instant there stops the load); otherwise it
#'   stays as read.
#' @param drop drop these columns after mapping
#' @param id_from_filename if TRUE and id missing, derive id from file stem when reading multiple files
#' @param validate Deprecated and ignored: `Tracks` invariants are always
#'   checked. Any value but `TRUE` warns.
#' @param id_collision how to handle a trajectory id that appears under more
#'   than one source file during a multi-file import: `"error"` (default) stops
#'   with a message naming the colliding id(s) and files; `"namespace"` rewrites
#'   every id as `<file>::<id>`, where `<file>` is the file name less extension,
#'   or the full file name where two files share that, or the trailing part of
#'   the path that tells them apart (`a/n1.csv`). Has no effect on single-file
#'   or data.frame input.
#' @param on_invalid How to handle rows whose x/y coordinates are missing or
#'   non-finite: "error" (default) aborts with a radiatR_invalid_rows condition
#'   listing the affected rows and ids; "drop" removes them and signals a
#'   radiatR_dropped_rows warning with the same payload. It covers nothing
#'   else: a missing time, id or weight is an error either way, and so is a
#'   malformed value (see "Malformed values").
#' @section Ids:
#' An id is a label, not a measurement. In a delimited file the id column
#' (the one named in `mapping`, or any column named `id`, `track`,
#' `trajectory`, `animal` or `subject`) is kept as the text in the file
#' wherever reading it as numbers would merge ids that are written
#' differently: `1.1` and `1.10`, `1` and `1.0`, `01` and `1`, or
#' `2024-01-01 00:00` and `2024-01-01 00:00:00`. Ids that read as distinct
#' numbers (or dates) stay numbers. Where the delimited files of one import do
#' not agree on the type of the id column, or two of them hold one number
#' (written `1.1` in one and `1.10` in the other, or alike), the ids of each
#' file are the text in that file (for an Excel file, the text of its cells);
#' two files that write one id alike collide (see `id_collision`). Where the
#' types still disagree in tables that cannot be read again for their text
#' (a dialect's tables, Parquet or Feather files), the load stops with a
#' `radiatR_id_type_mismatch` error. A data frame and an Excel or Arrow file
#' read alone keep the types they come with. Ids that differ only
#' by surrounding whitespace (`" 1"` and `"1"`) stop the load with a
#' `radiatR_ambiguous_ids` error, as readers disagree on whether they are one
#' id. An `anymaze` file is read as text, so the column its `id_col` names
#' keeps its text. For `ethovision`, `id_col` applies to a data frame only (a
#' file is one track, id `"1"`). Not covered: an id column that another
#' dialect is pointed to by an argument of its own (its ids may still be
#' merged).
#'
#' @section Reading delimited files:
#' Whichever reader is installed (readr, then data.table, then base R), a
#' delimited file is read as one table: `"` is the only quote character,
#' nothing is a comment (`#` and `'` are text), blank lines are skipped, an
#' empty cell is missing, a cell keeps its surrounding whitespace and column
#' names lose theirs. The fields of each line are counted first: a row with
#' more or fewer fields than the header (a last line cut off, an unclosed
#' quote, a table written with row names or with a separator at the end of
#' every row) stops the load with a `radiatR_malformed_file` error naming the
#' lines (`problem = "ragged"`), as does a reader returning fewer rows than
#' the file holds (`"rows"`) and a column name used twice
#' (`"duplicate_columns"`; a dialect handles its format's repeated columns
#' itself), and a delimited file that reads as a single column
#' (`"one_column"`).
#'
#' @section Malformed values:
#' A cell in an x, y, angle, weight or time column that is present but does not
#' parse (`"oops"`, `"1,5"`, a stray header or units row) stops the load with a
#' `radiatR_malformed_values` error, a subclass of `radiatR_invalid_rows`. Its
#' `columns`, `rows`, `values`, `source` and `dialect` fields name the column,
#' the data rows (counted from the first row after the header, within its own
#' file in a multi-file import), the offending strings, the file and the
#' dialect, `decimal` (the mark the file was read with) and `suggested_decimal`
#' (the other mark when the values look written with it, else `NULL`). Such a
#' value is never dropped or turned into `NA`. Blank cells and
#' `NA` are missing values; `nan` and `inf` are read as such. Files are read
#' with no digit-grouping mark, so `1,5` is malformed unless
#' `read_opts = list(decimal = ",")` says the comma is the decimal mark. Where
#' it is, a value holding a `.` is malformed in turn, since `1.234` could as
#' well be 1234. A semicolon-separated file is taken to use decimal commas
#' unless its first lines show decimal points and no decimal comma (`1.5` or
#' `0.125`, which cannot be digit grouping, or the fraction of a duration such
#' as `0:00:00.320`);
#' `read_opts = list(decimal = ".")` says so outright.
#' A point found that way must be shown by a column read for a role (x, y,
#' time, angle, weight): where such a column holds only values that could be
#' digit grouping (`523.117`, `1.234`) and the clear decimal points are all in
#' other columns (a label `1.5`), the load stops with a
#' `radiatR_ambiguous_decimal` error (fields `columns`, `values`, `evidence`,
#' `decimal`, `source`, `dialect`) rather than read `1.234` for what may be 1234.
#' So does a file where no data row of the first 20 lines is whole (a quoted
#' field runs past them), as the point cannot then be checked. Give the
#' mark with `read_opts`. The same holds for the built-in dialects that sniff the
#' mark (ANY-maze, the others read as tables, and EthoVision when not
#' comma-separated). Other tab- and comma-separated files are read with a point.
#'
#' Every built-in dialect checks
#' its source columns the same way, before any reshaping, so the error names
#' the header as the program wrote it and the row of the file's table. A
#' missing or non-finite position found afterwards (see `on_invalid`) is still
#' counted in the rows of the table the dialect returns, which for a wide
#' format is the reshaped one. ANY-maze writes time as a duration
#' (`0:01:02.500`), read as seconds; a cell that is not one is malformed.
#'
#' A time column given as text must hold either numbers throughout or
#' timestamps in one layout throughout: `YYYY-MM-DD HH:MM:SS`,
#' `YYYY-MM-DD HH:MM` or `YYYY-MM-DD` (with `-` or `/`), or ISO 8601
#' `YYYY-MM-DDTHH:MM:SS` with an optional `Z` for UTC. A bare date beside
#' date-times is midnight.
#' @param format Optional loader format name or list spec registered via [register_loader_format()]
#' @return Tracks
#' @examples
#' df <- data.frame(
#'   id   = rep(c("A", "B"), each = 4),
#'   time = rep(0:3, times = 2),
#'   x    = c(1, 1.2, 1.4, 1.5, -0.2, -0.1, 0, 0.15),
#'   y    = c(0, 0.1, 0.3, 0.4, 1.0, 1.1, 1.3, 1.5)
#' )
#' read_tracks(df, mapping = list(id = "id", time = "time", x = "x", y = "y"))
#'
#' # A DeepLabCut predictions file, recorded at 40 frames per second
#' dlc <- system.file("extdata", "dlc_single_wasp_predictions.csv",
#'                    package = "radiatR")
#' read_tracks(dlc, dialect = "deeplabcut", fps = 40)
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
    # Only what the caller supplied: a default passed on would override the
    # format's own setting (its angle unit, frame rate, time zone, ...).
    supplied <- setdiff(names(match.call())[-1L], c("x", "format"))
    return(.read_tracks_format(x, format, mget(supplied), angle_default = "radians"))
  }

  read_opts <- .check_read_opts(read_opts, "read_tracks")

  if (!isTRUE(validate))
    .Deprecated(msg = paste("`validate` is deprecated and has no effect:",
                            "Tracks invariants are always checked."))

  # A dialect is the only reader of its input: read_tracks() does not parse
  # the file itself first, so a loader for a layout the generic reader cannot
  # make sense of (headerless, several header rows, not a table) still works.
  prep <- NULL
  if (!is.null(dialect)) {
    if (!exists(dialect, envir = .loader_registry, inherits = FALSE)) {
      if (dialect %in% names(.replaced_dialects))
        rlang::abort(sprintf(
          "The '%s' dialect was removed in radiatR 0.1.3: dialect = \"%s\" reads those files.",
          dialect, .replaced_dialects[[dialect]]),
          class = "radiatR_removed_dialect", dialect = dialect)
      if (dialect %in% .removed_dialects)
        rlang::abort(sprintf(paste0(
          "The '%s' dialect was removed in radiatR 0.1.3. Register your own ",
          "loader with register_loader_dialect(\"%s\", <function>); see ",
          "vignette(\"loaders\")."), dialect, dialect),
          class = "radiatR_removed_dialect", dialect = dialect)
      stop("Unknown dialect '", dialect, "'")
    }
    fun <- get(dialect, envir = .loader_registry, inherits = FALSE)
    opts <- read_opts            # already checked, NULLs dropped
    if (length(opts)) {
      if (!.takes_read_opts(dialect))
        rlang::abort(sprintf(paste0(
          "read_tracks: the '%s' dialect takes no read_opts: it reads its own file. Give ",
          "its function a read_opts argument to accept them (see register_loader_dialect())."),
          dialect), class = "radiatR_invalid_argument", arg = "read_opts")
      if (!is.null(dialect_args[["read_opts"]]))
        rlang::abort("read_tracks: read_opts is given both to read_tracks() and in dialect_args; give it once.",
                     class = "radiatR_invalid_argument", arg = "read_opts")
    }
    # Called on a data frame or on one path. The table keeps the decimal mark
    # of its file (or the one read_opts gave), for the strict coercion of any
    # column left as text.
    prep <- function(input) {
      out <- do.call(fun, c(list(input), dialect_args,
                            if (length(opts)) list(read_opts = opts)))
      if (is.data.frame(out) && is.null(attr(out, "decimal")) &&
          (is.character(input) || !is.null(opts[["decimal"]])))
        attr(out, "decimal") <- opts[["decimal"]] %||% .file_decimal(input)
      out
    }
    # Dialects no longer convert frame indices to seconds themselves (that
    # single conversion now happens once, on demand, via elapsed_seconds());
    # adopt a dialect-only fps so it still becomes the Tracks frame_rate.
    if (is.null(fps) && !is.null(dialect_args$fps)) fps <- dialect_args$fps
  }

  # read
  dec <- "."
  evidence <- NULL
  tz_gaps <- list()
  if (is.character(x) && any(!file.exists(x)))
    stop("read_tracks: file(s) not found: ",
         paste(x[!file.exists(x)], collapse = ", "), call. = FALSE)
  if (is.character(x) && is.null(prep))
    for (p in x) .refuse_misfit_read_opts(read_opts, p, "read_tracks")
  if (is.data.frame(x)) {
    # a data frame was read already: only its decimal mark can be given
    extra <- setdiff(names(read_opts), "decimal")
    if (is.null(prep) && length(extra))
      .refuse_read_opt("read_tracks", extra[1L], "the input is a data frame, not a file",
                       refused_by = NULL)
    df <- if (is.null(prep)) x else .assert_dialect_table(prep(x), dialect)
    dec <- read_opts$decimal %||% attr(df, "decimal") %||% "."
    src <- NULL
  } else if (is.character(x) && length(x) == 1L) {
    df <- if (!is.null(prep)) .assert_dialect_table(prep(x), dialect, basename(x))
          else .read_any(x, ext = read_opts$ext, delim = read_opts$delim,
                         decimal = read_opts$decimal, sheet = read_opts$sheet, tz = tz,
                         id = mapping$id, unique_names = TRUE)
    dec <- attr(df, "decimal") %||% "."
    evidence <- attr(df, "decimal_evidence")
    src <- basename(x)
    tz_gaps <- list(list(source = src, cols = attr(df, "tz_gaps")))
  } else if (is.character(x) && length(x) > 1L) {
    combined <- .combine_track_files(x, mapping, id_from_filename, read_opts,
                                     list(time_type = time_type, tz = tz, fps = fps),
                                     reader = prep, dialect = dialect, keep = keep, drop = drop)
    df <- combined$df
    # Force the synthesized id column to be picked up by guess_columns() below,
    # so multi-file input never falls into the single-track "(multiple)" path.
    if (isTRUE(combined$synth)) mapping$id <- combined$id_col
    src <- "(multiple)"
    tz_gaps <- combined$tz_gaps
  } else {
    stop("read_tracks: 'x' must be a data.frame or path(s) to files")
  }
  # A dialect that knows the unit of its time column says so. Under the default
  # time_type that settles how the column is read; a contrary explicit
  # time_type is refused rather than obeyed (seconds divided by fps, or frames
  # passed off as seconds, are both silently wrong).
  declared <- if (is.null(dialect)) NULL
              else if (length(x) > 1L && is.character(x)) combined$time_unit
              else attr(df, "time_unit")
  # What the dialect reports about the points it masked: one record per file.
  attrition <- .check_attrition(
    if (is.null(dialect)) list()
    else if (length(x) > 1L && is.character(x)) combined$attrition
    else Filter(Negate(is.null), list(attr(df, "attrition"))), dialect)
  .inform_attrition(attrition, dialect, on_invalid)
  # What the dialect wants kept about each file: one record per file.
  source_meta <- if (is.null(dialect)) list()
                 else if (length(x) > 1L && is.character(x)) combined$source_meta
                 else Filter(Negate(is.null), list(
                   .source_meta_record(df, if (is.character(x)) x, dialect)))
  # The reader's notes have been read; they are not part of the Tracks data.
  attr(df, "decimal") <- attr(df, "decimal_evidence") <- attr(df, "time_unit") <-
    attr(df, "attrition") <-
    attr(df, "source_meta") <- attr(df, "tz_gaps") <- NULL
  if (!is.null(declared)) {
    if (!(identical(declared, "seconds") || identical(declared, "frames")))
      rlang::abort(sprintf(
        "read_tracks: dialect '%s' declared the time unit %s; it must be \"seconds\" or \"frames\".",
        dialect, paste(deparse(declared), collapse = " ")), class = "radiatR_time_type_mismatch")
    if (!time_type %in% c("auto", declared))
      rlang::abort(sprintf(
        "read_tracks: dialect '%s' reads this time column as %s; time_type = \"%s\" does not apply.",
        dialect, declared, time_type), class = "radiatR_time_type_mismatch")
    if (time_type == "auto" && declared == "seconds") {
      time_type <- "seconds"
      if (!is.null(fps))
        message("Time is in seconds (dialect '", dialect, "'): `fps` is not needed and is not used.")
    }
    if (time_type == "auto" && declared == "frames" && !is.null(fps)) time_type <- "frames"
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
  # A sniffed decimal point must be shown by a column read for a role.
  .check_decimal_evidence(evidence, c(time, angle, xcol, ycol, wcol), src, dialect)
  .abort_tz_gaps(tz_gaps, setdiff(c(roles, keep), drop), tz)
  # A kept column of date-times is read in tz, as the time column is (the
  # files of a multi-file import were so read one by one). The reader's note
  # of the date-times it typed has then been read.
  if (is.character(x) && length(x) == 1L)
    df <- .parse_kept_times(df, intersect(setdiff(keep, c(drop, roles)), names(df)),
                            tz, src, dec)
  attr(df, "datetime_text") <- NULL
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

  # " 1" and "1" are one id to some readers and two to others: take neither.
  if (!is.null(id) && (is.character(df[[id]]) || is.factor(df[[id]]))) {
    lab <- unique(as.character(df[[id]]))
    lab <- lab[!is.na(lab)]
    bare <- trimws(lab)
    amb <- bare %in% bare[duplicated(bare)]
    if (any(amb)) radiatR_abort_ambiguous_ids(id, lab[amb])
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
             "\nPass id_collision = \"namespace\" to prefix ids with the file's ",
             "name, or supply a mapping that yields file-unique ids.",
             call. = FALSE)
      } else {
        # "namespace": prefix ALL ids (not just colliding ones) with the file's
        # own label (see .source_labels()) so id "1" means the same thing
        # everywhere it appears.
        df[[id]] <- paste0(unname(.source_labels(path_vals)[path_vals]), "::", id_vals)
      }
    }
  }

  # A delimited file that came out as one column was not split: say so rather
  # than ask for columns it does not have. (A one-column sheet or Arrow file is
  # a real one-column table; the generic error below stands for it.)
  user_cols <- setdiff(names(df), c("..source_file", "..source_path"))
  one_ext <- tolower(read_opts$ext %||% tools::file_ext(if (is.character(x)) x[1L] else ""))
  one_ext <- sub("^\\.", "", one_ext)
  if (is.null(angle) && is.null(dialect) && is.character(x) && length(user_cols) == 1L &&
      !one_ext %in% c("xlsx", "xls", "parquet", "feather"))
    radiatR_abort_malformed_file(
      basename(x[1L]), "one_column", columns = user_cols, detail = sprintf(paste0(
        "read as a single column ('%s'). If it has more columns, %s or read a ",
        "tracker's export with its dialect (see list_loader_dialects()); ",
        "otherwise map the column."),
        user_cols, if (is.null(read_opts$delim)) "name its separator with read_opts = list(delim = )," else "check the separator you named,"))

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
  # Numeric time that is already seconds says so, or elapsed_seconds() would
  # take it for a frame index and ask for (then divide by) a frame rate.
  if (is.numeric(df[[time]]) && (time_type == "seconds" || time_is_duration))
    meta$time_unit <- "seconds"
  if (length(attrition)) meta$attrition <- do.call(rbind, attrition)
  if (length(source_meta)) meta$source_meta <- source_meta
  tracks(df,
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
#' @examples
#' df <- data.frame(
#'   id   = rep(c("A", "B"), each = 4),
#'   time = rep(0:3, times = 2),
#'   x    = c(1, 1.2, 1.4, 1.5, -0.2, -0.1, 0, 0.15),
#'   y    = c(0, 0.1, 0.3, 0.4, 1.0, 1.1, 1.3, 1.5)
#' )
#' csv_dir <- file.path(tempdir(), "csv_tracks")
#' dir.create(csv_dir)
#' write.csv(df, file.path(csv_dir, "day1.csv"), row.names = FALSE)
#' write.csv(transform(df, id = paste0(id, "2")), file.path(csv_dir, "day2.csv"),
#'           row.names = FALSE)
#' read_tracks_dir(csv_dir, pattern = "\\.csv$")
#' unlink(csv_dir, recursive = TRUE)
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
#' @examples
#' untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
#'       exdir = tempdir())
#' track_dir <- file.path(tempdir(), "tracks")
#' # dtrack writes headerless, tab-separated frame, x, y (and a flag) columns
#' dtrack_loader <- function(x) {
#'   d <- utils::read.delim(x, header = FALSE, colClasses = "numeric")
#'   data.frame(id = tools::file_path_sans_ext(basename(x)),
#'              frame = d[[1]], x = d[[2]], y = d[[3]])
#' }
#' register_loader_dialect("dtrack", dtrack_loader, overwrite = TRUE)
#' file_tbl <- import_tracks(track_dir)[1:3, ]
#' manifest <- import_info(system.file("extdata", "millipede_trials.csv",
#'                                     package = "radiatR"))
#' manifest <- manifest[manifest$file %in% file_tbl$basename, ]
#' ts <- load_manifest(file_tbl, track_dir, manifest = manifest,
#'                     manifest_cols = c(arc = "arc", type = "type"),
#'                     dialect = "dtrack")
#' ts
#' unique(ts@data[, c("id", "arc", "type")])
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

  # Only what the caller supplied (the defaults are read_tracks()'s own): a
  # default passed on would override a `format` given in `...`.
  reader_args <- mget(intersect(
    names(match.call())[-1L],
    c("mapping", "angle_unit", "time_type", "tz", "fps", "normalize_xy", "dialect",
      "keep", "drop")))

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

# ---- dialect helpers -----------------------------------------------------------
# The file-reading options read_tracks() hands a dialect (see read_opts).
.read_opt_names <- c("decimal", "delim", "sheet", "ext")

# The non-NULL entries of read_opts, checked: an unknown name or a decimal
# mark other than "." / "," stops (`who` names the caller in the message).
.check_read_opts <- function(read_opts, who) {
  bad <- function(..., option = NULL)
    rlang::abort(sprintf(...), class = "radiatR_invalid_argument", arg = "read_opts",
                 option = option)
  if (is.null(read_opts) || !length(read_opts)) return(list())
  if (!is.list(read_opts) || is.null(names(read_opts)) || any(!nzchar(names(read_opts))))
    bad("%s: read_opts must be a named list.", who)
  unknown <- setdiff(names(read_opts), .read_opt_names)
  if (length(unknown))
    bad("%s: read_opts has no entry %s; it takes %s.", who,
        paste0("'", unknown, "'", collapse = ", "), paste(.read_opt_names, collapse = ", "),
        option = unknown[1L])
  if (anyDuplicated(names(read_opts))) {
    dup <- names(read_opts)[duplicated(names(read_opts))][1L]
    bad("%s: read_opts names '%s' more than once.", who, dup, option = dup)
  }
  opts <- read_opts[!vapply(read_opts, is.null, logical(1))]
  dec <- opts[["decimal"]]
  if (!is.null(dec) && !(is.character(dec) && length(dec) == 1L && dec %in% c(".", ",")))
    bad('%s: read_opts$decimal must be "." or ",".', who, option = "decimal")
  opts
}

# A refusal of one read_opts entry: `option` names it, `refused_by` the
# dialect that refused it, if any (the app turns the pair into advice about
# its own controls). `who` names the caller in the message.
.refuse_read_opt <- function(who, option, why, refused_by = who)
  rlang::abort(sprintf("%s: read_opts$%s cannot be used: %s.", who, option, why),
               class = "radiatR_invalid_argument", arg = "read_opts", option = option,
               refused_by = refused_by)

# An entry that does not fit the kind of file at `path` (a sheet for text or
# Arrow, a delimiter or decimal mark for Excel or Arrow) is refused, as
# .read_any() would otherwise ignore it.
.refuse_misfit_read_opts <- function(opts, path, who, refused_by = NULL) {
  ext <- tolower(sub("^\\.", "", opts[["ext"]] %||% sub(".*\\.", "", path)))
  kind <- if (ext %in% c("xlsx", "xls")) "an Excel workbook"
          else if (ext %in% c("parquet", "feather")) "an Arrow file"
          else "a delimited text file"
  misfit <- switch(kind, "an Excel workbook" = c("delim", "decimal"),
                   "a delimited text file" = "sheet", c("delim", "decimal", "sheet"))
  for (o in intersect(names(opts), misfit))
    .refuse_read_opt(who, o, sprintf("'%s' is %s", basename(path), kind), refused_by)
  invisible(NULL)
}

# The read_opts a dialect was given, checked (.check_read_opts()); an entry
# the dialect cannot honour (`honours`), or any but `decimal` for a data
# frame, stops rather than being ignored. The condition's `option` names it.
.dialect_read_opts <- function(read_opts, dialect, honours, file = TRUE) {
  opts <- .check_read_opts(read_opts, dialect)
  ok <- if (file) honours else intersect(honours, "decimal")
  extra <- setdiff(names(opts), ok)
  if (length(extra))
    .refuse_read_opt(dialect, extra[1L],
                     if (!file) "the input is a data frame, not a file"
                     else paste("this dialect reads its own file layout and takes only",
                                paste0("read_opts$", honours, collapse = ", ")))
  opts
}

# Whether read_tracks() can hand `dialect` its read_opts: always when it reads
# the file itself (NULL), else when the dialect's function takes read_opts.
.takes_read_opts <- function(dialect) {
  if (is.null(dialect)) return(TRUE)
  exists(dialect, envir = .loader_registry, inherits = FALSE) &&
    "read_opts" %in% names(formals(get(dialect, envir = .loader_registry, inherits = FALSE)))
}

# What a built-in dialect works on: its table (read from one path with `read`,
# or handed over as a data frame), the file it came from, the decimal mark it
# was read with, and its headers normalised for matching (`names`). The table
# keeps the headers as written, so that errors can name them. `read` gets the
# read_opts its arguments name; a given decimal mark is the table's.
.dialect_input <- function(x, dialect, norm = identity, read = .read_any,
                           read_opts = list(), honours = .read_opt_names) {
  file <- is.character(x) && length(x) == 1L && file.exists(x)
  if (!file && !is.data.frame(x))
    rlang::abort(sprintf("%s: expected a data frame or the path of one file.", dialect),
                 class = "radiatR_dialect_error")
  opts <- .dialect_read_opts(read_opts, dialect, honours, file)
  if (file) {
    .refuse_misfit_read_opts(opts, x, dialect, refused_by = dialect)
    df <- do.call(read, c(list(x), opts[intersect(names(opts), names(formals(read)))]))
    src <- basename(x)
  } else {
    df <- x
    src <- NULL
  }
  # A given mark is not checked against the sniff's evidence. The evidence is
  # held apart from `df`, which a dialect may subset (dropping attributes).
  if (!is.null(opts[["decimal"]])) {
    attr(df, "decimal") <- opts[["decimal"]]
    attr(df, "decimal_evidence") <- NULL
  }
  list(df = df, source = src, dialect = dialect, norm = norm,
       decimal = attr(df, "decimal") %||% ".", names = norm(names(df)),
       evidence = attr(df, "decimal_evidence"))
}

# A column of a dialect table by its normalised name, and its header as written.
.dialect_col <- function(inp, col) inp$df[[match(col, inp$names)]]
.dialect_header <- function(inp, col) names(inp$df)[match(col, inp$names)]

# The column as numbers. A malformed cell aborts, naming the header as written,
# the row of the table as read, the file and the dialect.
.dialect_num <- function(inp, col)
  .as_numeric_strict(.dialect_col(inp, col), .dialect_header(inp, col),
                     inp$source, inp$dialect, inp$decimal)

# A sniffed decimal point must be shown by a column the dialect reads for a
# role (`cols`, normalised names): see .check_decimal_evidence().
.dialect_check_decimal <- function(inp, cols)
  .check_decimal_evidence(inp$evidence, .dialect_header(inp, cols), inp$source, inp$dialect)

# The time column, coerced under the unit the dialect knows it to have
# ("seconds", "frames" or NULL for unknown). No column means row order (frames).
.dialect_time <- function(inp, col, unit = NULL, fps = NULL) {
  if (is.null(col)) return(seq_len(nrow(inp$df)))
  type <- if (identical(unit, "seconds")) "seconds"
          else if (identical(unit, "frames") && !is.null(fps)) "frames" else "auto"
  .coerce_time(.dialect_col(inp, col), time_type = type, fps = fps,
               column = .dialect_header(inp, col), source = inp$source,
               dialect = inp$dialect, decimal = inp$decimal)
}

# A dialect's result, carrying for read_tracks() the unit of its time column,
# from a dialect that masks points its attrition record, and whatever else
# about the file is to be kept with the load (`source_meta`, a named list).
.dialect_output <- function(out, time_unit = NULL, attrition = NULL, source_meta = NULL) {
  attr(out, "time_unit") <- time_unit
  attr(out, "attrition") <- attrition
  attr(out, "source_meta") <- source_meta
  out
}

# A dialect's refusal: sprintf(...), naming the file when there is one (a
# multi-file read passes radiatR_ errors through, so the message must say which).
.dialect_failer <- function(source) function(...) {
  msg <- sprintf(...)
  if (!is.null(source)) msg <- sprintf("%s (file '%s').", sub("\\.$", "", msg), source)
  rlang::abort(msg, class = "radiatR_dialect_error", source = source)
}

# A column the caller named in `arg` must be one of the table's. It is matched
# as the headers are, normalised, and returned normalised.
.dialect_require <- function(inp, col, arg) {
  if (is.null(col)) return(NULL)
  fail <- .dialect_failer(inp$source)
  if (!is.character(col) || length(col) != 1L || is.na(col))
    fail("%s: `%s` must name one column.", inp$dialect, arg)
  key <- inp$norm(col)
  if (!key %in% inp$names)
    fail("%s: `%s = \"%s\"` names no column; the table has %s.",
         inp$dialect, arg, col, paste(names(inp$df), collapse = ", "))
  key
}

# The points of a flat table: the <point> of each <point>_x / <point>_y pair.
.dialect_point_names <- function(nms) {
  of <- function(suffix) sub(suffix, "", grep(suffix, nms, value = TRUE))
  intersect(of("_x$"), of("_y$"))
}

# The points to use: every one found (`sel` NULL), or the ones named, which
# must be distinct and among those found. `what` names a point in the errors.
.dialect_select <- function(sel, found, what, dialect, source = NULL) {
  if (is.null(sel)) return(found)
  fail <- .dialect_failer(source)
  if (!is.character(sel) || !length(sel) || anyNA(sel) || anyDuplicated(sel))
    fail("%s: `bodypart` must name one or more distinct %ss.", dialect, what)
  bad <- setdiff(sel, found)
  if (length(bad))
    fail("%s: %s(s) not found: %s; available: %s.", dialect, what,
         paste(bad, collapse = ", "), paste(found, collapse = ", "))
  sel
}

# A score (likelihood) column as numbers. A score weighs its point, so a
# negative or infinite one is malformed. Scores are not capped at 1: SLEAP's
# exceed it.
.dialect_score <- function(inp, col) {
  s <- .dialect_num(inp, col)
  bad <- which(!is.na(s) & (s < 0 | is.infinite(s)))
  if (length(bad))
    radiatR_abort_malformed_values(.dialect_header(inp, col), bad, s[bad],
                                   expected = "a finite score of 0 or more",
                                   source = inp$source, dialect = inp$dialect)
  s
}

# The x, y and score columns of the selected points as matrices: one row per
# row of the table, one column per point. `x_cols`, `y_cols` and `score_cols`
# are normalised column names, named by point. `score` has a column only for
# the points given one, and is NULL when none is.
.dialect_points <- function(inp, x_cols, y_cols, score_cols = NULL) {
  n <- nrow(inp$df)
  mat <- function(cols, read)
    matrix(unlist(lapply(cols, function(cc) as.numeric(read(inp, cc))), use.names = FALSE),
           nrow = n, ncol = length(cols), dimnames = list(NULL, names(cols)))
  list(x = mat(x_cols, .dialect_num), y = mat(y_cols, .dialect_num),
       score = if (length(score_cols)) mat(score_cols, .dialect_score))
}

# One position per row from its points (`pts`, from .dialect_points()).
# A point is masked in a row when its x or y is missing, when its score is 0
# (DeepLabCut writes x = y = 0, likelihood 0 for a frame with no detection),
# or when `score_min` is given and its score is missing or below it. The
# position is the score-weighted mean of the points; a point without a score
# weighs 1. A row with a masked point has no position (`partial = "na"`) or
# the mean of the points that remain (`partial = "mean"`). Rows are never
# removed. Also returned: `points`, the <point>_x / <point>_y columns with
# masked cells NA (for the bodypart_axis heading rule), and the attrition
# record, one row per point. `score_arg` is the dialect's name for `score_min`,
# for the error messages.
.dialect_position <- function(pts, score_min = NULL, partial = c("na", "mean"),
                              dialect, source = NULL, score_arg = "score_min") {
  partial <- match.arg(partial)
  x <- pts$x; y <- pts$y; score <- pts$score
  points <- colnames(x)
  stopifnot(length(points) > 0L, !anyDuplicated(points))
  if (!is.null(score_min)) {
    if (!(is.numeric(score_min) && length(score_min) == 1L && is.finite(score_min)))
      rlang::abort(sprintf("%s: `%s` must be one finite number.", dialect, score_arg),
                   class = "radiatR_dialect_error", source = source)
    unscored <- setdiff(points, colnames(score))
    if (length(unscored))
      rlang::abort(sprintf("%s: `%s` was given, but there is no score column for: %s.",
                           dialect, score_arg, paste(unscored, collapse = ", ")),
                   class = "radiatR_dialect_error", source = source)
  }
  masked <- !is.finite(x) | !is.finite(y)
  w <- matrix(1, nrow(x), ncol(x), dimnames = dimnames(x))
  for (p in intersect(points, colnames(score))) {
    s <- score[, p]
    masked[, p] <- masked[, p] | (!is.na(s) & s == 0)
    if (!is.null(score_min)) masked[, p] <- masked[, p] | is.na(s) | s < score_min
    w[, p] <- ifelse(is.na(s), 1, s)
  }
  w[masked] <- 0
  x0 <- x; y0 <- y
  x0[masked] <- 0; y0[masked] <- 0
  wsum <- rowSums(w)
  ok <- if (partial == "na") rowSums(masked) == 0L else wsum > 0
  px <- py <- rep(NA_real_, nrow(x))
  px[ok] <- rowSums(x0 * w)[ok] / wsum[ok]
  py[ok] <- rowSums(y0 * w)[ok] / wsum[ok]
  x[masked] <- NA_real_; y[masked] <- NA_real_
  pc <- as.data.frame(cbind(x, y)[, order(rep(seq_along(points), 2L)), drop = FALSE])
  names(pc) <- paste0(rep(points, each = 2L), c("_x", "_y"))
  list(x = px, y = py, points = pc,
       attrition = data.frame(
         source = rep(source %||% NA_character_, length(points)), point = points,
         masked = as.integer(colSums(masked)), frames = nrow(x),
         frames_no_position = sum(!ok), score_min = score_min %||% NA_real_,
         partial = partial, stringsAsFactors = FALSE))
}

# ---- built-in dialects -------------------------------------------------------
# A DeepLabCut predictions CSV as a table of text columns. Its header is three
# rows labelled scorer, bodyparts, coords, or four with an individuals row
# (multi-animal projects); the first column is the frame index. The columns
# are named `frame`, then <bodypart>_<coord>, or <individual>/<bodypart>_<coord>
# under an individuals row. attr(, "dlc") lists the points: individual (NA
# without an individuals row), bodypart, and the names of the x, y and
# likelihood columns (NA where the point has none). Header fields are trimmed
# and unquoted: DeepLabCut can end its header rows in CRLF and its data rows
# in LF, and a re-saved file may quote them.
.read_deeplabcut <- function(path) {
  src <- basename(path)
  fail <- function(...) rlang::abort(sprintf(...), class = "radiatR_dialect_error", source = src)
  hdr <- lapply(strsplit(.read_text_lines(path, n = 4L), ",", fixed = TRUE),
                function(f) gsub("^\"|\"$", "", trimws(f)))
  lab <- tolower(vapply(hdr, function(f) if (length(f)) f[1L] else "", character(1)))
  n_hdr <- if (identical(lab[1:3], c("scorer", "bodyparts", "coords"))) 3L
           else if (identical(lab, c("scorer", "individuals", "bodyparts", "coords"))) 4L
           else fail(paste0("deeplabcut: '%s' is not a DeepLabCut output file. Its first rows are ",
                            "labelled %s; a DeepLabCut file starts with rows labelled scorer, ",
                            "(individuals,) bodyparts, coords. A flat <bodypart>_x table is ",
                            "read as a data frame, not as a file."),
                     src, paste(sprintf("'%s'", substr(lab, 1L, 30L)), collapse = ", "))
  ind <- if (n_hdr == 4L) hdr[[2L]][-1L]
  bp <- hdr[[n_hdr - 1L]][-1L]
  co <- tolower(hdr[[n_hdr]][-1L])
  if (length(bp) != length(co) || (!is.null(ind) && length(ind) != length(bp)))
    fail("deeplabcut: the header rows of '%s' differ in length.", src)
  if (any(co == ""))
    fail(paste0("deeplabcut: '%s' has columns with no coords label, which a predictions file ",
                "never has. Labelled training data (CollectedData_*.csv) is not tracking ",
                "output: read the predictions file DeepLabCut writes when it analyses a video."),
         src)
  other <- setdiff(co, c("x", "y", "likelihood"))
  if (length(other))
    fail("deeplabcut: '%s' has coords other than x, y and likelihood (%s); 3-D output is not read.",
         src, paste(other, collapse = ", "))
  key <- paste0(if (!is.null(ind)) paste0(ind, "/"), bp, "_", co)
  if (anyDuplicated(key))
    fail("deeplabcut: '%s' repeats header columns (%s).", src,
         paste(unique(key[duplicated(key)]), collapse = ", "))
  # Every data row must have the header's fields: read.csv() pads a short row
  # and wraps a long one into a row of its own.
  nf <- utils::count.fields(path, sep = ",", quote = "\"", skip = n_hdr, comment.char = "")
  if (!length(nf)) fail("deeplabcut: '%s' has no data rows.", src)
  ragged <- which(is.na(nf) | nf != length(key) + 1L)
  if (length(ragged))
    fail("deeplabcut: data row %d of '%s' has %s field(s); its header names %d.",
         ragged[1L], src, nf[ragged[1L]], length(key) + 1L)
  d <- utils::read.csv(path, header = FALSE, skip = n_hdr, colClasses = "character",
                       check.names = FALSE, strip.white = TRUE)
  names(d) <- c("frame", key)
  pt <- unique(data.frame(individual = if (is.null(ind)) NA_character_ else ind,
                          bodypart = bp, stringsAsFactors = FALSE))
  col <- function(coord) {
    k <- paste0(if (!is.null(ind)) paste0(pt$individual, "/"), pt$bodypart, "_", coord)
    ifelse(k %in% names(d), k, NA_character_)
  }
  attr(d, "dlc") <- data.frame(pt, x = col("x"), y = col("y"), score = col("likelihood"),
                               stringsAsFactors = FALSE, row.names = NULL)
  d
}

# DeepLabCut (https://deeplabcut.org). A file is the predictions CSV (see
# .read_deeplabcut()): the time is the frame index, in frames, and each
# individual is a track (id "1" without an individuals row). A data frame is
# a flat table of <bodypart>_x / _y / _likelihood columns, with optional id
# and time columns.
#
# bodypart = NULL       -> every bodypart
# bodypart = c("h","t") -> the named ones
# The position is their likelihood-weighted centroid under the masking rule of
# .dialect_position() (`likelihood_min`, `partial`). `individuals` keeps some
# individuals of a multi-animal file.
#
# Per-bodypart <name>_x / <name>_y columns are always appended to the output so
# downstream heading rules (e.g. bodypart_axis) can access individual points;
# they are NA where the point is masked.
register_loader_dialect("deeplabcut", function(x, bodypart = NULL, likelihood_min = NULL,
                                                partial = c("na", "mean"), individuals = NULL,
                                                id_col = NULL, time_col = NULL, fps = NULL,
                                                read_opts = list()) {
  partial <- match.arg(partial)
  inp <- .dialect_input(x, "deeplabcut", read = .read_deeplabcut, read_opts = read_opts,
                        honours = "decimal")
  fail <- function(...) rlang::abort(sprintf(...), class = "radiatR_dialect_error",
                                     source = inp$source)
  nms <- inp$names; n <- nrow(inp$df)
  lay <- attr(inp$df, "dlc")
  idv <- NULL
  if (!is.null(lay)) {
    if (!is.null(id_col) || !is.null(time_col))
      fail(paste0("deeplabcut: `id_col` and `time_col` do not apply to a DeepLabCut file: ",
                  "the id is the individual and the time is the frame index."))
    if (all(is.na(.parse_number(inp$df$frame))))
      fail(if (all(is.na(lay$score)))
             paste0("deeplabcut: '%s' is labelled training data (its rows are indexed by image, ",
                    "with no likelihoods), not tracking output. Read the predictions file ",
                    "DeepLabCut writes when it analyses a video.")
           else
             paste0("deeplabcut: '%s' holds predictions for a folder of images: its rows are ",
                    "indexed by image name, not by frame number, so they have no time."),
           inp$source)
    unit <- "frames"
    time <- .dialect_time(inp, "frame", unit, fps)
  } else {
    bps <- .dialect_point_names(nms)
    if (!length(bps)) fail("deeplabcut: no <bodypart>_x / <bodypart>_y column pairs found.")
    lik <- paste0(bps, "_likelihood")
    lay <- data.frame(individual = NA_character_, bodypart = bps, x = paste0(bps, "_x"),
                      y = paste0(bps, "_y"), score = ifelse(lik %in% nms, lik, NA_character_),
                      stringsAsFactors = FALSE)
    id <- .dialect_require(inp, id_col, "id_col") %||%
      .guess_col(nms, c("id", "individual", "animal", "subject"))
    time_col <- .dialect_require(inp, time_col, "time_col") %||%
      .guess_col(nms, c("time", "timestamp", "t", "frame", "frames"))
    unit <- if (is.null(time_col) || grepl("^frames?$", time_col, ignore.case = TRUE)) "frames"
    time <- .dialect_time(inp, time_col, unit, fps)
    if (!is.null(id)) idv <- .dialect_col(inp, id)
  }

  inds <- unique(lay$individual)
  if (!is.null(individuals)) {
    if (!is.character(individuals) || !length(individuals) || anyNA(individuals))
      fail("deeplabcut: `individuals` must name one or more individuals.")
    if (all(is.na(inds)))
      fail("deeplabcut: `individuals` was given, but the input has no individuals row.")
    unknown <- setdiff(as.character(individuals), inds)
    if (length(unknown))
      fail("deeplabcut: no such individual(s): %s; the file has %s.",
           paste(unknown, collapse = ", "), paste(inds, collapse = ", "))
    inds <- inds[inds %in% as.character(individuals)]
    lay <- lay[lay$individual %in% inds, , drop = FALSE]
  }
  sel <- .dialect_select(bodypart, unique(lay$bodypart), "bodypart", "deeplabcut", inp$source)

  pts <- lapply(inds, function(k) {
    li <- if (is.na(k)) lay else lay[lay$individual == k, , drop = FALSE]
    li <- li[match(sel, li$bodypart), , drop = FALSE]
    gap <- is.na(li$x) | is.na(li$y)
    if (any(gap))
      fail(paste0("deeplabcut: %s has no x/y columns for bodypart(s): %s. Name the individuals ",
                  "to read with dialect_args = list(individuals = c(...)), or the bodyparts ",
                  "they share with `bodypart`."),
           if (is.na(k)) "the input" else sprintf("individual '%s'", k),
           paste(sel[gap], collapse = ", "))
    sc <- stats::setNames(li$score, sel)
    .dialect_points(inp, stats::setNames(li$x, sel), stats::setNames(li$y, sel), sc[!is.na(sc)])
  })
  if (length(unique(lapply(pts, function(p) colnames(p$score)))) > 1L)
    fail("deeplabcut: the individuals do not have likelihood columns for the same bodyparts.")
  bind <- function(what) do.call(rbind, lapply(pts, `[[`, what))
  pos <- .dialect_position(list(x = bind("x"), y = bind("y"), score = bind("score")),
                           likelihood_min, partial, "deeplabcut", inp$source, "likelihood_min")
  out <- data.frame(
    id = if (!is.null(idv)) idv else rep(if (all(is.na(inds))) "1" else inds, each = n),
    time = rep(time, times = length(inds)), x = pos$x, y = pos$y, stringsAsFactors = FALSE)
  .dialect_output(cbind(out, pos$points), unit, pos$attrition)
})

# idtracker.ai wide format: one x/y column pair per animal, written `x1,y1,...`
# (trajectories.csv) or `x_1,y_1,...`, with an optional time column. `seconds`
# is seconds, `frame`/`frames` and row order are frames. Missing positions are
# `nan`. Each column is checked on the table as read, before the reshape.
register_loader_dialect("idtrackerai_wide", function(x, time_col = NULL, fps = NULL, ids = NULL,
                                                      read_opts = list()) {
  inp <- .dialect_input(x, "idtrackerai_wide", read_opts = read_opts)
  fail <- .dialect_failer(inp$source)
  nms <- inp$names
  xs <- grep("^x_?[0-9]+$", nms, value = TRUE)
  if (!length(xs)) fail("idtrackerai_wide: no x<id>/y<id> column pairs found")
  ys <- sub("^x", "y", xs)
  if (!all(ys %in% nms))
    fail("idtrackerai_wide: no y column for %s", paste(xs[!ys %in% nms], collapse = ", "))
  animal <- sub("^x_?", "", xs)
  if (!is.null(ids)) {
    if (!all(as.character(ids) %in% animal))
      fail("idtrackerai_wide: no such id(s): %s; the file has %s",
           paste(setdiff(as.character(ids), animal), collapse = ", "),
           paste(animal, collapse = ", "))
    keep <- animal %in% as.character(ids)
    xs <- xs[keep]; ys <- ys[keep]; animal <- animal[keep]
  }
  time_col <- time_col %||% .guess_col(nms, c("time", "t", "frame", "frames", "seconds"))
  unit <- if (is.null(time_col) || grepl("^frames?$", time_col, ignore.case = TRUE)) "frames"
          else if (tolower(time_col) == "seconds") "seconds"
  .dialect_check_decimal(inp, c(xs, ys, time_col))
  tv <- .dialect_time(inp, time_col, unit, fps)
  rows <- Map(function(xc, yc, idv)
    data.frame(id = idv, time = tv, x = .dialect_num(inp, xc), y = .dialect_num(inp, yc),
               stringsAsFactors = FALSE),
    xs, ys, animal)
  .dialect_output(do.call(rbind, unname(rows)), unit)
})

# The lines of a text file as UTF-8 text: UTF-16 when it starts with a UTF-16
# byte-order mark, otherwise UTF-8 when every line is valid UTF-8, otherwise
# latin-1 (the Windows programs that write tracking exports). A UTF-8
# byte-order mark is removed. Bytes are never dropped: a line that is not
# valid UTF-8 makes the whole file latin-1, in which every byte is a character.
# `n` (default all) is how many lines to read; the encoding is then that of
# those lines.
.read_text_lines <- function(path, n = -1L) {
  bom <- readBin(path, "raw", n = 2L)
  if (identical(bom, as.raw(c(0xff, 0xfe))) || identical(bom, as.raw(c(0xfe, 0xff)))) {
    con <- file(path, encoding = "UTF-16")
    lines <- tryCatch(enc2utf8(readLines(con, n = n, warn = FALSE)), finally = close(con))
  } else {
    lines <- readLines(path, n = n, warn = FALSE)
    if (all(validUTF8(lines))) Encoding(lines) <- "UTF-8"
    else lines <- iconv(lines, "latin1", "UTF-8")
  }
  if (length(lines)) lines[1L] <- sub(paste0("^", intToUtf8(0xfeff)), "", lines[1L])
  lines
}

# An EthoVision XT text export as a table of text columns. Line 1 gives the
# number of header lines N. Lines 2 to N-2 are key/value pairs about the trial,
# line N-1 holds the column names, line N their units, and the data follow.
# Fields are separated by ";" (or "," or a tab), each line ends with a
# separator, and a value the program did not have is written "-" (NA here).
# The text is latin-1 unless it is valid UTF-8 or starts with a UTF-16
# byte-order mark. Every data row must have the fields of the names row.
# attr(, "decimal") is the decimal mark of the data rows alone: the header
# block of a file written with decimal points still holds "3,6 %".
# attr(, "ethovision") is list(header =, units =): the header block as a named
# character vector, and the units named by column.
.read_ethovision <- function(path, delim = NULL, decimal = NULL) {
  src <- basename(path)
  fail <- function(...) rlang::abort(sprintf(...), class = "radiatR_dialect_error", source = src)
  lines <- .read_text_lines(path)
  first <- if (length(lines)) lines[1L] else ""
  top <- regmatches(first, regexec("^\\s*\"?number of header lines:?\"?\\s*([;,\t])", first,
                                   ignore.case = TRUE))[[1L]]
  if (length(top) != 2L)
    fail(paste0("ethovision: '%s' is not an EthoVision text export. Its first line is '%s'; ",
                "an export starts with \"Number of header lines:\" and a count. A table whose ",
                "first line is its column header is read as a data frame, not as a file."),
         src, substr(first, 1L, 60L))
  if (!is.null(delim) && !identical(delim, top[2L]))
    rlang::abort(sprintf(
      "ethovision: line 1 of '%s' is separated by %s, not by the read_opts delimiter %s.",
      src, encodeString(top[2L], quote = "'"), encodeString(delim, quote = "'")),
      class = "radiatR_dialect_error", source = src, arg = "read_opts", option = "delim",
      refused_by = "ethovision")
  delim <- top[2L]
  fields <- function(ln)
    scan(text = ln, what = "", sep = delim, quote = "\"", quiet = TRUE, strip.white = TRUE,
         na.strings = character(0), blank.lines.skip = FALSE)
  count <- fields(first)[2L]
  if (is.na(count) || !grepl("^[0-9]{1,6}$", count))
    fail("ethovision: the number of header lines on line 1 of '%s' is '%s', not a whole number.",
         src, if (is.na(count)) "" else count)
  n_hdr <- as.integer(count)
  if (n_hdr < 3L)
    fail(paste0("ethovision: line 1 of '%s' gives %d header lines; an export has at least 3 ",
                "(the count, the names row and the units row)."), src, n_hdr)
  if (length(lines) < n_hdr)
    fail("ethovision: '%s' says it has %d header lines; it has %d line(s) in all.",
         src, n_hdr, length(lines))
  block <- lapply(lines[seq_len(n_hdr - 2L)[-1L]], fields)
  block <- block[vapply(block, function(f) length(f) > 0L && nzchar(f[1L]), logical(1))]
  header <- stats::setNames(
    vapply(block, function(f) if (length(f) > 1L) f[2L] else "", character(1)),
    vapply(block, `[`, character(1), 1L))
  nm <- fields(lines[n_hdr - 1L])
  un <- fields(lines[n_hdr])
  trail <- length(nm) > 1L && !nzchar(nm[length(nm)])
  if (trail) nm <- nm[-length(nm)]
  if (!length(nm) || any(!nzchar(nm)) || anyDuplicated(nm))
    fail("ethovision: line %d of '%s' should name the columns, each once; it reads '%s'.",
         n_hdr - 1L, src, substr(lines[n_hdr - 1L], 1L, 80L))
  if (length(un) != length(nm) + trail || (trail && nzchar(un[length(un)])))
    fail(paste0("ethovision: line %d of '%s' has %d field(s); the names row has %d. The units ",
                "row has one cell per column (empty for a unitless one)."),
         n_hdr, src, length(un), length(nm) + trail)
  un <- un[seq_along(nm)]
  data <- lines[-seq_len(n_hdr)]
  while (length(data) && !nzchar(trimws(data[length(data)]))) data <- data[-length(data)]
  if (!length(data)) fail("ethovision: '%s' has no data rows.", src)
  n_fields <- length(nm) + trail
  con <- textConnection(data)
  nf <- tryCatch(utils::count.fields(con, sep = delim, quote = "\"", comment.char = "",
                                     blank.lines.skip = FALSE),
                 finally = close(con))
  ragged <- which(is.na(nf) | nf != n_fields)
  if (length(ragged))
    fail(paste0("ethovision: line %d of '%s' has %s field(s); the names row has %d. If the ",
                "file was cut off there, delete the partial line."),
         n_hdr + ragged[1L], src, nf[ragged[1L]], n_fields)
  d <- utils::read.table(text = data, sep = delim, quote = "\"", header = FALSE,
                         colClasses = "character", comment.char = "",
                         na.strings = character(0), strip.white = TRUE)
  if (trail) {
    extra <- which(nzchar(d[[length(d)]]))
    if (length(extra))
      fail(paste0("ethovision: line %d of '%s' has a value after its last column; the names ",
                  "row ends there."), n_hdr + extra[1L], src)
  }
  d <- d[seq_along(nm)]
  names(d) <- nm
  d[d == "-"] <- NA_character_
  # A given mark is used as it is. Otherwise a point is assumed unless the rows
  # show only decimal commas; for a file whose separator leaves room for a
  # decimal comma, keep every column's evidence for the role check
  # (.dialect_check_decimal()), from the one pass that also decides the mark.
  if (!is.null(decimal)) {
    attr(d, "decimal") <- decimal
  } else if (delim == ",") {
    attr(d, "decimal") <- "."
  } else {
    vals <- .distinct_cells(as.list(d))
    marks <- lapply(vals, .mark_cells)
    comma <- any(vapply(marks, function(m) any(m$comma), logical(1)))
    point <- any(vapply(marks, function(m) any(m$dotted), logical(1)))
    attr(d, "decimal") <- if (comma && !point) "," else "."
    if (identical(attr(d, "decimal"), "."))
      attr(d, "decimal_evidence") <- .decimal_evidence(vals = vals, marks = marks)
  }
  attr(d, "ethovision") <- list(header = header, units = stats::setNames(un, nm))
  d
}

# EthoVision XT (Noldus). A file is the text export read by .read_ethovision():
# one subject in one trial, so one track with id "1", and its header block and
# units are kept as the load's source_meta. A data frame is a table with
# EthoVision's column names, with an optional id column named by `id_col`.
#
# The names row labels each body point "X <point>" / "Y <point>" ("X center",
# and with multi-point tracking "X nose", "X tail"). Headers are matched
# lower-cased, with runs of other characters as "_".
# bodypart = NULL            -> the center point
# bodypart = "nose"          -> that point
# bodypart = c("nose","tail") -> their mean, under the masking rule of
#                               .dialect_position() (`partial`)
# A point the program did not find ("-") is masked. Every point found is
# appended as <point>_x / <point>_y for heading rules such as bodypart_axis.
#
# The time is `Trial time` (seconds since the trial started) unless `time_col`
# names another column; a file's units row must give it in seconds.
register_loader_dialect("ethovision", function(x, bodypart = NULL, partial = c("na", "mean"),
                                               time_col = NULL, id_col = NULL, zone = NULL,
                                               read_opts = list()) {
  partial <- match.arg(partial)
  if (!is.null(zone))
    rlang::abort(paste0(
      "ethovision: `zone` is now `bodypart`. In EthoVision a zone is an area of the arena; ",
      "the argument names body points (center, nose, tail)."),
      class = "radiatR_dialect_error")
  norm <- function(v) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(v)))
  inp <- .dialect_input(x, "ethovision", norm, read = .read_ethovision, read_opts = read_opts,
                        honours = c("decimal", "delim"))
  fail <- function(...) rlang::abort(sprintf(...), class = "radiatR_dialect_error",
                                     source = inp$source)
  nms <- inp$names; n <- nrow(inp$df)
  lay <- attr(inp$df, "ethovision")
  # a table the caller read from an export still writes "-" for a value the
  # program did not have
  if (is.null(lay)) {
    text <- vapply(inp$df, function(v) is.character(v) || is.factor(v), logical(1))
    inp$df[text] <- lapply(inp$df[text], function(v) {
      v <- as.character(v)
      replace(v, which(trimws(v) == "-"), NA_character_)
    })
  }
  arg_col <- function(col, arg) {
    if (is.null(col)) return(NULL)
    if (!is.character(col) || length(col) != 1L || is.na(col))
      fail("ethovision: `%s` must name one column.", arg)
    .dialect_require(inp, col, arg)
  }

  of <- function(prefix) sub(prefix, "", grep(prefix, nms, value = TRUE))
  found <- intersect(of("^x_"), of("^y_"))
  if (!length(found))
    fail("ethovision: no `X <point>` / `Y <point>` column pairs found; the columns are: %s.",
         paste(names(inp$df), collapse = ", "))
  # "centre" is the same point as "center", in a header and in `bodypart`
  us <- function(v) sub("^centre$", "center", v)
  if (all(c("center", "centre") %in% found))
    fail("ethovision: the table has both %s; they are the same point.",
         paste(.dialect_header(inp, c("x_center", "x_centre")), collapse = " and "))
  x_cols <- stats::setNames(paste0("x_", found), us(found))
  y_cols <- stats::setNames(paste0("y_", found), us(found))
  points <- names(x_cols)
  if (is.null(bodypart) && !"center" %in% points)
    fail("ethovision: the table has no center point; name the point(s) to use with `bodypart`: %s.",
         paste(points, collapse = ", "))
  sel <- .dialect_select(if (is.null(bodypart)) "center"
                         else if (is.character(bodypart)) us(norm(bodypart)) else bodypart,
                         points, "body point", "ethovision", inp$source)

  tc <- arg_col(time_col, "time_col")
  if (is.null(tc)) {
    if (!"trial_time" %in% nms)
      fail("ethovision: no `Trial time` column; name the time column with `time_col`.")
    tc <- "trial_time"
  }
  if (!is.null(lay)) {
    unit <- lay$units[[match(tc, nms)]]
    if (!identical(tolower(gsub("[^A-Za-z]", "", unit)), "s"))
      fail("ethovision: the time column '%s' is in '%s', not in seconds (s).",
           .dialect_header(inp, tc), unit)
    if (!is.null(id_col))
      fail(paste0("ethovision: `id_col` does not apply to an EthoVision file, which holds one ",
                  "subject (id \"1\"). Load several files with id_collision = \"namespace\"."))
  }
  ic <- arg_col(id_col, "id_col")
  same <- nms %in% nms[duplicated(nms)] &
    nms %in% c(paste0("x_", found), paste0("y_", found), tc, ic)
  if (any(same))
    fail("ethovision: these headers name the same column once case and punctuation are set aside: %s.",
         paste(names(inp$df)[same], collapse = ", "))

  .dialect_check_decimal(inp, unname(c(x_cols, y_cols, tc)))
  all_pts <- .dialect_points(inp, x_cols, y_cols)
  pos <- .dialect_position(list(x = all_pts$x[, sel, drop = FALSE],
                                y = all_pts$y[, sel, drop = FALSE]),
                           NULL, partial, "ethovision", inp$source)
  out <- data.frame(id = if (is.null(ic)) rep("1", n) else .dialect_col(inp, ic),
                    time = .dialect_time(inp, tc, "seconds"),
                    x = pos$x, y = pos$y, stringsAsFactors = FALSE)
  out <- cbind(out, .dialect_position(all_pts, NULL, "na", "ethovision", inp$source)$points)
  attr(out, "decimal") <- inp$decimal
  .dialect_output(out, "seconds", pos$attrition, lay)
})

# TrackMate (Fiji) spot tables: TRACK_ID, FRAME, POSITION_T, POSITION_X,
# POSITION_Y. Up to version 6 there is one header row; version 7 writes three
# more (feature names, short names, units), which are dropped when they are
# recognised as such -- never merely because they fail to parse. Time is
# POSITION_T when the units row says it is in seconds, otherwise FRAME
# (frames). Spots that belong to no track carry TRACK_ID "None" (or nothing):
# `unlinked` says whether that stops the load or leaves them out.
register_loader_dialect("trackmate", function(x, id_col = NULL, time_col = NULL, fps = NULL,
                                              unlinked = c("error", "drop"),
                                              read_opts = list()) {
  unlinked <- match.arg(unlinked)
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  inp <- .dialect_input(x, "trackmate", norm, read_opts = read_opts)
  fail <- .dialect_failer(inp$source)
  nms <- inp$names
  time_col <- .dialect_require(inp, time_col, "time_col")
  id  <- .dialect_require(inp, id_col, "id_col") %||%
    .guess_col(nms, c("track_id", "trajectory_id", "id"))
  xc  <- .guess_col(nms, c("position_x", "x"))
  yc  <- .guess_col(nms, c("position_y", "y"))
  frame_c <- .guess_col(nms, "frame")
  t_c     <- .guess_col(nms, c("position_t", "time"))
  if (is.null(id) || is.null(xc) || is.null(yc) || is.null(time_col %||% frame_c %||% t_c))
    fail("trackmate: no track id, time and POSITION_X / POSITION_Y columns; the columns are: %s.",
         paste(names(inp$df), collapse = ", "))

  # Version 7 descriptor rows: two rows of names (no number under X or Y),
  # then a units row whose every cell is blank or "(unit)".
  t_unit <- NULL
  if (nrow(inp$df) >= 3L) {
    head3 <- vapply(inp$df, function(v) trimws(as.character(v[1:3])), character(3))
    head3[is.na(head3)] <- ""
    xy <- match(c(xc, yc), nms)
    if (all(nzchar(head3[1:2, xy])) && all(is.na(.parse_number(head3[1:2, xy], inp$decimal, dot_ok = TRUE))) &&
        all(head3[3, ] == "" | grepl("^\\(.*\\)$", head3[3, ]))) {
      if (!is.null(t_c)) t_unit <- tolower(head3[3, match(t_c, nms)])
      inp$df <- inp$df[-(1:3), , drop = FALSE]
    }
  }
  t_secs <- !is.null(t_unit) && t_unit %in% c("(s)", "(sec)", "(secs)", "(second)", "(seconds)")
  time <- time_col %||% (if (t_secs) t_c) %||% frame_c %||% t_c
  unit <- if (identical(time, frame_c)) "frames" else if (identical(time, t_c) && t_secs) "seconds"
  .dialect_check_decimal(inp, c(xc, yc, time))

  # Coerced on the spot table as read, so rows in an error count spots.
  out <- data.frame(id = .dialect_col(inp, id), time = .dialect_time(inp, time, unit, fps),
                    x = .dialect_num(inp, xc), y = .dialect_num(inp, yc),
                    stringsAsFactors = FALSE)
  idv <- trimws(as.character(out$id))
  loose <- is.na(idv) | idv %in% c("", "None", "NA")
  if (any(loose)) {
    if (unlinked == "error")
      rlang::abort(sprintf(paste0(
        "trackmate: %d of %d spot(s) belong to no track (%s is '%s')%s.\n  rows: %s\n",
        "  Export \"Spots in tracks\" from TrackMate, or pass ",
        "dialect_args = list(unlinked = \"drop\") to leave them out."),
        sum(loose), length(loose), .dialect_header(inp, id),
        paste(unique(idv[loose]), collapse = "', '"),
        if (!is.null(inp$source)) sprintf(" in file '%s'", inp$source) else "",
        paste0(paste(utils::head(which(loose), 20L), collapse = ", "),
               if (sum(loose) > 20L) ", ..." else "")),
        class = c("radiatR_unlinked_rows", "radiatR_invalid_rows"),
        rows = which(loose), ids = character(0), columns = .dialect_header(inp, id),
        values = idv[loose], source = inp$source, dialect = "trackmate", call = NULL)
    rlang::warn(sprintf("Dropped %d of %d spot(s) that belong to no track.",
                        sum(loose), length(loose)),
                class = "radiatR_dropped_rows", rows = which(loose), ids = character(0),
                columns = .dialect_header(inp, id), values = idv[loose])
    out <- out[!loose, , drop = FALSE]
    rownames(out) <- NULL
  }
  .dialect_output(out, unit)
})

# ToxTrac Tracking.txt, read when it has no header row (the first line is all
# numbers): frame, arena, track, x, y, label, with five bounding-box columns
# before the label in the 2026 layout. Read as text for the strict coercion.
.read_toxtrac <- function(path, decimal = NULL) {
  latin1 <- .is_latin1(path)
  first <- readLines(path, n = 1L, warn = FALSE)
  if (latin1) first <- iconv(first, "latin1", "UTF-8")
  first <- strsplit(first, "\t", fixed = TRUE)
  if (!length(first) || !length(first[[1L]]) || anyNA(.parse_number(trimws(first[[1L]]), decimal %||% ".")))
    return(.read_any(path, decimal = decimal))
  d <- utils::read.table(path, header = FALSE, sep = "\t", colClasses = "character",
                         blank.lines.skip = FALSE, quote = "", comment.char = "",
                         fileEncoding = if (latin1) "latin1" else "")
  if (latin1) d <- .as_utf8(d)
  cols <- list(c("frame", "arena", "track", "x", "y", "label"),
               c("frame", "arena", "track", "x", "y", "box_angle", "box_x", "box_y",
                 "box_width", "box_height", "label"))
  cols <- cols[lengths(cols) == ncol(d)]
  if (!length(cols))
    rlang::abort(sprintf(
      "toxtrac: headerless file '%s' has %d columns; a ToxTrac Tracking.txt has 6 or 11.",
      basename(path), ncol(d)), class = "radiatR_dialect_error", source = basename(path))
  stats::setNames(d, cols[[1L]])
}

# ToxTrac (layouts per the ToxTrac 2026 manual, doi:10.5281/zenodo.20397164).
# Tracking_RealSpace.txt: "Time (sec)", Arena, Track, "Pos. X (mm)",
# "Pos. Y (mm)", Label -- seconds. Tracking.txt: headerless, frames (see
# .read_toxtrac). The id is <arena>_<track>, because each arena numbers its
# tracks from 1. Label is ToxTrac's detection flag (0 predicted, 1 detected,
# 2 texture-matched, 3 interpolated) and is kept as a column, not filtered on.
register_loader_dialect("toxtrac", function(x, fps = NULL, read_opts = list()) {
  norm <- function(v)
    gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(sub("\\s*\\([^)]*\\)\\s*$", "", v))))
  inp <- .dialect_input(x, "toxtrac", norm, read = .read_toxtrac, read_opts = read_opts,
                        honours = "decimal")
  nms <- inp$names
  track <- .guess_col(nms, c("trackid", "track", "id", "animal", "subject"))
  time  <- .guess_col(nms, c("time_s", "time", "frame"))
  xc    <- .guess_col(nms, c("x", "pos_x", "x_mm", "x_px"))
  yc    <- .guess_col(nms, c("y", "pos_y", "y_mm", "y_px"))
  if (is.null(track) || is.null(time) || is.null(xc) || is.null(yc))
    .dialect_failer(inp$source)("toxtrac: no track, time and x / y columns; the columns are: %s.",
                         paste(names(inp$df), collapse = ", "))
  unit <- if (grepl("^frame", time)) "frames"
          else if (grepl("\\((s|sec|secs|seconds)\\)\\s*$", .dialect_header(inp, time),
                         ignore.case = TRUE)) "seconds"
  arena <- .guess_col(nms, "arena")
  idv <- .dialect_col(inp, track)
  if (!is.null(arena)) idv <- paste(.dialect_col(inp, arena), idv, sep = "_")
  .dialect_check_decimal(inp, c(xc, yc, time))
  out <- data.frame(id = idv, time = .dialect_time(inp, time, unit, fps),
                    x = .dialect_num(inp, xc), y = .dialect_num(inp, yc),
                    stringsAsFactors = FALSE)
  for (extra in intersect(c("arena", "label"), nms)) out[[extra]] <- .dialect_col(inp, extra)
  .dialect_output(out, unit)
})

# TRex (https://trex.run): per-individual CSV export
# Each file typically covers one individual; the individual index is inferred
# from the numeric suffix of the filename stem (e.g. "run_0.csv" -> id "0").
# Aggregated exports that include an id/individual column are also supported.
# A table with a track column (Track, TrackID, TRACK_ID, trajectory_id) is
# refused unless `id_col` names the id: TRex writes none, ToxTrac and TrackMate do.
# Position columns: TRex exports plain X / Y, or hashtag-tagged centroid
# variants -- X#wcentroid (weighted, TRex's recommended best estimate),
# X#centroid (geometric), and X#pcentroid (posture-based). These normalise to
# x_wcentroid / x_centroid / x_pcentroid. By default the dialect prefers plain
# X/Y, then wcentroid, then centroid, then pcentroid; set `centroid` to force
# one source ("wcentroid", "centroid", or "pcentroid").
register_loader_dialect("trex", function(x, id_col = NULL, time_col = NULL,
                                          fps = NULL, centroid = NULL, read_opts = list()) {
  # TRex annotates headers with a trailing unit in parentheses, e.g.
  # "X#wcentroid (cm)" or "SPEED#wcentroid (cm/s)"; strip it before normalising.
  strip_unit <- function(v) sub("\\s*\\([^)]*\\)\\s*$", "", v)
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(strip_unit(v)))
  inp <- .dialect_input(x, "trex", norm, read_opts = read_opts)
  fail <- .dialect_failer(inp$source)
  nms <- inp$names
  stem <- if (!is.null(inp$source)) tools::file_path_sans_ext(inp$source)

  # TRex writes no track column; ToxTrac and TrackMate do, and guessing the id
  # past one reads every track as one (or each TrackMate spot as its own).
  id_col <- .dialect_require(inp, id_col, "id_col")
  track <- sub("^_+|_+$", "", nms) %in% c("track", "trackid", "track_id", "trajectory_id")
  if (is.null(id_col) && any(track))
    fail(paste0("trex: `%s` looks like a track id, which a TRex export does not have. ",
                "If this is a ToxTrac or TrackMate export, choose that dialect; ",
                "otherwise name the id column with `id_col`."),
         names(inp$df)[which(track)[1L]])
  time_col <- .dialect_require(inp, time_col, "time_col")
  id <- id_col %||% .guess_col(nms, c("id","individual","individual_id","fish","animal","subject"))
  idv <- if (!is.null(id)) .dialect_col(inp, id) else {
    m <- if (!is.null(stem)) regmatches(stem, regexpr("[0-9]+$", stem)) else character(0)
    rep(if (length(m)) m else (stem %||% "1"), nrow(inp$df))
  }
  # TRex can write `frame` twice, which every file reader keeps as two
  # columns of one name (a table read elsewhere may have them renamed
  # frame_<position>). Identical copies stand for the one column.
  frame_c <- .guess_col(nms, "frame")
  copies <- which(nms == "frame")
  if (length(copies) < 2L && is.null(frame_c)) {
    copies <- grep("^frame_[0-9]+$", nms)
    if (length(copies) > 1L) frame_c <- nms[copies[1L]]
  }
  if (length(copies) > 1L) {
    same <- vapply(copies[-1L], function(k)
      identical(inp$df[[k]], inp$df[[copies[1L]]]), logical(1))
    if (!all(same))
      fail("trex: the repeated frame columns (%s) differ; name one with time_col",
           paste(names(inp$df)[copies], collapse = ", "))
  }
  time <- time_col %||% frame_c %||% .guess_col(nms, c("frames","time","t","timestamp"))
  # frame/frames and row order are frames; TRex's `time` is seconds
  unit <- if (is.null(time) || identical(time, frame_c) || time == "frames") "frames"
          else if (time == "time") "seconds"

  if (!is.null(centroid)) {
    centroid <- match.arg(centroid, c("wcentroid", "centroid", "pcentroid"))
    xc <- .guess_col(nms, paste0("x_", centroid))
    yc <- .guess_col(nms, paste0("y_", centroid))
    if (is.null(xc) || is.null(yc))
      fail("trex: centroid = '%s' requested but X#%s / Y#%s columns not found",
           centroid, centroid, centroid)
  } else {
    # priority: plain X/Y, then weighted, geometric, posture centroid
    xc <- .guess_col(nms, c("x","x_wcentroid","x_centroid","x_pcentroid",
                            "pos_x","position_x","x_px","cx","center_x"))
    yc <- .guess_col(nms, c("y","y_wcentroid","y_centroid","y_pcentroid",
                            "pos_y","position_y","y_px","cy","center_y"))
  }
  if (is.null(xc) || is.null(yc)) fail("trex: could not find x/y position columns")
  .dialect_check_decimal(inp, c(xc, yc, time))
  .dialect_output(data.frame(
    id   = idv,
    time = .dialect_time(inp, time, unit, fps),
    x    = .dialect_num(inp, xc),
    y    = .dialect_num(inp, yc),
    stringsAsFactors = FALSE
  ), unit)
})

# A delimited file whose first line is its header, as a table of text columns.
# Nothing is typed, so a dialect coerces each column itself and a label column
# keeps its text ("1.1" and "1.10" stay apart). The text is decoded as
# .read_text_lines() decodes it, and the separator (unless `delim` gives it) and
# the decimal mark (attr "decimal") are sniffed as .read_any() sniffs them
# (`decimal`, when given, only turns the evidence off; .dialect_input() sets the mark). Blank lines after the
# last row are not rows. Every row must have the header's fields:
# read.table() would pad a short row and wrap a long one.
.read_text_table <- function(path, dialect, delim = NULL, decimal = NULL) {
  src <- basename(path)
  fail <- function(...) rlang::abort(sprintf(...), class = "radiatR_dialect_error", source = src)
  lines <- .read_text_lines(path)
  while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
  if (length(lines) < 2L) fail("%s: '%s' has no data rows.", dialect, src)
  g <- .guess_delim(path, lines)
  # the sniff's cells are fields of its own delimiter
  if (!is.null(delim) && !identical(delim, g$delim)) { g$delim <- delim; g$cells <- NULL }
  con <- textConnection(lines)
  nf <- tryCatch(utils::count.fields(con, sep = g$delim, quote = "\"", comment.char = "",
                                     blank.lines.skip = FALSE),
                 finally = close(con))
  ragged <- which(is.na(nf) | nf != nf[1L])
  if (length(ragged))
    fail("%s: line %d of '%s' has %s field(s); its header names %d.",
         dialect, ragged[1L], src, nf[ragged[1L]], nf[1L])
  d <- utils::read.table(text = lines, header = TRUE, sep = g$delim, quote = "\"",
                         colClasses = "character", check.names = FALSE, comment.char = "",
                         na.strings = character(0), strip.white = TRUE)
  attr(d, "decimal") <- g$decimal
  # a given mark is not checked (.dialect_input() sets it on the table)
  if (is.null(decimal) && !is.null(g$cells))
    attr(d, "decimal_evidence") <- .sniffed_evidence(g$cells, names(d), src, dialect)
  d
}

# ANY-maze (Stoelting) CSV export: `Time`, `Centre position X`, `Centre
# position Y`, then one 0/1 column per zone of the arena (`In <zone>`), which
# are not read. There is no units row. Headers are matched lower-cased, with
# runs of other characters as "_". The time is a duration H:MM:SS.fff, read as
# seconds (see .parse_duration()); `time_col` names another time column. The
# export holds one animal, id "1", unless `id_col` names an id column, which is
# kept as written. A sample with no position is masked and counted under
# .dialect_position(); only the centre point is read, so there is no
# `bodypart`.
register_loader_dialect("anymaze", function(x, time_col = NULL, id_col = NULL, zone = NULL,
                                            bodypart = NULL, skip_units_row = NULL,
                                            read_opts = list()) {
  gone <- function(msg) rlang::abort(msg, class = "radiatR_dialect_error")
  if (!is.null(zone) || !is.null(bodypart))
    gone(paste0("anymaze: `zone` and `bodypart` are not arguments of this dialect. It reads ",
                "the centre position (`Centre position X` / `Centre position Y`) only."))
  if (!is.null(skip_units_row))
    gone(paste0("anymaze: `skip_units_row` was removed. An ANY-maze export has no units row, ",
                "and no row is skipped."))
  norm <- function(v) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(v)))
  inp <- .dialect_input(x, "anymaze", norm,
                        read = function(p, delim = NULL, decimal = NULL) .read_text_table(p, "anymaze", delim, decimal),
                        read_opts = read_opts, honours = c("decimal", "delim"))
  fail <- function(...) rlang::abort(sprintf(...), class = "radiatR_dialect_error",
                                     source = inp$source)
  nms <- inp$names; n <- nrow(inp$df)
  arg_col <- function(col, arg) {
    if (is.null(col)) return(NULL)
    if (!is.character(col) || length(col) != 1L || is.na(col))
      fail("anymaze: `%s` must name one column.", arg)
    .dialect_require(inp, col, arg)
  }
  xc <- .guess_col(nms, c("centre_position_x", "center_position_x"))
  yc <- .guess_col(nms, c("centre_position_y", "center_position_y"))
  if (is.null(xc) || is.null(yc))
    fail("anymaze: no `Centre position X` / `Centre position Y` columns; the columns are: %s.",
         paste(names(inp$df), collapse = ", "))
  tc <- arg_col(time_col, "time_col") %||% (if ("time" %in% nms) "time")
  if (is.null(tc))
    fail("anymaze: no `Time` column; name the time column with `time_col`.")
  ic <- arg_col(id_col, "id_col")
  same <- nms %in% nms[duplicated(nms)] & nms %in% c(xc, yc, tc, ic)
  if (any(same))
    fail("anymaze: these headers name the same column once case and punctuation are set aside: %s.",
         paste(names(inp$df)[same], collapse = ", "))

  .dialect_check_decimal(inp, c(xc, yc, tc))
  pos <- .dialect_position(.dialect_points(inp, c(center = xc), c(center = yc)),
                           NULL, "na", "anymaze", inp$source)
  out <- data.frame(id = if (is.null(ic)) rep("1", n) else .dialect_col(inp, ic),
                    time = .parse_duration(.dialect_col(inp, tc), .dialect_header(inp, tc),
                                           inp$source, "anymaze", inp$decimal),
                    x = pos$x, y = pos$y, stringsAsFactors = FALSE)
  attr(out, "decimal") <- inp$decimal
  .dialect_output(out, "seconds", pos$attrition)
})

# SLEAP (https://sleap.ai) analysis CSV: `track`, `frame_idx`, then <node>.x /
# .y / .score for each node. Headers are matched lower-cased, with runs of
# other characters as "_" (`Tail Base.x` is tail_base_x); `bodypart` is
# normalised the same way. The id is `track`, and an empty track column (an
# untracked, single-animal export) is one track. The time is `frame_idx`, in
# frames; a table without it is refused unless `time_col` names the time (a
# column of another name has no declared unit).
# `instance.score` is not a node and is not used. Two headers that normalise
# to the same column are refused.
#
# The position is the score-weighted centroid of the selected nodes (all of
# them when bodypart = NULL) under the masking rule of .dialect_position()
# (`score_min`, `partial`). Per-node <name>_x / <name>_y columns are always
# appended, NA where the node is masked.
register_loader_dialect("sleap", function(x, bodypart = NULL, score_min = NULL,
                                          partial = c("na", "mean"),
                                          id_col = NULL, time_col = NULL, fps = NULL,
                                          read_opts = list()) {
  partial <- match.arg(partial)
  norm <- function(v) gsub("[^a-z0-9]+", "_", tolower(v))
  inp <- .dialect_input(x, "sleap", norm, read_opts = read_opts)
  fail <- .dialect_failer(inp$source)
  nms <- inp$names; n <- nrow(inp$df)
  # Every SLEAP analysis CSV has frame_idx; other trackers' <name>_x / _y
  # columns (a bounding box, a midline) would otherwise be read as nodes.
  time_col <- .dialect_require(inp, time_col, "time_col") %||%
    (if ("frame_idx" %in% nms) "frame_idx")
  if (is.null(time_col))
    fail(paste0("sleap: no `frame_idx` column, so this does not look like a SLEAP analysis ",
                "CSV. Choose the dialect of the program that wrote it, or name the time ",
                "column with `time_col`."))
  same <- nms %in% nms[duplicated(nms)] & grepl("_(x|y|score)$", nms)
  if (any(same))
    fail("sleap: these headers name the same column once case and punctuation are set aside: %s.",
         paste(names(inp$df)[same], collapse = ", "))
  nodes <- .dialect_point_names(nms)
  if (!length(nodes)) fail("sleap: no <node>.x / <node>.y column pairs found.")
  sel <- .dialect_select(if (is.character(bodypart)) norm(bodypart) else bodypart,
                         nodes, "node", "sleap", inp$source)

  id <- .dialect_require(inp, id_col, "id_col") %||%
    .guess_col(nms, c("track", "id", "individual", "animal", "subject"))
  idv <- if (!is.null(id)) .dialect_col(inp, id)
  if (is.null(idv) || all(.is_missing_chr(trimws(as.character(idv))))) idv <- rep("1", n)
  unit <- if (grepl("^(frame_idx|frames?)$", time_col)) "frames"

  sc <- stats::setNames(paste0(sel, "_score"), sel)
  .dialect_check_decimal(inp, unname(c(paste0(sel, "_x"), paste0(sel, "_y"), sc[sc %in% nms], time_col)))
  pts <- .dialect_points(inp, stats::setNames(paste0(sel, "_x"), sel),
                         stats::setNames(paste0(sel, "_y"), sel), sc[sc %in% nms])
  pos <- .dialect_position(pts, score_min, partial, "sleap", inp$source)
  out <- data.frame(id = idv, time = .dialect_time(inp, time_col, unit, fps),
                    x = pos$x, y = pos$y, stringsAsFactors = FALSE)
  .dialect_output(cbind(out, pos$points), unit, pos$attrition)
})
