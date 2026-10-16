# Shiny-free upload helpers, sourced by app.R. Kept out of app.R so testServer
# (which cannot see top-level app.R functions) and direct-source unit tests can
# reach them.

`%||%` <- function(a, b) if (is.null(a)) b else a

# The format controls as read_opts: an entry for each control not on "auto".
format_read_opts <- function(delim_sel = NULL, decimal_sel = NULL) {
  set <- function(v) !is.null(v) && !identical(v, "auto")
  out <- c(if (set(delim_sel)) list(delim = delim_sel),
           if (set(decimal_sel)) list(decimal = decimal_sel))
  if (is.null(out)) list() else out
}

# Upload extensions to advertise, by input mode and installed optional deps.
# Delimited formats are always readable; Excel only when 'readxl' is present.
# ctrax/.mat was removed, so no binary special-case remains.
accepted_exts <- function(mode = "trajectories") {
  exts <- c(".csv", ".CSV", ".tsv", ".txt")
  if (requireNamespace("readxl", quietly = TRUE))
    exts <- c(exts, ".xlsx", ".xls")
  exts
}

# Single validated reader shared by preview, headings ingest, and the generic
# column-mapping preview. Dispatch keys on the ORIGINAL filename's extension
# (`name`) while bytes are read from Shiny's temp `path` (datapath). Delimiter,
# decimal mark and Excel sheet come from the wizard controls (NULL/"auto" -> sniff/first).
# `unique_names`: a column name used twice stops the read, as it does for
# read_tracks(); previews leave it to the dialect (TRex writes `frame` twice).
# `id`: label columns (a mapped id, a headings group), which keep their text
# where reading them as numbers would merge labels ("1.1" and "1.10").
upload_read <- function(path, name, delim_sel = NULL, sheet_sel = NULL,
                        unique_names = FALSE, id = NULL, decimal_sel = NULL) {
  ext <- tolower(tools::file_ext(name %||% path))
  ro <- format_read_opts(delim_sel, decimal_sel)
  radiatR:::.read_any(
    path,
    ext     = if (nzchar(ext)) ext else NULL,
    delim   = ro$delim,
    decimal = ro$decimal,
    sheet   = if (!is.null(sheet_sel) && nzchar(sheet_sel)) sheet_sel else NULL,
    unique_names = unique_names,
    id = id
  )
}

# What .read_any() reads a delimited upload with: the delimiter and decimal
# mark (each the control's, else sniffed) and whether the file is latin-1.
# NULL for an Excel file. Kept for the downloaded script's read call.
upload_read_format <- function(path, name, delim_sel = NULL, decimal_sel = NULL) {
  if (tolower(tools::file_ext(name %||% path)) %in% c("xlsx", "xls")) return(NULL)
  g <- radiatR:::.guess_delim(path)
  ro <- format_read_opts(delim_sel, decimal_sel)
  list(delim = ro$delim %||% g$delim, decimal = ro$decimal %||% g$decimal,
       latin1 = radiatR:::.is_latin1(path))
}

# read_opts for a trajectory load (and its script) from the format controls
# that apply to this file: Shiny keeps a removed control's last value, so a
# sheet is sent only for Excel and a delimiter / decimal mark only for text.
upload_read_opts <- function(name, delim_sel = NULL, decimal_sel = NULL, sheet_sel = NULL) {
  if (tolower(tools::file_ext(name %||% "")) %in% c("xlsx", "xls"))
    return(if (!is.null(sheet_sel) && nzchar(sheet_sel)) list(sheet = sheet_sel) else list())
  format_read_opts(delim_sel, decimal_sel)
}

# Preview of an upload. A file a tracker's dialect reads is shown as its first
# lines when the generic table read cannot make a table of it (EthoVision's
# preamble: one column under readr, an error under fread); otherwise its first
# rows. Reading the first lines is the preview's fallback, not the data path:
# the load reads the file with its dialect and reports any error itself. The
# warnings of that table read go with its result: dropped when the preview
# falls back to the lines (fread's about the preamble), raised when its table
# is shown. A missing file stops before any read (the preview follows the
# dialect control, so it is read again whenever that changes).
upload_preview <- function(path, name, dialect = "generic", delim_sel = NULL,
                           sheet_sel = NULL, n = 10L, ...) {
  if (!file.exists(path)) stop("upload_preview: file not found: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(name %||% path))
  raw_ok <- !identical(dialect, "generic") && !ext %in% c("xlsx", "xls")
  if (!raw_ok)
    return(utils::head(as.data.frame(upload_read(path, name, delim_sel, sheet_sel, ...)), n))
  warns <- list()
  tbl <- tryCatch(withCallingHandlers(
    upload_read(path, name, delim_sel, sheet_sel, ...),
    warning = function(w) {
      warns[[length(warns) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }), error = function(e) NULL)
  if (is.null(tbl) || ncol(tbl) < 2L)
    return(data.frame(Line = radiatR:::.read_text_lines(path, n)))
  for (w in warns) warning(w)
  utils::head(as.data.frame(tbl), n)
}

# The angle picker of a headings upload. A text column whose values are all
# numbers in the decimal mark other than the one the file was read with
# (attr(df, "decimal"); none recorded for Excel, so none flagged), and some
# only in that mark, is offered and preselected so Continue stops with advice
# rather than an id column being plotted as angles. Id-like label columns
# ("1.1", "1.10") are not angles and are never flagged, and nothing is when a
# numeric column with decimals confirms the mark. `current` (the user's
# choice) stays selected while it is offered.
.number_re <- c("." = "^[-+]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][-+]?[0-9]+)?$",
                "," = "^[-+]?([0-9]+(,[0-9]*)?|,[0-9]+)$")
hd_angle_choices <- function(df, current = NULL) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  mark <- attr(df, "decimal")
  confirmed <- any(vapply(df[num], function(v) any(is.finite(v) & v != round(v)),
                          logical(1)))
  flagged <- character(0)
  if (!is.null(mark) && !confirmed) {
    other <- if (identical(mark, ",")) "." else ","
    labels <- tolower(trimws(names(df))) %in% radiatR:::.id_candidates
    flagged <- names(df)[!labels & vapply(df, function(v) {
      if (!is.character(v)) return(FALSE)
      s <- trimws(v)
      s <- s[!is.na(s) & !toupper(s) %in% c("", "NA")]
      length(s) > 0L && all(grepl(.number_re[[other]], s)) &&
        any(!grepl(.number_re[[mark]], s))
    }, logical(1))]
  }
  choices <- c(num, flagged)
  selected <- if (!is.null(current) && current %in% choices) current
              else c(flagged, num)[1L]
  list(choices = choices, selected = selected, flagged = flagged)
}

# What to tell the user when the chosen angle column is one hd_angle_choices()
# flagged; NULL otherwise.
hd_mark_advice <- function(df, col) {
  if (is.null(col) || !col %in% hd_angle_choices(df)$flagged) return(NULL)
  if (identical(attr(df, "decimal"), ","))
    sprintf(paste("Column '%s' was read as text: its values look like numbers with a",
                  "decimal point. Set Decimal mark to Point (.) in step 1 (Back)."), col)
  else sprintf(paste("Column '%s' was read as text: its values look like numbers with a",
                     "decimal comma. Set Decimal mark to Comma (,) in step 1 (Back)."), col)
}
