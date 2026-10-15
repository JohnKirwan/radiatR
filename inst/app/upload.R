# Shiny-free upload helpers, sourced by app.R. Kept out of app.R so testServer
# (which cannot see top-level app.R functions) and direct-source unit tests can
# reach them.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Map the app's delimiter dropdown to a read_opts override. "auto" -> sniff.
delim_read_opts <- function(sel) {
  if (is.null(sel) || identical(sel, "auto")) list(delim = NULL) else list(delim = sel)
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
# (`name`) while bytes are read from Shiny's temp `path` (datapath). Delimiter
# and Excel sheet come from the wizard controls (NULL/"auto" -> sniff/first).
# `unique_names`: a column name used twice stops the read, as it does for
# read_tracks(); previews leave it to the dialect (TRex writes `frame` twice).
upload_read <- function(path, name, delim_sel = NULL, sheet_sel = NULL,
                        unique_names = FALSE) {
  ext <- tolower(tools::file_ext(name %||% path))
  radiatR:::.read_any(
    path,
    ext   = if (nzchar(ext)) ext else NULL,
    delim = delim_read_opts(delim_sel)$delim,
    sheet = if (!is.null(sheet_sel) && nzchar(sheet_sel)) sheet_sel else NULL,
    unique_names = unique_names
  )
}

# Preview of an upload. A file a tracker's dialect reads is shown as its first
# lines when the generic table read cannot make a table of it (EthoVision's
# preamble: one column under readr, an error under fread); otherwise its first
# rows. Reading the first lines is the preview's fallback, not the data path:
# the load reads the file with its dialect and reports any error itself.
upload_preview <- function(path, name, dialect = "generic", delim_sel = NULL,
                           sheet_sel = NULL, n = 10L, ...) {
  ext <- tolower(tools::file_ext(name %||% path))
  raw_ok <- !identical(dialect, "generic") && !ext %in% c("xlsx", "xls")
  tbl <- if (raw_ok) tryCatch(upload_read(path, name, delim_sel, sheet_sel, ...),
                              error = function(e) NULL)
         else upload_read(path, name, delim_sel, sheet_sel, ...)
  if (raw_ok && (is.null(tbl) || ncol(tbl) < 2L)) {
    return(data.frame(Line = utils::head(radiatR:::.read_text_lines(path), n)))
  }
  utils::head(as.data.frame(tbl), n)
}
