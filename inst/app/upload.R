# Shiny-free upload helpers, sourced by app.R. Kept out of app.R so testServer
# (which cannot see top-level app.R functions) and direct-source unit tests can
# reach them.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Map the app's delimiter dropdown to a read_opts override. "auto" -> sniff.
delim_read_opts <- function(sel) {
  if (is.null(sel) || identical(sel, "auto")) list(delim = NULL) else list(delim = sel)
}

# Single validated reader shared by preview, headings ingest, and the generic
# column-mapping preview. Dispatch keys on the ORIGINAL filename's extension
# (`name`) while bytes are read from Shiny's temp `path` (datapath). Delimiter
# and Excel sheet come from the wizard controls (NULL/"auto" -> sniff/first).
upload_read <- function(path, name, delim_sel = NULL, sheet_sel = NULL) {
  ext <- tolower(tools::file_ext(name %||% path))
  radiatR:::.read_any(
    path,
    ext   = if (nzchar(ext)) ext else NULL,
    delim = delim_read_opts(delim_sel)$delim,
    sheet = if (!is.null(sheet_sel) && nzchar(sheet_sel)) sheet_sel else NULL
  )
}
