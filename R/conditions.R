# Structured conditions for ingest/analysis failures. These make row-loss and
# non-estimable cases inspectable by class rather than by parsing a message
# string, so the app (which sees only exported functions) can classify them via
# inherits(e, "<class>") without needing these constructors exported.

radiatR_abort_invalid_rows <- function(rows, ids, columns, values, call = NULL) {
  msg <- sprintf(
    "Found %d row(s) with non-finite coordinates in column(s) %s.\n  rows: %s\n  ids: %s",
    length(rows), paste(columns, collapse = ", "),
    paste(rows, collapse = ", "),
    paste(utils::head(unique(ids), 10L), collapse = ", ")
  )
  rlang::abort(msg, class = "radiatR_invalid_rows",
               rows = as.integer(rows), ids = as.character(ids),
               columns = as.character(columns), values = as.character(values),
               call = call)
}

radiatR_warn_dropped_rows <- function(rows, ids, columns, values) {
  msg <- sprintf("Dropped %d row(s) with non-finite coordinates.", length(rows))
  rlang::warn(msg, class = "radiatR_dropped_rows",
              rows = as.integer(rows), ids = as.character(ids),
              columns = as.character(columns), values = as.character(values))
}

radiatR_abort_nonestimable <- function(method, group, n, reason, call = NULL) {
  msg <- sprintf("%s not estimable for group '%s' (n = %s): %s",
                 method, group %||% "all", format(n), reason)
  rlang::abort(msg, class = "radiatR_nonestimable",
               method = method, group = group,
               n = if (is.null(n)) NA_integer_ else as.integer(n),
               reason = reason, call = call)
}
