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

# `problems`: data frame with columns rule, column, row, id (one row per rule
# broken per row). `rows`/`ids` are the union across rules, named and typed as
# in radiatR_invalid_rows so consumers can treat the two conditions alike.
radiatR_abort_invalid_tracks <- function(problems, call = NULL) {
  lines <- .format_tracks_problems(problems, problems$id)
  rlang::abort(
    paste0("Invalid track data:\n", paste0("  ", lines, collapse = "\n")),
    class = "radiatR_invalid_tracks",
    problems = problems,
    rows = sort(unique(as.integer(problems$row))),
    ids = unique(as.character(problems$id)),
    call = call)
}

# A cell that is present but will not parse as `expected`. Distinct from a
# missing value, which stays NA and is governed by `on_invalid`: a malformed
# value is never dropped. Subclasses radiatR_invalid_rows so existing handlers
# catch it. `rows` index the data rows of the table the column was read from
# (header excluded); `ids` is empty because columns are checked before ids are
# resolved.
radiatR_abort_malformed_values <- function(column, rows, values,
                                           expected = "numeric", source = NULL,
                                           dialect = NULL, decimal_hint = FALSE) {
  where <- c(if (!is.null(source)) sprintf("file '%s'", source),
             if (!is.null(dialect)) sprintf("dialect '%s'", dialect))
  first <- function(x, n) paste0(paste(utils::head(x, n), collapse = ", "),
                                 if (length(x) > n) ", ...")
  msg <- sprintf(
    "Column '%s'%s has %d value(s) that are not %s.\n  rows: %s\n  values: %s",
    column,
    if (length(where)) sprintf(" (%s)", paste(where, collapse = ", ")) else "",
    length(rows), expected, first(rows, 20L),
    first(paste0("'", unique(values), "'"), 10L)
  )
  if (decimal_hint && any(grepl("^-?[0-9]+,[0-9]+$", values)))
    msg <- paste0(msg, "\n  Decimal commas? Pass read_opts = list(decimal = \",\").")
  rlang::abort(msg, class = c("radiatR_malformed_values", "radiatR_invalid_rows"),
               rows = as.integer(rows), ids = character(0),
               columns = as.character(column), values = as.character(values),
               expected = expected, source = source, dialect = dialect,
               call = NULL)
}
