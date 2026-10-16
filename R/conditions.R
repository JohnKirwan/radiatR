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
                                           dialect = NULL, decimal_hint = FALSE,
                                           decimal = ".") {
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
  # The values look like numbers (or H:MM:SS durations) written with the
  # other decimal mark. Not for Excel, whose cells read_opts cannot change.
  if (!is.null(source) && tolower(tools::file_ext(source)) %in% c("xlsx", "xls"))
    decimal_hint <- FALSE
  dur <- "^[0-9]+:[0-5][0-9]:[0-5][0-9]"
  suggested <- if (decimal_hint && identical(decimal, ",") &&
                   any(grepl("^[-+]?([0-9]+[.][0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", values) |
                       grepl(paste0(dur, "[.][0-9]+$"), values))) "."
               else if (decimal_hint && !identical(decimal, ",") &&
                        any(grepl("^-?[0-9]+,[0-9]+$", values) |
                            grepl(paste0(dur, ",[0-9]+$"), values))) ","
  if (identical(suggested, "."))
    msg <- paste0(msg, "\n  ", if (is.null(source)) "The values were" else "The file was",
                  " read with a decimal comma. If the decimal mark ",
                  "is a point, pass read_opts = list(decimal = \".\").")
  else if (identical(suggested, ","))
    msg <- paste0(msg, "\n  Decimal commas? Pass read_opts = list(decimal = \",\").")
  rlang::abort(msg, class = c("radiatR_malformed_values", "radiatR_invalid_rows"),
               rows = as.integer(rows), ids = character(0),
               columns = as.character(column), values = as.character(values),
               expected = expected, source = source, dialect = dialect,
               decimal = decimal, suggested_decimal = suggested,
               call = NULL)
}

# A file whose layout cannot be read as one table: a row with more or fewer
# fields than the header (`problem = "ragged"`, `lines` are line numbers in
# the file, header = 1), a column name used twice (`"duplicate_columns"`,
# `columns` the names), or a delimited file that read as one column
# (`"one_column"`, `columns` that column). Every reader stops on it rather than drop, wrap or
# shift rows. `caller` names the function in the message.
radiatR_abort_malformed_file <- function(source, problem, detail, lines = integer(0),
                                         columns = character(0), caller = "read_tracks") {
  rlang::abort(
    sprintf("%s: file '%s' is not one table: %s", caller, source, detail),
    class = "radiatR_malformed_file", source = source, problem = problem,
    lines = as.integer(lines), columns = as.character(columns), call = NULL)
}

# Ids that are distinct as written but the same once their surrounding
# whitespace is removed (" 1" and "1"). Readers disagree on whether that
# whitespace is kept, so such ids are taken as neither one track nor two.
radiatR_abort_ambiguous_ids <- function(column, ids) {
  rlang::abort(
    sprintf(paste0(
      "read_tracks: ids in column '%s' differ only by surrounding whitespace (%s%s). ",
      "Trim them, or make them distinct, before reading."),
      column, paste0("'", utils::head(ids, 10L), "'", collapse = ", "),
      if (length(ids) > 10L) ", ..." else ""),
    class = "radiatR_ambiguous_ids", columns = column, ids = as.character(ids),
    call = NULL)
}

# A decimal point sniffed from a file that no column read for a role backs:
# `columns` hold only dotted numbers that could as well be digit grouping
# ("1.234": 1.234, or 1234), with `values` among them, and the clear decimal
# points are in `evidence`, columns read for no role (empty when none shows
# one, e.g. a point seen only in the header).
radiatR_abort_ambiguous_decimal <- function(source, columns, values, evidence,
                                            dialect = NULL) {
  q <- function(v) paste0("'", v, "'", collapse = ", ")
  fields <- list(class = "radiatR_ambiguous_decimal", source = source,
                 columns = as.character(columns), values = as.character(values),
                 evidence = as.character(evidence), decimal = ".", dialect = dialect,
                 call = NULL)
  if (!length(columns))   # no whole data record in the lines sniffed
    return(do.call(rlang::abort, c(list(sprintf(paste0(
      "%s: the first lines of '%s' hold no whole row to check its decimal point by ",
      "(a quoted field runs on). Give the mark: read_opts = list(decimal = \".\") or ",
      "list(decimal = \",\")."), dialect %||% "read_tracks", source)), fields)))
  rlang::abort(sprintf(paste0(
    "%s: column(s) %s in '%s' hold values like '%s', which read as %s with a decimal ",
    "point or as %s with digit grouping, and %s. If the decimal mark is a point, pass ",
    "read_opts = list(decimal = \".\"). Digit grouping is not supported: if the decimal ",
    "mark is a comma, remove the grouping first."),
    dialect %||% "read_tracks", q(columns), source, values[1L], values[1L],
    gsub(".", "", values[1L], fixed = TRUE),
    if (length(evidence)) sprintf("the only clear decimal point is in %s", q(evidence))
    else "no column shows a clear decimal point"),
    class = fields$class, source = source, columns = fields$columns,
    values = fields$values, evidence = fields$evidence, decimal = ".",
    dialect = dialect, call = NULL)
}

# get_all_object_pos(): no file of the manifest holds a valid trial (each
# track starts too far from the centre), so there is no Tracks to return.
radiatR_abort_no_valid_trials <- function(n_files) {
  rlang::abort(sprintf(paste0(
    "get_all_object_pos: none of the %d file(s) holds a valid trial (each track ",
    "starts too far from the centre)."), n_files),
    class = "radiatR_no_valid_trials", n_files = as.integer(n_files), call = NULL)
}

# The files of one import hold the id column as different types (numbers in
# one, text in another) and a file cannot be read again for the ids as they
# are written in it (a dialect's table, Parquet, Feather).
radiatR_abort_id_type_mismatch <- function(column, sources, types, dialect = NULL) {
  shown <- !is.na(types)
  rlang::abort(sprintf(paste0(
    "read_tracks: the files do not hold id column '%s' as one type (%s)%s, and the ids ",
    "cannot be read again as the text in each file. Make the column the same type in ",
    "every file (as text, for labels)."),
    column, paste0(sources[shown], ": ", types[shown], collapse = ", "),
    if (!is.null(dialect)) sprintf(" under dialect '%s'", dialect) else ""),
    class = "radiatR_id_type_mismatch", columns = column, sources = sources[shown],
    types = types[shown], call = NULL)
}
