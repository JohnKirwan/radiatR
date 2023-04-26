#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param file_tbl .
#' @param df .
#' @param track_dir The folder with the tracks.
#' @param colnames A named list of column names to replace the hardcoded ones.
#' @examples
#' \dontrun{
#' # Create example data.frames
#' file_tbl <- data.frame(
#'   basename = c("file1", "file2", "file3"),
#'   track = c("track1", "track2", "track3")
#' )
#'
#' df <- data.frame(
#'   file = c("file1", "file2", "file3"),
#'   new_arc = c(1, 2, 3),
#'   new_type = c("A", "B", "C"),
#'   new_obstacle = c("obs1", "obs2", "obs3"),
#'   new_id = c("id1", "id2", "id3")
#' )
#'
#' track_dir <- "your_track_directory_here"
#'
#' # Specify the column names mapping
#' colnames_list <- list(
#'   arc = "new_arc",
#'   type = "new_type",
#'   obstacle = "new_obstacle",
#'   id = "new_id"
#' )
#'
#' # Call the load_tracks function
#' result <- load_tracks(file_tbl, df, track_dir, colnames_list)
#' }
#'
#' @importFrom rlang .data
#' @export
#
load_tracks2 <- function(file_tbl, df, track_dir, colnames) {

  if (any(!file_tbl$basename %in% df$file)) {warning("Missing file")}
  if (any(!df$file %in% file_tbl$basename)) {warning("Missing file")}

  file_tbl <- add_new_columns(file_tbl, colnames)

  i = 1
  while (i <= dim(df)[1]) {
    assign_columns(file_tbl, df, i, colnames)
    i = i + 1
  }

  validate_files(file_tbl, track_dir)

  return(file_tbl)
}

add_new_columns <- function(file_tbl, colnames) {
  for (colname in colnames) {
    file_tbl[[colname]] <- rep(NA, dim(file_tbl)[1])
  }
  return(file_tbl)
}

assign_columns <- function(file_tbl, df, i, colnames) {
  index <- which(file_tbl$basename == df$file[i])
  for (colname in names(colnames)) {
    file_tbl[[colname]][index] <- df[[colnames[colname]]][i]
  }
}

validate_files <- function(file_tbl, track_dir) {
  if (any(!file_tbl$track %in% list.files(normalizePath(track_dir), recursive = TRUE))) {
    stop(print(paste0("The following track file is missing:",
                      file_tbl$track[which(!file_tbl$track %in% list.files(
                        normalizePath(track_dir), recursive = TRUE))])))
  }

  if (any(!list.files(normalizePath(track_dir),
                      recursive = TRUE)[grep('*point02.txt$',
                                             list.files(normalizePath(track_dir), recursive = TRUE))] %in% file_tbl$track)) {
    stop(print(paste0("The following track file lacks a counterpart:",
                      list.files(normalizePath(track_dir), recursive = TRUE)[grep('*point02.txt$',
                                                                                  list.files(normalizePath(track_dir), recursive = TRUE))])))
  }
}
