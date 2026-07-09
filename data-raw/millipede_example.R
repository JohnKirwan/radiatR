# Data provenance
#
# Paper: Kirwan J.D. & Nilsson D.-E. (2019). A millipede compound eye
#        mediating low-resolution vision. Vision Research 165, 36-44.
#        https://doi.org/10.1016/j.visres.2019.09.003
#
# Experiment: Cylindroiulus punctatus millipedes placed individually at the
#   centre of a cylindrical arena under bright, downwelling light, with a dark
#   target stimulus of varying angular half-width (5, 10, 15, 20, 30, 40, 50
#   degrees, plus a featureless control) on the arena wall. The animal's path
#   was tracked to test whether it oriented toward the dark target (object
#   taxis). The stimulus was an isoluminant difference-of-Gaussians signal.
#
# Tracking software: dtrack (Smolka J., Bitbucket)
#   https://bitbucket.org/jochensmolka/dtrack
#   Files: _point01.txt = 2 landmark rows per trial (arena centre + target
#          location on the wall); _point02.txt = per-frame xy trajectory.
#   This two-file role split is specific to this experiment.
#
# Source manifest: supplementary file mmc2.xlsx, columns trial_name,
#   individual, target_halfwidth, stimulus_period, radian, degree.
#
# NOTE: the bundled tracks are a SUBSET of the full experiment. The published
#   study analysed many more trials (see Kirwan & Nilsson 2019); only the
#   tracks available in milli_trials/ are packaged here. Object naming uses the
#   species epithet throughout: cpunctatus (TrajSet) and cpunctatus_tracks
#   (tibble), for Cylindroiulus punctatus.
#
# This script regenerates inst/extdata/millipede_trials.csv, the bundled
# track files in inst/extdata/tracks/, and the data/*.rda datasets from the
# raw extract in milli_trials/. Run from the package root.

library(radiatR)

raw_dir <- "milli_trials"
ext_dir <- file.path("inst", "extdata", "tracks")
dir.create(ext_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. parse the supplementary manifest (minimal xlsx reader) --------------

read_xlsx_simple <- function(path) {
  td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE))
  utils::unzip(path, exdir = td)
  ss <- paste(readLines(file.path(td, "xl/sharedStrings.xml"), warn = FALSE),
              collapse = "")
  strings <- gsub("<[^>]+>", "",
                  regmatches(ss, gregexpr("<t[^>]*>.*?</t>", ss))[[1]])
  sheet <- paste(readLines(file.path(td, "xl/worksheets/sheet1.xml"),
                           warn = FALSE), collapse = "")
  rows <- regmatches(sheet, gregexpr("<row[^>]*>.*?</row>", sheet))[[1]]
  parse_row <- function(r) {
    cells <- regmatches(r, gregexpr("<c [^>]*?(/>|>.*?</c>)", r))[[1]]
    vapply(cells, function(c) {
      is_str <- grepl("t=\"s\"", c)
      v <- regmatches(c, regexpr("<v>.*?</v>", c))
      v <- if (length(v)) gsub("<[^>]+>", "", v) else NA_character_
      if (is_str && !is.na(v)) strings[as.integer(v) + 1L] else v
    }, character(1))
  }
  parsed <- lapply(rows, parse_row)
  mat <- t(vapply(parsed, function(x) { length(x) <- 6L; x }, character(6)))
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- as.character(df[1, ]); df[-1, ]
}

manifest_raw <- read_xlsx_simple(
  file.path(raw_dir, "1-s2.0-S0042698919301725-mmc2.xlsx")
)

# ---- 2. copy all track pairs into inst/extdata/tracks/ ----------------------

pairs <- sub("_point02.txt$", "",
             list.files(raw_dir, pattern = "_point02.txt$"))
# Drop trials whose trajectory or landmark file is empty (excluded trials).
nonempty <- function(tr) {
  p1 <- file.path(raw_dir, paste0(tr, "_point01.txt"))
  p2 <- file.path(raw_dir, paste0(tr, "_point02.txt"))
  file.exists(p1) && file.exists(p2) &&
    file.size(p1) > 0 && file.size(p2) > 0
}
dropped <- pairs[!vapply(pairs, nonempty, logical(1))]
if (length(dropped))
  message("Skipping ", length(dropped), " empty/invalid trial(s): ",
          paste(dropped, collapse = ", "))
pairs <- pairs[vapply(pairs, nonempty, logical(1))]
for (tr in pairs) {
  for (s in c("_point01.txt", "_point02.txt")) {
    file.copy(file.path(raw_dir, paste0(tr, s)),
              file.path(ext_dir, paste0(tr, s)), overwrite = TRUE)
  }
}
message("Copied ", length(pairs), " track pairs to ", ext_dir)

# ---- 3. write the package manifest CSV --------------------------------------
# Map the experiment columns onto the loader's expected fields:
#   file <- trial_name, arc <- target_halfwidth, id <- individual.
# type = "control" when target_halfwidth == 0, else "stimulus".
# (Any reference-centre offset is now applied post-load via apply_transform();
#  see the edge_to_centre recipe in ?apply_transform.)

manifest <- data.frame(
  file             = manifest_raw$trial_name,
  arc              = as.numeric(manifest_raw$target_halfwidth),
  id               = manifest_raw$individual,
  stimulus_period  = as.numeric(manifest_raw$stimulus_period),
  recorded_radian  = as.numeric(manifest_raw$radian),
  recorded_degree  = as.numeric(manifest_raw$degree),
  stringsAsFactors = FALSE
)
manifest$type     <- ifelse(manifest$arc == 0, "control", "stimulus")
manifest$obstacle <- "none"

manifest_path <- file.path("inst", "extdata", "millipede_trials.csv")
utils::write.csv(manifest, manifest_path, row.names = FALSE)
message("Wrote manifest: ", manifest_path, " (", nrow(manifest), " rows)")

# ---- 4. build the TrajSet and long-form datasets ----------------------------

track_dir <- ext_dir
file_tbl  <- import_tracks(track_dir)
file_tbl  <- load_tracks(file_tbl, manifest, track_dir)

cpunctatus <- suppressWarnings(
  get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir)
)

# Carry the per-trial condition metadata (target half-width, type, individual)
# from meta$trial_limits onto every frame, matching trial_id to vid_ord.
tl  <- cpunctatus@meta$trial_limits
meta_cols <- data.frame(
  trial_id   = tl$vid_ord,
  arc        = tl$arc,
  type       = tl$type,
  individual = tl$id,
  stringsAsFactors = FALSE
)
meta_cols <- meta_cols[!duplicated(meta_cols$trial_id), , drop = FALSE]
# target half-width is a categorical condition -> ordered factor
meta_cols$arc <- factor(meta_cols$arc,
                        levels = c(0, 5, 10, 15, 20, 30, 40, 50),
                        ordered = TRUE)
d <- cpunctatus@data
d <- merge(d, meta_cols, by = "trial_id", all.x = TRUE, sort = FALSE)

# Rescue control trials that are absent from the manifest: a "con" trial is a
# featureless control, i.e. a target half-width (angular subtense) of 0 deg.
# Recover its animal id from the filename (con_<id>). All other trials without
# a manifest entry are excluded from the published analysis -> drop them.
is_con <- is.na(d$arc) & grepl("^con", d$video)
d$arc[is_con]        <- factor("0", levels = levels(d$arc), ordered = TRUE)
d$type[is_con]       <- "control"
d$individual[is_con] <- sub("^con_?", "", d$video[is_con])
d <- d[!is.na(d$arc), , drop = FALSE]

d <- d[order(d$trial_id, d$frame), , drop = FALSE]
cpunctatus@data <- tibble::as_tibble(d)
methods::validObject(cpunctatus)

cpunctatus_tracks <- tibble::as_tibble(cpunctatus@data)

# Real capture rate: 1 frame / 5 s = 0.2 fps (Kirwan & Nilsson 2019).
cpunctatus@meta$frame_rate <- 0.2

usethis::use_data(cpunctatus, overwrite = TRUE, compress = "xz")
usethis::use_data(cpunctatus_tracks, overwrite = TRUE, compress = "xz")
message("Built cpunctatus (TrajSet, ",
        length(unique(cpunctatus@data$trial_id)), " trials) and ",
        "cpunctatus_tracks (tibble)")

# ---- 5. prune bundled tracks to the curated trial set -----------------------
# Keep only the track files for trials that survived curation (manifest match
# or rescued control), so inst/extdata/tracks/ matches the dataset.
kept <- unique(cpunctatus@data$video)
keep_files <- as.vector(outer(kept, c("_point01.txt", "_point02.txt"), paste0))
all_files  <- list.files(ext_dir, pattern = "_point0[12]\\.txt$")
drop_files <- setdiff(all_files, keep_files)
if (length(drop_files)) {
  file.remove(file.path(ext_dir, drop_files))
  message("Pruned ", length(drop_files), " track files from excluded trials")
}
