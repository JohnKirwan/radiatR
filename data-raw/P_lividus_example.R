# Data provenance
#
# Paper: Kirwan J.D., Li T., Ullrich-Lüter J., La Camera G., Nilsson D.-E.,
#        Arnone M.I. (2024). The sea urchin Paracentrotus lividus orients to
#        visual stimuli. bioRxiv. https://doi.org/10.1101/2024.01.05.574409
#
# Experiment: 50 P. lividus adults (Bay of Naples) placed individually at the
#   centre of a submerged cylindrical arena (~2 m diameter) and allowed to walk
#   freely toward a patterned wall stimulus. Five stimulus arc widths (0°, 15°,
#   30°, 45°, 60°, 150°) plus a uniform control were tested under diffuse,
#   downwelling light. Tracks recorded in pixel space from a webcam mounted
#   overhead.
#
# Tracking software: dtrack (Smolka J., Bitbucket)
#   https://bitbucket.org/jochensmolka/dtrack
#   Files: _point01.txt = 2 landmark rows per trial (arena centre + stimulus
#          edge on wall); _point02.txt = per-frame xy trajectory of the animal.
#   This two-file role split is specific to this experiment.
#
# Full dataset: https://github.com/JohnKirwan/P_lividus_vision
#
# To refresh or expand the extract, run this script from the package root.
# It downloads from GitHub and writes into inst/extdata/tracks/.

repo  <- "JohnKirwan/P_lividus_vision"
cond  <- "G1D_0_obstacle"
bases <- c(
  "WIN_20210201_11_24_19_Pro",
  "WIN_20210201_11_32_56_Pro",
  "WIN_20210204_16_55_13_Pro",
  "WIN_20210204_17_12_38_Pro",
  "WIN_20210204_17_30_19_Pro"
)

out_dir <- file.path("inst", "extdata", "tracks", cond)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (base in bases) {
  for (suffix in c("_point01.txt", "_point02.txt")) {
    fname    <- paste0(base, suffix)
    api_path <- paste0("data/tracks/", cond, "/", fname)
    url      <- paste0("https://raw.githubusercontent.com/", repo, "/main/", api_path)
    dest     <- file.path(out_dir, fname)
    download.file(url, dest, quiet = TRUE)
    message("Downloaded: ", fname)
  }
}
