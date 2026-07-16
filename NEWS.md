# radiatR (development version)

## Bug fixes

* `read_tracks()` and `read_tracks_dir()` now preserve trajectory identity when
  importing multiple files. Previously, reading a character vector of paths with
  no id column (the common case, relying on `id_from_filename = TRUE`) silently
  collapsed every file into a single `"(multiple)"` trajectory whose rows were
  then interleaved by time across files. Each file now yields its own trajectory
  keyed by the file-name stem. A new `id_collision = c("error", "namespace")`
  argument controls what happens when a real id value appears under more than one
  file: `"error"` (the default) stops with a message naming the colliding id and
  files, and `"namespace"` prefixes every id with its file stem
  (`file::id`). Multi-file imports with no id column and `id_from_filename = FALSE`
  now fail with a clear error instead of the silent collapse, and duplicate
  `(id, time)` keys produced by combining files are reported rather than passed
  through.
* `read_tracks(..., time_type = "frames", fps = )` no longer converts frame
  indices to seconds twice. The time column is now kept as a raw frame index
  and `elapsed_seconds()` (and everything built on it: `track_duration()`,
  `track_speed()`, velocity, and turning-rate) performs the frame -> seconds
  division exactly once, on demand, using the stored `frame_rate()`.
  Previously the loader divided by `fps` immediately and then stored the same
  `fps` as the frame rate, so downstream kinematics divided by `fps` a second
  time -- e.g. an expected 60 units/s speed came out as 3600 units/s. Loading
  with `time_type = "seconds"` no longer stores a spurious frame rate either.
* `wrappedcauchy_fit()`'s `convergence` column now matches its documentation.
  `circular::mle.wrappedcauchy()` reports convergence as a logical, which the
  previous code passed through `as.integer()`, yielding `1` for a *converged*
  fit -- the opposite of the documented "`0` = converged". The column is now
  `0` when the fit converged and `1` when it did not, and the (incorrect)
  "optim return code" wording in the docs has been corrected.

# radiatR 0.1.1

## New features

* New `test_gof()` tests whether a sample of headings fits a wrapped Cauchy
  distribution (Watson's U^2 on the probability-integral transform, with a
  parametric-bootstrap p-value), closing the wrapped-Cauchy half of the
  non-von-Mises goodness-of-fit gap. Jones-Pewsey GOF remains deferred.
* `test_uniformity()` gains `test = "pycke"`, the Pycke (2010) omnibus
  uniformity test — like Hermans-Rasson, powerful against multimodal and
  asymmetric departures from uniformity, with a Monte-Carlo p-value.

## Bug fixes

* `radiate()` no longer labels every trajectory by its id when a plot has many
  tracks. The automatic fallback to the id/group column now applies only when
  that column has at most 12 distinct values, so many-track plots (e.g. the
  235-trial `cpunctatus`) render cleanly instead of being buried under
  overlapping trial-id labels. Pass `label_col` explicitly to force labelling
  regardless of track count.
* `radiate(clip_tracks = TRUE)` (the default) now also clips beyond-circumference
  overshoot in the absolute frame (`coords = "absolute"`), not just the
  relative frame. The relative frame is a rotation of the absolute one, so
  `rho` is identical in both; the previous relative-only restriction let
  `coords = "absolute"` plots (e.g. in the package vignette) draw tracks
  visibly outside the unit circle.
* `get_all_object_pos()` now reports out-of-bounds track points (radius > 1)
  with a single aggregated message across the whole manifest, instead of one
  message per file.

## App

* Summary & stats now has a "Group comparison" card: `test_mean_directions`,
  `test_concentration`, and `test_distributions` run automatically across the
  selected Group-by column, showing whether groups differ in mean direction,
  concentration, or overall distribution (not just whether each is
  individually non-uniform).

# radiatR 0.1.0

* First public release.
* Read and construct movement `Tracks`, with support for absolute and relative
  coordinate frames and frame-rate / time metadata.
* Plot circular data with `radiate()`, including axial overlays, faceting,
  grouping, perimeter labelling and circular boxplots.
* Compute kinematics: speed, path sinuosity, per-row velocity angle and related
  trajectory summaries.
* Circular statistics: circular and concentration regression, model selection,
  and tests of mean direction, symmetry and unimodality.
* Interactive Shiny app (`inst/app`), also deployed at
  <https://johnkirwan.shinyapps.io/radiatR/>.
