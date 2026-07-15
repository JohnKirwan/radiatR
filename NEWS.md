# radiatR (development version)

* `get_all_object_pos()` now reports out-of-bounds track points (radius > 1)
  with a single aggregated message across the whole manifest, instead of one
  message per file.
# radiatR 0.1.1

## Bug fixes

* `radiate()` no longer labels every trajectory by its id when a plot has many
  tracks. The automatic fallback to the id/group column now applies only when
  that column has at most 12 distinct values, so many-track plots (e.g. the
  235-trial `cpunctatus`) render cleanly instead of being buried under
  overlapping trial-id labels. Pass `label_col` explicitly to force labelling
  regardless of track count.

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
