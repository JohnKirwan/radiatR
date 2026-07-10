# radiatR (development version)

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
