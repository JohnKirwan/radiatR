# Changelog

## radiatR 0.1.3

### Breaking changes

- `Tracks` objects now enforce the invariants the rest of the package
  assumes (see “Invariants” in
  [`?tracks`](https://johnkirwan.github.io/radiatR/reference/Tracks-class.md)):
  no missing ids; finite, strictly increasing time within each id;
  finite angles and coordinates; finite, non-negative weights.
  Previously a missing id, a non-finite or repeated time, an invalid
  weight, a non-finite relative coordinate (`rel_x`/`rel_y`), or any
  non-finite coordinate written after construction was accepted, and a
  missing time crashed the validator.
  [`tracks()`](https://johnkirwan.github.io/radiatR/reference/Tracks-class.md)
  now reports every such violation at once as a `radiatR_invalid_tracks`
  error whose `problems` field lists the rule, column, input row and id.
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  passes that error on, after its own checks for non-finite x/y
  (`radiatR_invalid_rows`) and for duplicate keys across files.
  Consequences: relative-coordinate columns must be finite when the
  object is built, so give them finite placeholders (or name the roles
  without storing the columns) if
  [`set_reference()`](https://johnkirwan.github.io/radiatR/reference/set_reference.md)
  is to fill them;
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  on a data frame fails when `group_col` does not identify single
  trajectories (it used to draw one path per group that hopped between
  trials), so pass the trajectory id as `group_col`; and a `Tracks`
  saved by an earlier version that breaks a rule fails at its next
  `validObject()` (any transform), not when it is loaded.

- `load_tracks()` and `load_tracks2()` have been removed. Despite the
  names they loaded no tracks; they joined manifest columns onto the
  file table used by the landmark pipeline. Pass the manifest to
  [`get_all_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_all_object_pos.md)
  instead, via its new `manifest` and `manifest_cols` arguments:
  `get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir, manifest = manifest)`.
  `manifest_cols` replaces `load_tracks2()`’s `colnames`.

- The `cpunctatus_tracks` dataset has been removed. It duplicated the
  table already stored in `cpunctatus`; use `as.data.frame(cpunctatus)`
  (or `cpunctatus@data`) for the same per-frame rows.

- `simulate_tracks(seed = )` no longer changes the session’s
  random-number state: the simulation is drawn under `seed` and the
  previous state is restored on exit. Seeded output is unchanged. Code
  that relied on the call to seed later draws (a bootstrap or
  Monte-Carlo test run afterwards) must now call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) itself.

- [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  stops on a malformed value instead of treating it as missing. A cell
  in an x, y, angle, weight or time column that is present but does not
  parse (`"oops"`, `"1,5"`, a stray header or units row) raises a
  `radiatR_malformed_values` error, a subclass of
  `radiatR_invalid_rows`, that names the column, the data rows, the
  offending values and the file (each file’s own rows in a multi-file
  import). `on_invalid = "drop"` does not apply to it: `"drop"` removes
  only rows whose coordinates are missing or non-finite. Blank cells and
  `NA` are still missing values, and `nan` and `inf` are read as such.
  Previously one bad cell made
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  report every row of the column as non-finite, and `"drop"` then
  dropped them all; a bad cell after the first 1000 rows of a file was
  read as missing when readr is installed; and a multi-file import
  failed with a type-clash error from vctrs.

  Files are now read with column types guessed from every row and with
  no digit-grouping mark, so `1,5` or `1,234.5` in a tab-separated file
  is malformed rather than read as 15 or 1234.5. Pass
  `read_opts = list(decimal = ",")` for decimal commas. Where the
  decimal mark is a comma (the default for semicolon-separated files),
  one bad cell names that cell only, not every `1,5` in its column.

  The check covers the columns
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  converts itself and the time column of every dialect but ANY-maze. An
  error raised inside a dialect does not yet name the file or the source
  column, and counts rows after the dialect’s own filtering and
  reshaping. Coordinates that a dialect converts itself (ANY-maze,
  EthoVision, DeepLabCut, SLEAP, Tracktor, TRex) are not yet checked
  this way.

- Text time columns are parsed strictly. Every value must match one
  timestamp layout in full: `YYYY-MM-DD HH:MM:SS`, `YYYY-MM-DD HH:MM` or
  `YYYY-MM-DD` (with `-` or `/`), or ISO 8601 `YYYY-MM-DDTHH:MM:SS` with
  an optional `Z` for UTC; a bare date beside date-times is midnight. An
  ISO 8601 timestamp, or one followed by stray text, used to be cut to
  midnight of its date without warning; the first now keeps its time of
  day and the second is an error. A text column of numbers is read as
  numbers (it was an error) and, with `fps`, stored as frames. A factor
  time column is read by its labels, not its level codes. A `Date`
  column becomes midnight timestamps instead of day counts, and a
  `difftime` column is converted to seconds whatever its units.
  `time_type = "frames"` with a date-time, date or duration column is
  now a `radiatR_time_type_mismatch` error; it used to store epoch
  seconds as frame numbers.

- [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  stops with a `radiatR_missing_columns` error when `mapping` names a
  column that is not in the data. A missing `angle` column used to be
  ignored without notice, and a missing x, y or weight column gave an
  error that did not name it.

- The `validate` argument of
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  is deprecated. It never skipped anything, because `Tracks` invariants
  are checked whenever an object is built; any value but `TRUE` now
  warns, and the argument will be removed.

### Changes

- The main workflow and simulation walkthroughs are now website-only
  articles (`vignettes/articles/`) rather than installed vignettes, so
  `vignette("radiatR")` and `vignette("simulate")` are no longer
  available offline; read them at
  <https://johnkirwan.github.io/radiatR/articles/>. Together with the
  dataset removal this brings the source tarball under CRAN’s 5 MB
  limit, and R CMD check now fails if it grows past 4.5 MB.

- The hosted Shiny app has moved from shinyapps.io to Posit Connect
  Cloud (git-backed deploy). The `shinyapps.yaml` GitHub Actions
  workflow is removed; Connect Cloud rebuilds `inst/app` from the repo
  and installs dependencies from the committed `inst/app/manifest.json`,
  regenerated by the new `connect-manifest` workflow.

- Reading several track files at once in
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  no longer needs the optional dplyr package (it is bound with
  [`vctrs::vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)).
  purrr is no longer a dependency; vctrs, already required by tibble and
  ggplot2, is now imported directly.

- [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  draws clipped tracks much faster: clipping paths to the unit circle
  (`clip_tracks = TRUE`, the default) is now vectorised, taking a plot
  of the `cpunctatus` example data from about 6 s to about 0.1 s. The
  drawn geometry is unchanged.
  [`line_circle_intercept()`](https://johnkirwan.github.io/radiatR/reference/line_circle_intercept.md)
  is now vectorised over its arguments.

### Bug fixes

- [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  ignores a guessed id column that holds no values. It uses the next
  id-like column if there is one, and otherwise reads the file as one
  trajectory, as it does when there is no id column. SLEAP writes an
  empty `track` column for single-animal exports, so `sleap_example.csv`
  now loads without `dialect = "sleap"`. An id column named in
  `mapping`, or one that is only partly empty, is still an error.

- x, y, angle and weight columns stored as text or as a factor load when
  every value is a number. Previously every row was reported as
  non-finite.

- The app’s error banner names the column and an example value when an
  upload has a malformed cell, instead of the generic “not valid track
  data”.

- `read_tracks(dialect = "sleap")` now reads an export whose `track`
  column is empty (SLEAP’s output for untracked, single-animal data) as
  one trajectory with id `"1"`. Previously such a file failed to load,
  or with a single row loaded with a missing id.

- An angle within floating-point error below zero (for example `-1e-17`,
  or a position whose `y` is float noise below 0) no longer makes
  [`tracks()`](https://johnkirwan.github.io/radiatR/reference/Tracks-class.md)
  fail. It wrapped to exactly `2*pi`, outside `[0, 2*pi)`; it now wraps
  to 0.

- The script downloaded from the Shiny app now derives headings in the
  frame the app used. In the app’s default relative frame the script
  omitted `coords`, so
  [`derive_headings()`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md)
  fell back to its `"absolute"` default and the script’s headings (and
  every statistic computed from them) differed from those shown in the
  app. The frame is now always written explicitly, as the one the app
  resolved: a dataset with no relative coordinates gets
  `coords = "absolute"`.

- The Shiny app’s method preview now follows the “Heading frame”
  control. It always illustrated the relative frame, so with the frame
  set to absolute (or with data that has no landmark frame) the preview
  showed a different frame from the one the Results derive headings in.

- The Shiny app no longer calls
  [`set.seed()`](https://rdrr.io/r/base/Random.html) on the shared R
  process when it computes Monte-Carlo uniformity p-values, so one
  session can no longer shift another’s random draws. The mean-direction
  bootstrap CI arc and the RANSAC heading rule (in both the method
  preview and the results) are now drawn under the same fixed seed, so
  they stay put across redraws, and the downloaded script sets that seed
  to reproduce them.

- [`circ_summary_headings()`](https://johnkirwan.github.io/radiatR/reference/circ_summary_headings.md)
  with the default `group_by = "id"` no longer errors (“numbers of
  columns of arguments do not match”) when any trial produced no
  heading; such trials are returned with `n = 0` and `NA` statistics.

- The `euroc_mav` loader dialect now reads the real EuRoC header
  (`#timestamp [ns]`, `p_RS_R_x [m]`, …), which it previously rejected
  with “required columns not found”. Its timestamp scaling is also
  fixed: epoch seconds were divided by 1e6 and microsecond stamps by
  1e9.

- The `csv_bbox` loader dialect now matches headers containing capitals
  (`XMin`, `Width`, …); these were previously mangled before matching.

- [`get_trial_limits()`](https://johnkirwan.github.io/radiatR/reference/get_trial_limits.md)
  (and so
  [`get_all_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_all_object_pos.md))
  no longer returns `NA` manifest metadata for the second and later
  trials of a video when the file table has fewer rows than the video
  has trials.

## radiatR 0.1.2

### Breaking changes

- The `ctrax` loader dialect and its optional `R.matlab` dependency have
  been removed. `.mat` files are no longer supported;
  `read_tracks(dialect = "ctrax")` now errors as an unknown dialect.
  Pre-computed orientation angles remain supported through the general
  `ellipse_axis` heading rule on any column.
- [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  now fails on non-finite coordinate rows by default, reporting the
  affected row numbers and ids; pass `on_invalid = "drop"` to drop them
  with a warning instead of the previous silent drop (**breaking**).

### Bug fixes

- The Shiny app no longer shows a previous analysis’s results after a
  new headings upload. In headings mode a failed file upload used to
  leave the earlier (e.g. example) headings in place, and the “example
  headings already loaded” shortcut could then display them as if they
  belonged to the file just selected. The upload now clears any prior
  analysis before reading and commits the parsed data only on success,
  and the shortcut to Results fires only for genuinely example-sourced
  headings.
- `c(Tracks, ...)` now rejects trajectory ids that collide across the
  objects being combined, and errors when the inputs carry conflicting
  `frame_rate`, `distance_scale`, `distance_unit`, or `normalize_xy`
  metadata (including one input having it set and another not).
  Previously these were silently merged, discarding one object’s
  calibration in favour of the other’s, and could interleave rows from
  two different trajectories that happened to share an id.
  Per-id/per-run metadata (`reference`, `trial_limits`,
  `sim_conditions`, `oob_points`, `oob_trials`) is now merged/summed
  across all combined inputs instead of only the first object’s being
  kept.
- [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  and
  [`read_tracks_dir()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_dir.md)
  now preserve trajectory identity when importing multiple files.
  Previously, reading a character vector of paths with no id column (the
  common case, relying on `id_from_filename = TRUE`) silently collapsed
  every file into a single `"(multiple)"` trajectory whose rows were
  then interleaved by time across files. Each file now yields its own
  trajectory keyed by the file-name stem. A new
  `id_collision = c("error", "namespace")` argument controls what
  happens when a real id value appears under more than one file:
  `"error"` (the default) stops with a message naming the colliding id
  and files, and `"namespace"` prefixes every id with its file stem
  (`file::id`). Multi-file imports with no id column and
  `id_from_filename = FALSE` now fail with a clear error instead of the
  silent collapse, and duplicate `(id, time)` keys produced by combining
  files are reported rather than passed through.
- `read_tracks(..., time_type = "frames", fps = )` no longer converts
  frame indices to seconds twice. The time column is now kept as a raw
  frame index and
  [`elapsed_seconds()`](https://johnkirwan.github.io/radiatR/reference/elapsed_seconds.md)
  (and everything built on it:
  [`track_duration()`](https://johnkirwan.github.io/radiatR/reference/track_duration.md),
  [`track_speed()`](https://johnkirwan.github.io/radiatR/reference/track_speed.md),
  velocity, and turning-rate) performs the frame -\> seconds division
  exactly once, on demand, using the stored
  [`frame_rate()`](https://johnkirwan.github.io/radiatR/reference/frame_rate.md).
  Previously the loader divided by `fps` immediately and then stored the
  same `fps` as the frame rate, so downstream kinematics divided by
  `fps` a second time – e.g. an expected 60 units/s speed came out as
  3600 units/s. Loading with `time_type = "seconds"` no longer stores a
  spurious frame rate either.
- [`wrappedcauchy_fit()`](https://johnkirwan.github.io/radiatR/reference/wrappedcauchy_fit.md)’s
  `convergence` column now matches its documentation.
  [`circular::mle.wrappedcauchy()`](https://rdrr.io/pkg/circular/man/mle.wrappedcauchy.html)
  reports convergence as a logical, which the previous code passed
  through [`as.integer()`](https://rdrr.io/r/base/integer.html),
  yielding `1` for a *converged* fit – the opposite of the documented
  “`0` = converged”. The column is now `0` when the fit converged and
  `1` when it did not, and the (incorrect) “optim return code” wording
  in the docs has been corrected.
- Between-group tests
  ([`test_mean_directions()`](https://johnkirwan.github.io/radiatR/reference/test_mean_directions.md),
  [`test_concentration()`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md),
  [`test_distributions()`](https://johnkirwan.github.io/radiatR/reference/test_distributions.md))
  now signal a structured `radiatR_nonestimable` condition (carrying
  method, group, n, and reason) for non-estimable inputs, so callers can
  distinguish expected non-estimable cases from bugs.

### App

- The app no longer assumes a capture rate of 30 fps. The frame-rate
  controls now start unset, and time/speed colouring, the kinematics
  plot and its downloads, and the generated kinematics code stay
  disabled until a positive finite frame rate is entered or carried by
  the loaded data. The app shows whether the active rate came from the
  data or was entered by the user, and downloaded kinematics code no
  longer emits a fabricated `set_frame_rate(ts, 30)` when no rate is
  set. Previously, uncalibrated numeric-frame data produced
  plausible-looking speed and turning-rate results based on the
  unsupported default.
- Ingest failures now show the original exception (with affected
  rows/ids) under “Technical details” and no longer suppress ingest
  warnings; the group-comparison table shows the non-estimable reason
  instead of a blank dash and re-raises unexpected errors; downloads are
  gated (`results_ok()`) so a failed or partial run cannot emit an
  apparently-valid result.

## radiatR 0.1.1

### New features

- New
  [`test_gof()`](https://johnkirwan.github.io/radiatR/reference/test_gof.md)
  tests whether a sample of headings fits a wrapped Cauchy distribution
  (Watson’s U^2 on the probability-integral transform, with a
  parametric-bootstrap p-value), closing the wrapped-Cauchy half of the
  non-von-Mises goodness-of-fit gap. Jones-Pewsey GOF remains deferred.
- [`test_uniformity()`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
  gains `test = "pycke"`, the Pycke (2010) omnibus uniformity test —
  like Hermans-Rasson, powerful against multimodal and asymmetric
  departures from uniformity, with a Monte-Carlo p-value.

### Bug fixes

- [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  no longer labels every trajectory by its id when a plot has many
  tracks. The automatic fallback to the id/group column now applies only
  when that column has at most 12 distinct values, so many-track plots
  (e.g. the 235-trial `cpunctatus`) render cleanly instead of being
  buried under overlapping trial-id labels. Pass `label_col` explicitly
  to force labelling regardless of track count.
- `radiate(clip_tracks = TRUE)` (the default) now also clips
  beyond-circumference overshoot in the absolute frame
  (`coords = "absolute"`), not just the relative frame. The relative
  frame is a rotation of the absolute one, so `rho` is identical in
  both; the previous relative-only restriction let `coords = "absolute"`
  plots (e.g. in the package vignette) draw tracks visibly outside the
  unit circle.
- [`get_all_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_all_object_pos.md)
  now reports out-of-bounds track points (radius \> 1) with a single
  aggregated message across the whole manifest, instead of one message
  per file.

### App

- Summary & stats now has a “Group comparison” card:
  `test_mean_directions`, `test_concentration`, and `test_distributions`
  run automatically across the selected Group-by column, showing whether
  groups differ in mean direction, concentration, or overall
  distribution (not just whether each is individually non-uniform).
- The app now exposes an explicit **Coordinates** control for the
  per-trajectory shape transform (`normalize_xy`), which was previously
  pinned on with no way to opt out. The transform fits each trajectory
  to the unit circle independently and preserves path shape, not
  bearings from a fixed arena origin; the control labels it as such
  rather than as a calibration. Downloaded figure, statistics, and
  kinematics scripts now emit the `normalize_xy` value the app actually
  loaded with, instead of silently falling back to the package default.

## radiatR 0.1.0

- First public release.
- Read and construct movement `Tracks`, with support for absolute and
  relative coordinate frames and frame-rate / time metadata.
- Plot circular data with
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md),
  including axial overlays, faceting, grouping, perimeter labelling and
  circular boxplots.
- Compute kinematics: speed, path sinuosity, per-row velocity angle and
  related trajectory summaries.
- Circular statistics: circular and concentration regression, model
  selection, and tests of mean direction, symmetry and unimodality.
- Interactive Shiny app (`inst/app`), also deployed at
  <https://johnkirwan.shinyapps.io/radiatR/>.
