# Changelog

## radiatR 0.1.3

### Breaking changes

- `derive_headings(carry = )` now stops when a carried column has the
  name of a column of the result (`id`, `time`, `heading`, …). It used
  to replace the derived values with the carried ones without a message,
  so a call that relied on that now fails with
  `radiatR_invalid_argument`: rename the column in the tracks first.

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
  read as missing when readr is installed; and in a multi-file import
  the bad cell surfaced as a type-clash error from vctrs.

  Files are now read with column types guessed from every row and with
  no digit-grouping mark, so `1,5` or `1,234.5` in a tab-separated file
  is malformed rather than read as 15 or 1234.5. Pass
  `read_opts = list(decimal = ",")` for decimal commas. Where the
  decimal mark is a comma, one bad cell names that cell only, not every
  `1,5` in its column, and a value holding a `.` is malformed in turn,
  since `1.234` could as well be 1234. A semicolon-separated file is
  taken to use decimal commas unless its first lines show decimal points
  and no decimal comma: a value such as `1.5` or `0.125`, which cannot
  be digit grouping. For a file
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  reads itself, `read_opts = list(decimal = )` settles it either way.

  The check covers the columns
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  converts itself and the time column of every dialect but ANY-maze. An
  error raised inside a dialect does not yet name the file or the source
  column, and counts rows after the dialect’s own filtering and
  reshaping. Coordinates that a dialect converts itself (ANY-maze,
  EthoVision, DeepLabCut, SLEAP) are not yet checked this way.

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
  ignored without notice, and a missing x or y column gave an error that
  did not name it. A `regex_mapping` pattern (see
  [`read_tracks_format()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_format.md))
  that matches no column is the same error, naming the role and the
  pattern.

- The `validate` argument of
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  is deprecated. It never skipped anything, because `Tracks` invariants
  are checked whenever an object is built; any value but `TRUE` now
  warns, and the argument will be removed.

- The `trackmate` dialect stops with a `radiatR_unlinked_rows` error (a
  `radiatR_invalid_rows`) when a file holds spots that belong to no
  track (`TRACK_ID` `None` or blank, as in an “all spots” export). They
  used to be pooled into one trajectory called `None`. Pass
  `dialect_args = list(unlinked = "drop")` to leave them out, with a
  `radiatR_dropped_rows` warning. Without a units row saying
  `POSITION_T` is in seconds, the dialect now takes `FRAME` as the time
  column rather than `POSITION_T`.

- Built-in loader dialects are now kept only for tracking software that
  is widely used and still developed. Eleven dialects are removed:
  `tracktor`, `boris_xy`, `dtrack`, `motchallenge`, `csv_bbox`,
  `wide_prefix_xy`, `tum_traj`, `euroc_mav`, `gpx`, `geojson_linestring`
  and `nmea_gprmc`. `read_tracks(dialect = )` with one of these names
  stops with a `radiatR_removed_dialect` error. To keep reading such
  files, register your own loader with
  [`register_loader_dialect()`](https://johnkirwan.github.io/radiatR/reference/register_loader_dialect.md);
  a loader registered under a removed name works as before. The bundled
  `tracktor_example.csv` is removed with its dialect, and `xml2` is no
  longer a suggested package.

- `dtrack_read()` is removed.
  [`vignette("loaders")`](https://johnkirwan.github.io/radiatR/articles/loaders.md)
  shows a dtrack loader of a dozen lines, registered with
  [`register_loader_dialect()`](https://johnkirwan.github.io/radiatR/reference/register_loader_dialect.md),
  that reads the bundled millipede track files.

- A dialect is now the only reader of its input.
  `read_tracks(path, dialect = )` used to parse the file with the
  generic reader first and discard the result, which printed
  repaired-name messages for headerless files and could fail before the
  dialect ran. With several paths the dialect is called once per file; a
  custom dialect that expected the whole vector of paths must be changed
  to take one path. `read_tracks("<dialect name>")`, which passed the
  name itself to the dialect as input, is no longer accepted.

### Changes

- New vignette
  [`vignette("agent-guide")`](https://johnkirwan.github.io/radiatR/articles/agent-guide.md):
  the angle conventions, calibration rules, failure behaviour and common
  pitfalls an AI agent (or anyone scripting radiatR) needs. `AGENTS.md`
  at the repository root points to it. The
  [`circ_summarise()`](https://johnkirwan.github.io/radiatR/reference/circ_summarise.md)
  help now states that `mean_dir_deg` is in the display convention
  rather than a plain conversion of `mean_dir`, and the
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  help that `keep` adds to the mapped columns (it said `NULL` keeps
  every column; unmapped columns are dropped).

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

- `read_tracks(tz = )` now applies to every timestamp without a time
  zone in a delimited or Excel file that
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  reads itself. A timestamp the file reader typed (`YYYY-MM-DD HH:MM:SS`
  with readr or data.table installed, or an Excel date-time cell) was
  read as UTC whatever `tz` was, while one left as text
  (`YYYY/MM/DD HH:MM:SS`) was read in `tz`: the same wall-clock time
  came out hours apart, between two files of one import or between two
  machines with different readers installed. A timestamp marked `Z` is
  still UTC, and a bare date beside such timestamps is midnight in `tz`.
  A wall-clock time that does not exist in `tz` (the hour skipped when
  clocks go forward) stops the load instead of being moved an hour on.
  `tz` does not yet reach a file read by a dialect, or a Parquet or
  Feather file.

- `read_tracks(format = )` honours the format. It passed all of its own
  defaults on to
  [`read_tracks_format()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_format.md),
  where they overrode the format: the format’s `mapping` and
  `regex_mapping` were discarded and the columns guessed from their
  names (a format mapping x to `pos_x` read a column named `x` if the
  file had one), and its angle unit, time type, time zone and frame rate
  gave way to the defaults, so degrees were read as radians. Now only
  the arguments the caller supplies override the format, `mapping` role
  by role, and `read_opts` is passed on.

- [`circ_summarise()`](https://johnkirwan.github.io/radiatR/reference/circ_summarise.md)
  and
  [`circ_summary()`](https://johnkirwan.github.io/radiatR/reference/circ_summary.md)
  on a `Tracks` object now return the von Mises concentration `kappa`;
  it was always `NA`. The estimator was looked up as
  `circular::est.kappa`, a function the circular package does not have,
  so every caller silently got its fallback instead. `kappa` is now
  computed directly as the maximum-likelihood estimate from the mean
  resultant length (the value
  [`circular::mle.vonmises()`](https://rdrr.io/pkg/circular/man/mle.vonmises.html)
  gives), weighted where weights are in use. Both report `NA` for fewer
  than three valid angles (as
  [`circ_summarise()`](https://johnkirwan.github.io/radiatR/reference/circ_summarise.md)
  already did) and `Inf` when all angles coincide, where the estimate is
  unbounded.
  [`circ_summary_headings()`](https://johnkirwan.github.io/radiatR/reference/circ_summary_headings.md),
  [`boot_kappa_ci()`](https://johnkirwan.github.io/radiatR/reference/boot_kappa_ci.md)
  and the starting value in
  [`concentration_regression()`](https://johnkirwan.github.io/radiatR/reference/concentration_regression.md)
  already used this estimate and are unchanged.

- The `trackmate`, `toxtrac`, `idtrackerai_wide` and `trex` dialects now
  read the files those programs write. Checked against real exports:

  - TrackMate 7 spot tables load; their three extra header rows used to
    be reported as malformed values. They are dropped only when
    recognised as names and units rows.
  - ToxTrac `Tracking_RealSpace.txt` (`Time (sec)`, `Track`,
    `Pos. X (mm)`) and the headerless `Tracking.txt` (6 or 11 columns)
    load; both used to stop with “required columns not found”. The id is
    `<arena>_<track>`.
  - idtracker.ai `trajectories.csv` (`seconds,x1,y1,...`) loads; it used
    to stop with “no x\_/y\_ pairs found”. An x column with no y column
    is now an error, as is an `ids` entry the file does not have.
  - TRex files that write `frame` twice use it as the time; they used to
    fall back to the `time` column. These dialects check their source
    columns strictly, so a malformed value is reported by the header as
    written, the row of the file and the file name.

- Dialects now state the unit of their time column, so seconds are no
  longer taken for frames when an `fps` is supplied (elapsed time and
  speeds were divided by it). See “Time units from a dialect” in
  [`?read_tracks`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md).
  An `fps` given for a time column in seconds is not used, with a
  message; an explicit `time_type` that contradicts the dialect is a
  `radiatR_time_type_mismatch` error.

- App: tracking software is now recognised from the file’s header for
  TrackMate, ToxTrac and idtracker.ai exports. The header was reduced
  before it was lower-cased, so no header containing a capital letter
  ever matched. A file whose time is in seconds no longer needs a frame
  rate for the kinematics.

- Numeric time loaded as seconds is now treated as seconds. A `Tracks`
  read with `read_tracks(time_type = "seconds")`, or from a duration
  (`difftime`) column, was handled as a frame index:
  [`elapsed_seconds()`](https://johnkirwan.github.io/radiatR/reference/elapsed_seconds.md),
  [`track_speed()`](https://johnkirwan.github.io/radiatR/reference/track_speed.md)
  and the other kinematics stopped and asked for a frame rate, and once
  one was set they divided the seconds by it. Such a `Tracks` now
  records `time_unit = "seconds"` in its metadata and needs no frame
  rate; a frame rate set on it is kept but not applied.
  [`c()`](https://rdrr.io/r/base/c.html) refuses to combine `Tracks`
  whose time units differ. Numeric time of unstated unit is still a
  frame index, and the error that asks for a frame rate now mentions
  `time_type = "seconds"`.

- [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  and
  [`read_tracks_dir()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_dir.md)
  now work with `dialect` and several files. They used to fail with
  `is.data.frame(df) is not TRUE`. Each file is read by the dialect and
  coerced on its own, so a malformed value is reported against its own
  file and row; ids repeated across files follow `id_collision`, and a
  dialect that returns no id column gets one id per file. A failure
  inside the dialect is re-raised as a `radiatR_dialect_error` naming
  the file, unless it is already a radiatR condition.

- [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
  names the file(s) that do not exist instead of reporting that `x` must
  be a data frame or path.

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
