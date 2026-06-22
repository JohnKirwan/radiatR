# Changelog

## radiatR 0.4.1

### Breaking changes

- The central S4 class `TrajSet` is renamed to **`Tracks`**, its
  constructor to the lowercase
  [`tracks()`](https://johnkirwan.github.io/radiatR/reference/Tracks-class.md),
  and the loader functions to snake_case: `TrajSet_read()` -\>
  [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md),
  `TrajSet_read_dir()` -\>
  [`read_tracks_dir()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_dir.md),
  `TrajSet_read_format()` -\>
  [`read_tracks_format()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_format.md),
  `TrajSet_load_manifest()` -\>
  [`load_manifest()`](https://johnkirwan.github.io/radiatR/reference/load_manifest.md).
  No behaviour, slot, or output changed. Bundled `cpunctatus` is now a
  `Tracks` object.

### Infrastructure

- `headings_frame` is now a tibble subclass whose class and display
  convention survive dplyr verbs
  (`mutate`/`filter`/`select`/`bind_rows`) via the `dplyr_reconstruct`
  contract, so the orientation no longer leaks. New exported
  [`hf_display()`](https://johnkirwan.github.io/radiatR/reference/hf_accessors.md)/[`hf_heading_col()`](https://johnkirwan.github.io/radiatR/reference/hf_accessors.md)/[`hf_colour_col()`](https://johnkirwan.github.io/radiatR/reference/hf_accessors.md)/[`hf_coords()`](https://johnkirwan.github.io/radiatR/reference/hf_accessors.md)
  accessors read the metadata (and work on a plain data frame).
  [`derive_headings()`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md)
  now returns a `headings_frame`; orientation is consolidated into a
  single `display` attribute (the vestigial
  `display_convention`/`angle_convention` strings are removed). See
  [`vignette("design")`](https://johnkirwan.github.io/radiatR/articles/design.md).

### Statistics

- New
  [`circ_regression()`](https://johnkirwan.github.io/radiatR/reference/circ_regression.md)
  fits Fisher-Lee circular-linear regression of a heading on linear
  covariates (formula interface over
  [`circular::lm.circular`](https://rdrr.io/pkg/circular/man/lm.circular.html)),
  with [`summary()`](https://rdrr.io/r/base/summary.html) (tidy
  coefficients),
  [`predict()`](https://rdrr.io/r/stats/predict.html)/[`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  and [`print()`](https://rdrr.io/r/base/print.html) methods.
  [`simulate_tracks()`](https://johnkirwan.github.io/radiatR/reference/simulate_tracks.md)
  gains a per-condition `mean_slope` so the predictor shifts the mean
  heading, enabling end-to-end recovery.

- New
  [`fitted_directions()`](https://johnkirwan.github.io/radiatR/reference/fitted_directions.md)
  turns a
  [`circ_regression()`](https://johnkirwan.github.io/radiatR/reference/circ_regression.md)
  fit into mean-direction arrows for
  [`add_circ_mean()`](https://johnkirwan.github.io/radiatR/reference/add_circ_mean.md),
  so the fitted heading-vs-covariate relationship can be drawn on the
  circular panel, colour-graded by the predictor.

- [`simulate_tracks()`](https://johnkirwan.github.io/radiatR/reference/simulate_tracks.md)
  is now modality- and shape-aware. Per-condition `modality` (`uniform`
  / `unimodal` / `axial` / `multimodal` with `n_modes`) sets the
  distribution of per-track headings, and `track_shape` (`directed` or
  `oscillatory`) sets within-track geometry — `oscillatory` produces
  back-and-forth axial tracks whose line-width (`line_width`) is
  independent of tortuosity, so the per-track axial methods recover the
  axis at default settings. The ground truth is recorded in new output
  columns and in the `TrajSet` `meta$sim_conditions`. The default output
  is unchanged.

### Track metrics

- The bundled `cpunctatus` example now carries its true capture rate
  (0.2 fps, 1 frame / 5 s), so its kinematics plots read real seconds
  out of the box without a manual
  [`set_frame_rate()`](https://johnkirwan.github.io/radiatR/reference/frame_rate.md).

- [`track_speed()`](https://johnkirwan.github.io/radiatR/reference/track_speed.md)
  and
  [`step_speed()`](https://johnkirwan.github.io/radiatR/reference/step_speed.md)
  report trajectory speed in real units (distance per second), using the
  track’s frame rate / timestamps. With the default unit-arena
  coordinates that is arena-units (radii) per second.

- [`instantaneous_speed()`](https://johnkirwan.github.io/radiatR/reference/instantaneous_speed.md)
  gives per-observation speed (the per-row sibling of
  [`elapsed_seconds()`](https://johnkirwan.github.io/radiatR/reference/elapsed_seconds.md)),
  and `radiate(track_colour = "speed")` colours each path by it.
  Arena-units per second with the default coordinates.

- [`velocity_vector()`](https://johnkirwan.github.io/radiatR/reference/velocity_vector.md)
  gives per-observation velocity components (`vx`, `vy`;
  distance-calibrated when a scale is set), and
  [`angular_velocity()`](https://johnkirwan.github.io/radiatR/reference/angular_velocity.md)
  the signed turning rate (counter-clockwise positive; radians or
  degrees per second).

- New
  [`velocity_angle()`](https://johnkirwan.github.io/radiatR/reference/velocity_angle.md)
  returns the per-row movement direction (the heading of the velocity
  vector) in `[0, 2*pi)`, the directional sibling of
  [`instantaneous_speed()`](https://johnkirwan.github.io/radiatR/reference/instantaneous_speed.md).
  It shares the heading convention, so it feeds straight into
  [`circ_summary()`](https://johnkirwan.github.io/radiatR/reference/circ_summary.md)
  /
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md).

- Per-track summaries:
  [`track_velocity()`](https://johnkirwan.github.io/radiatR/reference/track_velocity.md)
  (net average velocity vector, distance-calibrated) and
  [`track_turning()`](https://johnkirwan.github.io/radiatR/reference/track_turning.md)
  (turning-rate summary – `mean_abs` magnitude by default, plus
  `mean`/`max_abs`/`median_abs`; radians or degrees).

- Optional distance calibration:
  [`set_distance_scale()`](https://johnkirwan.github.io/radiatR/reference/distance_scale.md)
  /
  [`calibrate_distance()`](https://johnkirwan.github.io/radiatR/reference/distance_scale.md)
  attach a physical scale + unit, so
  [`track_length()`](https://johnkirwan.github.io/radiatR/reference/track_length.md),
  [`track_speed()`](https://johnkirwan.github.io/radiatR/reference/track_speed.md),
  [`instantaneous_speed()`](https://johnkirwan.github.io/radiatR/reference/instantaneous_speed.md)
  and `radiate(track_colour = "speed")` report real units (e.g. mm,
  mm/s). Unset, everything stays in arena/coordinate units.

### Visualisation

- New
  [`perimeter_labs()`](https://johnkirwan.github.io/radiatR/reference/perimeter_labs.md)
  labels a radial plot’s circumference in domain units, with built-in
  scales
  [`scale_cardinal()`](https://johnkirwan.github.io/radiatR/reference/scale_cardinal.md)
  (compass),
  [`scale_clock()`](https://johnkirwan.github.io/radiatR/reference/scale_clock.md)
  (hours),
  [`scale_months()`](https://johnkirwan.github.io/radiatR/reference/scale_months.md),
  and
  [`scale_seconds()`](https://johnkirwan.github.io/radiatR/reference/scale_seconds.md).
  `radiate(angle_labels = "cardinal" | "hours" | "months" | "seconds")`
  selects them directly and aligns the tick count to the scale.

- [`plot_profile()`](https://johnkirwan.github.io/radiatR/reference/plot_profile.md)
  draws a non-circular kinematics profile – instantaneous speed or
  turning rate against elapsed time, one line per track (the ggplot
  sibling of
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)).
  Needs a frame rate; distance-calibrated when a scale is set.

- [`plot_profile()`](https://johnkirwan.github.io/radiatR/reference/plot_profile.md)
  gains `metric = "direction"`, plotting the per-observation movement
  direction
  ([`velocity_angle()`](https://johnkirwan.github.io/radiatR/reference/velocity_angle.md))
  over elapsed time as points (direction is circular, so it is drawn as
  points rather than a line).

- New
  [`plot_speed_direction()`](https://johnkirwan.github.io/radiatR/reference/plot_speed_direction.md)
  scatters each observation’s speed against its movement direction
  ([`velocity_angle()`](https://johnkirwan.github.io/radiatR/reference/velocity_angle.md)).
  Its speed axis – and
  [`plot_profile()`](https://johnkirwan.github.io/radiatR/reference/plot_profile.md)’s
  for `metric = "speed"` – is robustly clipped by default (`max_speed`,
  the 99.5% quantile) so single-frame tracking artifacts no longer crush
  the display; off-scale points are reported in a caption.
  `max_speed = Inf` restores the raw range.

- New
  [`plot_speed_histogram()`](https://johnkirwan.github.io/radiatR/reference/plot_speed_histogram.md)
  shows the pooled distribution of step speeds with the median and
  coefficient of variation (CV) annotated, and the same robust speed
  clipping (`max_speed`) as the other kinematics plots.

- American spellings are now accepted throughout: every `colour...`
  argument and the `assign_colour_*` / `cycle_colours` / `hf_colour_col`
  functions have `color...` aliases (British remains canonical;
  supplying both errors).

- Tracks can carry a capture **frame rate**:
  [`set_frame_rate()`](https://johnkirwan.github.io/radiatR/reference/frame_rate.md)
  /
  [`frame_rate()`](https://johnkirwan.github.io/radiatR/reference/frame_rate.md)
  store fps in the object,
  [`elapsed_seconds()`](https://johnkirwan.github.io/radiatR/reference/elapsed_seconds.md)
  and
  [`track_duration()`](https://johnkirwan.github.io/radiatR/reference/track_duration.md)
  report real time, and `radiate(track_colour = "time")` colours each
  path by elapsed time (POSIXct time works without a frame rate).
  `simulate_tracks(frame_rate=)` sets it.

- [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  gains `track_colour = "sequence"` to colour each trajectory path by
  its point’s position from start to finish (a per-track normalized
  gradient with a continuous “start -\> finish” colourbar). The Shiny
  app’s Results figure gains a matching **Track colour** selector.

- New
  [`circ_boxplot_stats()`](https://johnkirwan.github.io/radiatR/reference/circ_boxplot_stats.md)
  and
  [`add_circular_boxplot()`](https://johnkirwan.github.io/radiatR/reference/add_circular_boxplot.md)
  implement the Tukey-like circular boxplot of Buttarazzi, Pandolfo &
  Porzio (2018) for circular and axial heading data: a depth-ranked box
  around the circular median, a closed-form von Mises fence multiplier,
  whiskers, and far-out values. Validated against the bpDir reference
  implementation.

### Shiny app

- The Kinematics sub-tab gains a **Track** selector, defaulting to a
  single track (the per-track view) with an “All tracks (overlay)”
  option, so many-track datasets no longer render as overlapping
  spaghetti. The app also adopts a loaded `Tracks`’s own capture frame
  rate when one is set.

- The Summary & stats sub-tab now offers reproducible R code for the
  analysis (`circ_summarise`, `test_uniformity`, `circ_model_select`,
  `straightness_index`) to copy or download, alongside the figure code
  on the Circular plots tab.

- The Results step is reorganised into **Circular plots** and **Summary
  & stats** sub-tabs, each with its own options sidebar (the circular
  figure + its reproduce/export controls on one, the summary and
  model-selection tables on the other).

- The Results figure’s **Track colour** selector gains “By elapsed
  time”, with a frame-rate (fps) input; an unset/invalid frame rate
  falls back to sequence colouring with a note.

- The Results figure’s **Track colour** selector gains “By speed”
  (instantaneous speed), sharing the frame-rate input with “By elapsed
  time”; an unset/invalid frame rate falls back to sequence colouring
  with a note.

- The Results figure gains a **Circular boxplot** overlay toggle
  (directional and axial), wired through the figure-code export so the
  emitted script reproduces it. The layer toggles now reflow into two
  columns when space allows, and a note explains when the boxplot is not
  drawn (near-uniform data or too few points).

## radiatR 0.4.0

### Statistics

- The circular-statistics core gains an `axial = TRUE` option for
  bidirectional (mod-pi) data:
  [`circ_summary()`](https://johnkirwan.github.io/radiatR/reference/circ_summary.md),
  [`circ_summarise()`](https://johnkirwan.github.io/radiatR/reference/circ_summarise.md),
  [`circ_dispersion()`](https://johnkirwan.github.io/radiatR/reference/circ_dispersion.md),
  [`test_uniformity()`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md),
  [`test_mean_directions()`](https://johnkirwan.github.io/radiatR/reference/test_mean_directions.md),
  [`test_concentration()`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md),
  [`compute_circ_mean()`](https://johnkirwan.github.io/radiatR/reference/compute_circ_mean.md),
  and
  [`compute_circ_interval()`](https://johnkirwan.github.io/radiatR/reference/compute_circ_interval.md)
  compute the axial mean (an axis in \[0, 180)), axial resultant length,
  and the corresponding tests via the standard angle-doubling method.
  The directional default (`axial = FALSE`) is unchanged.

- New
  [`circ_model_select()`](https://johnkirwan.github.io/radiatR/reference/circ_model_select.md)
  ranks three candidate circular models – uniform, unimodal von Mises,
  and axial (symmetric bimodal) von Mises – by AICc, with Akaike
  weights, to identify whether a heading sample is best described as
  uniform, directionally, or axially oriented. Reuses
  [`vonmises_fit()`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md)
  and `circular`’s densities; no new dependency.

- The app’s statistical summary gains an always-on **Rao spacing**
  omnibus row alongside Rayleigh, flagging departures from uniformity
  (multimodal/clustered patterns) that the focused Rayleigh test misses.
  Rao spacing yields only a coarse significance level, so it is shown as
  a bracket (`< 0.05`, `> 0.10`, …), computed on raw angles regardless
  of the Data model.

- [`test_uniformity()`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
  gains `test = "hermans_rasson"`, the Hermans-Rasson omnibus test of
  circular uniformity (Landler, Ruxton & Malkemper 2019) — far more
  powerful than Rayleigh against multimodal / non-symmetric
  alternatives. Its p-value is by Monte-Carlo simulation (`n_sim`,
  default 9999; seed via
  [`set.seed()`](https://rdrr.io/r/base/Random.html)), and it honours
  `group_col`, `p_adjust`, and `axial`.

- [`vonmises_fit()`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
  [`wrappedcauchy_fit()`](https://johnkirwan.github.io/radiatR/reference/wrappedcauchy_fit.md),
  [`add_vonmises_density()`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md),
  and
  [`add_wrappedcauchy_density()`](https://johnkirwan.github.io/radiatR/reference/add_wrappedcauchy_density.md)
  gain `axial = TRUE` to fit and draw axial (bidirectional, mod-pi)
  distributions via the doubled-angle method: the mean is reported as an
  axis in \[0, pi) (with the concentration estimated in the
  doubled-angle frame) and the density curve shows two equal antipodal
  peaks.

### Plotting

- The mean-direction and interval overlays gain `axial = TRUE` for
  bidirectional data:
  [`add_circ_mean()`](https://johnkirwan.github.io/radiatR/reference/add_circ_mean.md)/[`add_heading_arrow()`](https://johnkirwan.github.io/radiatR/reference/add_heading_arrow.md)
  draw a double-headed axis through the centre,
  [`add_circ_interval()`](https://johnkirwan.github.io/radiatR/reference/add_circ_interval.md)/[`add_heading_interval()`](https://johnkirwan.github.io/radiatR/reference/add_heading_interval.md)
  draw the CI at both poles, and
  [`add_critical_v_line()`](https://johnkirwan.github.io/radiatR/reference/add_critical_v_line.md)
  mirrors its boundary.

- The empirical individual-data layers gain `axial = TRUE` to draw
  bidirectional (period-pi) data at both ends:
  [`add_heading_points()`](https://johnkirwan.github.io/radiatR/reference/add_heading_points.md),
  [`add_heading_vectors()`](https://johnkirwan.github.io/radiatR/reference/add_heading_vectors.md),
  and
  [`add_stacked_headings()`](https://johnkirwan.github.io/radiatR/reference/add_stacked_headings.md)
  plot each datum at both poles, and the empirical densities
  ([`compute_circular_density()`](https://johnkirwan.github.io/radiatR/reference/compute_circular_density.md)
  /
  [`add_heading_density()`](https://johnkirwan.github.io/radiatR/reference/add_heading_density.md),
  [`add_circular_kde()`](https://johnkirwan.github.io/radiatR/reference/add_circular_kde.md),
  [`add_angle_rose()`](https://johnkirwan.github.io/radiatR/reference/add_angle_rose.md))
  estimate on the both-ends-augmented sample. In the Shiny app the
  headings-only “Axial” checkbox becomes a “Data model” (Directional /
  Axial) selector in the Configure step that applies to both input
  types, soft-syncs from inherently axial heading methods (movement-axis
  / PCA / RANSAC), mirrors the whole figure, and relabels the summary’s
  Rayleigh test as “Rayleigh (axial)”.

### Coordinates

- The per-trajectory reference direction is now first-class:
  [`reference()`](https://johnkirwan.github.io/radiatR/reference/reference.md)
  reads it and
  [`set_reference()`](https://johnkirwan.github.io/radiatR/reference/set_reference.md)
  changes it, re-deriving the relative frame
  (`rel_theta`/`rel_x`/`rel_y`) consistently so it cannot drift. New
  exported
  [`derive_coords()`](https://johnkirwan.github.io/radiatR/reference/derive_coords.md)
  is the single source of the unit-circle -\> polar/relative math (also
  used internally by the loader mapping). For a reference-frame offset,
  prefer
  [`set_reference()`](https://johnkirwan.github.io/radiatR/reference/set_reference.md)
  over a manual
  [`apply_transform()`](https://johnkirwan.github.io/radiatR/reference/apply_transform.md).
- `as.data.frame(ts)` now returns the full frame even when derived
  coordinate columns
  (`rel_theta`/`rel_x`/`rel_y`/`radius`/`trans_rho`/`abs_theta`) are not
  stored, computing any missing ones from the canonical position and the
  trajectory reference. Internal analyses read through it, so they no
  longer depend on those columns being physically present.
- `TrajSet` objects built by the loader pipeline no longer store the
  redundant derived coordinate columns
  (`trans_rho`/`abs_theta`/`rel_x`/`rel_y`), and no construction path
  stores a `rho`/`radius` column; `as.data.frame(ts)` computes them on
  demand from the canonical position and the trajectory reference. The
  canonical `trans_x`/`trans_y`/`raw_*` and the analysis angle
  `rel_theta` are kept. Storage of the derived frame is now an
  implementation detail; existing data (e.g. the bundled `cpunctatus`)
  that still carries the columns behaves identically.

### Terminology

- The exported `animal_track` parameter of
  [`get_trial_limits()`](https://johnkirwan.github.io/radiatR/reference/get_trial_limits.md),
  [`get_tracked_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_tracked_object_pos.md),
  and
  [`get_all_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_all_object_pos.md)
  is renamed to `track` (breaking). Residual animal-specific framing in
  the documentation is generalised to “subject”. Example references to
  the bundled millipede/urchin datasets and the column-name guesses used
  by importers are unchanged.

### Loaders

- `TrajSet_read()` and `TrajSet_read_dir()` now auto-detect the field
  separator (comma, semicolon, tab, or pipe) and decimal mark from file
  *content* rather than the extension, so semicolon-separated and
  European decimal-comma exports, and tab-delimited files saved as
  `.csv`, load correctly. A new `read_opts` argument (`delim`,
  `decimal`, `sheet`) overrides detection when needed. Excel workbooks
  (`.xlsx`/`.xls`) can be read directly (first sheet by default, or
  `read_opts$sheet`), via the soft-dependency `readxl`. In the Shiny app
  the first-rows preview now reflects the actual detected parse, and a
  new Delimiter control lets you correct a misdetected separator in
  place.
- `TrajSet_read()` now loads single-track CSVs with custom column names:
  column guessing is case-insensitive and matches separator-suffixed
  coordinates (e.g. `Track1_X`/`Track1_Y`); a missing id column is
  treated as a single trajectory and a missing time/frame column falls
  back to row order (each with a message), and rows with non-finite
  coordinates are dropped. New exported
  [`guess_columns()`](https://johnkirwan.github.io/radiatR/reference/guess_columns.md)
  reports the guessed role of each column. The Shiny app shows
  pre-filled X/Y/Time/ID dropdowns for Generic CSV uploads.
- [`load_tracks()`](https://johnkirwan.github.io/radiatR/reference/load_tracks.md)
  and
  [`get_trial_limits()`](https://johnkirwan.github.io/radiatR/reference/get_trial_limits.md)
  now carry every column from the manifest / `file_tbl` onto trials (and
  into the resulting `TrajSet`), rather than a fixed
  `arc`/`type`/`obstacle`/`id` list left over from one experiment.
  Structural file-reference columns (`basename`/`landmark`/`track`) are
  excluded. Use `load_tracks2(colnames = )` to rename or restrict the
  columns carried.

### Transformations

- New
  [`apply_transform()`](https://johnkirwan.github.io/radiatR/reference/apply_transform.md)
  applies a user-supplied transformation to a loaded `TrajSet`
  (per-trajectory or whole-frame), returning a modified `TrajSet` and
  recording the step in its `transform_history`. Two worked recipes are
  documented: an edge-referenced -\> centre-referenced reference offset
  (a per-trial offset read from stimulus-width metadata) and a
  polarization direction -\> axis (angle-doubling) remap.
- Removed the hard-coded 1st-Hermitian (`type == "Herm"`)
  reference-angle adjustment and the now-unused `midpoint` argument from
  [`get_trial_limits()`](https://johnkirwan.github.io/radiatR/reference/get_trial_limits.md);
  experiment-specific reference corrections are now expressed via
  [`apply_transform()`](https://johnkirwan.github.io/radiatR/reference/apply_transform.md).
  The bundled `cpunctatus` data is unchanged.
- The `TrajSet` row-order validity check no longer requires a global
  sort by id (which depended on string collation / locale); it now
  checks the actual invariant – each trajectory’s rows form a
  contiguous, time-ordered block.

### Documentation

- Repositioned the package documentation from “animal movement in
  circular arenas” toward the general case – analysis and visualisation
  of headings and trajectories in circular space – reflecting that
  angles can be supplied directly (not only reconstructed from tracking
  data). Animal-movement tracking remains a primary supported
  application. No code or API changes.

### Statistics and headings

- New `velocity_axis` heading rule:
  `derive_headings(ts, rule = "velocity_axis")` reduces each trajectory
  to its movement axis (the axial mean of step directions, in \[0,
  180)), the bidirectional counterpart of `velocity_mean`. Pair it with
  the axial statistics and plots (`axial = TRUE`). Available in the
  Shiny app’s heading-method dropdown.
- [`derive_headings()`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md)
  gains an `on_missing` argument (`"warn"` (default), `"error"`, or
  `"quiet"`) and now reports how many trials produced no heading. Failed
  trials are always retained as `NA` rows, and the result carries
  `n_total`, `n_missing`, and `missing_ids` attributes. Rule-based
  failures are often non-random and can bias circular statistics, so
  they are surfaced by default.
- [`circ_summarise()`](https://johnkirwan.github.io/radiatR/reference/circ_summarise.md)
  gains requestable `n_total` and `n_missing` stats so the
  excluded-trial denominator can travel with the summary (default output
  unchanged).

### Shiny app

- The heading-method dropdown is now grouped into **Directional** and
  **Axial** methods, and a contextual note flags a mismatch with the
  Data model – folding a directional heading to an axis (informational),
  or analysing an axial method as Directional (a warning, since that
  biases the statistics).

- The Summary panel gains a selectable **omnibus test** (Rao spacing or
  Hermans-Rasson) and a **model-selection readout** – a “Best model”
  column plus a full AICc comparison card
  ([`circ_model_select()`](https://johnkirwan.github.io/radiatR/reference/circ_model_select.md):
  uniform / unimodal / axial with Akaike weights) – so the data, not
  just a declared model, indicates whether a sample is uniform,
  directional, or axial.

- The Results screen now shows an attrition banner when trials were
  excluded – loud, with a bias caveat, for headings derived from
  trajectories, and a neutral note for provided headings. The summary
  table gains an `Excluded` column.

### Plotting

- [`radiate.headings_frame()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  now draws the same theme-responsive radial chrome as
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  for trajectories (circumference, ticks, degree labels, radial grid,
  origin) instead of a fixed bold circle, and gains `show_markers`,
  `colour_col`, `legend`, `display`, `grid`, `grid_colour`,
  `circumference`, and `origin` arguments. `show_markers = FALSE`
  returns the radial frame only, so callers can layer their own marker
  and statistic overlays.

### Shiny app

- The app now accepts a dataset of pre-computed headings (one angle per
  trial) in place of trajectories. Choose the input type on the upload
  screen, map the angle column, units, and convention (unit-circle or
  compass) plus an optional grouping column, and the Results figure
  draws the stacked dots / points, mean arrow, confidence interval, and
  Rayleigh / V-test from the angles directly. A “Load example headings”
  link derives a demonstration set from the bundled millipede data.

## radiatR 0.3.1

### Plotting

- The fallback circumference (drawn on grid-less themes such as `void`)
  is now drawn at `theme_classic`’s axis-line weight (0.5) rather than a
  bold 1.2, so it reads like an axis rather than a heavy frame.
- The radial axis chrome – the circumference (unit circle), angular tick
  marks, and degree labels – now takes its styling from the base theme
  chosen via `radiate(theme = )`, mapped from that theme’s `axis.line` /
  `axis.ticks` / `axis.text`. Picking a different theme restyles them
  (colour, line width, label size and family), and colour is kept
  legible against a dark panel.
  [`add_ticks()`](https://johnkirwan.github.io/radiatR/reference/add_ticks.md)
  gains `colour`, `linewidth`, `length`, and `n`;
  [`degree_labs()`](https://johnkirwan.github.io/radiatR/reference/degree_labs.md)
  gains `size` and `family`;
  [`add_circ()`](https://johnkirwan.github.io/radiatR/reference/add_circ.md)
  gains `linetype` and `colour`/`linewidth` aliases. (Styling follows
  the theme passed to
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md),
  not a `+ theme()` added afterwards, since the chrome is drawn as
  layers.)
- On grid-bearing themes the radial grid’s outer ring now reaches the
  unit circle, so it marks the boundary; the separate circumference is
  drawn only as a fallback where no grid delineates it (control with
  `radiate(circumference = )`). A consequence: on light grid themes
  (`minimal`, `bw`, `light`) the boundary is now a subtle grid-coloured
  ring rather than a bold dark circle – add a bold ring back with
  `+ add_circ(colour = "black", linewidth = 1.2)` if desired.
- `radiate(origin = )` now defaults to `FALSE` (the centre point is
  opt-in on all themes) and, when drawn, takes the theme’s axis ink
  colour.

### Shiny app

- Stacked heading dots are spaced a little further apart and start a
  little further inward from the circumference (the `step` and
  `start_sep` defaults the app uses).

## radiatR 0.3.0

### Plotting

- **Behaviour change:** for grid-bearing themes (`bw`, `grey`, `light`,
  `dark`, `linedraw`, `minimal`),
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  now replaces the Cartesian grid behind the unit circle with a
  theme-styled *radial* grid – a circular disc in the theme’s panel
  colour, quadrant crosshairs and a ring at 0.5 (major), and 45 degree
  diagonals plus rings at 0.25/0.75 (minor). Control it with the new
  `grid = c("radial", "cartesian", "none")` argument (default
  `"radial"`); `grid = "cartesian"` restores the previous square grid.
  `grid_colour` overrides the derived colour.
- New exported
  [`add_radial_grid()`](https://johnkirwan.github.io/radiatR/reference/add_radial_grid.md)
  and
  [`add_origin_point()`](https://johnkirwan.github.io/radiatR/reference/add_origin_point.md)
  composable layers, so the radial grid can be hand-built or restyled
  like any ggplot2 layer. The existing `quadrants`/`rings` arguments
  still add a-la-carte guides when `grid != "radial"`.

## radiatR 0.2.1

### Coordinate handling

- **Fix:** `normalize_xy = TRUE` (the `TrajSet()` and `TrajSet_read()`
  default) now arena-scales each trajectory instead of collapsing every
  point onto the unit circle. Previously it replaced each point with its
  unit vector (radius → 1), destroying trajectory shape: radius- and
  displacement-based heading rules (`net`, `distal`, `crossing`, `exit`,
  `origin_mean`, `straight`, …) returned wrong or `NA` headings, and
  uploaded data in the Shiny app was affected. Each trajectory is now
  centred on its bounding-box midpoint and scaled so its furthest point
  sits at radius 1, preserving shape and placing the arena centre at the
  origin (what the radius-based rules expect). Raw coordinates are still
  kept in `<x>_raw`/`<y>_raw`, and `rho` now holds the relative radius
  rather than a constant 1. Landmark-based mapping, when available,
  remains the accurate path; this only improves the no-landmark
  fallback. Stored `x`/`y`/`rho` values change for
  `normalize_xy = TRUE`; pass `normalize_xy = FALSE` to keep raw
  coordinates.
- Landmark-mapped tracks are never rescaled (they are built
  `normalize_xy = FALSE`), preserving legitimate excursions past the
  arena boundary. Track points that fall outside the boundary (radius
  \> 1) are now reported once per dataset – “N points across M trials
  exceeded the arena boundary (radius \> 1)” – instead of a separate
  warning per trial.

### Camera calibration

- **Breaking:** removed the camera-calibration layer entirely
  (`read_calibration()`, `cal_model()`, `cam_cal_pt()`,
  `cam_cal_many()`, `calibrate_positions()`, and the `CalModel` class),
  along with its vignette. radiatR normalises each trajectory to a unit
  arena, so its outputs (headings, mean direction, resultant length,
  circular statistics) are scale-invariant and never needed metric
  calibration; lens-distortion correction and scaling to real-world
  units are better handled in the upstream tracking pipeline (the
  tracker’s own calibration, or OpenCV `undistort`) before import. This
  completes the narrowing begun in 0.2.0, which removed the calibration
  *estimator*. The `R.matlab`, `yaml`, and `jsonlite` suggested
  dependencies remain — they are still used by the loaders.

### Shiny app

- The upload screen now loads faster: `ggplot2` and `radiatR` (and,
  through `radiatR`, `circular`/`boot`/`mvtnorm`) are attached lazily
  the first time the user loads data, rather than at app startup. Under
  the WebAssembly (shinylive) build, attaching those packages is the
  bulk of the in-browser R boot, so the initial upload screen now paints
  on `shiny` + `bslib` alone and the heavier load happens when a file or
  the example is chosen.
- The Results figure no longer prints a “Heading method: …” subtitle
  above the plot. (The plot spec can still carry a subtitle; the app
  just no longer sets one.)
- Restored the **Quadrant lines** and **Guide rings** toggles, which had
  become inert when the Results figure moved to the shared plot spec:
  the spec now carries them and passes `quadrants` / `rings` to
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md),
  and the **R code** export emits them, so every plot control
  round-trips through the export again.

## radiatR 0.2.0

A large feature release that broadens *radiatR* from a focused plotting
toolkit into an end-to-end pipeline for circular-arena tracking: many
more data sources, a full circular-statistics layer, richer distribution
overlays, and a no-code graphical interface.

### Data loading

- Dialect-based loader system covering 20+ tracking tools. New or
  extended dialects include **TRex** (positional CSV, `#wcentroid` /
  `#centroid` / `#pcentroid` variants, and `(cm)` / `(px)` unit
  annotations), **ANY-maze**, **Tracktor**, **Ctrax** (`.mat` via
  `R.matlab`, with ellipse `theta`/`a`/`b`), **TrackMate**,
  **idtracker.ai**, **ToxTrac**, **BORIS**, and **dtrack**.
- Pose-estimation support for **DeepLabCut** and **SLEAP**: load a
  single bodypart or a likelihood-weighted centroid of several, with
  per-bodypart columns retained for the `bodypart_axis` heading rule.
- Multi-zone support for EthoVision and ANY-maze (nose/tail/centre zone
  centroid or single-zone selection).
- Register custom formats at runtime with
  [`register_loader_dialect()`](https://johnkirwan.github.io/radiatR/reference/register_loader_dialect.md).
- Bundled single-file examples (`inst/extdata/`) for DeepLabCut, SLEAP,
  and Tracktor so every dialect can be tried without external data.

### Headings

- [`pose_to_headings()`](https://johnkirwan.github.io/radiatR/reference/pose_to_headings.md)
  derives many per-frame headings from multi-point pose data — suited to
  tethered-animal / gaze-direction designs.
- Additional heading rules registered through
  [`register_heading_rule()`](https://johnkirwan.github.io/radiatR/reference/register_heading_rule.md),
  including `bodypart_axis` and `ellipse_axis`.
- `derive_headings(..., return_coords = TRUE)` now also works for
  `distal`, `net`, `straight`, and `pca_axis` (previously only
  `crossing`), attaching each rule’s construction coordinates so the
  geometry behind a heading can be drawn.
- New
  [`bin_angles()`](https://johnkirwan.github.io/radiatR/reference/bin_angles.md)
  snaps angles to fixed-width circular bin centres. `phase = 0`
  (default) centres bins on the reference direction; `phase = width / 2`
  reproduces the edge-aligned bins of
  [`circular::plot.circular`](https://rdrr.io/pkg/circular/man/plot.circular.html);
  arbitrary phases allow e.g. quadrant binning. Intended as the
  precursor to a stacked dot plot
  ([`bin_angles()`](https://johnkirwan.github.io/radiatR/reference/bin_angles.md)
  then
  [`stack_headings()`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md)).
- [`stack_headings()`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md)
  and
  [`add_stacked_headings()`](https://johnkirwan.github.io/radiatR/reference/add_stacked_headings.md)
  gain a `start_sep` argument (the analogue of
  [`circular::plot.circular`](https://rdrr.io/pkg/circular/man/plot.circular.html)’s
  `start.sep`): a radial offset of the first dot from the reference
  circle, so a stack can abut the periphery rather than straddle it. The
  existing `step` argument (the analogue of `sep`) sets the gap between
  dots. Both default to circular’s behaviour at the package level.
- [`stack_headings()`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md)
  and
  [`add_stacked_headings()`](https://johnkirwan.github.io/radiatR/reference/add_stacked_headings.md)
  gain a `group` argument that stacks dots within each group
  independently (e.g. one stacking per facet).

### Track metrics

- New `path_straightness(x, y)` and `straightness_index(ts)` compute the
  path straightness index (net displacement / path length, 0–1) per
  trajectory.
- New `path_tortuosity(x, y)` and `tortuosity_ratio(ts)` compute the
  classic tortuosity ratio (path length / net displacement, ≥ 1), the
  reciprocal of the straightness index.

### Circular statistics

- Parametric fits:
  [`vonmises_fit()`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md)
  (with standard errors and a CI on the mean direction) and
  [`wrappedcauchy_fit()`](https://johnkirwan.github.io/radiatR/reference/wrappedcauchy_fit.md).
- [`circ_dispersion()`](https://johnkirwan.github.io/radiatR/reference/circ_dispersion.md)
  (mean direction, resultant length, circular SD) and
  [`sector_summary()`](https://johnkirwan.github.io/radiatR/reference/sector_summary.md)
  (per-sector dwell proportions).
- [`circ_cor()`](https://johnkirwan.github.io/radiatR/reference/circ_cor.md)
  — circular-linear and circular-circular correlation.
- Hypothesis tests returning tidy data frames, all with
  multiple-comparison correction via `p_adjust`:
  [`test_uniformity()`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
  (Rayleigh / Kuiper / Rao / Watson),
  [`test_mean_directions()`](https://johnkirwan.github.io/radiatR/reference/test_mean_directions.md)
  (Watson-Williams, omnibus or pairwise), and
  [`test_concentration()`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md).

### Visualisation

- Distribution overlays that compose onto a
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  plot with `+`:
  [`add_angle_rose()`](https://johnkirwan.github.io/radiatR/reference/add_angle_rose.md),
  [`add_vonmises_density()`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md),
  [`add_wrappedcauchy_density()`](https://johnkirwan.github.io/radiatR/reference/add_wrappedcauchy_density.md),
  and
  [`add_circular_kde()`](https://johnkirwan.github.io/radiatR/reference/add_circular_kde.md).
- Significance geometry:
  [`add_critical_r()`](https://johnkirwan.github.io/radiatR/reference/add_critical_r.md)
  draws the Rayleigh / V-test critical circle, and
  [`add_critical_v_line()`](https://johnkirwan.github.io/radiatR/reference/add_critical_v_line.md)
  draws the V-test decision boundary (a straight line perpendicular to
  the hypothesised direction).
- [`add_critical_r()`](https://johnkirwan.github.io/radiatR/reference/add_critical_r.md)
  gains a `colour_by_group` argument (default `TRUE`). With
  `per_group = TRUE` the per-panel circles are mapped to the group by
  default; set `colour_by_group = FALSE` to draw them in a fixed
  `colour` while still attaching the group column so the circles facet –
  avoiding a clash with an existing colour scale (e.g. the trajectory
  colours in the Shiny app).
- [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  gains an `arrow_colour_col` argument: when set, the built-in mean
  resultant arrow is drawn once per level of that column (within each
  panel, if faceted) and coloured by it, so the arrow can follow a
  colour grouping independently of faceting. The composable
  [`add_heading_arrow()`](https://johnkirwan.github.io/radiatR/reference/add_heading_arrow.md)
  and
  [`add_heading_interval()`](https://johnkirwan.github.io/radiatR/reference/add_heading_interval.md)
  already take `colour_col` for the same effect on the mean-direction
  arrow and its confidence interval; the circular-statistics vignette
  now shows this grouped-overlay pattern.
- New exported
  [`cycle_colours()`](https://johnkirwan.github.io/radiatR/reference/cycle_colours.md)
  – the order-stable primitive behind
  [`assign_cycle_colours()`](https://johnkirwan.github.io/radiatR/reference/assign_cycle_colours.md)
  and
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)’s
  `colour_cycle`. It maps a key to a cycled `1:n` colour index and
  accepts an explicit level order, so two data frames sharing a key
  (e.g. tracks and an overlay) can be coloured identically.
  [`assign_cycle_colours()`](https://johnkirwan.github.io/radiatR/reference/assign_cycle_colours.md)
  now delegates to it, and the Shiny app uses it too, so the
  colour-cycling logic has a single source of truth.
- New exported
  [`assign_colour_key()`](https://johnkirwan.github.io/radiatR/reference/assign_colour_key.md)
  attaches a shared colour-key column to a TrajSet or data frame (cycled
  for the trajectory or a high-cardinality key, raw values + legend for
  a low-cardinality grouping; `reference` keeps the key consistent
  across frames), so tracks and overlays colour identically. It replaces
  the Shiny app’s internal colour helpers.
- [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  gains an `angle_labels` argument: `"degrees"` (default, e.g. `45°`),
  `"none"`, or `"radians"` (now rendered as π fractions, e.g. `π/4`,
  rather than decimals).
  [`degree_labs()`](https://johnkirwan.github.io/radiatR/reference/degree_labs.md)
  gains a matching `units` argument. `degrees = FALSE` remains as a
  back-compatible alias for `angle_labels = "none"`.
- **Breaking:** the quadrant lines (two dashed lines through the origin)
  are no longer drawn by default.
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  gains a `quadrants` argument (default `FALSE`) to opt back in.
- [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  gains a `rings` argument (default `FALSE`) drawing concentric guide
  rings (the radial analogue of a grid). Both the quadrant lines and the
  guide rings take their colour and width from the chosen theme’s grid
  lines, so they match the theme (and fall back to a subtle grey for
  themes without a grid, e.g. `void` and `classic`).
- **Breaking:** the
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  `style` argument (`"classic"`/`"minimal"`) is replaced by `theme`,
  named for the ggplot2 base themes: `"void"` (default), `"minimal"`,
  `"classic"`, `"bw"`, `"grey"`, `"light"`, `"dark"`, and `"linedraw"`.
  Each gives the matching `ggplot2::theme_*()` appearance (panel
  background, grid, border). The exported `sparse_theme()` and
  `spartan_theme()` are removed in favour of the new
  `radial_theme(name)`.
- Overlay elements (unit circle, ticks, degree labels) now adapt to the
  theme: they use light “ink” on the dark theme so they stay legible.
  [`add_ticks()`](https://johnkirwan.github.io/radiatR/reference/add_ticks.md)
  and
  [`degree_labs()`](https://johnkirwan.github.io/radiatR/reference/degree_labs.md)
  gain a `colour` argument.
- [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  gains a `show_tracks` argument (default `TRUE`) to draw the arena and
  overlays without the trajectory paths, symmetric with `show_arrow` and
  `show_labels`.

### Graphical interface

- [`launch_app()`](https://johnkirwan.github.io/radiatR/reference/launch_app.md)
  starts a browser-based wizard (Upload → Configure → Results) for users
  who do not write R, including condition detection and a Rayleigh test.
  The app also builds as a static `shinylive` site.
- The package is the single source of truth and the app a thin
  presentation layer over it: the Results figure is resolved into a
  plain spec that both renders the on-screen plot (calling only exported
  functions) and emits the equivalent radiatR/ggplot2 script. A
  round-trip test asserts the emitted code reproduces the rendered
  figure (comparing layer coordinates), so the two paths cannot silently
  diverge.
- The Results step gains an **R code** panel that shows the
  radiatR/ggplot2 code reproducing the current figure, with **Copy** and
  **Download .R** buttons. The emitted script calls only exported
  package functions.
- The upload step now offers a one-click **“Load the example millipede
  dataset”** so the app can be tried end to end without supplying a
  tracking file. It loads the bundled `cpunctatus` example and jumps
  straight to the configure step.
- The Configure step gains a live **method preview**: a unit-circle
  panel beside the controls that draws a few example tracks and
  re-renders to show how the selected heading rule derives its heading,
  updating as the method or ring radii change. Several rules now draw
  their construction: `crossing` its two detection rings, ring-crossing
  dots, and dashed vector to the rim; `distal` the furthest-from-centre
  point; `net` the first-to-last chord; `straight` the longest straight
  run; and `pca_axis` the principal axis. Remaining rules mark just the
  derived heading point. Constructions and heading markers take each
  trajectory’s colour.
- The Configure step’s heading-method picker is now a dropdown exposing
  all of the package’s parameter-free heading rules (plus ring
  `crossing`), with a one-line description of the selected rule shown
  beneath it.
- The Results Display panel’s “Heading points” toggle is replaced by a
  **Heading display** dropdown (Points (overlapping) / Stacked dots
  (inward) / None) that selects how the per-trial heading markers are
  drawn. Stacked dots fan coincident headings radially inward to reduce
  overplotting, stacked within each condition group when the plot is
  faceted.
- A new **None (no headings)** option plots the tracks and path metrics
  only: the plot gains a straightness caption, the summary table reduces
  to Group and Straightness, and all circular-statistics overlays are
  suppressed.
- In None mode the data download exports the per-trial path-metrics
  table (“Metrics (CSV)”) instead of headings. The data-download button
  is also restyled so it no longer appears greyed out.
- The Results summary table gains a **Straightness** column: the mean
  path straightness index across each group’s trials.
- Plot colour now distinguishes the **trajectory** (in sequential order)
  by default, not the panel/facet variable. The “Facet by” selector
  (formerly “Group by condition”) now only facets; a new **“Colour by”**
  selector in the Display panel controls colour independently –
  defaulting to “Trajectory”, or any grouping column (condition, cohort,
  individual, …). Tracks and heading markers (points and stacked) share
  one colour key, so each marker matches its own trajectory/group even
  when faceted. A grouping with up to 20 levels gets a distinct colour
  per level and a legend; a higher-cardinality key (the trajectory, or
  e.g. an individual id with many animals) cycles a capped set of 20
  colours with no legend.
- The “Heading vectors” overlay inherits each trajectory’s (or group’s)
  colour, matching the tracks and markers, instead of being drawn in one
  colour.
- The mean-direction confidence interval, the Rayleigh critical circle,
  and the V-test decision boundary are all part of the shared plot spec,
  drawn by the exported
  [`add_heading_interval()`](https://johnkirwan.github.io/radiatR/reference/add_heading_interval.md),
  [`add_critical_r()`](https://johnkirwan.github.io/radiatR/reference/add_critical_r.md),
  and
  [`add_critical_v_line()`](https://johnkirwan.github.io/radiatR/reference/add_critical_v_line.md)
  that the **R code** panel emits, so every statistical overlay on the
  Results figure is reproduced by the code export. The figure’s subtitle
  (heading method) and caption (path-metrics summary, in the no-headings
  mode) are carried by the spec and emitted too, so the entire figure —
  body, overlays, and annotations — is reproduced by the exported
  script.
- The Results step’s Display panel gains a **Theme** dropdown (Void,
  Minimal, Classic, Black & white, Grey, Light, Dark, Line draw) that
  restyles the on-screen plot and the download, an **Angle labels**
  dropdown (Degrees / None / Radians), and **Quadrant lines** and
  **Guide rings** toggles (both off by default; both follow the selected
  theme’s grid styling).
- The Results step has on/off toggles for the trajectories, heading
  points, directedness arrow, and a mean-direction confidence-interval
  arc (95% bootstrap CI, off by default). The toggles drive both the
  on-screen plot and the plot download, and live in a “Display” card in
  the right-hand column, giving the plot panel the full width of its
  column.
- Plot downloads can now be exported as **PDF** or **SVG** (editable
  vector formats) as well as PNG, with editable width/height (inches)
  and, for PNG, a resolution control. A “Preview size” slider scales the
  on-screen plot canvas without affecting the exported file dimensions,
  and the preview tracks the chosen aspect ratio.
- Fixed the “Stacked dots (inward)” heading display, which superimposed
  every dot at the rim instead of stacking. Headings are now binned
  (5-degree bins centred on the reference direction) before stacking,
  producing inward radial columns; the stack sits just inside the unit
  circle (dots abut the periphery line rather than straddle it) with a
  slightly wider gap between dots.
- Fixed an error (“NAs are not allowed in subscripted assignments”) when
  stacking headings that include two or more `NA` values (trials with no
  defined heading). The internal angle-wrapping helper is now NA-safe.

### Camera calibration

- Refocused the calibration layer on *importing* calibrations from
  established tools rather than estimating them. `read_calibration()`
  reads camera intrinsics and Brown-Conrady distortion coefficients from
  the **MATLAB Computer Vision Toolbox** (`.mat`), **OpenCV**
  `FileStorage` (YAML/JSON), or a plain **CSV**, and `cal_model()`
  builds a `CalModel` from coefficients you already hold. Both handle
  radiatR’s transposed intrinsic-matrix convention and the 1-based
  (MATLAB) vs 0-based (OpenCV) principal-point difference for you.
- Removed the in-package calibration *estimator* and its interactive
  point-capture tools (`calibration_session()`,
  `calibration_from_points()`, `checkerboard_points()`, the
  `calibration_points_*` / `*_calibration_points` helpers, and
  `calibration_switch_axes()`). Estimating intrinsics from checkerboard
  images is better served by the mature toolboxes above; the bundled
  `calibration_corners.csv` / `calibration_truth.csv` fixtures are gone
  with it. `cam_cal_pt()`, `cam_cal_many()`, and `calibrate_positions()`
  are unchanged.

### Bug fixes

- [`add_angle_rose()`](https://johnkirwan.github.io/radiatR/reference/add_angle_rose.md),
  [`add_circular_kde()`](https://johnkirwan.github.io/radiatR/reference/add_circular_kde.md),
  [`add_vonmises_density()`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md),
  and
  [`add_wrappedcauchy_density()`](https://johnkirwan.github.io/radiatR/reference/add_wrappedcauchy_density.md)
  now honor the plot’s `display` rotation (via a new `display` argument,
  the input’s `display` attribute, or the identity default), so they
  stay aligned with the tracks and heading markers on a rotated display
  (e.g. a clock-oriented plot) instead of being drawn ~90 degrees off.
  Behaviour on the default plot is unchanged.
- The `crossing` heading rule now computes the heading as the bearing of
  the inner→outer ring-crossing vector projected onto the arena boundary
  (the unit circle) — the method described in the vignette and used in
  the original analysis — rather than the slope of the crossing segment.
  The two agree for radial tracks but differ for oblique ones, so
  crossing-based circular statistics change for non-radial trajectories.
  `derive_headings(rule = "crossing", return_coords = TRUE)` now also
  returns the outer crossing (`x_outer`/`y_outer`).
- The mean-direction (directedness) arrow now respects the clock display
  convention. In clock-display plots the trajectories are rotated 90
  degrees (East to North) but the arrow was left in unit-circle
  coordinates, so it pointed ~90 degrees away from the tracks and
  heading markers it summarises.
  [`compute_circ_mean()`](https://johnkirwan.github.io/radiatR/reference/compute_circ_mean.md)
  carries the input’s `display` attribute onto its output and the arrow
  is rotated with the rest of the plot, on screen and in the exported
  code.
- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  `TrajSet` now works for users of the installed package. It had been
  defined as an S4 method on the base S3 generic, which is only
  reachable from within the package’s own namespace, so user code got
  “cannot coerce class TrajSet to a data.frame”. It is now a registered
  S3 method.

### Example data

- The bundled example dataset is now `cpunctatus` (and the raw tracks
  `cpunctatus_tracks`), a subset of the *Cylindroiulus punctatus*
  millipede visual-orientation experiment of Kirwan & Nilsson (2019,
  *Vision Research* 165:36–44). This replaces the previous
  *Paracentrotus lividus* sea-urchin example.

### Documentation

- New vignette *Circular Statistics and Distribution Overlays* covering
  the fitting, testing, correlation, and overlay layers end to end.
- `pkgdown` site with a grouped reference index and a changelog.
