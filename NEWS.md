# radiatR 0.1.3

## Breaking changes

* A semicolon-separated file whose decimal point was sniffed from a column
  `read_tracks()` does not read for a role (a label `1.5`), while its x, y,
  time, angle or weight columns hold only values that could be digit grouping
  (`1.234`), now stops with `radiatR_ambiguous_decimal` instead of loading
  values that may be wrong by a factor of 1000. `read_opts = list(decimal = ".")`
  loads it as before. Files whose role columns show a decimal point (a time
  column `0.040`) are unaffected. The same applies to EthoVision exports that
  are not comma-separated, and to the other dialects that sniff the mark.
  A `;` file whose decimal sniff finds a point, and whose first data row's
  quoted field runs past the first 20 lines, now needs the mark given in
  `read_opts`.
* `read_tracks(read_opts = )` now reaches the built-in dialects, which used to
  ignore it: every dialect takes `decimal`; the `.read_any()`-based ones
  (`trackmate`, `trex`, `sleap`, `idtrackerai_wide`) also `delim` and `ext`
  (text files) and `sheet` (Excel files); `ethovision` and `anymaze` `delim`.
  An option a dialect cannot use or that does not fit the file's type (`delim`
  or `decimal` for an Excel file, `sheet` for a text file),
  or any option for a registered dialect without a `read_opts` argument, now
  stops with `radiatR_invalid_argument` instead of being silently ignored.
  `read_opts` entries are checked on every path: an unknown name (which `$`
  used to match partially: `decimals = ","` was read as `decimal`) or a
  decimal mark other than `"."` / `","` stops, as does a name given twice. Every
  refusal carries the offending entry as `option`. The decimal-mark hint on
  malformed values now applies to dialects and to `H:MM:SS` durations, no
  longer to Excel files, and the error carries `decimal` and
  `suggested_decimal`.

* `read_tracks()` without a dialect now refuses a `read_opts` entry that does
  not fit the file's type, as dialects do: `delim` or `decimal` for an Excel or
  Arrow file, `sheet` for a text or Arrow file (Arrow files were also let
  through by the dialects). A folder of mixed text and Arrow files read with
  `read_tracks_dir(read_opts = list(delim = ))` therefore stops. For a data
  frame, `read_opts` takes only `decimal`: `delim`, `sheet` and `ext` stop
  instead of being accepted without effect.

* `headings_frame()` stops on an angle column held as text (character or
  factor), with `radiatR_malformed_values` naming the values that are not
  numbers, or `radiatR_invalid_argument` when all are. With
  `units = "radians"` and the unit-circle convention a character column used
  to load as a text heading column; otherwise these columns failed with bare
  base-R errors ("non-numeric argument to binary operator", "'abs' not
  meaningful for factors").

* A bare date whose midnight does not exist in `tz` (a DST change at
  midnight, as in Cairo or Sao Paulo) is the first instant of that day
  (01:00 there) under every reader, and so is an Excel date cell at such a
  midnight. readr and `fread()` gave 01:00 for a column of such dates and
  `read.table()` an error; a date beside date-times stopped under readr. A
  date that does not exist at all in `tz` is an error.

* Two date-times that used to load now stop with an error naming them, as
  bugs fixed: a leap second (`23:59:60`, or `235960` in ISO 8601 basic)
  under readr, which rolled it over to the next minute, now stops with an
  error saying leap seconds cannot be represented, under every reader; and a
  date-time on a day `tz` skips altogether (2011-12-30 in Pacific/Apia)
  under `data.table::fread()` and `read.table()`, which moved it a day back,
  now stops with an error saying it does not exist in `tz`.

* `derive_headings(rule = "straight")` measures turns between moving steps
  only. A pause (a step of zero length) has no direction; it was read as a
  turn of 0, so a pause hid any turn around it, padded runs, and a run that
  began on a pause had heading 0 (East). Pauses now neither break a run nor
  count toward `min_len`. On `cpunctatus`, 47 of 235 trials have no straight
  run (20 before: 30 lost their run, 3 gained one), and 104 of the 185 with
  a heading either way change by more than a degree (median 90 degrees
  among those that change).
* `derive_headings(rule = "pca_axis", source = "velocity")` takes the axis of
  the steps about zero, not about their mean, so a track of equal steps has
  their direction as its axis (it had none, or one set by rounding). Under
  either source a track whose net displacement has no component along the
  axis (one that ends where it started) has heading `NA`: the axis has no
  sign, which had been left to the eigenvector routine. On `cpunctatus`,
  `source = "velocity"` changes 198 of 235 headings by more than a degree
  (median 6.4, max 86.4); `source = "position"` changes none; no new `NA`.
* A built-in `derive_headings()` rule stops with a `radiatR_invalid_argument`
  error naming any option in `...` it does not read (e.g. `rule = "distal",
  frame_select = "all"`, which only the body-part rules read), and lists the
  options it takes. A misplaced option was silently ignored, and a longer
  misspelling was taken by partial matching (`min_length = 50` acted as
  `min_len`); both now stop. `return_coords` is accepted by every built-in
  rule. Registered and function rules still receive `...` unchecked.
  `angle_convention`, which no rule ever read (a heading is in the unit-circle
  convention), is among the options that now stop: drop it from such calls.

* The `ethovision` dialect now reads the text export EthoVision XT writes: a
  first line giving the number of header lines, a block of trial details, a
  names row and a units row, `;`-separated (or `,` or tab), in latin-1, UTF-8
  or UTF-16, with `-` for a value the program did not have. Neither it nor
  `anymaze` read a real export before. A file is one track (id `"1"`); its
  time is `Trial time`, in seconds; a row with the wrong number of fields
  stops the load, naming the line, so a file cut off mid-row has to have its
  partial last line removed. A table whose first line is its column header is
  read as a data frame, no longer as a file.
* `ethovision`: `zone =` is now `bodypart =` (a zone is an area of the arena
  in EthoVision; the argument names body points). The default is the centre
  point; `bodypart = c("nose", "tail")` gives their mean. `zone =` stops with
  an error naming the new argument. The nose and tail columns are read by the
  names EthoVision documents (`X nose`, `Y tail`), which no export available
  to us contains.
* The `anymaze` dialect now reads ANY-maze's `Time`, `Centre position X`,
  `Centre position Y` export. `Time` is a duration (`0:01:02.500`), read as
  seconds. The centre point alone is read: `zone =` is removed, as is
  `skip_units_row =` (the dialect deleted the first row whenever its position
  was not a number, which on a real export is the first sample). The columns
  it used to look for (`X Centre`, `Nose X Centre`) are in no export found.
  A file without a time column is an error, and so is a `Time` of plain
  numbers, whose unit cannot be told: give a data frame with a `difftime`
  column instead. Pass the file path, or read `Time` as text (e.g.
  `readr::read_csv(f, col_types = readr::cols(Time = "c"))`): readr's own
  time parsing drops the fraction of a second.
* Both dialects follow the rule for missing points (see "Masked points" in
  `?read_tracks`): a sample without a position has an `NA` position, is
  counted in `@meta$attrition`, and stops the load under
  `on_invalid = "error"` or is dropped under `"drop"`. A malformed cell stops
  the load (`radiatR_malformed_values`); both dialects coerced silently
  before. Both declare their time as seconds. The mean of several EthoVision
  points divided the sum of y by the count of x present; it now masks a point
  missing either coordinate.
* `draw_tracks()`, `line_circle_intercept_df()`,
  `line_circle_intercept_traj()` and `new_headings_frame()` are no longer
  exported. They are internal helpers: draw tracks with `radiate()` or
  `gg_traj()`, intersect a segment with the unit circle with
  `line_circle_intercept()`, and build a headings frame with
  `headings_frame()`.
* `read_tracks()` reads a delimited file the same way whichever reader is
  installed (see "Reading delimited files" in `?read_tracks`), and stops
  where the readers used to disagree. The fields of each line are counted
  first, and a row with more or fewer fields than the header is a
  `radiatR_malformed_file` error naming its line: `fread()` kept the rows
  before it, `read.table()` wrapped a long row past the fifth line into
  extra rows, and readr shifted the fields, with at most a warning; readr
  dropped a last line cut off mid-row without one. So is a line of spaces
  (readr skipped it; `fread()` stopped reading there), an unclosed quote,
  a table written with row names or a separator at the end of every row
  (one name fewer than fields), which each reader read differently, a first
  line `fread()` skipped as a preamble, and a column name used twice in a
  file read without a dialect (readr renamed both, so the file lost its id
  column and became one track). Every row of the file must be read. Under
  `read.table()` a `#` no longer cuts a cell short, an apostrophe no longer
  swallows rows and an empty cell is missing (a blank id was a track of its
  own); `fread()` reads past a blank line and keeps a text cell's
  surrounding whitespace, as the other readers do. Column names lose their
  surrounding whitespace under every reader (readr kept `" id"`, so no id
  was found). A gzip, bzip2 or xz file is read under `fread()` without
  R.utils (it stopped asking for R.utils, or read an xz file as text). A
  line break quoted in a column name or cell of a file with Windows (CRLF)
  line endings is read as `"\n"` under every reader, as `read.table()` read
  it (readr and `fread()` kept `"\r\n"`, so `mapping = list(time =
  "fr\name")` found no column).
* Ids that differ only by surrounding whitespace (`" 1"` and `"1"`) stop
  `read_tracks()` with a `radiatR_ambiguous_ids` error: readr and
  `read.table()` read them as two tracks and `fread()` as one.
* `get_all_object_pos()` reads its landmark and track files strictly: a cell
  that is not a number (a decimal comma, a word) is a
  `radiatR_malformed_values` error naming the file, row and column, where it
  failed with `non-numeric argument to binary operator`; a line with fewer
  than three fields is a `radiatR_malformed_file` error (fields past the
  third are not read, and need not be on every line). An
  odd number of landmark rows is a `radiatR_malformed_file` error naming the
  file (it was a warning, then an error that named none). When no file holds
  a valid trial it stops with `radiatR_no_valid_trials` instead of returning
  `NULL` with a warning.

* The `deeplabcut` dialect now reads the predictions CSV DeepLabCut writes:
  header rows `scorer` / `bodyparts` / `coords`, and `individuals` for
  multi-animal projects, which load as one track per individual
  (`dialect_args = list(individuals = )` selects some). The frame index is the
  time, in frames. `deeplabcut_multiheader` is removed; `read_tracks()` names
  `deeplabcut` as its replacement (`radiatR_removed_dialect`). A flat
  `<bodypart>_x` table is still accepted as a data frame, no longer as a file.
  Labelled training data (`CollectedData_*.csv`, indexed by image) is refused
  with an error saying so; the bundled example is now
  `dlc_single_wasp_predictions.csv`, a predictions file.
* `deeplabcut` and `sleap` apply one rule to missing and low-confidence
  points (see "Masked points" in `?read_tracks`). A point is masked when its
  coordinates are missing, its score is 0, or it is below `likelihood_min` /
  `score_min`. A frame with a masked point has no position, so it stops the
  load under `on_invalid = "error"` and is dropped under `"drop"`;
  `dialect_args = list(partial = "mean")` averages the points that remain
  instead. Before, a threshold on one bodypart removed rows without saying so,
  a threshold on several averaged whatever passed, and DeepLabCut's (0, 0)
  placeholder for a frame with no detection was read as a position. The
  per-bodypart columns are `NA` where the point is masked.
* What those dialects masked is reported in one message (class
  `radiatR_attrition`) and kept in `Tracks` metadata (`@meta$attrition`): per
  file and point, the rows masked, the rows read and the rows left without a
  position. `c()` row-binds the records of its inputs.
* A threshold now needs a score column for every selected point, and must be
  one number; a negative score is malformed (`radiatR_malformed_values`).
  DeepLabCut and SLEAP columns are checked strictly, like the other dialects':
  a threshold used to be compared as text when the score column was text
  (`"1e-3" >= 0.9`). `bodypart` must name distinct points, and two SLEAP
  headers that differ only in case or punctuation are refused.
* DeepLabCut and SLEAP time is declared as frames (files, and data frames
  whose time column is a frame column), so `fps` alone gives elapsed seconds
  and `time_type = "seconds"` is refused for them.
* `derive_headings(carry = )` now stops when a carried column has the name of
  a column of the result (`id`, `time`, `heading`, ...). It used to replace
  the derived values with the carried ones without a message, so a call that
  relied on that now fails with `radiatR_invalid_argument`: rename the column
  in the tracks first.
* `Tracks` objects now enforce the invariants the rest of the package assumes
  (see "Invariants" in `?tracks`): no missing ids; finite, strictly increasing
  time within each id; finite angles and coordinates; finite, non-negative
  weights. Previously a missing id, a non-finite or repeated time, an invalid
  weight, a non-finite relative coordinate (`rel_x`/`rel_y`), or any
  non-finite coordinate written after construction was accepted, and a missing
  time crashed the validator. `tracks()` now reports every such violation at
  once as a `radiatR_invalid_tracks` error whose `problems` field lists the
  rule, column, input row and id. `read_tracks()` passes that error on, after
  its own checks for non-finite x/y (`radiatR_invalid_rows`) and for duplicate
  keys across files. Consequences: relative-coordinate columns must be finite
  when the object is built, so give them finite placeholders (or name the
  roles without storing the columns) if `set_reference()` is to fill them;
  `radiate()` on a data frame fails when `group_col` does not identify single
  trajectories (it used to draw one path per group that hopped between
  trials), so pass the trajectory id as `group_col`; and a `Tracks` saved by an
  earlier version that breaks a rule fails at its next `validObject()` (any
  transform), not when it is loaded.

* `load_tracks()` and `load_tracks2()` have been removed. Despite the names
  they loaded no tracks; they joined manifest columns onto the file table used
  by the landmark pipeline. Pass the manifest to `get_all_object_pos()`
  instead, via its new `manifest` and `manifest_cols` arguments:
  `get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir, manifest = manifest)`.
  `manifest_cols` replaces `load_tracks2()`'s `colnames`.

* The `cpunctatus_tracks` dataset has been removed. It duplicated the table
  already stored in `cpunctatus`; use `as.data.frame(cpunctatus)` (or
  `cpunctatus@data`) for the same per-frame rows.

* `simulate_tracks(seed = )` no longer changes the session's random-number
  state: the simulation is drawn under `seed` and the previous state is restored
  on exit. Seeded output is unchanged. Code that relied on the call to seed
  later draws (a bootstrap or Monte-Carlo test run afterwards) must now call
  `set.seed()` itself.

* `read_tracks()` stops on a malformed value instead of treating it as missing.
  A cell in an x, y, angle, weight or time column that is present but does not
  parse (`"oops"`, `"1,5"`, a stray header or units row) raises a
  `radiatR_malformed_values` error, a subclass of `radiatR_invalid_rows`, that
  names the column, the data rows, the offending values and the file (each
  file's own rows in a multi-file import). `on_invalid = "drop"` does not apply
  to it: `"drop"` removes only rows whose coordinates are missing or
  non-finite. Blank cells and `NA` are still missing values, and `nan` and
  `inf` are read as such. Previously one bad cell made `read_tracks()` report
  every row of the column as non-finite, and `"drop"` then dropped them all; a
  bad cell after the first 1000 rows of a file was read as missing when readr
  is installed; and in a multi-file import the bad cell surfaced as a
  type-clash error from vctrs.

  Files are now read with column types guessed from every row and with no
  digit-grouping mark, so `1,5` or `1,234.5` in a tab-separated file is
  malformed rather than read as 15 or 1234.5. Pass
  `read_opts = list(decimal = ",")` for decimal commas. Where the decimal mark
  is a comma, one bad cell names that cell only, not every `1,5` in its
  column, and a value holding a `.` is malformed in turn, since `1.234` could
  as well be 1234. A semicolon-separated file is taken to use decimal commas
  unless its first lines show decimal points and no decimal comma: a value
  such as `1.5` or `0.125`, which cannot be digit grouping. For a file
  `read_tracks()` reads itself, `read_opts = list(decimal = )` settles it
  either way.

  The check covers the columns `read_tracks()` converts itself and the time
  column of every dialect but ANY-maze. An error raised inside a dialect does
  not yet name the file or the source column, and counts rows after the
  dialect's own filtering and reshaping. Coordinates that a dialect converts
  itself (ANY-maze, EthoVision, DeepLabCut, SLEAP) are not yet checked this
  way.

* Text time columns are parsed strictly. Every value must match one timestamp
  layout in full: `YYYY-MM-DD HH:MM:SS`, `YYYY-MM-DD HH:MM` or `YYYY-MM-DD`
  (with `-` or `/`), or ISO 8601 `YYYY-MM-DDTHH:MM:SS` with an optional `Z`
  for UTC; a bare date beside date-times is midnight. An ISO 8601 timestamp,
  or one followed by stray text, used to be cut to midnight of its date
  without warning; the first now keeps its time of day and the second is an
  error. A text column of numbers is read as numbers (it was an error) and,
  with `fps`, stored as frames. A factor time column is read by its labels,
  not its level codes. A `Date` column becomes midnight timestamps instead of
  day counts, and a `difftime` column is converted to seconds whatever its
  units. `time_type = "frames"` with a date-time, date or duration column is
  now a `radiatR_time_type_mismatch` error; it used to store epoch seconds as
  frame numbers.

* `read_tracks()` stops with a `radiatR_missing_columns` error when `mapping`
  names a column that is not in the data. A missing `angle` column used to be
  ignored without notice, and a missing x or y column gave an error that did
  not name it. A `regex_mapping` pattern (see `read_tracks_format()`) that
  matches no column is the same error, naming the role and the pattern.

* The `validate` argument of `read_tracks()` is deprecated. It never skipped
  anything, because `Tracks` invariants are checked whenever an object is
  built; any value but `TRUE` now warns, and the argument will be removed.

* The `trackmate` dialect stops with a `radiatR_unlinked_rows` error (a
  `radiatR_invalid_rows`) when a file holds spots that belong to no track
  (`TRACK_ID` `None` or blank, as in an "all spots" export). They used to be
  pooled into one trajectory called `None`. Pass
  `dialect_args = list(unlinked = "drop")` to leave them out, with a
  `radiatR_dropped_rows` warning. Without a units row saying `POSITION_T` is
  in seconds, the dialect now takes `FRAME` as the time column rather than
  `POSITION_T`.

* Built-in loader dialects are now kept only for tracking software that is
  widely used and still developed. Eleven dialects are removed: `tracktor`,
  `boris_xy`, `dtrack`, `motchallenge`, `csv_bbox`, `wide_prefix_xy`,
  `tum_traj`, `euroc_mav`, `gpx`, `geojson_linestring` and `nmea_gprmc`.
  `read_tracks(dialect = )` with one of these names stops with a
  `radiatR_removed_dialect` error. To keep reading such files, register your
  own loader with `register_loader_dialect()`; a loader registered under a
  removed name works as before. The bundled `tracktor_example.csv` is removed
  with its dialect, and `xml2` is no longer a suggested package.

* `dtrack_read()` is removed. `vignette("loaders")` shows a dtrack loader of
  a dozen lines, registered with `register_loader_dialect()`, that reads the
  bundled millipede track files.

* A dialect is now the only reader of its input. `read_tracks(path, dialect = )`
  used to parse the file with the generic reader first and discard the result,
  which printed repaired-name messages for headerless files and could fail
  before the dialect ran. With several paths the dialect is called once per
  file; a custom dialect that expected the whole vector of paths must be
  changed to take one path. `read_tracks("<dialect name>")`, which passed the
  name itself to the dialect as input, is no longer accepted.
* The raw example track files for `cpunctatus` now ship as one archive,
  `inst/extdata/tracks.tar.gz`, instead of the 470-file directory
  `inst/extdata/tracks/`, which cuts about 2 MB from the installed size.
  `system.file("extdata", "tracks", package = "radiatR")` no longer finds
  anything; unpack the archive first with
  `untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
  exdir = tempdir())` and use `file.path(tempdir(), "tracks")`. The files
  themselves are unchanged.
* `derive_headings()` rules `velocity_mean`, `vm_fit`, `velocity_axis` and
  `origin_mean` leave out what has no direction: a step of zero length (a
  pause) and a position at the origin. Each was counted as pointing along the
  x axis (`atan2(0, 0)` is 0), which pulled the heading of any track with
  pauses towards it. In `cpunctatus` a quarter of the steps are pauses, and
  the default `velocity_mean` heading moves by more than 1 degree in 194 of
  235 tracks (median shift 12 degrees). A track that never moves now has
  heading `NA`; it had heading 0.
* The weighting options of those rules now do something. `weight_by` (for
  `velocity_mean` and `vm_fit`) and `r_power` (for `origin_mean`) were passed
  to `circular::mean.circular()`, which has no weights argument, so every
  value gave the unweighted mean. `weight_by = "step_length"` now weights each
  step by its length, which is the direction from the first position to the
  last, as `net` gives; `r_power = 1` gives the bearing of the centroid.
* `weight_by` defaults to `"uniform"` for `velocity_mean` and `vm_fit` (the
  argument default was `"step_length"`, which had no effect), so their
  headings remain the mean of the step directions. `velocity_axis` keeps
  `"step_length"`, which it already applied. `vm_fit` returns the same heading
  as `velocity_mean`: its von Mises fit raised an error that was caught, and
  the maximum-likelihood mean direction is the mean direction in any case.
* `origin_mean` stops with `radiatR_invalid_argument` when `r_power` is not one
  finite number; such a value was read as 0.
* The other `derive_headings()` rules no longer read a displacement of zero
  length as pointing along the x axis. `net`, `straight`, `goal_bias`,
  `maxspeed_window` and `window_net` give `NA` for a track that never moves
  (they gave 0), `pca_axis` gives `NA` when the positions, or the steps under
  `source = "velocity"`, have no spread (it gave pi/2), and `distal` gives `NA`
  for a track that stays at the origin. `net` also gives `NA` for a track that
  ends where it started, and `straight` for a straight run made of pauses only
  (3 of 235 tracks in `cpunctatus`). All are reported by `on_missing`.
* `window_net` gives `NA` for a window whose first and last positions are the
  same (350 of 39631 windows in `cpunctatus`, which had heading 0), so
  `on_missing` now warns about them.
* `get_tracked_object_pos()` and `get_all_object_pos()` with
  `radius_criterion = "first_past"` give a landmark (`x0`/`y0`, `x1`/`y1` in
  `meta$trial_limits`) of `NA`, with a warning naming the trial, when no point
  of the trial reaches that radius. The farthest point was substituted, with
  a warning only for the inner radius. A trial that is not mapped (it starts
  too far from the centre) has `NA` landmarks and `order`/`vid_ord` in
  `trial_limits`, not 0. `get_all_object_pos()` keeps the `trial_limits` rows
  of a file none of whose trials is valid. The bundled `cpunctatus` follows
  suit: trial `con_19_1`, which never reaches the inner radius, now has `NA`
  landmarks in `meta$trial_limits` instead of its farthest point (0.059,
  0.015) for both. Nothing else in the dataset changes.
* readr no longer decides how a delimited file's date-times are read: like
  `data.table::fread()`, it hands them back as the text in the file, and one
  parser reads them for every reader. That parser now also reads ISO 8601
  basic and hour-minute times, zoned or not (`20240101T100000`,
  `20240101T100000Z`, `2024-01-01T1000+02:00`, `2024-01-01T10:00`). A time
  column that mixes zoned (`Z`, `+02:00`) and unzoned date-times, or holds
  hours alone (`2024-01-01 10`), a trailing decimal point, seconds on some
  values only or a basic date and time split by a space (`20240101 100000`)
  now stops under readr too, with an error naming the values; so does an
  offset after a space (`2024-01-01 10:00:00 +0200`), which `fread()` read.
  A kept column (`keep =`) whose every value is in one date-time layout (or
  is a date) is read as date-times in `tz` under every reader, where
  `fread()` and `read.table()` gave its text; any other kept column is kept
  as read, the same under every reader (readr typed some, such as hours
  alone). So a kept column can now stop the load under `fread()` and
  `read.table()`, where it stayed text: one holding a time that does not
  exist in `tz` (a DST gap), a leap second, or zoned and unzoned date-times
  in one layout. Files whose unused date-time columns were typed differently (a CSV
  beside an xlsx) bind again. A time column of date-times read with
  `time_type = "seconds"` or `"frames"` now stops with
  `radiatR_malformed_values` under every reader, as it did under
  `read.table()`; readr gave epoch seconds (or, for frames, a
  `radiatR_time_type_mismatch` error).

* A file given to the wrong built-in dialect now stops instead of loading
  wrong positions or ids. The `sleap` dialect needs a `frame_idx` column
  unless `time_col` names the time: it read any `<name>_x` / `<name>_y` pair
  as a node, so a ToxTrac file loaded its bounding box and a TRex file its
  midline. The `trex` dialect refuses a track column (`Track`, `TrackID`,
  `TRACK_ID`, `trajectory_id`) unless `id_col` names the id: it ignored one,
  so a ToxTrac file's tracks became one id and a TrackMate file's spots one
  track each. `id_col` and `time_col` of `trex` and `trackmate` are now
  matched as the headers are (`"Track"` failed with an untyped error) and a
  name that matches no column stops with `radiatR_dialect_error`, as do the
  other refusals of `trex`, `trackmate`, `toxtrac` and `idtrackerai_wide`,
  which were plain errors. These refusals, `sleap`'s, and a misnamed
  `bodypart`, `id_col` or `time_col` in any built-in dialect name the file,
  which a multi-file read did not say. `sleap` matches `id_col` / `time_col`
  as it matches its headers, too.

## Changes

* The app has a "Decimal mark" control (Auto-detect / Point / Comma) beside
  Delimiter for text uploads. It applies to previews, trajectory and headings
  loads and the downloaded scripts, and an error about values written with
  the other mark says which setting to choose. A tab or comma file with
  decimal commas, and a `;` file whose decimals look like digit grouping,
  can now be loaded in the app.
* The `sleap` dialect accepts a node name as it is written in the file
  (`bodypart = "Tail Base"`).
* A dialect can keep facts about the file it read with
  `attr(out, "source_meta")` (see `?register_loader_dialect`).
  `read_tracks()` stores one record per file in `Tracks@meta$source_meta`,
  with the file's name and path; `c()` concatenates them. `ethovision` keeps
  the export's header block (`header`: trial, arena and subject names, start
  time, ...) and the units of its columns (`units`) there.
* The message about masked points offers `partial = "mean"` only when the
  position was taken from more than one point.
* The app detects EthoVision and ANY-maze uploads.
* The app detects DeepLabCut files by their header rows, multi-animal files
  included. A flat `<bodypart>_likelihood` table is still offered to the
  `deeplabcut` dialect, which now refuses it by name; choose "Generic CSV" to
  load it as plain columns. A dialect's refusal of a file is shown as the app's error headline.
* The app can load a file that has rows with no position. When a load stops
  on rows with a missing x/y, or on TrackMate spots that belong to no track,
  the error offers to drop those rows and continue. Nothing is dropped unless
  the user agrees, and the app asks again on every later load. The number of rows
  dropped is shown above the results, and the downloaded scripts load the file
  the same way (`on_invalid = "drop"`,
  `dialect_args = list(unlinked = "drop")`). This lets idtracker.ai files,
  TrackMate "all spots" exports and DeepLabCut or SLEAP files with masked
  frames load in the app, which refused them before. Malformed values and
  duplicate id/time rows are still errors.
* New vignette `vignette("agent-guide")`: the angle conventions, calibration
  rules, failure behaviour and common pitfalls an AI agent (or anyone
  scripting radiatR) needs. `AGENTS.md` at the repository root points to it.
  The `circ_summarise()` help now states that `mean_dir_deg` is in the
  display convention rather than a plain conversion of `mean_dir`, and the
  `read_tracks()` help that `keep` adds to the mapped columns (it said
  `NULL` keeps every column; unmapped columns are dropped).
* The main workflow and simulation walkthroughs are now website-only articles
  (`vignettes/articles/`) rather than installed vignettes, so
  `vignette("radiatR")` and `vignette("simulate")` are no longer available
  offline; read them at <https://johnkirwan.github.io/radiatR/articles/>.
  Together with the dataset removal this brings the source tarball under
  CRAN's 5 MB limit, and R CMD check now fails if it grows past 4.5 MB.

* The hosted Shiny app has moved from shinyapps.io to Posit Connect Cloud
  (git-backed deploy). The `shinyapps.yaml` GitHub Actions workflow is removed;
  Connect Cloud rebuilds `inst/app` from the repo and installs dependencies from
  the committed `inst/app/manifest.json`, regenerated by the new
  `connect-manifest` workflow.

* Reading several track files at once in `read_tracks()` no longer needs the
  optional dplyr package (it is bound with `vctrs::vec_rbind()`). purrr is no
  longer a dependency; vctrs, already required by tibble and ggplot2, is now
  imported directly.

* `radiate()` draws clipped tracks much faster: clipping paths to the unit
  circle (`clip_tracks = TRUE`, the default) is now vectorised, taking a plot
  of the `cpunctatus` example data from about 6 s to about 0.1 s. The drawn
  geometry is unchanged. `line_circle_intercept()` is now vectorised over its
  arguments.
* Nineteen help pages in the circular-plotting and circular-boxplot groups now
  have runnable examples: `circ_display()`, `directedness_arrow()`,
  `compute_circ_interval()`, `add_circ_interval()`, `add_heading_interval()`,
  `compute_circ_mean()`, `add_circ_mean()`, `add_heading_arrow()`,
  `add_stacked_headings()`, `line_circle_intercept()`, `gg_traj()`,
  `add_angle_rose()`, `add_vonmises_density()`, `add_circular_kde()`,
  `add_wrappedcauchy_density()`, `add_critical_r()`, `add_critical_v_line()`,
  `circ_boxplot_stats()` and `add_circular_boxplot()`.

## Bug fixes

* `read_tracks(<data frame>, read_opts = list(decimal = ","))` reads a column
  of "1,5" text as 1.5; the option was ignored, and the error about such
  values now suggests it for a data frame too.

* App: a headings file read with the wrong decimal mark no longer has its id
  column plotted as the angles. The angle picker only listed numeric columns,
  so an angle column read as text was missing and the first numeric column
  (often the id) was preselected. A text column of numbers in the other mark
  is now listed and preselected, with a note saying which Decimal mark to
  set, and Continue stops with that advice.

* App: the script downloaded for a headings upload reads the file with the
  delimiter and decimal mark the app read it with. With the delimiter control
  on "auto" (the default) a `;` file was read with `read.csv()`, which
  stopped, and a `;` file with decimal points was read with `read.csv2()`,
  which left its numbers as text. A latin-1 file is read with
  `fileEncoding = "latin1"`.

* `read_tracks()` reads a zip archive without readr or data.table installed.
  The `read.table()` fallback was handed the archive itself and stopped with
  a misleading "0 row(s) were read" error; it now reads the archive's first
  member, as readr and `fread()` do, a latin-1 one included.

* `launch_app()` no longer writes to the global environment. The app sourced
  its helper files there (functions and constants such as `build_plot_spec()`
  and `APP_SEED`), which CRAN policy forbids; they now live in the app's own
  environment.

* A latin-1 delimited file (as Windows programs write) reads as its text
  under every reader: a byte such as `µ` in a column name or `é` in an id
  stopped the load with an untyped "invalid UTF-8" error, and one in another
  text column was kept as an invalid string. As for a tracker's
  dialect file, one line that is not valid UTF-8 makes the whole file
  latin-1, so in a file that mixes the two, its UTF-8 characters read as two
  latin-1 ones each (`µ` as `Âµ`).

* `read_tracks(dialect = "deeplabcut")`: a latin-1 body-part name (a header
  re-saved by a Windows program) reads as its text. It made the file "not a
  DeepLabCut output file".

* App: a headings upload's group columns, and the generic preview's mapped
  id column, keep their text where reading them as numbers would merge
  labels (`1.1` and `1.10`), as `read_tracks()` already did for loads. The
  downloaded script names such group columns in `colClasses` so they are read
  as text too.

* App: a file read by a tracker's dialect that the generic preview cannot
  make a table of (a raw EthoVision export) is previewed as its first lines.
  It showed one column of preamble text under readr and could not be
  previewed under `data.table::fread()`.

* A time that does not exist in `tz` (the hour skipped when clocks go
  forward) stops with an error saying so under every reader; under `fread()`
  and `read.table()` it said only "not a date-time". (A skipped day and a
  leap second: see Breaking changes.) A column of zoned and unzoned
  date-times in one layout stops with an error saying the zones are mixed,
  not that a valid value is no date-time. A bare date on a day `tz` skips
  reports its own row. An Excel date cell on a day `tz` skips altogether
  stops the load only when its column is used or kept, and reports its row.

* A multi-file import whose time column holds bare dates in one file and
  date-times in another (a CSV of dates beside a CSV or an xlsx of
  date-times) binds under every reader, each date the first instant of its
  day in `tz`. `fread()` stopped with a type clash, and under readr a date
  on a day `tz` skips was reported as a missing time.

* `derive_headings()` no longer calls headings "trials" for a rule that gives
  several per trial (`window_net`, `crossing`): the `on_missing` message
  counts missing headings and the trials they come from, and the app's
  attrition banner does the same. `missing_ids` lists each trial once, and
  the new `n_trials` attribute gives the number of trials.
* `read_tracks(id_collision = "namespace")` gave files that share a name less
  extension (`n1.csv` and `n1.xlsx`, or `a/n1.csv` and `b/n1.csv`) the same
  prefix, so their ids still collided. Such files are now prefixed with their
  full name, or the end of their path that tells them apart (`a/n1.csv`).
* Ids written with surrounding spaces and no unpadded twin (` 1`, ` 10`) are
  the same ids under every reader: readr kept them as the text `" 1"`, where
  `data.table::fread()` and `read.table()` read the number 1.

* The separator sniff of `read_tracks()` counts fields as the readers do, so
  a separator inside a quoted field no longer counts: a `;` file with `;` in
  a quoted label is read as `;`. A file whose header alone has a different
  number of fields (a table written with row names) is named as such; it was
  read as one column. A delimited file that still reads as a single column
  stops with a `radiatR_malformed_file` error saying no separator was found,
  instead of "Need either an angle column or both x and y columns".
* `read_tracks()` no longer stops with "invalid UTF-8" in the separator sniff
  when the first lines of a delimited file hold a byte that is not UTF-8 in a
  data cell that is not an id (a latin-1 label): the separator and decimal mark
  are sniffed from such lines too. A latin-1 byte in a column name or in an id
  column still stops the read.
* `read_tracks()` with a `tz` other than `"UTC"` put a timestamp that names
  its zone (`2024-01-01T10:00:00Z`, `...+02:00`) at the wrong instant when
  the file was read by `data.table::fread()` (readr not installed): the
  instant was written back as UTC wall-clock text and read again in `tz`,
  moving it by the zone's offset (10:00Z read in New York as 15:00Z), and its
  fraction of a second was dropped. Such columns are now read again as the
  text in the file. Text time also reads a UTC offset (`+02:00`, `-0130`,
  `+02`, at most 14 hours), so a file with offsets gives the same instants
  under every reader; `read.table()` refused them before. A `Z` or offset
  is read after any accepted layout (`2024-01-01 10:00:00Z` too). On the
  text path a column that mixes zoned and unzoned date-times is malformed,
  as one mixing `Z` with unmarked times was.
* `gg_traj()` with `facets =` stopped when the plot was drawn: it asked
  `facet_wrap()` for free scales, which `coord_equal()` does not allow. The
  panels now share fixed scales, so trajectories on the unit circle are drawn
  at the same scale in every panel.
* Ids are kept as labels in more cases (see "Ids" in `?read_tracks`). Two
  files of one import whose ids read as one number, written differently
  (`1.1` in one file and `1.10` in the other), are two ids: they were
  reported as colliding, and `id_collision = "namespace"` labelled both
  `1.1`. An id column typed as a date or time keeps its text where two
  labels are one instant (`2024-01-01 00:00` and `2024-01-01 00:00:00` were
  one track). An Excel file's ids are compared with a delimited file's as
  the text of their cells (`100000`), and `fread()`'s large whole numbers
  (integer64) as text too. Where the id types still disagree, in tables
  that cannot be read again for their text (two tables from one dialect, a
  Parquet file), the load stops with `radiatR_id_type_mismatch`; it
  stopped with an internal `Can't combine` error.
* `derive_headings()` and `circ_summary_headings()` now require `rule`.
  Called without it, `derive_headings()` stopped with an internal
  `'length = 16' in coercion to 'logical(1)'` error, and
  `circ_summary_headings()` fell back to `"crossing"`, which then stopped for
  want of `circ0` and `circ1`. Both now stop with an error that lists the
  available rules. `derive_headings()` also refuses a `rule` that is not a
  single name or a function (a vector, `NA`, a number), which previously gave
  the same internal error.

* `read_tracks(tz = )` now applies to every timestamp without a time zone in
  a delimited or Excel file that `read_tracks()` reads itself. A timestamp the
  file reader typed (`YYYY-MM-DD HH:MM:SS` with readr or data.table installed,
  or an Excel date-time cell) was read as UTC whatever `tz` was, while one
  left as text (`YYYY/MM/DD HH:MM:SS`) was read in `tz`: the same wall-clock
  time came out hours apart, between two files of one import or between two
  machines with different readers installed. A timestamp marked `Z` is still
  UTC, and a bare date beside such timestamps is midnight in `tz`. A
  wall-clock time that does not exist in `tz` (the hour skipped when clocks go
  forward) stops the load instead of being moved an hour on. `tz` does not
  yet reach a file read by a dialect, or a Parquet or Feather file.

* `read_tracks(format = )` honours the format. It passed all of its own
  defaults on to `read_tracks_format()`, where they overrode the format: the
  format's `mapping` and `regex_mapping` were discarded and the columns
  guessed from their names (a format mapping x to `pos_x` read a column named
  `x` if the file had one), and its angle unit, time type, time zone and
  frame rate gave way to the defaults, so degrees were read as radians. Now
  only the arguments the caller supplies override the format, `mapping` role
  by role, and `read_opts` is passed on. An argument given as `NULL` counts
  as not supplied (`fps = NULL` or `keep = NULL` used to delete the format's
  frame rate or kept columns), a `dialect` the caller names replaces the
  format's, and `load_manifest(format = )` follows the same rule: it passed
  on all of its own defaults, so a degrees format was read as radians there
  too. A format's `dialect_args` reach its dialect: they were handed to
  `read_tracks()` as arguments of its own, which stopped with "unused
  argument".

* Ids that are written differently are no longer merged into one track. A
  file reader types an id column as numbers when it can, so in any delimited
  file ids `1.1` and `1.10`, `1` and `1.0`, `1e1` and `10` (and `01` and `1`
  without readr) were read as the same id, with no condition.
  `read_tracks()` now keeps such a column as the text in the file (`T` and
  `TRUE`, read as one logical value, likewise). The same holds for the mapped
  id column and for any column named `id`, `track`, `trajectory`, `animal` or
  `subject`. Ids that read as distinct numbers are numbers as before. Where
  the delimited files of one import disagree on the type of the id column,
  the ids of each file are the text in that file; the import used to stop
  with a type error, or read `TRUE` beside numeric ids as 1.

* A time that does not exist in `tz` (the hour skipped when clocks go
  forward), in an Excel date-time cell or in a delimited file read with
  readr, stops the load with a `radiatR_malformed_values` error naming the
  file, column, row and value, when it is in a column `read_tracks()` uses:
  the time column or one kept with `keep = `. It was read as `NA`, which
  blanked a kept column (without a word from an Excel file, with readr's
  "parsing issues" warning from a delimited one) and gave a time column an
  error that named neither the value nor the cause. In a column that is not
  used it is still `NA`.

* `circ_summarise()` and `circ_summary()` on a `Tracks` object now return the
  von Mises concentration `kappa`; it was always `NA`. The estimator was
  looked up as `circular::est.kappa`, a function the circular package does not
  have, so every caller silently got its fallback instead. `kappa` is now
  computed directly as the maximum-likelihood estimate from the mean resultant
  length (the value `circular::mle.vonmises()` gives), weighted where weights
  are in use. Both report `NA` for fewer than three valid angles (as
  `circ_summarise()` already did) and `Inf` when all angles coincide, where
  the estimate is unbounded.
  `circ_summary_headings()`, `boot_kappa_ci()` and the starting value in
  `concentration_regression()` already used this estimate and are unchanged.

* The `trackmate`, `toxtrac`, `idtrackerai_wide` and `trex` dialects now read
  the files those programs write. Checked against real exports:
  - TrackMate 7 spot tables load; their three extra header rows used to be
    reported as malformed values. They are dropped only when recognised as
    names and units rows.
  - ToxTrac `Tracking_RealSpace.txt` (`Time (sec)`, `Track`, `Pos. X (mm)`)
    and the headerless `Tracking.txt` (6 or 11 columns) load; both used to stop
    with "required columns not found". The id is `<arena>_<track>`.
  - idtracker.ai `trajectories.csv` (`seconds,x1,y1,...`) loads; it used to
    stop with "no x_<id>/y_<id> pairs found". An x column with no y column is
    now an error, as is an `ids` entry the file does not have.
  - TRex files that write `frame` twice use it as the time; they used to fall
    back to the `time` column.
  These dialects check their source columns strictly, so a malformed value is
  reported by the header as written, the row of the file and the file name.

* Dialects now state the unit of their time column, so seconds are no longer
  taken for frames when an `fps` is supplied (elapsed time and speeds were
  divided by it). See "Time units from a dialect" in `?read_tracks`. An `fps`
  given for a time column in seconds is not used, with a message; an explicit
  `time_type` that contradicts the dialect is a `radiatR_time_type_mismatch`
  error.

* App: tracking software is now recognised from the file's header for
  TrackMate, ToxTrac and idtracker.ai exports. The header was reduced before
  it was lower-cased, so no header containing a capital letter ever matched.
  A file whose time is in seconds no longer needs a frame rate for the
  kinematics.

* Numeric time loaded as seconds is now treated as seconds. A `Tracks` read
  with `read_tracks(time_type = "seconds")`, or from a duration (`difftime`)
  column, was handled as a frame index: `elapsed_seconds()`, `track_speed()`
  and the other kinematics stopped and asked for a frame rate, and once one
  was set they divided the seconds by it. Such a `Tracks` now records
  `time_unit = "seconds"` in its metadata and needs no frame rate; a frame
  rate set on it is kept but not applied. `c()` refuses to combine `Tracks`
  whose time units differ. Numeric time of unstated unit is still a frame
  index, and the error that asks for a frame rate now mentions
  `time_type = "seconds"`.

* `read_tracks()` and `read_tracks_dir()` now work with `dialect` and several
  files. They used to fail with `is.data.frame(df) is not TRUE`. Each file is
  read by the dialect and coerced on its own, so a malformed value is reported
  against its own file and row; ids repeated across files follow
  `id_collision`, and a dialect that returns no id column gets one id per
  file. A failure inside the dialect is re-raised as a `radiatR_dialect_error`
  naming the file, unless it is already a radiatR condition.

* `read_tracks()` names the file(s) that do not exist instead of reporting
  that `x` must be a data frame or path.

* `read_tracks()` ignores a guessed id column that holds no values. It uses
  the next id-like column if there is one, and otherwise reads the file as one
  trajectory, as it does when there is no id column. SLEAP writes an empty
  `track` column for single-animal exports, so `sleap_example.csv` now loads
  without `dialect = "sleap"`. An id column named in `mapping`, or one that is
  only partly empty, is still an error.

* x, y, angle and weight columns stored as text or as a factor load when every
  value is a number. Previously every row was reported as non-finite.

* The app's error banner names the column and an example value when an upload
  has a malformed cell, instead of the generic "not valid track data".

* `read_tracks(dialect = "sleap")` now reads an export whose `track` column is
  empty (SLEAP's output for untracked, single-animal data) as one trajectory
  with id `"1"`. Previously such a file failed to load, or with a single row
  loaded with a missing id.

* An angle within floating-point error below zero (for example `-1e-17`, or a
  position whose `y` is float noise below 0) no longer makes `tracks()` fail.
  It wrapped to exactly `2*pi`, outside `[0, 2*pi)`; it now wraps to 0.

* The script downloaded from the Shiny app now derives headings in the frame
  the app used. In the app's default relative frame the script omitted
  `coords`, so `derive_headings()` fell back to its `"absolute"` default and
  the script's headings (and every statistic computed from them) differed from
  those shown in the app. The frame is now always written explicitly, as the
  one the app resolved: a dataset with no relative coordinates gets
  `coords = "absolute"`.

* The Shiny app's method preview now follows the "Heading frame" control. It
  always illustrated the relative frame, so with the frame set to absolute (or
  with data that has no landmark frame) the preview showed a different frame
  from the one the Results derive headings in.

* The Shiny app no longer calls `set.seed()` on the shared R process when it
  computes Monte-Carlo uniformity p-values, so one session can no longer shift
  another's random draws. The mean-direction bootstrap CI arc and the RANSAC
  heading rule (in both the method preview and the results) are now drawn under
  the same fixed seed, so they stay put across redraws, and the downloaded
  script sets that seed to reproduce them.

* `circ_summary_headings()` with the default `group_by = "id"` no longer errors
  ("numbers of columns of arguments do not match") when any trial produced no
  heading; such trials are returned with `n = 0` and `NA` statistics.

* `get_trial_limits()` (and so `get_all_object_pos()`) no longer returns `NA`
  manifest metadata for the second and later trials of a video when the file
  table has fewer rows than the video has trials.

* `circ_summary()` on weighted `Tracks` now weights `mean_dir`. It was the
  unweighted mean beside a weighted `resultant_R` and `kappa`, because
  `circular::mean.circular()` ignores a `weights` argument. Results change for
  any `Tracks` with a weight column, or with `w = `. `kappa` is `NA` unless
  three angles have a positive weight; zero-weight angles counted before.

* `circ_summary()` stops with an error of class `radiatR_nonestimable`, naming
  the id, when a group's weights are all zero. It returned `resultant_R = NaN`
  without comment.

* `circ_summary(w = )` checks the column it is given: a name that is not a
  column, or a column with missing, non-finite or negative values, is an error
  of class `radiatR_invalid_argument`. A misspelt name was ignored and the
  summary computed unweighted; rows with a missing weight were dropped.

* `circ_summarise(display = )` requires a `circ_display()` object and says so
  (class `radiatR_invalid_argument`). `display = NULL` failed with "argument is
  of length zero".

# radiatR 0.1.2

## Breaking changes

* The `ctrax` loader dialect and its optional `R.matlab` dependency have been
  removed. `.mat` files are no longer supported; `read_tracks(dialect = "ctrax")`
  now errors as an unknown dialect. Pre-computed orientation angles remain
  supported through the general `ellipse_axis` heading rule on any column.
* `read_tracks()` now fails on non-finite coordinate rows by default, reporting
  the affected row numbers and ids; pass `on_invalid = "drop"` to drop them with
  a warning instead of the previous silent drop (**breaking**).

## Bug fixes

* The Shiny app no longer shows a previous analysis's results after a new
  headings upload. In headings mode a failed file upload used to leave the
  earlier (e.g. example) headings in place, and the "example headings already
  loaded" shortcut could then display them as if they belonged to the file
  just selected. The upload now clears any prior analysis before reading and
  commits the parsed data only on success, and the shortcut to Results fires
  only for genuinely example-sourced headings.
* `c(Tracks, ...)` now rejects trajectory ids that collide across the objects
  being combined, and errors when the inputs carry conflicting `frame_rate`,
  `distance_scale`, `distance_unit`, or `normalize_xy` metadata (including one
  input having it set and another not). Previously these were silently merged,
  discarding one object's calibration in favour of the other's, and could
  interleave rows from two different trajectories that happened to share an
  id. Per-id/per-run metadata (`reference`, `trial_limits`, `sim_conditions`,
  `oob_points`, `oob_trials`) is now merged/summed across all combined inputs
  instead of only the first object's being kept.
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
* Between-group tests (`test_mean_directions()`, `test_concentration()`,
  `test_distributions()`) now signal a structured `radiatR_nonestimable`
  condition (carrying method, group, n, and reason) for non-estimable inputs,
  so callers can distinguish expected non-estimable cases from bugs.

## App

* The app no longer assumes a capture rate of 30 fps. The frame-rate controls
  now start unset, and time/speed colouring, the kinematics plot and its
  downloads, and the generated kinematics code stay disabled until a positive
  finite frame rate is entered or carried by the loaded data. The app shows
  whether the active rate came from the data or was entered by the user, and
  downloaded kinematics code no longer emits a fabricated `set_frame_rate(ts,
  30)` when no rate is set. Previously, uncalibrated numeric-frame data produced
  plausible-looking speed and turning-rate results based on the unsupported
  default.
* Ingest failures now show the original exception (with affected rows/ids)
  under "Technical details" and no longer suppress ingest warnings; the
  group-comparison table shows the non-estimable reason instead of a blank dash
  and re-raises unexpected errors; downloads are gated (`results_ok()`) so a
  failed or partial run cannot emit an apparently-valid result.

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
* The app now exposes an explicit **Coordinates** control for the per-trajectory
  shape transform (`normalize_xy`), which was previously pinned on with no way
  to opt out. The transform fits each trajectory to the unit circle
  independently and preserves path shape, not bearings from a fixed arena
  origin; the control labels it as such rather than as a calibration. Downloaded
  figure, statistics, and kinematics scripts now emit the `normalize_xy` value
  the app actually loaded with, instead of silently falling back to the package
  default.

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
