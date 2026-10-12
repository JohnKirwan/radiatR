# Construct a Tracks from a data.frame or file(s)

Construct a Tracks from a data.frame or file(s)

## Usage

``` r
read_tracks(
  x,
  mapping = list(id = NULL, time = NULL, x = NULL, y = NULL, angle = NULL, weight = NULL),
  angle_unit = c("radians", "degrees", "auto"),
  time_type = c("auto", "posix", "seconds", "frames"),
  tz = "UTC",
  fps = NULL,
  normalize_xy = FALSE,
  origin = NULL,
  radius = NULL,
  dialect = NULL,
  dialect_args = list(),
  read_opts = list(delim = NULL, decimal = NULL, sheet = NULL),
  mutate = NULL,
  keep = NULL,
  drop = NULL,
  id_from_filename = TRUE,
  validate = TRUE,
  format = NULL,
  id_collision = c("error", "namespace"),
  on_invalid = c("error", "drop")
)
```

## Arguments

- x:

  data.frame, file path, or character vector of file paths

- mapping:

  named list for column mapping: id, time, x, y, angle, weight. Any
  missing will be guessed when possible.

- angle_unit:

  "radians","degrees", or "auto" to guess from values

- time_type:

  one of "auto","posix","seconds","frames". Numeric time is taken to be
  a frame index unless \`time_type = "seconds"\` (or the column is a
  duration): seconds are recorded as such in the Tracks metadata
  (\`time_unit\`), so \[elapsed_seconds()\] and the kinematics use them
  directly, with no frame rate.

- tz:

  timezone for POSIX times (default "UTC")

- fps:

  Frames-per-second. Required when time_type = "frames" (the time column
  is kept as a raw frame index). When a single finite positive value is
  supplied and the time column is frame-indexed, it is stored as the
  Tracks capture frame rate (see \[frame_rate()\]);
  \[elapsed_seconds()\] then performs the frame -\> seconds conversion
  once, on demand, so kinematics and the app adopt it automatically
  without converting twice.

- normalize_xy:

  FALSE (default) keeps (x,y) as supplied. TRUE independently centres
  each trajectory on its own bounding-box midpoint and scales it to the
  unit circle – shape-preserving only, NOT calibration (it does not
  preserve bearing from a fixed arena origin). See \[tracks()\] for the
  full explanation.

- origin, radius:

  Optional fixed unit-circle calibration forwarded to \[tracks()\]:
  \`origin = c(x, y)\` centre and positive scalar \`radius\` in raw
  coordinate units. Supplied together, they map every trajectory
  uniformly via \`(xy - origin) / radius\`, preserving bearings from the
  origin. Mutually exclusive with \`normalize_xy = TRUE\`.

- dialect:

  optional registered dialect name. The dialect reads \`x\` itself
  (\`read_tracks()\` does not parse the file first) and is called once
  per file when \`x\` holds several paths. See
  \[register_loader_dialect()\].

- dialect_args:

  named list of extra arguments forwarded to the dialect function (e.g.
  `list(bodypart = c("head","thorax"))`)

- read_opts:

  list of file-reading overrides: \`delim\` (field separator),
  \`decimal\` (decimal mark), and \`sheet\` (Excel worksheet name or
  number). Any \`NULL\` element is auto-detected. Defaults to all-auto.

- mutate:

  list of transformations applied after reading (named functions or
  formulas)

- keep:

  only keep these columns (NULL = keep all)

- drop:

  drop these columns after mapping

- id_from_filename:

  if TRUE and id missing, derive id from file stem when reading multiple
  files

- validate:

  Deprecated and ignored: \`Tracks\` invariants are always checked. Any
  value but \`TRUE\` warns.

- format:

  Optional loader format name or list spec registered via
  \[register_loader_format()\]

- id_collision:

  how to handle a trajectory id that appears under more than one source
  file during a multi-file import: \`"error"\` (default) stops with a
  message naming the colliding id(s) and files; \`"namespace"\` rewrites
  every id to \`paste0(file_stem, "::", id)\` so ids are unique per
  file. Has no effect on single-file or data.frame input.

- on_invalid:

  How to handle rows whose x/y coordinates are missing or non-finite:
  "error" (default) aborts with a radiatR_invalid_rows condition listing
  the affected rows and ids; "drop" removes them and signals a
  radiatR_dropped_rows warning with the same payload. It covers nothing
  else: a missing time, id or weight is an error either way, and so is a
  malformed value (see "Malformed values").

## Value

Tracks

## Time units from a dialect

A dialect that knows the unit of its time column says so, and under the
default \`time_type = "auto"\` that decides how the column is read:
TrackMate \`POSITION_T\` (when the version 7 units row gives seconds),
ToxTrac \`Time (sec)\`, idtracker.ai \`seconds\` and TRex \`time\` are
seconds, and need no \`fps\`; \`FRAME\`/\`frame\` columns are frames. A
contrary explicit \`time_type\` stops with a
\`radiatR_time_type_mismatch\` error, as do files of one import that
disagree.

## Malformed values

A cell in an x, y, angle, weight or time column that is present but does
not parse (\`"oops"\`, \`"1,5"\`, a stray header or units row) stops the
load with a \`radiatR_malformed_values\` error, a subclass of
\`radiatR_invalid_rows\`. Its \`columns\`, \`rows\`, \`values\`,
\`source\` and \`dialect\` fields name the column, the data rows
(counted from the first row after the header, within its own file in a
multi-file import), the offending strings, the file and the dialect.
Such a value is never dropped or turned into \`NA\`. Blank cells and
\`NA\` are missing values; \`nan\` and \`inf\` are read as such. Files
are read with no digit-grouping mark, so \`1,5\` is malformed unless
\`read_opts = list(decimal = ",")\` says the comma is the decimal mark.

The \`trackmate\`, \`toxtrac\`, \`idtrackerai_wide\` and \`trex\`
dialects check their source columns the same way, before any reshaping,
so the error names the header as the program wrote it and the row of the
file's table. A missing or non-finite position found afterwards (see
\`on_invalid\`) is still counted in the rows of the table the dialect
returns, which for a wide format is the reshaped one. Coordinates that
the pose and zone dialects convert themselves (ANY-maze, EthoVision,
DeepLabCut, SLEAP) are not yet checked this way.

A time column given as text must hold either numbers throughout or
timestamps in one layout throughout: \`YYYY-MM-DD HH:MM:SS\`,
\`YYYY-MM-DD HH:MM\` or \`YYYY-MM-DD\` (with \`-\` or \`/\`), or ISO
8601 \`YYYY-MM-DDTHH:MM:SS\` with an optional \`Z\` for UTC. A bare date
beside date-times is midnight.
