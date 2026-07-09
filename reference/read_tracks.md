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
  normalize_xy = TRUE,
  dialect = NULL,
  dialect_args = list(),
  read_opts = list(delim = NULL, decimal = NULL, sheet = NULL),
  mutate = NULL,
  keep = NULL,
  drop = NULL,
  id_from_filename = TRUE,
  validate = TRUE,
  format = NULL
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

  one of "auto","posix","seconds","frames"

- tz:

  timezone for POSIX times (default "UTC")

- fps:

  Frames-per-second. Used to convert frame indices to seconds when
  time_type = "frames", and, when a single finite positive value is
  supplied, stored as the Tracks capture frame rate (see
  \[frame_rate()\]) so kinematics and the app adopt it automatically.

- normalize_xy:

  TRUE to normalize (x,y) to unit circle when both provided

- dialect:

  optional registered dialect name to pre-process raw input

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

  if TRUE run S4 validity checks

- format:

  Optional loader format name or list spec registered via
  \[register_loader_format()\]

## Value

Tracks
