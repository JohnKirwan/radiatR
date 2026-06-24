# Make ggplot object of tracks radiating from circle centre.

Accepts either a precomputed data frame of polar/cartesian coordinates
or a \`Tracks\`. When a \`Tracks\` is supplied, column mappings are
inferred from the object and handed off to the plotting helpers.

## Usage

``` r
radiate(data, ...)

# S3 method for class 'Tracks'
radiate(data, ...)

# Default S3 method
radiate(
  data,
  x_col = "rel_x",
  y_col = "rel_y",
  geom = "path",
  group_col = NULL,
  colour_col = NULL,
  colour_cycle = NULL,
  track_colour = c("trajectory", "sequence", "time", "speed"),
  time_units = c("seconds", "minutes", "milliseconds"),
  coords = c("relative", "absolute"),
  panel_by = NULL,
  ncol = NULL,
  strip_labels = NULL,
  strip_position = c("top", "bottom", "left", "right", "inside"),
  strip_label_size = 11,
  ticks = NULL,
  degrees = NULL,
  legend = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  axes = NULL,
  angle_labels = c("degrees", "none", "radians", "cardinal", "hours", "months",
    "seconds"),
  theme = c("void", "minimal", "classic", "bw", "grey", "gray", "light", "dark",
    "linedraw"),
  quadrants = FALSE,
  rings = FALSE,
  grid = c("radial", "cartesian", "none"),
  grid_colour = NULL,
  origin = FALSE,
  circumference = TRUE,
  show_labels = NULL,
  label_col = NULL,
  label_size = 3,
  label_padding = 1.08,
  label_use_repel = TRUE,
  show_tracks = TRUE,
  show_arrow = NULL,
  arrow_angle_col = NULL,
  arrow_colour = "black",
  arrow_colour_col = NULL,
  arrow_size = 2,
  display = circ_display(),
  ...,
  color_col = NULL,
  color_cycle = NULL,
  track_color = NULL,
  grid_color = NULL,
  arrow_color = NULL,
  arrow_color_col = NULL
)

# S3 method for class 'headings_frame'
radiate(
  data,
  col = NULL,
  step = 0.025,
  start_sep = step/2,
  tol = NULL,
  direction = "inward",
  base_r = 1,
  shade = FALSE,
  shape = FALSE,
  panel_by = NULL,
  ncol = NULL,
  ticks = TRUE,
  degrees = TRUE,
  angle_labels = c("degrees", "none", "radians", "cardinal", "hours", "months",
    "seconds"),
  title = NULL,
  theme = c("void", "minimal", "classic", "bw", "grey", "gray", "light", "dark",
    "linedraw"),
  quadrants = FALSE,
  rings = FALSE,
  grid = c("radial", "cartesian", "none"),
  grid_colour = NULL,
  circumference = TRUE,
  origin = FALSE,
  colour_col = NULL,
  legend = FALSE,
  display = circ_display(),
  show_markers = TRUE,
  ...
)
```

## Arguments

- data:

  Data frame or \`Tracks\`.

- ...:

  Additional arguments forwarded to \[draw_tracks()\].

- x_col:

  Name of the x-coordinate column. Default `"rel_x"`.

- y_col:

  Name of the y-coordinate column. Default `"rel_y"`.

- geom:

  Geom specification passed to \[draw_tracks()\].

- group_col:

  Optional column for grouping aesthetics.

- colour_col, color_col:

  Optional column for colour aesthetics. Mutually exclusive with
  \`colour_cycle\`. \`color_col\` is the American-spelling alias.

- colour_cycle, color_cycle:

  Optional cycling colour specification. Either a positive integer \`n\`
  (trajectories are assigned colours 1–n, cycling back to 1 after every
  \`n\` trajectories) or a character vector of colour values (e.g.
  \`c("red","blue","green")\`). When \`panel_by\` is set the cycle
  restarts independently within each panel. Mutually exclusive with
  \`colour_col\`. \`color_cycle\` is the American-spelling alias.

- track_colour, track_color:

  How trajectory paths are coloured. \`"trajectory"\` (default) keeps
  the existing per-track colouring. \`"sequence"\` colours each path by
  its point's normalized position from start (0) to finish (1) within
  the track, applying a continuous viridis scale with a \`"start -\>
  finish"\` colourbar. \`"time"\` colours each path by real elapsed time
  from its own track's start (see \[elapsed_seconds()\]), applying a
  continuous viridis scale with an \`"elapsed (s)"\` colourbar; numeric
  (frame) time requires a \[frame_rate()\] (set one with
  \`set_frame_rate(ts, fps)\`). \`"speed"\` colours each path by its
  instantaneous speed (see \[instantaneous_speed()\]), applying a
  continuous viridis scale with a \`"speed (units/s)"\` colourbar;
  numeric (frame) time likewise requires a \[frame_rate()\].
  \`"sequence"\`, \`"time"\`, and \`"speed"\` own the colour aesthetic
  and so cannot be combined with \`colour_col\`/\`colour_cycle\`;
  overlays render in a fixed colour. The per-track order is taken from
  the \`Tracks\` time column, falling back to row order (with a message)
  when no usable time column is present. \`track_color\` is the
  American-spelling alias.

- time_units:

  Units for \`track_colour = "time"\`: \`"seconds"\` (default),
  \`"minutes"\`, or \`"milliseconds"\`. Sets the colourbar title and
  scale.

- coords:

  Which reference frame to plot: \`"relative"\` (default) uses the
  landmark-relative position (\`rel_x\`/\`rel_y\`) and relative
  headings; \`"absolute"\` uses the arena-native unit-circle position
  (\`x\`/\`y\`) and un-rotated absolute headings, useful as an
  experimental control. Ignored when explicit \`x_col\`/\`y_col\` are
  supplied. Errors if \`"absolute"\` is requested but the \`Tracks\` has
  no \`x\`/\`y\` columns registered.

- panel_by:

  NULL, a column name, or a character vector of column names to facet by
  (via \[ggplot2::facet_wrap()\]). The named column(s) must be present
  in the data.

- ncol:

  Number of columns passed to \[ggplot2::facet_wrap()\] when
  \`panel_by\` is set.

- strip_labels:

  Logical or \`NULL\`. Whether to show a label identifying the panel
  variable value on each panel. Defaults to \`TRUE\` when \`panel_by\`
  is set, \`FALSE\` otherwise. Ignored when \`panel_by\` is \`NULL\`.

- strip_position:

  Position of the panel label. One of \`"top"\` (default), \`"bottom"\`,
  \`"left"\`, \`"right"\` (ggplot2 strip positions), or \`"inside"\`
  (places a text annotation inside the plot area, centred below the unit
  circle at y = -1.25).

- strip_label_size:

  Font size for strip labels. Applies to both strip text and the
  in-panel \`"inside"\` annotation.

- ticks, degrees, legend, title, xlab, ylab, axes:

  Additional styling options. \`degrees\` is retained for
  back-compatibility; \`degrees = FALSE\` is equivalent to
  \`angle_labels = "none"\`. Tick styling (colour, width, length)
  follows the chosen \`theme\`'s \`axis.ticks\`.

- angle_labels:

  Circumference labels. \`"degrees"\` (default; e.g. \`45°\`),
  \`"radians"\` (e.g. \`π/4\`), or \`"none"\` use diagonal angle labels
  via \[degree_labs()\]. The domain scales \`"cardinal"\` (N/E/S/W),
  \`"hours"\` (24-hour clock), \`"months"\` (Jan...Dec), and
  \`"seconds"\` instead label the perimeter via \[perimeter_labs()\],
  aligning the tick count to the scale. For finer control (8-point
  compass, 12-hour clock, month formats) add an explicit
  \`perimeter_labs()\` layer. Label styling follows the chosen
  \`theme\`'s \`axis.text\`.

- theme:

  Plot appearance, named for the ggplot2 base themes: one of \`"void"\`
  (default), \`"minimal"\`, \`"classic"\`, \`"bw"\`, \`"grey"\`,
  \`"light"\`, \`"dark"\`, or \`"linedraw"\`. See \[radial_theme()\].

- quadrants:

  Logical; draw the two dashed lines through the origin that demarcate
  the quadrants. Default \`FALSE\`. Their colour and width follow the
  chosen \`theme\`'s grid lines (see \[radial_theme()\]).

- rings:

  Logical; draw concentric guide rings (the radial analogue of a grid).
  Default \`FALSE\`. Their colour and width follow the chosen
  \`theme\`'s grid lines.

- grid:

  One of \`"radial"\` (default), \`"cartesian"\`, or \`"none"\`.
  \`"radial"\` replaces the theme's Cartesian grid with theme-styled
  radial guides (circular disc + major/minor crosshairs and rings) for
  grid-bearing themes, and draws nothing for grid-less themes (\`void\`,
  \`classic\`). \`"cartesian"\` keeps the theme's square grid;
  \`"none"\` removes all gridlines.

- grid_colour, grid_color:

  Optional colour overriding the theme-derived guide colour.
  \`grid_color\` is the American-spelling alias.

- origin:

  Logical; draw a centre point. Default \`FALSE\`. When drawn it takes
  the theme's axis ink colour.

- circumference:

  Logical; draw the unit-circle circumference as a fallback boundary
  when no radial grid marks it (grid-less themes, \`grid = "none"\`).
  Default \`TRUE\`. Styled from the theme's \`axis.line\` (else the axis
  ink). On grid-bearing themes the grid's outer ring marks the boundary
  instead, so this has no effect there.

- show_labels:

  Whether to place labels at the perimeter.

- label_col:

  Column containing label values.

- label_size:

  Text size for perimeter labels.

- label_padding:

  Multiplier applied to the unit circle when placing labels.

- label_use_repel:

  Use \`ggrepel::geom_text_repel()\` when available.

- show_tracks:

  Whether to draw the trajectory paths. Default \`TRUE\`. Set to
  \`FALSE\` to render the unit circle and any overlays (arrow, circle,
  ticks) without the track geometry.

- show_arrow:

  Whether to draw a mean resultant arrow from the centre.

- arrow_angle_col:

  Column containing angles (radians) to summarise for the arrow.

- arrow_colour, arrow_color:

  Arrow colour (a single fixed colour). Ignored when
  \`arrow_colour_col\` is set. \`arrow_color\` is the American-spelling
  alias.

- arrow_colour_col, arrow_color_col:

  Optional grouping column. When supplied, one mean resultant arrow is
  drawn per level of this column (within each panel, if \`panel_by\` is
  also set) and mapped to the colour aesthetic, so the arrow can follow
  a colour grouping independently of faceting. Default \`NULL\` draws a
  single arrow in \`arrow_colour\`. \`arrow_color_col\` is the
  American-spelling alias.

- arrow_size:

  Arrow linewidth.

- display:

  A \[\`circ_display\`\] object controlling how angles are rendered.
  Default \`circ_display()\` puts North at top with clockwise-positive
  degrees. Use \`circ_display(zero = 0)\` when the reference direction
  lies at East in unit-circle coordinates (e.g. the \`cpunctatus\`
  dataset).

- col:

  Name of the angle column in `data`. Defaults to the `heading_col`
  attribute when `data` is a
  [`headings_frame`](https://johnkirwan.github.io/radiatR/reference/headings_frame.md).

- step, start_sep, tol, direction, base_r, shade, shape:

  Passed to
  [`add_stacked_headings`](https://johnkirwan.github.io/radiatR/reference/add_stacked_headings.md).
  See that function for details.

- show_markers:

  When `TRUE` (default) the stacked-dot markers are drawn; `FALSE`
  returns the themed radial frame only, for callers that layer their own
  marker and statistic overlays.

## Value

A \`ggplot2\` object.

## American spellings

Every \`colour...\` argument and the \`assign_colour\_\*\` /
\`cycle_colours\` / \`hf_colour_col\` functions accept the American
\`color...\` spelling as an alias (e.g. \`color\`, \`color_col\`,
\`track_color\`). British spelling is canonical; supplying both
spellings of a pair is an error.

## Examples

``` r
tracks_demo <- simulate_tracks(conditions = data.frame(n_trials = 1L),
                               n_points = 200, seed = 1)
radiate(tracks_demo, x_col = "rel_x", y_col = "rel_y", group_col = "trial_id")

radiate(cpunctatus, coords = "absolute")
```
