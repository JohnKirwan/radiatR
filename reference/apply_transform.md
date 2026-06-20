# Apply a bespoke transformation to a Tracks

Applies a user-supplied function to a loaded \[\`Tracks\`\] – for
example a reference-frame correction or an angular remapping – after
loading and before summary or plotting, returning a modified \`Tracks\`
and recording the step in its \[\`transform_history\`\]. The function is
written against the \`Tracks\`'s column *roles* (\`x@cols\`), not
hard-coded column names.

## Usage

``` r
apply_transform(x, fn, ..., by = c("trajectory", "all"), step = NULL)
```

## Arguments

- x:

  A \[\`Tracks\`\].

- fn:

  A function. With \`by = "trajectory"\` (default) it is called once per
  trajectory as \`fn(df, cols, ...)\`, where \`df\` is that trajectory's
  rows (all columns, including metadata covariates) and \`cols\` is
  \`x@cols\`. With \`by = "all"\` it is called once on the whole data
  frame. It must return a data frame with the same rows (same
  \`id\`/\`time\`); a transform adjusts values, it does not add or drop
  trials.

- ...:

  Further arguments passed to \`fn\`.

- by:

  Either \`"trajectory"\` (default; per-trajectory) or \`"all"\`
  (whole-frame).

- step:

  Label recorded in \`transform_history\`. Defaults to the name of
  \`fn\`, or \`"apply_transform"\` for anonymous functions.

## Value

A \[\`Tracks\`\] with \`@data\` replaced by the transformed result and
one step appended to its \`transform_history\`.

## See also

\[transform_history()\], \[log_transform()\]

## Examples

``` r
data(cpunctatus)

# Recipe A -- a reference-frame offset (e.g. edge-referenced -> centre-
# referenced stimulus) is a frame change, so use set_reference(), not
# apply_transform(): it re-derives rel_theta/rel_x/rel_y consistently. Because
# rel_theta = abs - reference, offsetting the heading by +half (half the
# stimulus's angular width) is a reference change of (reference - half).
ts     <- set_reference(cpunctatus, 0)        # ensure a reference exists
half   <- (20 / 2) * pi / 180                 # a 20-degree-wide stimulus
newref <- reference(ts)$ref_theta - half      # per-trajectory if widths differ
ts2    <- set_reference(ts, stats::setNames(newref, reference(ts)$id))

# Recipe B -- polarization direction -> axis. Polarized-light e-vectors are
# axial (defined mod pi); doubling folds direction into orientation for axial
# circular statistics. (Doubling remaps the angular space, so rel_x/rel_y are
# not physical positions afterwards -- use it for the angle/stat path.)
direction_to_axis <- function(df, cols) {
  df[[cols$angle]] <- (2 * df[[cols$angle]]) %% (2 * pi)
  df
}
ts_axial <- apply_transform(cpunctatus, direction_to_axis, by = "all",
                            step = "direction_to_axis")
```
