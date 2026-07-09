# Per-trajectory reference direction of a Tracks

The reference direction (unit-circle radians) against which each
trajectory's relative frame (\`rel_theta\`/\`rel_x\`/\`rel_y\`) is
defined. Trajectories with no recorded reference default to \`0\`
(relative frame equals absolute frame).

## Usage

``` r
reference(x)

# S4 method for class 'Tracks'
reference(x)
```

## Arguments

- x:

  A \[\`Tracks\`\].

## Value

For \`reference()\`, a data frame with \`id\` and \`ref_theta\` (or
\`NULL\` when none is set).

## See also

\[set_reference()\], \[derive_coords()\]
