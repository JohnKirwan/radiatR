# Set the per-trajectory reference and re-derive the relative frame

Updates each trajectory's reference direction and re-derives its
relative columns (\`rel_theta\`/\`rel_x\`/\`rel_y\`) from the
unit-circle position via \[derive_coords()\], so the relative frame
stays consistent. The step is recorded in \[transform_history()\]. This
is the drift-safe way to change the reference frame – prefer it over a
manual \[apply_transform()\].

## Usage

``` r
set_reference(x, value)

# S4 method for class 'TrajSet'
set_reference(x, value)
```

## Arguments

- x:

  A \[\`TrajSet\`\] with position roles (\`cols\$x\`/\`cols\$y\`) and
  relative roles (\`cols\$angle\`/\`cols\$rel_x\`/\`cols\$rel_y\`)
  registered.

- value:

  Reference direction(s) in unit-circle radians: a scalar applied to all
  trajectories, or a named numeric vector / two-column
  (\`id\`,\`ref_theta\`) data frame setting them per trajectory.

## Value

The updated \[\`TrajSet\`\].

## See also

\[reference()\], \[derive_coords()\]
