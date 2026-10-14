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

# S4 method for class 'Tracks'
set_reference(x, value)
```

## Arguments

- x:

  A \[\`Tracks\`\] with position roles (\`cols\$x\`/\`cols\$y\`) and
  relative roles (\`cols\$angle\`/\`cols\$rel_x\`/\`cols\$rel_y\`)
  registered.

- value:

  Reference direction(s) in unit-circle radians: a scalar applied to all
  trajectories, or a named numeric vector / two-column
  (\`id\`,\`ref_theta\`) data frame setting them per trajectory.

## Value

The updated \[\`Tracks\`\].

## See also

\[reference()\], \[derive_coords()\]

## Examples

``` r
x <- cpunctatus[1:2]
# one reference for every trajectory, in unit-circle radians
reference(set_reference(x, pi / 2))
#> # A tibble: 2 × 2
#>   id      ref_theta
#>   <chr>       <dbl>
#> 1 10_1_1       1.57
#> 2 10_10_1      1.57
# or one per trajectory, named by id; a reference of 0 makes the
# relative frame equal the absolute one
x <- set_reference(x, stats::setNames(c(0, pi), ids(x)))
head(as.data.frame(x)[c("trial_id", "abs_theta", "rel_theta")])
#> # A tibble: 6 × 3
#>   trial_id abs_theta rel_theta
#>   <chr>        <dbl>     <dbl>
#> 1 10_1_1       0         0    
#> 2 10_1_1       0.541     0.541
#> 3 10_1_1       0.985     0.985
#> 4 10_1_1       1.57      1.57 
#> 5 10_1_1       5.03      5.03 
#> 6 10_1_1       5.18      5.18 
```
