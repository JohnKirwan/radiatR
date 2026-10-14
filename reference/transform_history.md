# Transform history helpers for Tracks objects

Transform history helpers for Tracks objects

## Usage

``` r
transform_history(x)

# S4 method for class 'Tracks'
transform_history(x)

log_transform(
  x,
  step,
  traj_ids = NULL,
  implementation = step,
  params = NULL,
  order = NULL,
  depends_on = NULL
)

# S4 method for class 'Tracks'
log_transform(
  x,
  step,
  traj_ids = NULL,
  implementation = step,
  params = NULL,
  order = NULL,
  depends_on = NULL
)

set_transform_history(x, history)

# S4 method for class 'Tracks'
set_transform_history(x, history)
```

## Arguments

- x:

  A \`Tracks\` object.

- step:

  Character identifier for the transform step.

- traj_ids:

  Character vector of trajectory identifiers affected by the step.
  Defaults to all trajectories in \`x\` when \`NULL\`.

- implementation:

  Character label for the implementation used to apply the step.
  Defaults to \`step\`.

- params:

  List-column of per-trajectory parameter sets (recycled when a single
  entry is provided).

- order:

  Optional integer giving the execution order. When omitted the step is
  appended to the end of the log.

- depends_on:

  Optional character vector naming prerequisite step(s).

- history:

  Tibble or list describing the full transform history to replace.

## Value

For \`transform_history\`, a tibble describing the recorded steps. For
\`log_transform\` and \`set_transform_history\`, the updated \`Tracks\`
object.

## Examples

``` r
df <- data.frame(id = "a", time = 1:3, x = c(0, 0.3, 0.6), y = c(0, 0.2, 0.5))
ts <- tracks(df, x = "x", y = "y")
ts <- log_transform(ts, "smooth", params = list(list(window = 3)))
transform_history(ts)
#> # A tibble: 1 × 6
#>   step   order id    implementation params           depends_on
#>   <chr>  <int> <chr> <chr>          <list>           <list>    
#> 1 smooth     1 a     smooth         <named list [1]> <chr [0]> 
# replace the whole log, here with an empty one
ts <- set_transform_history(ts, transform_history(ts)[0, ])
nrow(transform_history(ts))
#> [1] 0
```
