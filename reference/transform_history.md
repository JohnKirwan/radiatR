# Transform history helpers for TrajSet objects

Transform history helpers for TrajSet objects

## Usage

``` r
transform_history(x)

# S4 method for class 'TrajSet'
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

# S4 method for class 'TrajSet'
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

# S4 method for class 'TrajSet'
set_transform_history(x, history)
```

## Arguments

- x:

  A \`TrajSet\` object.

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
\`log_transform\` and \`set_transform_history\`, the updated \`TrajSet\`
object.
