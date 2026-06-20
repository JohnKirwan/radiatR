# Plot trajectories from a Tracks (overlay or faceted)

Plot trajectories from a Tracks (overlay or faceted)

## Usage

``` r
gg_traj(
  x,
  color = NULL,
  linetype = NULL,
  alpha = NULL,
  size = 0.6,
  panel_by = NULL,
  coord = c("polar", "cartesian"),
  geom = c("path"),
  thin = 1L,
  ncol = NULL
)

# S4 method for class 'Tracks'
gg_traj(
  x,
  color = NULL,
  linetype = NULL,
  alpha = NULL,
  size = 0.6,
  panel_by = NULL,
  coord = c("polar", "cartesian"),
  geom = c("path"),
  thin = 1L,
  ncol = NULL
)
```

## Arguments

- x:

  Tracks

- color, linetype, alpha, size:

  Optional column names (strings) mapped to aesthetics

- panel_by:

  NULL, a single string, or a character vector of columns to facet by

- coord:

  "polar" (unit circle) or "cartesian"

- geom:

  "path" or "point" (or both, as c("path","point"))

- thin:

  Keep every n-th point per id (for very long tables)

- ncol:

  Number of facet columns (when faceting)

## Value

ggplot object
