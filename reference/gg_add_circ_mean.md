# Add mean direction arrows to a ggplot polar plot

Add mean direction arrows to a ggplot polar plot

## Usage

``` r
gg_add_circ_mean(
  p,
  segments_df,
  color = "black",
  linewidth = 0.8,
  arrow_spec = ggplot2::arrow(length = grid::unit(0.02, "npc")),
  inherit_aes = FALSE
)
```

## Arguments

- p:

  ggplot built from gg_traj(...) or your own polar builder

- segments_df:

  data from circ_mean_segments()

- color:

  arrow color

- linewidth:

  segment linewidth

- arrow_spec:

  ggplot2::arrow(...) for arrow heads

- inherit_aes:

  whether to inherit aes (usually FALSE)
