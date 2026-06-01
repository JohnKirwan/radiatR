# Build a data frame of arrow segments representing mean direction vectors Length equals resultant_R; angle equals mean_dir

Build a data frame of arrow segments representing mean direction vectors
Length equals resultant_R; angle equals mean_dir

## Usage

``` r
circ_mean_segments(stats_df, x0 = 0, y0 = 0, scale = 1)
```

## Arguments

- stats_df:

  output of circ_summary() or circ_summary_headings()

- x0, y0:

  arrow origin (defaults 0,0)

- scale:

  multiply resultant_R by this factor when drawing

## Value

data.frame with x,y,xend,yend and any grouping columns kept
