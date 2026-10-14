# Construct a Tracks from a \*format\* spec (registered name or inline list)

Construct a Tracks from a \*format\* spec (registered name or inline
list)

## Usage

``` r
read_tracks_format(x, format, ...)
```

## Arguments

- x:

  data.frame or path(s)

- format:

  registered format name or list spec

- ...:

  extra args override spec fields

## Value

A \`Tracks\` object, as returned by \[read_tracks()\].

## Examples

``` r
raw <- data.frame(animal = "m1", seconds = c(0, 0.5, 1),
                  pos_x = c(0.1, 0.2, 0.3), pos_y = c(0, 0.05, 0.1))
# An inline spec: columns found by pattern, time in seconds
spec <- list(
  regex_mapping = list(id = "^animal", time = "^sec",
                       x = "^pos_?x", y = "^pos_?y"),
  time = list(type = "seconds")
)
read_tracks_format(raw, spec)
#> Tracks: 1 trajectories, 3 observations
#> Columns: id='animal', time='seconds', angle='angle' (radians), x='pos_x', y='pos_y'
#>   animal seconds pos_x pos_y     angle
#> 1     m1     0.0   0.1  0.00 0.0000000
#> 2     m1     0.5   0.2  0.05 0.2449787
#> 3     m1     1.0   0.3  0.10 0.3217506

# The same spec registered under a name
register_loader_format("my_tracker", spec, overwrite = TRUE)
read_tracks_format(raw, "my_tracker")
#> Tracks: 1 trajectories, 3 observations
#> Columns: id='animal', time='seconds', angle='angle' (radians), x='pos_x', y='pos_y'
#>   animal seconds pos_x pos_y     angle
#> 1     m1     0.0   0.1  0.00 0.0000000
#> 2     m1     0.5   0.2  0.05 0.2449787
#> 3     m1     1.0   0.3  0.10 0.3217506
```
