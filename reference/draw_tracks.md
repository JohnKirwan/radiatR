# Create geom layers for Cartesian track coordinates

Create geom layers for Cartesian track coordinates

## Usage

``` r
draw_tracks(data, x_col, y_col, geom = "path", mapping = NULL, ...)
```

## Arguments

- data:

  Data frame that will be plotted.

- x_col:

  Name of the column mapped to the x aesthetic.

- y_col:

  Name of the column mapped to the y aesthetic.

- geom:

  Either a character vector (\`"path"\`/\`"point"\`) or a ggplot2 geom
  function (e.g. \[ggplot2::geom_path\]).

- mapping:

  Optional aesthetics created with \[ggplot2::aes()\].

- ...:

  Additional arguments passed on to the geom function.

## Value

A list containing a single ggplot2 layer.
