# Read the canonical attributes of a heading frame

Accessors for the metadata a \[headings_frame\] carries. They also work
on a plain data frame, returning sensible defaults, so any function can
read the display convention without assuming the input is classed.

## Usage

``` r
hf_display(x)

hf_heading_col(x)

hf_colour_col(x)

hf_color_col(x)

hf_coords(x)
```

## Arguments

- x:

  A \`headings_frame\` or plain data frame.

## Value

\`hf_display()\` a \[circ_display()\] object; \`hf_heading_col()\` /
\`hf_coords()\` a string; \`hf_colour_col()\` a string or \`NULL\`.

## Examples

``` r
hf <- headings_frame(data.frame(h = c(0.1, 1.2, 3)), h, units = "radians")
hf_heading_col(hf)
#> [1] "h"
hf_coords(hf)
#> [1] "absolute"
hf_display(hf)
#> $zero
#> [1] 1.570796
#> 
#> $clockwise
#> [1] TRUE
#> 
#> $units
#> [1] "degrees"
#> 
#> attr(,"class")
#> [1] "circ_display"
# a plain data frame gives the defaults
hf_heading_col(data.frame(h = 1))
#> [1] "heading"
```
