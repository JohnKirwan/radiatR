# Circumference scale: seconds (or minutes)

Draws 60 ticks and labels a sparse subset. Also serves a minutes dial.

## Usage

``` r
scale_seconds(every = 15)
```

## Arguments

- every:

  Label every \`every\` divisions (default \`15\`). Must evenly divide
  60.

## Value

A circumference-scale list for \[circumference_labs()\].

## See also

\[circumference_labs()\], \[scale_cardinal()\], \[scale_clock()\],
\[scale_months()\]

## Examples

``` r
scale_seconds(every = 10)
#> $n
#> [1] 60
#> 
#> $at
#> [1] 1.5707963 0.5235988 5.7595865 4.7123890 3.6651914 2.6179939
#> 
#> $labels
#> [1] "0"  "10" "20" "30" "40" "50"
#> 
#> $name
#> [1] "seconds"
#> 
```
