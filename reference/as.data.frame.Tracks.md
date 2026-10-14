# Coerce a Tracks to a data frame

Returns the long-form trajectory table held in the Tracks's \`data\`
slot.

## Usage

``` r
# S3 method for class 'Tracks'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A \[Tracks-class\] object.

- row.names, optional:

  Accepted for S3 generic consistency; ignored.

- ...:

  Ignored.

## Value

A data frame: the Tracks's \`data\` slot.

## Details

Registered as an S3 method so that \`as.data.frame(ts)\` dispatches
correctly from any caller. (A bare S4 method on the base S3 generic is
only reached from within the package's own namespace, not from user code
using the installed package.)

## Examples

``` r
head(as.data.frame(cpunctatus))
#> # A tibble: 6 × 18
#>   trial_id frame     x     y trans_x  trans_y trans_rho abs_theta rel_theta
#>   <chr>    <int> <dbl> <dbl>   <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 10_1_1       1  456.  372. 0.00511  0         0.00511     0         6.21 
#> 2 10_1_1       2  458.  370. 0.0128   0.00770   0.0149      0.541     0.464
#> 3 10_1_1       3  456.  370. 0.00511  0.00770   0.00924     0.985     0.908
#> 4 10_1_1       4  454.  370. 0        0.00770   0.00770     1.57      1.49 
#> 5 10_1_1       5  456.  377. 0.00511 -0.0153    0.0162      5.03      4.96 
#> 6 10_1_1       6  459.  382. 0.0154  -0.0307    0.0343      5.18      5.10 
#> # ℹ 9 more variables: rel_x <dbl>, rel_y <dbl>, video <chr>, order <chr>,
#> #   vid_ord <chr>, radius <dbl>, arc <ord>, type <chr>, individual <chr>
```
