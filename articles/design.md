# radiatR design contract

``` r

if (requireNamespace("pkgload", quietly = TRUE)) {
  # Always load from source so the vignette reflects the current development state.
  pkgload::load_all("..", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else if (requireNamespace("radiatR", quietly = TRUE)) {
  library(radiatR)
} else {
  stop("Package 'radiatR' not installed and 'pkgload' not available.")
}
```

radiatR keeps a tidyverse-shaped surface: data flows as plain tibbles
addressed by column name, and rich objects are reserved for structure a
flat table cannot hold safely.

## Rich objects, only where structure is irreducible

- `TrajSet` — trajectories with arena geometry, a per-trajectory
  reference frame, and transform history (invariants a bare tibble
  cannot enforce).
- [`circ_display()`](https://johnkirwan.github.io/radiatR/reference/circ_display.md)
  — an orientation convention (zero direction, rotation).
- [`circ_regression()`](https://johnkirwan.github.io/radiatR/reference/circ_regression.md)
  — a fitted model (like `lm`).

## Everything else is a plain tibble, in and out

Plain tibbles are addressed by a column-name argument
(`angle_col = "heading"`). The `circular` package’s `circular` objects
are an internal implementation detail — built inside functions, never in
a public signature or return.

## `headings_frame`: a dplyr-durable tibble subclass

`headings_frame` is a tibble subclass that carries the display
convention durably through dplyr verbs (via the `dplyr_reconstruct`
contract), so a
`mutate()`/[`filter()`](https://rdrr.io/r/stats/filter.html) never
silently drops the orientation. Read its metadata with the `hf_*`
accessors. A plain data frame with a heading column is always an
acceptable substitute — the class is an enhancement, never a
requirement:

``` r

data(cpunctatus)
hd <- derive_headings(cpunctatus, rule = "distal")
hd2 <- dplyr::filter(hd, is.finite(heading))   # class + display preserved
class(hd2)
#> [1] "headings_frame" "tbl_df"         "tbl"            "data.frame"
hf_display(hd2)
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
```
