# Map periodic time, date, or numeric data onto circular angles

Converts a vector of timestamps, dates, or a generic periodic numeric
into unit-circle angles in radians (\`\[0, 2\*pi)\`), so periodic data —
events by time of day (chronobiology) or time of year (phenology) — can
be analysed and drawn with the circular tools (\`circ_summary()\`,
\`vonmises_fit()\`, \`radiate()\`, the distribution overlays). It is the
data-side counterpart of the \[circumference_labs()\] /
\[scale_clock()\] / \[scale_months()\] labels: the cycle start is placed
at the top and time increases clockwise, so a \`06:00\` timestamp lands
exactly on the \`"6"\` hours label and the 1st of each month on its
month label.

## Usage

``` r
as_angle(x, period)
```

## Arguments

- x:

  A \`POSIXct\`, \`Date\`, or \`numeric\` vector.

- period:

  The cycle. \`"day"\` (time of day; \`x\` must be \`POSIXct\`),
  \`"year"\` (time of year; \`Date\` or \`POSIXct\`, day-accurate and
  leap-safe), or a single positive number giving the cycle length in the
  units of a numeric \`x\`.

## Value

A \`numeric\` vector of angles in radians (\`\[0, 2\*pi)\`), the same
length as \`x\`; \`NA\` entries propagate. Wrap it with
\[headings_frame()\] (or use it as a \`heading\` column) to feed the
circular tools.

## See also

\[circumference_labs()\], \[scale_clock()\], \[scale_months()\],
\[headings_frame()\], \[circ_summary()\], \[radiate()\]

## Examples

``` r
# events through a day -> a clock-face circular distribution
set.seed(1)
times <- as.POSIXct("2020-06-01", tz = "UTC") +
  rnorm(200, mean = 9 * 3600, sd = 2 * 3600)        # clustered around 09:00
ang <- as_angle(times, "day")
circ_summarise(data.frame(heading = ang), "heading", units = "radians")
#> # A tibble: 1 × 5
#>       n mean_dir mean_dir_deg resultant_R kappa
#>   <int>    <dbl>        <dbl>       <dbl> <dbl>
#> 1   200     5.48         136.       0.889    NA

# a generic numeric cycle of length 10
as_angle(c(0, 2.5, 5, 7.5), period = 10)
#> [1] 1.570796 0.000000 4.712389 3.141593
```
