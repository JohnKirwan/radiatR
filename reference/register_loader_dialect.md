# Register a custom loader dialect The function must accept (x, ...) and return a data.frame in long form with columns at least id,time and one of (angle) or (x,y)

Register a custom loader dialect The function must accept (x, ...) and
return a data.frame in long form with columns at least id,time and one
of (angle) or (x,y)

## Usage

``` r
register_loader_dialect(name, fun, overwrite = FALSE)
```

## Arguments

- name:

  Unique dialect name

- fun:

  Function that accepts (x, ...) and returns a data.frame with
  id/time/angle or id/time/x/y

- overwrite:

  Replace an existing dialect registered with the same name
