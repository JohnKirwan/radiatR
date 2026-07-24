# Launch the radiatR Shiny companion app

Opens a browser-based graphical interface for uploading tracking data,
selecting a heading method, and viewing circular track plots and summary
statistics. No R coding is required to use the app.

## Usage

``` r
launch_app(port = NULL, launch_browser = TRUE)
```

## Arguments

- port:

  Integer; TCP port to listen on. `NULL` (default) lets Shiny pick a
  free port automatically.

- launch_browser:

  Logical; open the system browser automatically (default `TRUE`). Set
  to `FALSE` when running headless.

## Value

Called for its side-effect; returns the result of
[`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) invisibly.

## Details

The same app directory is hosted on Posit Connect Cloud (git-backed) and
can be deployed to any Shiny-compatible server. For a one-off manual
deploy:


      rsconnect::deployApp(system.file("app", package = "radiatR"))
