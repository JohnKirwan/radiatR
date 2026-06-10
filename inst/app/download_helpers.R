# Shiny-free helpers for the plot Download panel: format -> graphics device
# resolution and the theme override that makes a plot's background transparent.
# Kept out of app.R (and free of any shiny calls) so they can be sourced and
# unit-tested directly -- see tests/testthat/test-app-download.R.

# Resolve a download format to a graphics device for ggsave(). Vector formats
# prefer svglite/cairo so the output is editable in vector tools; PNG and JPG
# use the default raster devices. JPG maps to the "jpeg" device (and has no
# alpha channel, so it cannot be saved transparently).
.plot_device <- function(fmt) {
  switch(fmt,
    png = "png",
    jpg = "jpeg",
    jpeg = "jpeg",
    pdf = grDevices::cairo_pdf,
    svg = if (requireNamespace("svglite", quietly = TRUE)) {
      svglite::svglite
    } else if (isTRUE(capabilities("cairo"))) {
      grDevices::svg
    } else {
      stop("SVG output needs the 'svglite' package or cairo support in R.")
    },
    stop("Unknown plot format: ", fmt)
  )
}

# A ggplot2 theme overlay (added with `+`) that clears the opaque rectangles a
# theme would otherwise paint, so an exported PNG/SVG/PDF has a see-through
# background. ggsave(bg = "transparent") only sets the device canvas; the
# theme's plot.background / panel.background rects sit on top of it and must be
# made transparent here too, along with the legend backgrounds.
.transparent_theme <- function() {
  clear <- ggplot2::element_rect(fill = "transparent", colour = NA)
  ggplot2::theme(
    plot.background      = clear,
    panel.background     = clear,
    legend.background    = clear,
    legend.box.background = clear
  )
}
