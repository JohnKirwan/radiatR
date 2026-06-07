# radiatR Shiny app — per-method construction overlays for the Configure preview.
# Sourced by app.R after preview.R. Each constructor maps a heading method to a
# list of ggplot2 layers illustrating how that rule derives its heading, drawn on
# the demo tracks and coloured per trajectory (via the .cycle_colour column set
# in build_method_preview). A constructor returns NULL when the headings frame
# lacks the columns it needs, so the method falls back to a plain heading point.
# Depends only on the radiatR package (assumed attached) and ggplot2.

.construct_crossing <- function(ts, hd, circ0, circ1) {
  u_x <- cos(hd$heading); u_y <- sin(hd$heading)
  x0  <- hd$x_inner;      y0  <- hd$y_inner
  b   <- x0 * u_x + y0 * u_y
  r2  <- x0^2 + y0^2
  t_out <- -b + sqrt(pmax(b^2 - (r2 - circ1^2), 0))  # outer ring crossing c1
  t_rim <- -b + sqrt(pmax(b^2 - (r2 - 1),       0))  # unit-circle periphery
  seg  <- data.frame(x_in = x0, y_in = y0,
                     x_out = x0 + t_rim * u_x, y_out = y0 + t_rim * u_y,
                     .cycle_colour = hd$.cycle_colour)
  dots <- data.frame(px = c(x0, x0 + t_out * u_x),
                     py = c(y0, y0 + t_out * u_y),
                     .cycle_colour = rep(hd$.cycle_colour, 2L))
  list(
    add_multiple_circles(radii = c(circ0, circ1),
                         circle_color = "grey60", circle_size = 0.4),
    ggplot2::geom_segment(
      data    = seg,
      mapping = ggplot2::aes(x = .data$x_in,   y = .data$y_in,
                             xend = .data$x_out, yend = .data$y_out,
                             colour = .data[[".cycle_colour"]]),
      linetype = "dotted", inherit.aes = FALSE),
    ggplot2::geom_point(
      data    = dots,
      mapping = ggplot2::aes(x = .data$px, y = .data$py,
                             colour = .data[[".cycle_colour"]]),
      size = 2, inherit.aes = FALSE)
  )
}

# Lookup of method -> constructor. Methods without an entry get no construction
# overlay (just the common heading point added by build_method_preview).
.preview_constructions <- list(
  crossing = .construct_crossing
)

# Return the list of construction layers for a method, or NULL when there is no
# constructor or the headings frame lacks the needed columns.
preview_construction_layers <- function(method, ts, hd, circ0, circ1) {
  fn <- .preview_constructions[[method]]
  if (is.null(fn)) return(NULL)
  fn(ts, hd, circ0, circ1)
}
