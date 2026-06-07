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

.construct_distal <- function(ts, hd, circ0, circ1) {
  if (!all(c("x_distal", "y_distal") %in% names(hd))) return(NULL)
  df <- data.frame(px = hd$x_distal, py = hd$y_distal,
                   .cycle_colour = hd$.cycle_colour)
  list(ggplot2::geom_point(
    data    = df,
    mapping = ggplot2::aes(x = .data$px, y = .data$py,
                           colour = .data[[".cycle_colour"]]),
    shape = 16, size = 2.5, inherit.aes = FALSE))
}

.construct_net <- function(ts, hd, circ0, circ1) {
  if (!all(c("x_start", "y_start", "x_end", "y_end") %in% names(hd))) return(NULL)
  df <- data.frame(x0 = hd$x_start, y0 = hd$y_start,
                   x1 = hd$x_end,   y1 = hd$y_end,
                   .cycle_colour = hd$.cycle_colour)
  list(ggplot2::geom_segment(
    data    = df,
    mapping = ggplot2::aes(x = .data$x0, y = .data$y0,
                           xend = .data$x1, yend = .data$y1,
                           colour = .data[[".cycle_colour"]]),
    linewidth = 0.5, inherit.aes = FALSE))
}

.construct_straight <- function(ts, hd, circ0, circ1) {
  if (!all(c("x_seg0", "y_seg0", "x_seg1", "y_seg1") %in% names(hd))) return(NULL)
  df <- data.frame(x0 = hd$x_seg0, y0 = hd$y_seg0,
                   x1 = hd$x_seg1, y1 = hd$y_seg1,
                   .cycle_colour = hd$.cycle_colour)
  df <- df[is.finite(df$x0) & is.finite(df$x1), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  list(ggplot2::geom_segment(
    data    = df,
    mapping = ggplot2::aes(x = .data$x0, y = .data$y0,
                           xend = .data$x1, yend = .data$y1,
                           colour = .data[[".cycle_colour"]]),
    linewidth = 1.2, lineend = "round", inherit.aes = FALSE))
}

.construct_pca_axis <- function(ts, hd, circ0, circ1) {
  if (!all(c("x_centroid", "y_centroid", "axis_x", "axis_y") %in% names(hd)))
    return(NULL)
  cx <- hd$x_centroid; cy <- hd$y_centroid
  ux <- hd$axis_x;     uy <- hd$axis_y
  b    <- cx * ux + cy * uy
  disc <- pmax(b^2 - (cx^2 + cy^2 - 1), 0)     # centroid is inside the circle
  t_p  <- -b + sqrt(disc); t_m <- -b - sqrt(disc)
  df <- data.frame(x0 = cx + t_m * ux, y0 = cy + t_m * uy,
                   x1 = cx + t_p * ux, y1 = cy + t_p * uy,
                   .cycle_colour = hd$.cycle_colour)
  list(ggplot2::geom_segment(
    data    = df,
    mapping = ggplot2::aes(x = .data$x0, y = .data$y0,
                           xend = .data$x1, yend = .data$y1,
                           colour = .data[[".cycle_colour"]]),
    linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE))
}

# Lookup of method -> constructor. Methods without an entry get no construction
# overlay (just the common heading point added by build_method_preview).
.preview_constructions <- list(
  crossing = .construct_crossing,
  distal   = .construct_distal,
  net      = .construct_net,
  straight = .construct_straight,
  pca_axis = .construct_pca_axis
)

# Return the list of construction layers for a method, or NULL when there is no
# constructor or the headings frame lacks the needed columns.
preview_construction_layers <- function(method, ts, hd, circ0, circ1) {
  fn <- .preview_constructions[[method]]
  if (is.null(fn)) return(NULL)
  fn(ts, hd, circ0, circ1)
}
