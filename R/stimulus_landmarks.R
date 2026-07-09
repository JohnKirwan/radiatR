# Stimulus and landmark annotation layers for circular plots.

#' Add a stimulus arc to a circular plot
#'
#' Draws a filled ribbon on the rim of the unit circle, spanning `width` in
#' angular extent centred on `bearing`, depicting a dark bar or spot stimulus
#' in an orientation arena. A wide `width` reads as a bar; a narrow `width` as a
#' spot. The bearing is given in **display units** (the value read off the axis),
#' so the arc lands at the labelled position under any [circ_display()]
#' convention.
#'
#' @param bearing Centre of the stimulus, in display units (degrees by default).
#'   Default `0`.
#' @param width Angular extent of the stimulus, in display units. Default `20`.
#' @param thickness Radial extent of the ribbon, in data units. Default `0.05`
#'   (straddles the circumference).
#' @param r Centre radius of the ribbon. Default `1` (on the unit circle).
#' @param fill Fill colour. Default `"black"`.
#' @param colour,color Border colour. Default `NA` (no border). `color` is the
#'   American-spelling alias.
#' @param alpha Fill alpha transparency. Default `1`.
#' @param display A [circ_display()] giving the plot's angle convention. Default
#'   `circ_display()`. Pass the same value used in [radiate()].
#'
#' @return A list containing one `ggplot2::annotate("polygon")` layer.
#'
#' @inheritSection radiatR-package American spellings
#' @seealso [add_landmark()], [add_circ()]
#' @importFrom ggplot2 annotate
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() + add_circ() +
#'   add_stimulus_arc(bearing = 90, width = 30)
add_stimulus_arc <- function(bearing = 0, width = 20, thickness = 0.05, r = 1,
                             fill = "black", colour = NA, alpha = 1,
                             display = circ_display(),
                             color = NULL) {
  .apply_spelling_aliases()
  half  <- width / 2
  n     <- max(2L, ceiling(width)) + 1L
  vals  <- seq(bearing - half, bearing + half, length.out = n)
  theta <- .display_to_uc_angle(vals, display)
  r_out <- r + thickness / 2
  r_in  <- r - thickness / 2
  outer <- .uc_to_display_coords(r_out * cos(theta), r_out * sin(theta), display)
  inner <- .uc_to_display_coords(r_in  * cos(theta), r_in  * sin(theta), display)
  list(
    ggplot2::annotate(
      "polygon",
      x = c(outer$x, rev(inner$x)),
      y = c(outer$y, rev(inner$y)),
      fill = fill, colour = colour, alpha = alpha
    )
  )
}

#' Add a landmark marker to a circular plot
#'
#' Draws a styled point just outside the rim at `bearing`, with a sun-like
#' default (a gold asterisk), plus an optional text label. Change `shape`,
#' `colour`/`fill` and `label` to mark any landmark (nest, feeder, light
#' source). The bearing is given in **display units** (the value read off the
#' axis), so the marker lands at the labelled position under any
#' [circ_display()] convention.
#'
#' @param bearing Direction of the landmark, in display units (degrees by
#'   default). Default `0`.
#' @param r Radius at which to draw the point. Default `1.12` (just outside the
#'   unit circle).
#' @param shape Point shape. Default `8` (asterisk, sun-ray-like).
#' @param size Point size. Default `4`.
#' @param colour,color Point colour. Default `"goldenrod"`. `color` is the
#'   American-spelling alias.
#' @param fill Fill colour, used only by fillable shapes (21--25). Default
#'   `"gold"`.
#' @param label Optional text label, drawn just beyond the point. `NULL`
#'   (default) draws no label.
#' @param display A [circ_display()] giving the plot's angle convention. Default
#'   `circ_display()`. Pass the same value used in [radiate()].
#' @param ... Further arguments passed to [ggplot2::geom_point()].
#'
#' @return A list of ggplot layers: a `geom_point()` layer, plus a
#'   `geom_text()` layer when `label` is supplied.
#'
#' @inheritSection radiatR-package American spellings
#' @seealso [add_stimulus_arc()], [add_origin_point()]
#' @importFrom ggplot2 geom_point geom_text aes
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() + add_circ() +
#'   add_landmark(bearing = 45, label = "sun")
add_landmark <- function(bearing = 0, r = 1.12, shape = 8, size = 4,
                         colour = "goldenrod", fill = "gold", label = NULL,
                         display = circ_display(), ...,
                         color = NULL) {
  .apply_spelling_aliases()
  theta <- .display_to_uc_angle(bearing, display)
  pos   <- .uc_to_display_coords(r * cos(theta), r * sin(theta), display)
  layers <- list(
    ggplot2::geom_point(
      data        = data.frame(x = pos$x, y = pos$y),
      mapping     = ggplot2::aes(x = .data$x, y = .data$y),
      shape = shape, size = size, colour = colour, fill = fill,
      inherit.aes = FALSE, ...
    )
  )
  if (!is.null(label)) {
    lr   <- r + 0.08
    lpos <- .uc_to_display_coords(lr * cos(theta), lr * sin(theta), display)
    layers <- c(layers, list(
      ggplot2::geom_text(
        data        = data.frame(x = lpos$x, y = lpos$y, label = label),
        mapping     = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        inherit.aes = FALSE
      )
    ))
  }
  layers
}
