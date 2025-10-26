# Circular plotting utilities for radiatR trajectories
#

# ---- annotation layers -------------------------------------------------------

#' Create tick marks at the cardinal directions.
#'
#' Generates a `geom_segment()` layer containing small tick marks at north,
#' south, east, and west. The layer can be added to any ggplot.
#'
#' @return A `geom_segment()` layer.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   add_ticks()
#' @export
add_ticks <- function() {
  tick_df <- data.frame(
    x = c(0, .66, .95, .66, 0, -.66, -.95, -.66),
    y = c(.95, .66, 0, -.66, -.95, -.66, 0, .66),
    xend = c(0, .74, 1.05, .74, 0, -.74, -1.05, -.74),
    yend = c(1.05, .74, 0, -.74, -1.05, -.74, 0, .74)
  )

  ggplot2::geom_segment(
    data = tick_df,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$xend,
      yend = .data$yend
    ),
    inherit.aes = FALSE
  )
}

#' Draw a circular guide.
#'
#' Creates a list of annotation layers that render a circle with the requested
#' radius and appearance. The returned list can be added directly to a ggplot.
#'
#' @param radius Radius of the circle, expressed in the same units as the plot coordinates.
#' @param circle_color Line colour for the circle.
#' @param circle_alpha Alpha transparency for the circle.
#' @param circle_size Line width for the circle.
#'
#' @return A list containing a single ggplot annotation layer.
#'
#' @importFrom ggplot2 annotate
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   add_circ(radius = 1)
add_circ <- function(
  radius = 1, circle_color = "grey60", circle_alpha = 1, circle_size = 1) {

  list(
    ggplot2::annotate(
      "path",
      x = radius * cos(seq(0, 2 * pi, length.out = 1000)),
      y = radius * sin(seq(0, 2 * pi, length.out = 1000)),
      color = circle_color, alpha = circle_alpha, linewidth = circle_size
    )
  )
}

#' Add multiple concentric circles to a ggplot object
#'
#' This function creates a list of layers for concentric circles with specified radii that can be added to a ggplot object.
#' The function takes optional arguments to customize the appearance of the circles.
#'
#' @param radii A vector of radii for the concentric circles (default is c(0.25, 0.5, 0.75))
#' @param circle_color The color of the circles (default is "grey40")
#' @param circle_alpha The transparency of the circles (default is 1)
#' @param circle_size The size of the circle lines (default is 1)
#' @return A list of layers for concentric circles
#'
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() + add_multiple_circles()
#'
#' @export
add_multiple_circles <- function(radii = c(0.25, 0.5, 0.75),
                                 circle_color = "grey20",
                                 circle_alpha = 1,
                                 circle_size = 0.5) {
  lapply(radii, function(radius) add_circ(radius, circle_color, circle_alpha, circle_size))
}

#' Label the four diagonal directions.
#'
#' Provides a list of annotation layers that mark 45, 135, 225, and 315 degrees on a
#' unit circle.
#'
#' @return A list of ggplot2 annotation layers.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   degree_labs()
#' @export
degree_labs <- function() {
  list(
    ggplot2::annotate("text", x =  .85, y =  .85, label = paste0("45", "\U00B0")),
    ggplot2::annotate("text", x =  .85, y = -.85, label = paste0("135", "\U00B0")),
    ggplot2::annotate("text", x = -.85, y = -.85, label = paste0("225", "\U00B0")),
    ggplot2::annotate("text", x = -.85, y =  .85, label = paste0("315", "\U00B0"))
  )
}

#' Make mean resultant length arrow
#'
#' Computes the circular mean direction and resultant length, returning a
#' `geom_segment()` layer that can be added to an existing ggplot.
#'
#' @param data Data frame containing the angle column.
#' @param angle_col Column containing angles in radians.
#' @param arrow_head_cm Length of the arrowhead in centimetres.
#' @param colour Colour of the arrow.
#' @param size Width of the arrow segment (applied to the geom's `linewidth`).
#' @return A `geom_segment()` layer.
#' @export
#' @importFrom ggplot2 geom_segment
#' @importFrom circular mean.circular rho.circular
#' @importFrom grid arrow unit
#' @importFrom rlang .data ensym as_string
#' @importFrom tibble tibble
#' @importFrom ggplot2 aes
directedness_arrow <- function(data, angle_col, arrow_head_cm = 0.2,
                               colour = "gray", size = 2) {
  if (missing(angle_col)) {
    stop("`angle_col` must be supplied and should reference a column of angles in radians.")
  }
  angle_sym <- rlang::ensym(angle_col)
  angles <- data[[rlang::as_string(angle_sym)]]
  if (is.null(angles) || length(angles) == 0) {
    warning("No data available to compute directedness arrow; skipping layer.", call. = FALSE)
    return(ggplot2::geom_blank())
  }
  if (inherits(angles, "circular")) {
    angles <- as.numeric(angles)
  }
  if (!is.numeric(angles)) {
    warning("`angle_col` must refer to a numeric column expressed in radians; skipping arrow layer.", call. = FALSE)
    return(ggplot2::geom_blank())
  }
  mean_angle <- circular::mean.circular(angles)
  rho <- circular::rho.circular(angles)

  arrow_df <- tibble::tibble(
    x = 0,
    y = 0,
    xend = rho * cos(mean_angle),
    yend = rho * sin(mean_angle)
  )

  ggplot2::geom_segment(
    data = arrow_df,
    mapping = ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
    arrow = grid::arrow(length = grid::unit(arrow_head_cm, "cm")),
    colour = colour,
    linewidth = size,
    inherit.aes = FALSE
  )
}

# ---- themes ------------------------------------------------------------------

#' Sparse overlay theme for radial plots.
#'
#' Removes axes, grid lines, and strip decorations to keep attention on the
#' radial geometry. Useful when adding concentric guides and track overlays.
#'
#' @param base_theme Base theme to start from. Defaults to
#'   [ggplot2::theme_classic()].
#'
#' @return A ggplot2 theme object.
#'
#' @importFrom ggplot2 theme theme_classic element_blank element_rect element_text
#' @importFrom grid unit
#' @export
sparse_theme <- function(base_theme = ggplot2::theme_classic()) {
  base_theme +
    ggplot2::theme(
      axis.title        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_blank(),
      axis.ticks        = ggplot2::element_blank(),
      axis.line         = ggplot2::element_blank(),
      panel.spacing     = grid::unit(0.5, "cm"),
      plot.title        = ggplot2::element_text(size = 14, hjust = 0.5),
      strip.background  = ggplot2::element_blank(),
      strip.text        = ggplot2::element_blank(),
      panel.background  = ggplot2::element_rect(fill = "transparent"),
      plot.background   = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )
}

#' Minimalist theme for radial track plots.
#'
#' Wrapper around [ggplot2::theme_grey()] that strips panel adornments to
#' emphasise the radial geometry used by the package.
#'
#' @param ... Additional arguments passed to [ggplot2::theme_grey()].
#'
#' @return A ggplot2 theme object.
#'
#' @importFrom ggplot2 theme element_blank element_rect %+replace%
#' @export
spartan_theme <- function(...) {
  ggplot2::theme_grey(...) %+replace%
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background  = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ---- trajectory plotting -----------------------------------------------------

#' Plot trajectories from a TrajSet (overlay or faceted)
#'
#' @param x TrajSet
#' @param color,linetype,alpha,size Optional column names (strings) mapped to aesthetics
#' @param panel_by NULL, a single string, or a character vector of columns to facet by
#' @param coord "polar" (unit circle) or "cartesian"
#' @param geom "path" or "point" (or both, as c("path","point"))
#' @param thin Keep every n-th point per id (for very long tables)
#' @param ncol Number of facet columns (when faceting)
#' @return ggplot object
#' @importFrom stats ave
#' @export
setGeneric("gg_traj", function(x, color = NULL, linetype = NULL, alpha = NULL, size = 0.6,
                               panel_by = NULL, coord = c("polar", "cartesian"),
                               geom = c("path"), thin = 1L, ncol = NULL) standardGeneric("gg_traj"))

#' @rdname gg_traj
#' @export
setMethod("gg_traj", "TrajSet",
  function(x, color = NULL, linetype = NULL, alpha = NULL, size = 0.6,
           panel_by = NULL, coord = c("polar", "cartesian"),
           geom = c("path"), thin = 1L, ncol = NULL) {

    coord <- match.arg(coord)
    geom  <- match.arg(geom, several.ok = TRUE)

    d   <- x@data
    idc <- x@cols$id
    xc  <- x@cols$x
    yc  <- x@cols$y
    th  <- x@cols$angle

    if (is.null(xc) || is.null(yc)) {
      d$`.ux` <- cos(d[[th]])
      d$`.uy` <- sin(d[[th]])
      xc <- ".ux"; yc <- ".uy"
    }

    if (thin > 1L) {
      keep_idx <- unlist(lapply(split(seq_len(nrow(d)), d[[idc]]), function(idx) {
        idx[seq(1, length(idx), by = thin)]
      }), use.names = FALSE)
      d <- d[keep_idx, , drop = FALSE]
    }

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = .data[[xc]], y = .data[[yc]], group = .data[[idc]])
    )

    if (!is.null(color)    && color    %in% names(d)) p <- p + ggplot2::aes(color    = .data[[color]])
    if (!is.null(linetype) && linetype %in% names(d)) p <- p + ggplot2::aes(linetype = .data[[linetype]])
    if (!is.null(alpha)    && alpha    %in% names(d)) p <- p + ggplot2::aes(alpha    = .data[[alpha]])

    if ("path"  %in% geom) p <- p + ggplot2::geom_path(lineend = "round", linewidth = size, na.rm = TRUE)
    if ("point" %in% geom) p <- p + ggplot2::geom_point(size = size * 1.5, stroke = 0, na.rm = TRUE)

    if (!is.null(panel_by)) {
      if (is.character(panel_by)) {
        missing <- setdiff(panel_by, names(d))
        if (length(missing)) stop("panel_by column(s) not found: ", paste(missing, collapse = ", "))
        p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(panel_by, collapse = "+"))),
                                     scales = "free", ncol = ncol)
      } else stop("panel_by must be NULL or character vector of column names")
    }

    if (coord == "polar") {
      p <- p + ggplot2::coord_equal() +
        ggplot2::labs(x = NULL, y = NULL)
    } else {
      p <- p + ggplot2::coord_equal() + ggplot2::labs(x = xc, y = yc)
    }

    p + ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
})

#' Create geom layers for Cartesian track coordinates
#'
#' @inheritParams ggplot2::geom_path
#' @param data Data frame that will be plotted.
#' @param x_col Name of the column mapped to the x aesthetic.
#' @param y_col Name of the column mapped to the y aesthetic.
#' @param geom Either a character vector (`"path"`/`"point"`) or a ggplot2 geom
#'   function (e.g. [ggplot2::geom_path]).
#' @param mapping Optional aesthetics created with [ggplot2::aes()].
#' @param ... Additional arguments passed on to the geom function.
#'
#' @return A list containing a single ggplot2 layer.
#'
#' @export
draw_tracks <- function(data, x_col, y_col, geom = "path", mapping = NULL, ...) {
  if (missing(x_col) || missing(y_col)) {
    stop("Both `x_col` and `y_col` must be specified.")
  }

  geom_fun <- resolve_geom(geom)
  x_sym <- rlang::sym(x_col)
  y_sym <- rlang::sym(y_col)

  if (!is.null(mapping)) {
    if (!inherits(mapping, "uneval")) {
      stop("`mapping` must be created with ggplot2::aes().")
    }
    mapping$x <- x_sym
    mapping$y <- y_sym
    base_mapping <- mapping
  } else {
    base_mapping <- ggplot2::aes(x = !!x_sym, y = !!y_sym)
  }

  layer <- rlang::exec(
    geom_fun,
    data = data,
    mapping = base_mapping,
    ...
  )

  list(layer)
}

resolve_geom <- function(geom) {
  if (is.character(geom)) {
    geom <- tolower(geom)
    geom <- match.arg(geom, c("path", "point", "line"))
    switch(
      geom,
      path = ggplot2::geom_path,
      line = ggplot2::geom_path,
      point = ggplot2::geom_point
    )
  } else if (is.function(geom)) {
    geom
  } else {
    stop("`geom` must be either a character shortcut or a ggplot2 geom function.")
  }
}

# ---- geometry helpers --------------------------------------------------------

#' Find the intercept of a line with the unit circle
#'
#' @param x0,y0 Starting coordinates of the vector.
#' @param x1,y1 Ending coordinates of the vector.
#' @return A tibble with the intersection point closest to `(x1, y1)`.
#' @export
line_circle_intercept <- function(x0, y0, x1, y1) {
  h <- 0; k <- 0; radius <- 1
  a <- (x1 - x0)^2 + (y1 - y0)^2
  b <- (2 * (x1 - x0) * (x0 - h)) + (2 * (y1 - y0) * (y0 - k))
  c <- (x0 - h)^2 + (y0 - k)^2 - radius^2
  D <- (b^2) - (4 * a * c)
  if (a <= 0) warning("Vector length is not positive!")
  if (D <= 0) warning("Discriminant is not positive with x0=", x0, " y0=", y0, " x1=", x1, " y1=", y1)
  if (c >= 0) warning("Starting point is outside radius!")

  t <- c(
    (-b + sqrt(D)) / (2 * a),
    (-b - sqrt(D)) / (2 * a)
  )

  ints <- rbind(
    c(t[1] * (x1 - x0) + x0, t[1] * (y1 - y0) + y0),
    c(t[2] * (x1 - x0) + x0, t[2] * (y1 - y0) + y0)
  )

  d <- c(sqrt(ints[1, 1]^2 + ints[1, 2]^2), sqrt(ints[2, 1]^2 + ints[2, 2]^2))
  if (!isTRUE(all.equal(d[1], d[2]))) warning("Intersect is off unit circle!")

  dist2track <- c(
    sqrt((ints[1, 1] - x1)^2 + (ints[1, 2] - y1)^2),
    sqrt((ints[2, 1] - x1)^2 + (ints[2, 2] - y1)^2)
  )
  xy <- ints[which.min(dist2track), ]

  tibble::tibble(x_int = xy[1], y_int = xy[2])
}

#' Intersection helper using track rows
#'
#' Convenience wrapper that accepts a data frame (or `TrajSet@data`) containing
#' coordinates and evaluates the intersection between two rows.
#'
#' @param df Data frame with `x` and `y` columns.
#' @param row_in,row_out Row indices (or names) identifying the start and end of the vector.
#' @return Tibble with `x_int`/`y_int`.
#' @export
line_circle_intercept_df <- function(df, row_in, row_out) {
  if (!all(c("x", "y") %in% names(df))) {
    stop("`df` must contain x/y columns.")
  }
  start <- df[row_in, , drop = TRUE]
  end <- df[row_out, , drop = TRUE]
  line_circle_intercept(start[["x"]], start[["y"]], end[["x"]], end[["y"]])
}

#' Intersection helper for TrajSet trajectories
#'
#' Extracts the first/last rows for a given id/time range and computes the
#' intersection with the unit circle.
#'
#' @param traj TrajSet
#' @param id Identifier of the trajectory
#' @param range Numeric index vector (e.g., rows within a trial)
#' @return Tibble with `x_int`/`y_int`
#' @export
line_circle_intercept_traj <- function(traj, id, range) {
  df <- traj@data
  id_col <- traj@cols$id
  x_col <- traj@cols$x
  y_col <- traj@cols$y
  if (is.null(x_col) || is.null(y_col)) {
    stop("TrajSet must encode x/y coordinates for intersection.")
  }
  subset <- df[df[[id_col]] == id, , drop = FALSE][range, , drop = FALSE]
  line_circle_intercept_df(
    data.frame(x = subset[[x_col]], y = subset[[y_col]]),
    1, nrow(subset)
  )
}

# ---- high-level plotting -----------------------------------------------------

#' Make ggplot object of tracks radiating from circle centre.
#'
#' Accepts either a precomputed data frame of polar/cartesian coordinates or a
#' `TrajSet`. When a `TrajSet` is supplied, column mappings are inferred from
#' the object and handed off to the plotting helpers.
#'
#' @inheritParams draw_tracks
#' @inheritParams directedness_arrow
#' @inheritParams add_ticks
#' @inheritParams sparse_theme
#' @param data Data frame or `TrajSet`.
#' @param geom Geom specification passed to [draw_tracks()].
#' @param group_col Optional column for grouping aesthetics.
#' @param colour_col Optional column for colour aesthetics.
#' @param style Either `"classic"` (default) or `"minimal"`.
#' @param show_labels Whether to place labels at the perimeter.
#' @param label_col Column containing label values.
#' @param label_padding Multiplier applied to the unit circle when placing labels.
#' @param label_use_repel Use `ggrepel::geom_text_repel()` when available.
#' @param show_arrow Whether to draw a mean resultant arrow from the centre.
#' @param arrow_angle_col Column containing angles (radians) to summarise for the arrow.
#' @param arrow_colour Arrow colour.
#' @param arrow_size Arrow linewidth.
#' @param ticks,degrees,legend,title,xlab,ylab,axes Additional styling options.
#' @param ... Additional arguments forwarded to [draw_tracks()].
#' @return A `ggplot2` object.
#' @export
radiate <- function(
  data,
  x_col = "rel_x", y_col = "rel_y",
  geom = "path",
  group_col = NULL,
  colour_col = NULL,
  ticks = NULL,
  degrees = NULL, legend = NULL, title = NULL,
  xlab = NULL, ylab = NULL, axes = NULL,
  style = c("classic", "minimal"),
  show_labels = NULL,
  label_col = NULL,
  label_size = 3,
  label_padding = 1.08,
  label_use_repel = TRUE,
  show_arrow = NULL,
  arrow_angle_col = NULL,
  arrow_colour = "black",
  arrow_size = 2,
  ...){
  if (is.null(ticks)) {ticks = TRUE}
  if (is.null(degrees)) {degrees = TRUE}
  if (is.null(legend)) {legend = FALSE}
  if (is.null(axes)) {axes = FALSE}
  style <- match.arg(style)
  if (is.null(show_labels)) {
    show_labels <- identical(style, "classic")
  }

  is_trajset <- inherits(data, "TrajSet")
  if (is_trajset) {
    ts <- data
    data <- ts@data
    if (!is.null(ts@cols$x) && identical(x_col, "rel_x")) x_col <- ts@cols$x
    if (!is.null(ts@cols$y) && identical(y_col, "rel_y")) y_col <- ts@cols$y
    if (is.null(group_col)) group_col <- ts@cols$id
    if (is.null(arrow_angle_col)) arrow_angle_col <- ts@cols$angle
  }

  x_sym <- rlang::sym(x_col)
  y_sym <- rlang::sym(y_col)

  g <- ggplot2::ggplot(data = data,
                       mapping = ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
    ggplot2::coord_fixed()

  g <- g + spartan_theme()
  g <- g + add_multiple_circles()
  g <- g + add_multiple_circles(radii = c(0.1, 0.2), circle_color = "skyblue")

  layer_mapping <- NULL
  if (!is.null(group_col) || !is.null(colour_col)) {
    mapping_list <- list()
    if (!is.null(group_col)) mapping_list$group <- rlang::sym(group_col)
    if (!is.null(colour_col)) mapping_list$colour <- rlang::sym(colour_col)
    layer_mapping <- do.call(ggplot2::aes, mapping_list)
  }

  g <- g + draw_tracks(
    data = data,
    x_col = x_col,
    y_col = y_col,
    geom = geom,
    mapping = layer_mapping,
    ...
  )

  if (style == "minimal") {
    g <- g + spartan_theme()
    g <- g + add_multiple_circles(radii = c(0.25, 0.5, 0.75), circle_color = "grey80", circle_size = 0.5)
    g <- g + add_circ(circle_color = "grey60", circle_size = 1)
    if (degrees) g <- g + degree_labs()
    if (ticks) g <- g + add_ticks()
  } else {
    g <- g + sparse_theme()
    g <- g + add_multiple_circles(radii = c(0.25, 0.5, 0.75), circle_color = "grey80", circle_size = 0.5)
    g <- g + add_circ(circle_color = "black", circle_size = 1.5)
    g <- g + add_ticks()
    if (degrees) g <- g + degree_labs()
  }

  label_col <- resolve_label_column(data, label_col, group_col)
  if (show_labels && !is.null(label_col)) {
    label_data <- build_label_data(
      data, label_col, x_col = x_col, y_col = y_col,
      colour_col = colour_col, padding = label_padding
    )
    if (!is.null(label_data)) {
      label_aes <- ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$label_value)
      if (!is.null(colour_col) && "colour_value" %in% names(label_data)) {
        label_aes$colour <- quote(.data$colour_value)
      }
      if (label_use_repel && requireNamespace("ggrepel", quietly = TRUE)) {
        g <- g + ggrepel::geom_text_repel(
          data = label_data,
          mapping = label_aes,
          size = label_size,
          inherit.aes = FALSE,
          max.overlaps = Inf
        )
      } else {
        g <- g + ggplot2::geom_text(
          data = label_data,
          mapping = label_aes,
          size = label_size,
          inherit.aes = FALSE
        )
      }
    }
  }

  if (is.null(show_arrow)) {
    show_arrow <- identical(style, "classic")
  }
  if (show_arrow) {
    angle_col <- resolve_angle_column(data)
    if (!is.null(arrow_angle_col)) angle_col <- arrow_angle_col
    if (!is.null(angle_col) &&
        angle_col %in% names(data) &&
        is.numeric(data[[angle_col]]) &&
        any(!is.na(data[[angle_col]]))) {
      g <- g + directedness_arrow(
        data = data.frame(theta = data[[angle_col]]),
        angle_col = theta,
        colour = arrow_colour,
        size = arrow_size
      )
    } else if (!is.null(arrow_angle_col)) {
      warning("Arrow requested but column `", arrow_angle_col,
              "` is unavailable or not numeric; skipping arrow layer.", call. = FALSE)
    }
  }

  if (legend == FALSE) {
    g <- g + ggplot2::theme(
      legend.position = "none",
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.box.background = ggplot2::element_rect(fill = "transparent"))
  }
  if (axes == FALSE) {
    g <- g + ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank())
  }
  if (is.null(xlab)) g <- g + ggplot2::xlab('') else g <- g + ggplot2::xlab(as.character(xlab))
  if (is.null(ylab)) g <- g + ggplot2::ylab('') else g <- g + ggplot2::ylab(as.character(ylab))

  if (is.null(title)) {
    g <- g + ggplot2::theme(plot.title = ggplot2::element_blank())
  } else {
    g <- g + ggplot2::ggtitle(as.character(title)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5))
  }

  g
}

resolve_label_column <- function(data, label_col, group_col) {
  if (!is.null(label_col) && label_col %in% names(data)) {
    return(label_col)
  }
  if (!is.null(group_col) && group_col %in% names(data)) {
    return(group_col)
  }
  fallback_cols <- c("trial_num", "id", "vid_ord", "group")
  available <- fallback_cols[fallback_cols %in% names(data)]
  if (length(available) > 0) {
    return(available[[1]])
  }
  NULL
}

resolve_angle_column <- function(data) {
  candidates <- c("rel_theta", "abs_theta")
  available <- candidates[candidates %in% names(data)]
  if (length(available) > 0) available[[1]] else NULL
}

build_label_data <- function(data, label_col, x_col, y_col, colour_col = NULL, padding = 1.08) {
  if (!(label_col %in% names(data))) {
    return(NULL)
  }
  label_vals <- data[[label_col]]
  if (all(is.na(label_vals))) {
    return(NULL)
  }
  valid <- !is.na(label_vals)
  if (!any(valid)) {
    return(NULL)
  }

  data <- data[valid, , drop = FALSE]
  label_vals <- label_vals[valid]
  split_data <- split(data, label_vals, drop = TRUE)

  label_list <- lapply(split_data, function(df) {
    radii <- df[[x_col]]^2 + df[[y_col]]^2
    idx <- which.max(radii)
    if (length(idx) == 0 || length(idx) > nrow(df)) {
      return(NULL)
    }
    row <- df[idx, , drop = FALSE]
    r <- sqrt(row[[x_col]]^2 + row[[y_col]]^2)
    if (!is.finite(r) || r == 0) {
      return(NULL)
    }
    edge_x <- row[[x_col]] / r
    edge_y <- row[[y_col]] / r
    data.frame(
      label_value = row[[label_col]],
      edge_x = edge_x,
      edge_y = edge_y,
      label_x = edge_x * padding,
      label_y = edge_y * padding,
      colour_value = if (!is.null(colour_col) && colour_col %in% names(row)) row[[colour_col]] else NA,
      stringsAsFactors = FALSE
    )
  })

  non_null <- !vapply(label_list, is.null, logical(1))
  if (!any(non_null)) {
    return(NULL)
  }
  label_data <- do.call(rbind, label_list[non_null])
  if (is.null(label_data) || nrow(label_data) == 0) {
    return(NULL)
  }
  if (!is.null(colour_col) && "colour_value" %in% names(label_data) &&
      all(is.na(label_data$colour_value))) {
    label_data$colour_value <- NULL
  }
  label_data
}

# ---- circular angle utilities ------------------------------------------------

#' Wrap angles to (-pi, pi]
#'
#' @param theta Angles in radians using the conventional unit-circle orientation.
#' @return Angles wrapped to (-pi, pi].
#' @examples
#' theta <- seq(from = -5, to = 5, length.out = 6)
#' rad_shepherd(theta)
#' @export
rad_shepherd <- function(theta) {
  theta <- as.numeric(theta)
  if (any(!is.finite(theta))) {
    warning("Non-finite values detected; returning NA for those entries.", call. = FALSE)
    theta[!is.finite(theta)] <- NA_real_
  }
  ((theta + pi) %% (2 * pi)) - pi
}

#' Wrap clockwise angles to [0, 2*pi)
#'
#' @param theta Angles in radians measured clockwise (clock format).
#' @return Angles wrapped to [0, 2*pi).
#' @examples
#' theta <- seq(from = -2, to = 8, length.out = 6)
#' rad_shepherd_clock(theta)
#' @export
rad_shepherd_clock <- function(theta) {
  theta <- as.numeric(theta)
  if (any(!is.finite(theta))) {
    warning("Non-finite values detected; returning NA for those entries.", call. = FALSE)
    theta[!is.finite(theta)] <- NA_real_
  }
  wrapped <- theta %% (2 * pi)
  wrapped[wrapped < 0] <- wrapped[wrapped < 0] + 2 * pi
  wrapped
}

#' Convert unit-circle angles to clock orientation
#'
#' @param theta Angle (radians) using the standard unit-circle convention.
#' @return Angle in radians measured clockwise with zero at the top.
#' @examples
#' theta <- seq(from = pi/2, to = -pi/2, length.out = 5)
#' rad2clock(theta)
#' @export
rad2clock <- function(theta) {
  theta <- as.numeric(theta)
  if (any(!is.finite(theta))) {
    warning("Non-finite values detected; returning NA for those entries.", call. = FALSE)
    theta[!is.finite(theta)] <- NA_real_
  }
  clock_theta <- (pi / 2) - theta
  rad_shepherd_clock(clock_theta)
}

#' Convert clock-oriented angles back to unit-circle orientation
#'
#' @param theta Angle (radians) measured clockwise with zero at the top.
#' @return Angle in radians using the standard unit-circle convention.
#' @examples
#' theta <- seq(from = 0, to = 2 * pi, length.out = 5)
#' rad_unclock(theta)
#' @export
rad_unclock <- function(theta) {
  theta <- as.numeric(theta)
  if (any(!is.finite(theta))) {
    warning("Non-finite values detected; returning NA for those entries.", call. = FALSE)
    theta[!is.finite(theta)] <- NA_real_
  }
  ttheta <- (pi / 2) - theta
  rad_shepherd(ttheta)
}
