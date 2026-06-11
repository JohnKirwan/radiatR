# radiatR Shiny app -- plot spec (shiny-free). Resolves the Results-figure choices
# into a single list (build_plot_spec), renders it to a ggplot (spec_to_plot), and
# emits the equivalent radiatR script (spec_to_code). Sourced by app.R; unit-tested
# directly. Depends only on the radiatR package (assumed attached) and ggplot2.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Shared defaults (kept here so spec_to_plot and spec_to_code agree).
SPEC_CYCLE_N         <- 20L
SPEC_STACK_BIN_WIDTH <- pi / 36   # 5 degrees
SPEC_STACK_STEP      <- 0.06
SPEC_STACK_START_SEP <- 0.05
SPEC_MARKER_SIZE     <- 2.5
SPEC_MARKER_ALPHA    <- 0.8
SPEC_TRAJ_KEY        <- "__trajectory__"
SPEC_CRIT_COLOUR     <- "firebrick"   # Rayleigh critical circle
SPEC_CRIT_LWD        <- 0.7
SPEC_VTEST_MU0       <- pi / 2        # V-test hypothesised direction (display top)
SPEC_VTEST_COLOUR    <- "steelblue"
SPEC_VTEST_LWD       <- 0.8

# Resolve the figure choices into a spec list.
#   ts     : the loaded TrajSet.
#   hd     : the headings frame, or NULL (rule "none").
#   method : the heading rule name (or "none").
#   data   : list(source = "file"|"example", path, dialect).
#   inputs : a plain list of the relevant input values (see fields used below).
build_plot_spec <- function(ts, hd, method, data, inputs) {
  id_col <- ts@cols$id
  df     <- as.data.frame(ts)

  gc <- if (!is.null(inputs$cond_col) && nzchar(inputs$cond_col))
    inputs$cond_col else NULL

  cb <- inputs$colour_by
  by_traj <- is.null(cb) || !nzchar(cb) || identical(cb, SPEC_TRAJ_KEY)
  if (!by_traj && !(cb %in% names(df))) by_traj <- TRUE   # stale selection
  key_col  <- if (by_traj) id_col else cb
  n_levels <- length(unique(df[[key_col]]))
  legend   <- !by_traj && n_levels <= SPEC_CYCLE_N        # distinct -> legend

  rule_params <- if (identical(method, "crossing"))
    list(circ0 = inputs$circ0 %||% 0.3, circ1 = inputs$circ1 %||% 0.6) else list()

  list(
    data     = data,
    headings = c(list(rule = method), rule_params),
    group_col = id_col,
    facet_by  = gc,
    colour    = list(by = if (by_traj) "trajectory" else cb,
                     cap = SPEC_CYCLE_N, legend = legend),
    theme        = inputs$plot_theme %||% "void",
    angle_labels = inputs$angle_labels %||% "degrees",
    display      = list(zero = 0),
    heading_display = inputs$heading_display %||% "points",
    # Resolved annotation text (computed app-side); stored so the code export
    # reproduces it verbatim. NULL/"" means no label.
    subtitle = inputs$subtitle,
    caption  = inputs$caption,
    show = list(tracks    = isTRUE(inputs$show_tracks),
                arrow     = isTRUE(inputs$show_arrow),
                vectors   = isTRUE(inputs$show_vectors),
                rayleigh  = isTRUE(inputs$show_rayleigh),
                ci        = isTRUE(inputs$show_ci),
                vtest     = isTRUE(inputs$show_vtest),
                quadrants = isTRUE(inputs$show_quadrants),
                rings     = isTRUE(inputs$show_rings))
  )
}

# Render a spec to a ggplot, using only exported radiatR functions. `ts`/`hd` are
# the in-memory objects; the spec's `data` block matters only for spec_to_code().
spec_to_plot <- function(spec, ts, hd) {
  disp <- circ_display(zero = spec$display$zero)
  by   <- spec$colour$by
  cap  <- spec$colour$cap

  ts <- assign_colour_key(ts, by = by, n = cap)

  p <- radiate(
    ts,
    group_col    = spec$group_col,
    colour_col   = ".colour",
    panel_by     = spec$facet_by,
    colour_cycle = NULL,
    legend       = spec$colour$legend,
    show_tracks  = spec$show$tracks,
    show_arrow   = FALSE,                  # arrow added explicitly below
    show_labels  = FALSE,
    theme        = spec$theme,
    angle_labels = spec$angle_labels,
    quadrants    = isTRUE(spec$show$quadrants),
    rings        = isTRUE(spec$show$rings),
    display      = disp
  )
  if (spec$colour$legend)
    p <- p + ggplot2::labs(colour = by)

  # Annotation labels (resolved app-side). Applied before the none-mode early
  # return so the tracks-only figure carries them too.
  lab_args <- list()
  if (!is.null(spec$subtitle) && nzchar(spec$subtitle)) lab_args$subtitle <- spec$subtitle
  if (!is.null(spec$caption)  && nzchar(spec$caption))  lab_args$caption  <- spec$caption
  if (length(lab_args)) p <- p + do.call(ggplot2::labs, lab_args)

  if (identical(spec$headings$rule, "none") || is.null(hd))
    return(p)

  # The headings frame may not carry the facet column; attach it (matched by
  # trajectory id) so the markers/arrow route to the right facet and stack per
  # facet.
  if (!is.null(spec$facet_by) && !spec$facet_by %in% names(hd)) {
    df <- as.data.frame(ts)
    hd <- merge(hd, unique(df[, c(spec$group_col, spec$facet_by)]),
                by.x = "id", by.y = spec$group_col, all.x = TRUE)
  }

  hd <- assign_colour_key(hd, by = by, n = cap, reference = ts)
  attr(hd, "display") <- disp

  if (!identical(spec$heading_display, "none")) {
    if (identical(spec$heading_display, "stacked")) {
      hd$heading <- bin_angles(hd$heading, width = SPEC_STACK_BIN_WIDTH)
      p <- p + add_stacked_headings(hd, colour_col = ".colour", group = spec$facet_by,
                 step = SPEC_STACK_STEP, start_sep = SPEC_STACK_START_SEP,
                 size = SPEC_MARKER_SIZE, alpha = SPEC_MARKER_ALPHA)
    } else {
      p <- p + add_heading_points(hd, colour_col = ".colour",
                 size = SPEC_MARKER_SIZE, alpha = SPEC_MARKER_ALPHA)
    }
  }

  if (spec$show$arrow) {
    arrow_df <- compute_circ_mean(hd, colour_col = spec$facet_by)
    p <- p + add_circ_mean(arrow_df, colour = "black")
  }
  if (spec$show$vectors && all(c("x_inner", "y_inner") %in% names(hd)))
    p <- p + add_heading_vectors(hd, colour_col = ".colour")

  # Mean-direction bootstrap CI arc (per facet group when faceted). hd already
  # carries the display attribute (set above) so the arc orients correctly.
  if (isTRUE(spec$show$ci))
    p <- p + add_heading_interval(hd, colour_col = spec$facet_by,
               stat = "bootstrap_ci")

  # Rayleigh critical circle (alpha = 0.05). Per-panel when faceted, drawn in a
  # fixed colour so it never collides with the trajectory colour scale.
  if (isTRUE(spec$show$rayleigh))
    p <- p + add_critical_r(hd, test = "rayleigh", group_col = spec$facet_by,
               per_group = !is.null(spec$facet_by), colour_by_group = FALSE,
               colour = SPEC_CRIT_COLOUR, linewidth = SPEC_CRIT_LWD)

  # V-test decision boundary against mu0 (display top). One boundary per panel
  # when faceting, a single pooled boundary otherwise. Always a fixed colour.
  if (isTRUE(spec$show$vtest)) {
    v <- add_critical_v_line(hd, mu0 = SPEC_VTEST_MU0, angle_col = "heading",
           group_col = spec$facet_by, per_group = !is.null(spec$facet_by),
           colour = SPEC_VTEST_COLOUR, linewidth = SPEC_VTEST_LWD)
    if (!is.null(v)) p <- p + v
  }

  p
}

# Emit the radiatR script (a single string) that reproduces spec_to_plot(spec).
spec_to_code <- function(spec) {
  # encodeString escapes embedded quotes/backslashes so the emitted literal
  # parses back to the original string (matters for free-text subtitle/caption).
  q   <- function(s) encodeString(s, quote = '"')
  L   <- character(0)
  add <- function(...) L[[length(L) + 1L]] <<- paste0(...)

  add("library(radiatR)")
  add("library(ggplot2)")
  add("")

  if (identical(spec$data$source, "example")) {
    add("data(cpunctatus)")
    add("ts <- cpunctatus")
  } else {
    dia <- if (!is.null(spec$data$dialect))
      paste0(", dialect = ", q(spec$data$dialect)) else ""
    add("ts <- TrajSet_read(", q(spec$data$path), dia, ")")
  }

  has_hd <- !identical(spec$headings$rule, "none")
  if (has_hd) {
    add("")
    hp <- if (identical(spec$headings$rule, "crossing"))
      paste0(", circ0 = ", spec$headings$circ0, ", circ1 = ", spec$headings$circ1) else ""
    # Heading vectors need the crossing construction coords; request them so the
    # emitted script can draw them (only the crossing rule produces x_inner/y_inner).
    rc <- if (spec$show$vectors && identical(spec$headings$rule, "crossing"))
      ", return_coords = TRUE" else ""
    add("hd <- derive_headings(ts, rule = ", q(spec$headings$rule), hp, rc, ")")
    if (!is.null(spec$facet_by))
      add("hd <- merge(hd, unique(as.data.frame(ts)[, c(", q(spec$group_col), ", ",
          q(spec$facet_by), ")]), by.x = \"id\", by.y = ", q(spec$group_col),
          ", all.x = TRUE)")
  }

  add("")
  add("ts <- assign_colour_key(ts, by = ", q(spec$colour$by), ")")
  if (has_hd)
    add("hd <- assign_colour_key(hd, by = ", q(spec$colour$by), ", reference = ts)")
  add("")
  add("disp <- circ_display(zero = ", spec$display$zero, ")")
  # Tag the headings frame with the display convention so the heading overlays
  # (points/vectors/arrow) orient the same way as the radiate() plot below --
  # otherwise they fall back to circ_display()'s default and rotate out of step.
  if (has_hd)
    add("attr(hd, \"display\") <- disp")

  if (has_hd && identical(spec$heading_display, "stacked")) {
    add("")
    add("hd$heading <- bin_angles(hd$heading, width = pi / 36)")
  }
  if (has_hd && spec$show$arrow) {
    cc <- if (is.null(spec$facet_by)) "" else paste0(", colour_col = ", q(spec$facet_by))
    add("")
    add("arrow_df <- compute_circ_mean(hd", cc, ")")
  }

  add("")
  pby <- if (is.null(spec$facet_by)) "" else paste0(", panel_by = ", q(spec$facet_by))
  # Only emit quadrants/rings when on -- they default to FALSE in radiate(), so
  # the common case keeps a clean call.
  qr <- paste0(if (isTRUE(spec$show$quadrants)) ", quadrants = TRUE" else "",
               if (isTRUE(spec$show$rings))     ", rings = TRUE"     else "")
  add("radiate(ts, group_col = ", q(spec$group_col),
      ", colour_col = \".colour\"", pby,
      ", legend = ", if (spec$colour$legend) "TRUE" else "FALSE",
      ", theme = ", q(spec$theme),
      ", angle_labels = ", q(spec$angle_labels), qr,
      ", show_labels = FALSE, show_arrow = FALSE, display = disp)")

  tail <- character(0)
  if (spec$colour$legend)
    tail <- c(tail, paste0("labs(colour = ", q(spec$colour$by), ")"))
  if (has_hd && identical(spec$heading_display, "stacked")) {
    grp <- if (is.null(spec$facet_by)) "" else paste0(", group = ", q(spec$facet_by))
    tail <- c(tail, paste0("add_stacked_headings(hd, colour_col = \".colour\"", grp,
                           ", step = 0.06, start_sep = 0.05, size = 2.5, alpha = 0.8)"))
  }
  if (has_hd && identical(spec$heading_display, "points"))
    tail <- c(tail, "add_heading_points(hd, colour_col = \".colour\", size = 2.5, alpha = 0.8)")
  if (has_hd && spec$show$arrow)
    tail <- c(tail, "add_circ_mean(arrow_df, colour = \"black\")")
  if (has_hd && spec$show$vectors && identical(spec$headings$rule, "crossing"))
    tail <- c(tail, "add_heading_vectors(hd, colour_col = \".colour\")")
  if (has_hd && isTRUE(spec$show$ci)) {
    cc <- if (is.null(spec$facet_by)) "" else paste0(", colour_col = ", q(spec$facet_by))
    tail <- c(tail, paste0(
      "add_heading_interval(hd", cc, ", stat = \"bootstrap_ci\")"))
  }
  if (has_hd && isTRUE(spec$show$rayleigh)) {
    gca <- if (is.null(spec$facet_by)) ", group_col = NULL, per_group = FALSE"
           else paste0(", group_col = ", q(spec$facet_by), ", per_group = TRUE")
    tail <- c(tail, paste0(
      "add_critical_r(hd, test = \"rayleigh\"", gca,
      ", colour_by_group = FALSE, colour = ", q(SPEC_CRIT_COLOUR),
      ", linewidth = ", SPEC_CRIT_LWD, ")"))
  }
  if (has_hd && isTRUE(spec$show$vtest)) {
    gpg <- if (is.null(spec$facet_by)) ", group_col = NULL, per_group = FALSE"
           else paste0(", group_col = ", q(spec$facet_by), ", per_group = TRUE")
    tail <- c(tail, paste0(
      "add_critical_v_line(hd, mu0 = pi / 2, angle_col = \"heading\"", gpg,
      ", colour = ", q(SPEC_VTEST_COLOUR), ", linewidth = ", SPEC_VTEST_LWD, ")"))
  }
  lab_parts <- character(0)
  if (!is.null(spec$subtitle) && nzchar(spec$subtitle))
    lab_parts <- c(lab_parts, paste0("subtitle = ", q(spec$subtitle)))
  if (!is.null(spec$caption) && nzchar(spec$caption))
    lab_parts <- c(lab_parts, paste0("caption = ", q(spec$caption)))
  if (length(lab_parts))
    tail <- c(tail, paste0("labs(", paste(lab_parts, collapse = ", "), ")"))

  if (length(tail)) {
    L[[length(L)]] <- paste0(L[[length(L)]], " +")
    for (i in seq_along(tail))
      add("  ", tail[[i]], if (i < length(tail)) " +" else "")
  }

  paste(L, collapse = "\n")
}
