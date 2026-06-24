# radiatR Shiny app -- plot spec (shiny-free). Resolves the Results-figure choices
# into a single list (build_plot_spec), renders it to a ggplot (spec_to_plot), and
# emits the equivalent radiatR script (spec_to_code). Sourced by app.R; unit-tested
# directly. Depends only on the radiatR package (assumed attached) and ggplot2.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Resolve the effective track-colour mode for the Results figure. "time" colours
# by elapsed time but needs a valid frame rate (the package hard-errors otherwise),
# so an unset/invalid fps falls back to "sequence" -- the app never asks radiate()
# for time-mode without a usable frame rate. Returns the effective mode, whether it
# is a continuous gradient (owns the colour scale), the validity flag, and the fps.
.resolve_track_colour <- function(spec, headings_mode) {
  tc  <- spec$track_colour %||% "trajectory"
  if (headings_mode) tc <- "trajectory"          # no tracks to colour in headings mode
  fps <- spec$frame_rate
  fps_ok    <- is.numeric(fps) && length(fps) == 1L && is.finite(fps) && fps > 0
  needs_fps <- tc %in% c("time", "speed")
  effective <- if (needs_fps && fps_ok) tc
               else if (tc %in% c("time", "speed", "sequence")) "sequence"  # bad fps -> sequence
               else "trajectory"
  list(effective = effective,
       gradient  = effective %in% c("sequence", "time", "speed"),
       fps_ok    = fps_ok,
       needs_fps = needs_fps,
       fps       = fps)
}

# Normalise an app-provided data frame of angles into a `headings_frame` whose
# angle column is named "heading". `col` is the (string) angle column; `units`
# and `convention` feed headings_frame()'s validation/conversion (convention
# "unit_circle" or "clock"). An optional `group` column is preserved for
# faceting / colour / per-group statistics. Used by both the uploaded-file path
# and the cpunctatus example path (whose angles are already radians/unit_circle).
build_headings_input <- function(df, col, units, convention, group = NULL) {
  # as.name() makes headings_frame()'s ensym() capture work with a string col.
  hf <- do.call(headings_frame,
                list(data = df, col = as.name(col), units = units,
                     angle_convention = convention))
  hc <- attr(hf, "heading_col")
  if (!identical(hc, "heading")) {
    hf[["heading"]]          <- hf[[hc]]
    attr(hf, "heading_col")  <- "heading"
  }
  hf
}

# Attrition message for the Results banner, or NULL when there is no attrition.
# `derived` selects the loud, bias-caveat wording (headings derived from tracks
# via a rule) vs the neutral wording (angles provided directly). Shiny-free so
# it can be unit-tested.
attrition_note <- function(n_total, n_missing, derived, rule = NULL) {
  if (is.null(n_missing) || is.null(n_total) || n_missing <= 0L) return(NULL)
  used <- n_total - n_missing
  pct  <- round(100 * n_missing / n_total)
  if (isTRUE(derived)) {
    sprintf(paste0("%d of %d trials produced a heading; %d (%d%%) were excluded ",
                   "by rule '%s'. Rule-based exclusion is often non-random and ",
                   "can bias circular statistics — inspect the excluded ",
                   "trials before interpreting."),
            used, n_total, n_missing, pct, rule %||% "?")
  } else {
    sprintf("%d of %d rows have no angle and are excluded from the statistics.",
            n_missing, n_total)
  }
}

# Shared defaults (kept here so spec_to_plot and spec_to_code agree).
SPEC_CYCLE_N         <- 20L
SPEC_STACK_BIN_WIDTH <- pi / 36   # 5 degrees
SPEC_STACK_STEP      <- 0.08
SPEC_MARKER_SIZE     <- 2.5
SPEC_MARKER_ALPHA    <- 0.8
SPEC_TRAJ_KEY        <- "__trajectory__"
SPEC_CRIT_COLOUR     <- "firebrick"   # Rayleigh critical circle
SPEC_CRIT_LWD        <- 0.7
SPEC_VTEST_MU0       <- pi / 2        # V-test hypothesised direction (display top)
SPEC_VTEST_COLOUR    <- "steelblue"
SPEC_VTEST_LWD       <- 0.8

# Resolve the figure choices into a spec list.
#   ts     : the loaded Tracks.
#   hd     : the headings frame, or NULL (rule "none").
#   method : the heading rule name (or "none").
#   data   : list(source = "file"|"example", path, dialect).
#   inputs : a plain list of the relevant input values (see fields used below).
build_plot_spec <- function(ts, hd, method, data, inputs) {
  mode <- data$mode %||% "trajectories"
  headings_mode <- identical(mode, "headings")

  if (headings_mode) {
    df     <- as.data.frame(hd)
    id_col <- "id"                          # derive_headings/headings frames key on "id" when present
  } else {
    id_col <- ts@cols$id
    df     <- as.data.frame(ts)
  }

  gc <- if (!is.null(inputs$cond_col) && nzchar(inputs$cond_col))
    inputs$cond_col else NULL
  # In headings mode a group only exists if it is a column of hd.
  if (headings_mode && !is.null(gc) && !(gc %in% names(df))) gc <- NULL

  cb <- inputs$colour_by
  by_traj <- is.null(cb) || !nzchar(cb) || identical(cb, SPEC_TRAJ_KEY)
  if (headings_mode && by_traj) {
    # No per-trajectory identity for headings; fall back to the group column if any.
    cb <- gc; by_traj <- is.null(gc)
  }
  if (!by_traj && !(cb %in% names(df))) by_traj <- TRUE   # stale selection
  key_col  <- if (by_traj) id_col else cb
  # key_col may be absent in headings mode with no group (id_col "id" not in an uploaded table)
  n_levels <- if (key_col %in% names(df)) length(unique(df[[key_col]])) else 1L
  legend   <- !by_traj && n_levels <= SPEC_CYCLE_N        # distinct -> legend

  rule_params <- if (identical(method, "crossing"))
    list(circ0 = inputs$circ0 %||% 0.3, circ1 = inputs$circ1 %||% 0.6) else list()

  list(
    data      = data,
    mode      = mode,
    axial     = isTRUE(inputs$axial),
    # in headings mode method is NULL, so headings$rule is absent by design (every reader gates on mode first)
    headings = c(list(rule = method), rule_params),
    group_col = id_col,
    facet_by  = gc,
    colour    = list(by = if (by_traj) "trajectory" else cb,
                     cap = SPEC_CYCLE_N, legend = legend),
    theme        = inputs$plot_theme %||% "void",
    track_colour = inputs$track_colour %||% "trajectory",
    coords = inputs$frame %||% "relative",
    frame_rate = inputs$frame_rate,
    angle_labels = inputs$angle_labels %||% "degrees",
    display      = list(zero = 0),
    heading_display = inputs$heading_display %||% "points",
    # Resolved annotation text (computed app-side); stored so the code export
    # reproduces it verbatim. NULL/"" means no label.
    subtitle = inputs$subtitle,
    caption  = inputs$caption,
    # Resolution of the Summary & stats grouping, mirroring summary_ctx() in
    # app.R: headings mode groups by the hd group column (pooled -> a one-level
    # ".all" sentinel); trajectory mode groups by the condition column or "id".
    # Consumed by spec_to_stats_code() so the emitted analysis matches the table.
    stats = local({
      if (headings_mode) {
        list(by_col  = gc %||% ".all",
             pooled  = is.null(gc),
             omnibus = inputs$omnibus_test %||% "rao",
             axial   = isTRUE(inputs$axial))
      } else {
        list(by_col  = gc %||% id_col,
             pooled  = FALSE,
             omnibus = inputs$omnibus_test %||% "rao",
             axial   = isTRUE(inputs$axial))
      }
    }),
    show = list(tracks    = !headings_mode && isTRUE(inputs$show_tracks),
                arrow     = isTRUE(inputs$show_arrow),
                vectors   = isTRUE(inputs$show_vectors),
                rayleigh  = isTRUE(inputs$show_rayleigh),
                ci        = isTRUE(inputs$show_ci),
                vtest     = isTRUE(inputs$show_vtest),
                quadrants = isTRUE(inputs$show_quadrants),
                rings     = isTRUE(inputs$show_rings),
                boxplot   = isTRUE(inputs$show_boxplot))
  )
}

# Render a spec to a ggplot, using only exported radiatR functions. `ts`/`hd` are
# the in-memory objects; the spec's `data` block matters only for spec_to_code().
spec_to_plot <- function(spec, ts, hd) {
  disp <- circ_display(zero = spec$display$zero)
  by   <- spec$colour$by
  cap  <- spec$colour$cap
  headings_mode <- identical(spec$mode, "headings")
  # Sequence track colouring owns the continuous colour scale, so it cannot
  # share it with the discrete `.colour` key or contest it with coloured
  # heading overlays. Gate it off in headings mode (no tracks to colour).
  rtc            <- .resolve_track_colour(spec, headings_mode)
  gradient_track <- rtc$gradient

  if (headings_mode) {
    # No group column -> "trajectory" sentinel means a single colour. An uploaded
    # angle table has no "id" column, so assign_colour_key(by="trajectory") is not
    # valid; use a one-level constant key instead.
    if (identical(by, "trajectory")) {
      hd$.colour <- factor("all")
    } else {
      hd <- assign_colour_key(hd, by = by, n = cap)
    }
    attr(hd, "display") <- disp
    p <- radiate(
      hd,
      show_markers = FALSE,
      colour_col   = ".colour",
      panel_by     = spec$facet_by,
      legend       = spec$colour$legend,
      theme        = spec$theme,
      angle_labels = spec$angle_labels,
      quadrants    = isTRUE(spec$show$quadrants),
      rings        = isTRUE(spec$show$rings),
      display      = disp
    )
  } else {
    ts <- assign_colour_key(ts, by = by, n = cap)
    if (rtc$effective %in% c("time", "speed")) ts <- set_frame_rate(ts, rtc$fps)
    p <- radiate(
      ts,
      group_col    = spec$group_col,
      colour_col   = if (gradient_track) NULL else ".colour",
      track_colour = rtc$effective,
      coords       = spec$coords %||% "relative",
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
  }
  if (spec$colour$legend)
    p <- p + ggplot2::labs(colour = by)

  # Annotation labels (resolved app-side). Applied before the none-mode early
  # return so the tracks-only figure carries them too.
  lab_args <- list()
  if (!is.null(spec$subtitle) && nzchar(spec$subtitle)) lab_args$subtitle <- spec$subtitle
  if (!is.null(spec$caption)  && nzchar(spec$caption))  lab_args$caption  <- spec$caption
  if (length(lab_args)) p <- p + do.call(ggplot2::labs, lab_args)

  # Trajectory mode with rule "none" or no headings: tracks-only figure.
  if (!headings_mode && (identical(spec$headings$rule, "none") || is.null(hd)))
    return(p)

  if (!headings_mode) {
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
  }

  if (!identical(spec$heading_display, "none")) {
    # In sequence track mode the continuous scale owns the colour aesthetic, so
    # heading overlays render in a fixed colour rather than the discrete key.
    pt_colour_col <- if (gradient_track) NULL else ".colour"
    pt_colour     <- if (gradient_track) "grey20" else NULL
    if (identical(spec$heading_display, "stacked")) {
      hd$heading <- bin_angles(hd$heading, width = SPEC_STACK_BIN_WIDTH)
      p <- p + add_stacked_headings(hd, colour_col = pt_colour_col,
                 colour = pt_colour, group = spec$facet_by,
                 step = SPEC_STACK_STEP,
                 size = SPEC_MARKER_SIZE, alpha = SPEC_MARKER_ALPHA,
                 axial = isTRUE(spec$axial))
    } else {
      p <- p + add_heading_points(hd, colour_col = pt_colour_col,
                 colour = pt_colour,
                 size = SPEC_MARKER_SIZE, alpha = SPEC_MARKER_ALPHA,
                 axial = isTRUE(spec$axial))
    }
  }

  if (spec$show$arrow) {
    arrow_df <- compute_circ_mean(hd, colour_col = spec$facet_by, axial = isTRUE(spec$axial))
    p <- p + add_circ_mean(arrow_df, colour = "black", axial = isTRUE(spec$axial))
  }
  if (spec$show$vectors && all(c("x_inner", "y_inner") %in% names(hd)))
    p <- p + add_heading_vectors(hd, colour_col = ".colour",
               axial = isTRUE(spec$axial))

  # Mean-direction bootstrap CI arc (per facet group when faceted). hd already
  # carries the display attribute (set above) so the arc orients correctly.
  if (isTRUE(spec$show$ci))
    p <- p + add_heading_interval(hd, colour_col = spec$facet_by,
               stat = "bootstrap_ci", axial = isTRUE(spec$axial))

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
           colour = SPEC_VTEST_COLOUR, linewidth = SPEC_VTEST_LWD,
           axial = isTRUE(spec$axial))
    if (!is.null(v)) p <- p + v
  }

  # Circular boxplot overlay (Buttarazzi et al. 2018), drawn last. NULL (no-op
  # under +) and a warning when the data is not drawable.
  if (isTRUE(spec$show$boxplot))
    p <- p + add_circular_boxplot(hd, axial = isTRUE(spec$axial),
                                  theme = spec$theme %||% "void",
                                  panel_by = spec$facet_by)

  p
}

# Emit the data-load + headings-derivation lines shared by the figure-code and
# stats-code scripts: `data(cpunctatus)`/`read.csv`/`read_tracks` + `ts`/`hd`
# (+ the facet merge). Figure-specific lines (colour keys, frame rate, disp) stay
# in the caller. `add`/`q` are the caller's line-appender / string-quoter.
.emit_data_preamble <- function(spec, add, q) {
  headings_mode <- identical(spec$mode, "headings")
  has_hd <- headings_mode || !identical(spec$headings$rule, "none")
  if (headings_mode) {
    if (identical(spec$data$source, "example")) {
      add("data(cpunctatus)")
      add("hd <- derive_headings(cpunctatus, rule = \"distal\", coords = \"relative\")")
      if (!is.null(spec$facet_by))
        add("hd <- merge(hd, unique(as.data.frame(cpunctatus)[, c(\"trial_id\", ",
            q(spec$facet_by), ")]), by.x = \"id\", by.y = \"trial_id\", all.x = TRUE)")
      add("hd <- headings_frame(hd, col = heading, units = \"radians\")")
    } else {
      add("df <- read.csv(", q(spec$data$path), ")")
      add("hd <- headings_frame(df, col = ", spec$data$col,
          ", units = ", q(spec$data$units),
          ", angle_convention = ", q(spec$data$convention), ")")
      if (!identical(spec$data$col, "heading")) {
        add("names(hd)[names(hd) == ", q(spec$data$col), "] <- \"heading\"")
        add("attr(hd, \"heading_col\") <- \"heading\"")
      }
    }
  } else {
    if (identical(spec$data$source, "example")) {
      add("data(cpunctatus)")
      add("ts <- cpunctatus")
    } else {
      dia <- if (!is.null(spec$data$dialect))
        paste0(", dialect = ", q(spec$data$dialect)) else ""
      add("ts <- read_tracks(", q(spec$data$path), dia, ")")
    }
    if (has_hd) {
      add("")
      hp <- if (identical(spec$headings$rule, "crossing"))
        paste0(", circ0 = ", spec$headings$circ0, ", circ1 = ", spec$headings$circ1) else ""
      # Heading vectors need the crossing construction coords; request them so the
      # emitted script can draw them (only the crossing rule produces x_inner/y_inner).
      rc <- if (spec$show$vectors && identical(spec$headings$rule, "crossing"))
        ", return_coords = TRUE" else ""
      coa <- if (identical(spec$coords %||% "relative", "absolute"))
        ", coords = \"absolute\"" else ""
      add("hd <- derive_headings(ts, rule = ", q(spec$headings$rule), hp, rc, coa, ")")
      if (!is.null(spec$facet_by))
        add("hd <- merge(hd, unique(as.data.frame(ts)[, c(", q(spec$group_col), ", ",
            q(spec$facet_by), ")]), by.x = \"id\", by.y = ", q(spec$group_col),
            ", all.x = TRUE)")
    }
  }
  list(headings_mode = headings_mode, has_hd = has_hd)
}

# Runnable radiatR script reproducing the Summary & stats analysis (the real
# statistics behind the table; the app applies display formatting on top).
spec_to_stats_code <- function(spec) {
  q   <- function(s) encodeString(s, quote = '"')
  L   <- character(0)
  add <- function(...) L[[length(L) + 1L]] <<- paste0(...)

  add("library(radiatR)")
  add("")
  pre <- .emit_data_preamble(spec, add, q)
  add("")
  add("# Summary & stats analysis (display formatting is applied in the app)")

  st     <- spec$stats %||% list()
  by_col <- if (isTRUE(st$pooled)) NULL else st$by_col
  axial  <- if (isTRUE(st$axial)) ", axial = TRUE" else ""
  byarg  <- if (!is.null(by_col)) paste0(", .by = ", q(by_col)) else ""

  # `hd` keys trials on "id"; the grouping column lives on the source data, so
  # join it onto `hd` (unless it is already a column, e.g. from the facet merge).
  if (!is.null(by_col) && !identical(by_col, spec$facet_by)) {
    src <- if (pre$headings_mode) "cpunctatus" else "ts"
    src_id <- if (pre$headings_mode) q("trial_id") else q(spec$group_col)
    add("if (!", q(by_col), " %in% names(hd)) hd <- merge(hd, unique(as.data.frame(",
        src, ")[, c(", src_id, ", ", q(by_col), ")]), by.x = \"id\", by.y = ",
        src_id, ", all.x = TRUE)")
  }

  add("summ <- circ_summarise(hd, \"heading\", units = \"radians\"", byarg,
      ", stats = c(\"n\", \"n_missing\", \"mean_dir_deg\", \"resultant_R\")",
      ", display = circ_display(zero = 0)", axial, ")")
  add("summ")
  add("")
  add("test_uniformity(hd, test = \"rayleigh\"", axial, ")")
  omni <- if (identical(st$omnibus, "hermans_rasson")) "hermans_rasson" else "rao"
  add("test_uniformity(hd, test = ", q(omni), ")")
  if (!is.null(by_col)) {
    add("")
    add("circ_model_select(hd, group_col = ", q(by_col), ")")
  } else if (isTRUE(pre$has_hd)) {
    add("")
    add("circ_model_select(hd)")
  }
  if (!pre$headings_mode) {           # straightness needs the Tracks
    add("")
    add("straightness_index(ts)")
  }
  paste(L, collapse = "\n")
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

  headings_mode <- identical(spec$mode, "headings")
  # Sequence track colouring owns the continuous colour scale (see spec_to_plot);
  # emit it on the radiate() call and render heading overlays in a fixed colour.
  rtc            <- .resolve_track_colour(spec, headings_mode)
  gradient_track <- rtc$gradient
  tc <- switch(rtc$effective,
               time     = ", track_colour = \"time\"",
               speed    = ", track_colour = \"speed\"",
               sequence = ", track_colour = \"sequence\"",
               "")
  ax <- if (isTRUE(spec$axial)) ", axial = TRUE" else ""
  co <- if (identical(spec$coords %||% "relative", "absolute"))
    ", coords = \"absolute\"" else ""

  # Data-load + heading-derivation (shared with the stats-code emitter); returns
  # headings_mode/has_hd. Headings mode always has a headings frame; trajectory
  # mode has one unless the heading rule is "none". has_hd gates the figure-specific
  # colour-key emission below and the shared overlay/tail emission further down.
  pre <- .emit_data_preamble(spec, add, q)
  headings_mode <- pre$headings_mode
  has_hd        <- pre$has_hd

  if (headings_mode) {
    add("")
    if (identical(spec$colour$by, "trajectory")) {
      add("hd$.colour <- factor(\"all\")")          # single-colour: no group column
    } else {
      add("hd <- assign_colour_key(hd, by = ", q(spec$colour$by), ")")
    }
  } else {
    add("")
    add("ts <- assign_colour_key(ts, by = ", q(spec$colour$by), ")")
    if (rtc$effective %in% c("time", "speed"))
      add("ts <- set_frame_rate(ts, ", rtc$fps, ")")
    if (has_hd)
      add("hd <- assign_colour_key(hd, by = ", q(spec$colour$by), ", reference = ts)")
  }

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
    add("arrow_df <- compute_circ_mean(hd", cc, ax, ")")
  }

  add("")
  pby <- if (is.null(spec$facet_by)) "" else paste0(", panel_by = ", q(spec$facet_by))
  # Only emit quadrants/rings when on -- they default to FALSE in radiate(), so
  # the common case keeps a clean call.
  qr <- paste0(if (isTRUE(spec$show$quadrants)) ", quadrants = TRUE" else "",
               if (isTRUE(spec$show$rings))     ", rings = TRUE"     else "")
  if (headings_mode) {
    add("radiate(hd, show_markers = FALSE, colour_col = \".colour\"", pby,
        ", legend = ", if (spec$colour$legend) "TRUE" else "FALSE",
        ", theme = ", q(spec$theme),
        ", angle_labels = ", q(spec$angle_labels), qr,
        ", display = disp)")
  } else {
    cc <- if (gradient_track) ", colour_col = NULL" else ", colour_col = \".colour\""
    add("radiate(ts, group_col = ", q(spec$group_col),
        cc, tc, co, pby,
        ", legend = ", if (spec$colour$legend) "TRUE" else "FALSE",
        ", theme = ", q(spec$theme),
        ", angle_labels = ", q(spec$angle_labels), qr,
        ", show_labels = FALSE, show_arrow = FALSE, display = disp)")
  }

  tail <- character(0)
  if (spec$colour$legend)
    tail <- c(tail, paste0("labs(colour = ", q(spec$colour$by), ")"))
  # In sequence track mode the heading overlays drop the discrete colour key and
  # render in a fixed grey (mirrors spec_to_plot's pt_colour_col/pt_colour).
  pt_cc <- if (gradient_track) "colour = \"grey20\"" else "colour_col = \".colour\""
  if (has_hd && identical(spec$heading_display, "stacked")) {
    grp <- if (is.null(spec$facet_by)) "" else paste0(", group = ", q(spec$facet_by))
    tail <- c(tail, paste0("add_stacked_headings(hd, ", pt_cc, grp,
                           ", step = ", SPEC_STACK_STEP,
                           ", size = ", SPEC_MARKER_SIZE, ", alpha = ", SPEC_MARKER_ALPHA, ax, ")"))
  }
  if (has_hd && identical(spec$heading_display, "points"))
    tail <- c(tail, paste0("add_heading_points(hd, ", pt_cc, ", size = 2.5, alpha = 0.8", ax, ")"))
  if (has_hd && spec$show$arrow)
    tail <- c(tail, paste0("add_circ_mean(arrow_df, colour = \"black\"", ax, ")"))
  if (has_hd && spec$show$vectors && identical(spec$headings$rule, "crossing"))
    tail <- c(tail, paste0("add_heading_vectors(hd, colour_col = \".colour\"", ax, ")"))
  if (has_hd && isTRUE(spec$show$ci)) {
    cc <- if (is.null(spec$facet_by)) "" else paste0(", colour_col = ", q(spec$facet_by))
    tail <- c(tail, paste0(
      "add_heading_interval(hd", cc, ", stat = \"bootstrap_ci\"", ax, ")"))
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
      ", colour = ", q(SPEC_VTEST_COLOUR), ", linewidth = ", SPEC_VTEST_LWD, ax, ")"))
  }
  if (has_hd && isTRUE(spec$show$boxplot)) {
    bpb <- if (is.null(spec$facet_by)) "" else paste0(", panel_by = ", q(spec$facet_by))
    tail <- c(tail, paste0("add_circular_boxplot(hd", ax,
                           ", theme = ", q(spec$theme %||% "void"), bpb, ")"))
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

# ---- Kinematics figure (Results "Kinematics" sub-tab) ------------------------
# The non-circular sibling of build_plot_spec/spec_to_plot/spec_to_code: resolves
# the speed/turning-vs-time profile to a plain spec that both renders via the
# exported plot_profile() and emits matching radiatR code.

build_kinematics_spec <- function(ts, inputs) {
  cb <- inputs$kin_colour_by
  colour_by <- if (!is.null(cb) && nzchar(cb) && cb %in% names(as.data.frame(ts)))
    cb else NULL
  tr <- inputs$kin_track
  track <- if (!is.null(tr) && nzchar(tr) && tr %in% as.character(ids(ts)))
    tr else NULL
  list(
    metric    = inputs$kin_metric %||% "speed",
    units     = inputs$kin_units  %||% "radians",
    colour_by = colour_by,
    track     = track,
    fps       = inputs$fps,
    src_id    = ts@cols$id,
    data      = inputs$data,
    # mode + a "none" heading rule make .emit_data_preamble emit only the ts-load
    # (has_hd == FALSE), never a derive_headings() the kinematics figure does not use.
    mode      = "trajectories",
    headings  = list(rule = "none")
  )
}

kinematics_spec_to_plot <- function(spec, ts) {
  # `%||% 30` mirrors spec_to_kinematics_code so render and emit never diverge on
  # an unset fps (the app always supplies one; this keeps the triad self-consistent).
  if (!is.null(spec$track)) ts <- ts[spec$track]
  ts <- set_frame_rate(ts, spec$fps %||% 30)
  plot_profile(ts, metric = spec$metric, units = spec$units,
               colour_by = spec$colour_by)
}

spec_to_kinematics_code <- function(spec) {
  q   <- function(s) encodeString(s, quote = '"')
  L   <- character(0)
  add <- function(...) L[[length(L) + 1L]] <<- paste0(...)

  add("library(radiatR)")
  add("")
  .emit_data_preamble(spec, add, q)              # trajectory ts-load only
  add("")
  if (!is.null(spec$track)) add("ts <- ts[", q(spec$track), "]")
  add("ts <- set_frame_rate(ts, ", spec$fps %||% 30, ")")
  unit_arg <- if (identical(spec$metric, "turning"))
    paste0(", units = ", q(spec$units)) else ""
  col_arg  <- if (!is.null(spec$colour_by))
    paste0(", colour_by = ", q(spec$colour_by)) else ""
  add("plot_profile(ts, metric = ", q(spec$metric), unit_arg, col_arg, ")")
  paste(L, collapse = "\n")
}
