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
    show = list(tracks  = isTRUE(inputs$show_tracks),
                arrow   = isTRUE(inputs$show_arrow),
                vectors = isTRUE(inputs$show_vectors))
  )
}
