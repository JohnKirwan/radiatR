# radiatR — Shiny companion app
# Launch with: radiatR::launch_app()
# To deploy: rsconnect::deployApp(system.file("app", package = "radiatR"))

library(shiny)
library(bslib)

# The analysis/plot packages are attached lazily (see ensure_pkgs()), not here,
# so the upload screen paints on shiny + bslib alone. ggplot2 and radiatR (the
# latter pulling in circular -> boot, mvtnorm) attach the first time the user
# loads data -- which, under shinylive, is the bulk of the in-browser R boot. The
# library() calls below stay in the source so shinylive still bundles them into
# the WASM image; they only run when ensure_pkgs() is first called.
ensure_pkgs <- function() {
  if (!"radiatR" %in% .packages()) library(radiatR)
  if (!"ggplot2" %in% .packages()) library(ggplot2)
  invisible()
}

# Configure-step preview helpers (demo_tracks(), build_method_preview()).
source("preview.R", local = FALSE)
source("preview_constructions.R", local = FALSE)
source("download_helpers.R", local = FALSE)
source("ui_helpers.R", local = FALSE)
source("plot_spec.R", local = FALSE)

# ---- dialect registry --------------------------------------------------------

DIALECT_CHOICES <- c(
  "Auto-detect"                   = "auto",
  "EthoVision XT (Noldus)"        = "ethovision",
  "DeepLabCut"                    = "deeplabcut",
  "DeepLabCut (multi-header CSV)" = "deeplabcut_multiheader",
  "SLEAP"                         = "sleap",
  "TrackMate (Fiji)"              = "trackmate",
  "TRex"                          = "trex",
  "ANY-maze"                      = "anymaze",
  "idtracker.ai"                  = "idtrackerai_wide",
  "ToxTrac"                       = "toxtrac",
  "BORIS (with XY)"               = "boris_xy",
  "Tracktor"                      = "tracktor",
  "Ctrax (.mat file)"             = "ctrax",
  "Generic CSV"                   = "generic"
)

# ---- helpers -----------------------------------------------------------------

guess_dialect <- function(path) {
  hdr <- tryCatch(
    readLines(path, n = 3L),
    error = function(e) character(0L)
  )
  if (length(hdr) >= 2L &&
      grepl("scorer",    tolower(hdr[1L]), fixed = TRUE) &&
      grepl("bodyparts", tolower(hdr[2L]), fixed = TRUE)) {
    return("deeplabcut_multiheader")
  }
  df <- tryCatch(
    utils::read.csv(
      path, nrows = 3L,
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(df)) return("generic")
  nm <- tolower(gsub("[^a-z0-9]+", "_", names(df)))

  if (any(grepl("_likelihood$", nm)) && any(grepl("_x$", nm)))
    return("deeplabcut")
  if ("frame_idx" %in% nm && any(grepl("_score$", nm)))
    return("sleap")
  if ("track_id" %in% nm && "position_x" %in% nm)
    return("trackmate")
  if ("x_center" %in% nm || "x_centre" %in% nm)
    return("ethovision")
  if ("x_1" %in% nm && "y_1" %in% nm)
    return("idtrackerai_wide")
  if ("trial_time" %in% nm || any(grepl("^x_centr", nm)))
    return("anymaze")
  if ("trackid" %in% nm)
    return("toxtrac")
  "generic"
}

# Low-cardinality categorical columns usable for *faceting* (the Facet-by
# selector): character/factor with 2-12 distinct values, excluding the structural
# id and time columns (faceting into many panels is unhelpful).
grouping_cols <- function(ts) {
  df   <- as.data.frame(ts)
  cats <- names(df)[vapply(df, function(v) {
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) && nu >= 2L && nu <= 12L
  }, logical(1))]
  setdiff(cats, c(ts@cols$id, ts@cols$time))
}

# Categorical columns usable for *colouring* (the Colour-by selector): any
# character/factor column with >= 2 levels (no upper cap -- high-cardinality keys
# like an individual id are fine because colours cycle/cap), excluding time and
# the trajectory id (the latter is the default "Trajectory" option).
colour_cols <- function(ts) {
  df   <- as.data.frame(ts)
  cats <- names(df)[vapply(df, function(v) {
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) && nu >= 2L
  }, logical(1))]
  setdiff(cats, c(ts@cols$id, ts@cols$time))
}

# Categorical columns of a headings data frame usable for faceting/grouping (the
# headings analogue of grouping_cols(): char/factor with 2-12 levels, excluding
# the id and heading columns). Used to populate the headings "Facet by" selector.
.hd_group_choices <- function(hd) {
  if (is.null(hd)) return(character(0))
  hc   <- attr(hd, "heading_col") %||% "heading"
  cats <- names(hd)[vapply(hd, function(v) {
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) && nu >= 2L && nu <= 12L
  }, logical(1))]
  setdiff(cats, c("id", hc))
}

# Identify a candidate condition column in a loaded Tracks.
# Looks for character columns with 2-12 unique values that are not
# structural (id, time, position, derivatives).
detect_cond_col <- function(ts) {
  df   <- as.data.frame(ts)
  skip <- unique(c(
    ts@cols$id, ts@cols$time,
    ts@cols$x,  ts@cols$y,
    ts@cols$rel_x, ts@cols$rel_y,
    grep(
      "^(rho|angle|radius|raw_|x_raw|y_raw|rel_|trans_|dist)",
      names(df), value = TRUE
    )
  ))
  cands <- setdiff(names(df), c(skip, NA_character_))
  hits <- Filter(function(col) {
    v  <- df[[col]]
    nu <- length(unique(stats::na.omit(v)))
    (is.character(v) || is.factor(v)) &&
      nu >= 2L && nu <= 12L && nu < nrow(df)
  }, cands)
  if (length(hits)) hits[1L] else NULL
}

load_ts <- function(path, dialect, mapping = list(), read_opts = list(delim = NULL)) {
  if (is.null(dialect) || dialect %in% c("auto", "generic"))
    return(read_tracks(path, mapping = mapping, read_opts = read_opts))
  read_tracks(path, dialect = dialect, mapping = mapping, read_opts = read_opts)
}

# Map the app's delimiter dropdown to a read_opts override. "auto" -> sniff.
delim_read_opts <- function(sel) {
  if (is.null(sel) || identical(sel, "auto")) list(delim = NULL) else list(delim = sel)
}

# The bundled Cylindroiulus punctatus millipede example, as a Tracks, so
# users can try the app without supplying their own tracking file.
example_ts <- function() {
  e <- new.env()
  utils::data("cpunctatus", package = "radiatR", envir = e)
  e$cpunctatus
}

# Heading methods that are inherently axial (per-track movement axis, not a
# single direction). Selecting one soft-syncs Directionality to "axial".
# Extend here when the modality piece adds more axial methods.
AXIAL_METHODS <- c("velocity_axis", "pca_axis", "ransac_straight")

# Package website (pkgdown). Used by the header brand + Documentation link.
SITE_URL <- "https://johnkirwan.github.io/radiatR/"

# A compact inline on/off toggle for a results-plot layer. Wraps a standard
# Shiny checkbox (so it registers as input[[id]]) and styles it as a switch.
.layer_switch <- function(id, label, value) {
  div(
    class = "form-switch small",
    checkboxInput(id, label, value = value, width = "auto")
  )
}

# Sentinel "Colour by" value meaning "colour each trajectory" (the default),
# as opposed to a real grouping column name. Must match SPEC_TRAJ_KEY in
# plot_spec.R (build_plot_spec reads input$colour_by against it).
TRAJ_COLOUR_KEY <- "__trajectory__"

# A finite scalar from a possibly-NULL/NA numeric input, else a default.
num_or <- function(v, default) {
  if (is.null(v) || length(v) != 1L || is.na(v) || !is.finite(v)) default else v
}

# Per-rule one-line descriptions for the Configure-step help line, keyed by the
# selectInput value. "none" is an app-level sentinel (skip headings), not a
# derive_headings() rule.
method_help_text <- c(
  none            = "Plot the tracks and path metrics only - no headings or circular statistics.",
  distal          = "Heading when the subject was furthest from the centre. No setup needed.",
  net             = "Straight-line direction from start to end. Simple and always applicable.",
  crossing        = "Heading as the subject crosses a detection ring. Set the ring radii below.",
  straight        = "Direction of the longest near-straight run in the path.",
  window_net      = "Net direction over a sliding window, smoothing local wobble.",
  origin_mean     = "Mean of the directions from the centre to each point.",
  velocity_mean   = "Mean of the step (velocity) directions along the path.",
  velocity_axis   = "Axial mean of step directions - the back-and-forth movement axis per track. Set Directionality to Axial for correct circular statistics.",
  maxspeed_window = "Direction over the window of the subject's fastest movement.",
  vm_fit          = "Von Mises fit to the step directions (peak of the fitted distribution).",
  pca_axis        = "Principal (long) axis of the visited positions.",
  ransac_straight = "Robust straight-line fit that ignores outlying points (RANSAC).",
  goal_bias       = "Net step direction weighted toward outward (away-from-centre) movement."
)

# One-line path-metrics caption for the no-headings ("none") mode. Reads the
# same exported metric functions the metrics table uses so numbers match. Extend
# by appending more "label/values" clauses as metrics are added.
straightness_caption <- function(ts, gc = NULL) {
  idc <- ts@cols$id
  st  <- straightness_index(ts)
  sn  <- sinuosity(ts)
  if (is.null(gc)) {
    ms <- mean(st$straightness, na.rm = TRUE)
    if (!is.finite(ms)) return("")
    msin <- mean(sn$sinuosity, na.rm = TRUE)
    out  <- sprintf("Mean straightness: %.2f", ms)
    if (is.finite(msin)) out <- paste0(out, sprintf(" | Mean sinuosity: %.2f", msin))
    out
  } else {
    cond_map <- unique(as.data.frame(ts)[, c(idc, gc), drop = FALSE])
    clause <- function(tbl, val, label) {
      tbl <- merge(tbl, cond_map, by = idc)
      agg <- tapply(tbl[[val]], as.character(tbl[[gc]]),
                    function(v) mean(v, na.rm = TRUE))
      paste0(label, " - ",
             paste(sprintf("%s: %.2f", names(agg), as.numeric(agg)),
                   collapse = ", "))
    }
    paste(clause(st, "straightness", "Straightness"),
          clause(sn, "sinuosity",    "Sinuosity"), sep = " | ")
  }
}

# Combined per-track path-metrics table for the no-headings ("none") CSV: id +
# length + straightness + tortuosity + sinuosity, each from its exported package
# function, joined on the id column.
path_metrics_table <- function(ts) {
  idc <- ts@cols$id
  Reduce(function(a, b) merge(a, b, by = idc),
         list(track_length(ts), straightness_index(ts),
              tortuosity_ratio(ts), sinuosity(ts)))
}

derive_hd <- function(ts, method, circ0, circ1, coords = "relative") {
  # Use the requested frame; relative aligns headings with the rel_x/rel_y display
  # (stimulus fixed at East). Datasets without a relative coordinate system can
  # only produce absolute headings, so a relative request floors to absolute.
  has_rel <- !is.null(ts@cols$rel_x) && !is.null(ts@cols$rel_y)
  use <- if (identical(coords, "relative") && !has_rel) "absolute" else coords
  args <- list(x = ts, coords = use)
  # The app surfaces attrition via the Results banner, not the R console; quiet
  # still attaches the n_total/n_missing/missing_ids attributes the banner reads.
  args$on_missing <- "quiet"
  if (method == "crossing") {
    args$rule          <- "crossing"
    args$circ0         <- circ0
    args$circ1         <- circ1
    args$return_coords <- TRUE
  } else {
    args$rule <- method
  }
  do.call(derive_headings, args)
}

rayleigh_p_fmt <- function(angles, axial = FALSE) {
  tryCatch({
    ang <- angles[is.finite(angles)]
    if (isTRUE(axial)) ang <- (2 * ang) %% (2 * pi)
    a <- circular::circular(ang, units = "radians", type = "angles")
    p <- circular::rayleigh.test(a)$p.value
    if (is.na(p))   return("—")
    if (p < 0.001)  return("< 0.001")
    sprintf("%.3f", p)
  }, error = function(e) "—")
}

# Omnibus Rao spacing test, formatted as a coarse significance bracket. Unlike
# the focused Rayleigh row this is model-agnostic: always computed on RAW angles
# (never doubled), as an always-on departure-from-uniformity backstop.
# rao.spacing.test only carries the bracket in its print method (the returned
# object's $alpha is the input parameter, not the achieved level), so parse the
# printed "P-value" line. Brackets: "< 0.001" / "< 0.01" / "< 0.05" / "< 0.10" /
# "> 0.10". n < 4 (Rao's table floor) or any error -> "—".
rao_spacing_fmt <- function(angles) {
  tryCatch({
    ang <- angles[is.finite(angles)]
    if (length(ang) < 4L) return("—")
    a   <- circular::circular(ang, units = "radians", type = "angles")
    out <- utils::capture.output(suppressWarnings(circular::rao.spacing.test(a)))
    line <- grep("P-value", out, value = TRUE)
    if (length(line) == 0L) return("—")
    m <- regmatches(line[1L], regexpr("[<>] ?0?\\.[0-9]+", line[1L]))
    if (length(m) == 0L) return("—")
    gsub("\\s+", " ", trimws(m))
  }, error = function(e) "—")
}

# Omnibus Hermans-Rasson test p-value, formatted for the summary. Monte-Carlo, so
# a fixed seed is set per call for render stability (identical inputs -> identical
# p across re-renders). A modest n_sim keeps the reactive responsive. n < 3 or any
# error -> "—".
hermans_p_fmt <- function(angles, n_sim = 999L) {
  tryCatch({
    a <- angles[is.finite(angles)]
    if (length(a) < 3L) return("—")
    set.seed(20260617L)
    p <- test_uniformity(data.frame(heading = a),
                         test = "hermans_rasson", n_sim = n_sim)$p_value
    if (is.na(p))  return("—")
    if (p < 0.001) return("< 0.001")
    sprintf("%.3f", p)
  }, error = function(e) "—")
}

# Display-ready circular summary (n, mean direction, resultant R, Rayleigh p) for
# a headings frame, grouped by `by_col`. Returns a data frame with the group
# column renamed to "Group"; callers may append trajectory-only columns
# (e.g. Straightness) keyed on the returned "Group" values.
circ_summary_table <- function(hd, by_col, axial = FALSE,
                               omnibus = "rao", model_sel = NULL) {
  cm <- circ_summarise(
    hd, "heading", units = "radians", .by = by_col,
    stats = c("n", "n_missing", "mean_dir_deg", "resultant_R"),
    display = circ_display(zero = 0), axial = axial
  )
  groups <- unique(hd[[by_col]])
  p_vals <- vapply(groups, function(g)
    rayleigh_p_fmt(hd$heading[hd[[by_col]] == g], axial = axial), character(1L))
  if (identical(omnibus, "hermans_rasson")) {
    omni_label <- "Hermans-Rasson p"
    omni_vals  <- vapply(groups, function(g)
      hermans_p_fmt(hd$heading[hd[[by_col]] == g]), character(1L))
  } else {
    omni_label <- "Rao spacing"
    omni_vals  <- vapply(groups, function(g)
      rao_spacing_fmt(hd$heading[hd[[by_col]] == g]), character(1L))
  }
  rayleigh_label <- if (isTRUE(axial)) "Rayleigh (axial) p" else "Rayleigh p"
  p_df <- stats::setNames(
    data.frame(groups, p_vals, omni_vals, stringsAsFactors = FALSE),
    c(by_col, rayleigh_label, omni_label))

  if (!is.null(model_sel) && nrow(model_sel) > 0L && by_col %in% names(model_sel)) {
    parts <- lapply(split(model_sel, model_sel[[by_col]]), function(x) {
      best <- x[1L, ]                              # circ_model_select sorts best-first
      data.frame(g = best[[by_col]],
                 bm = sprintf("%s (%.2f)", best$model, best$weight),
                 stringsAsFactors = FALSE)
    })
    bm <- do.call(rbind, parts)
    bm <- stats::setNames(bm, c(by_col, "Best model"))
    p_df <- merge(p_df, bm, by = by_col, sort = FALSE)
  }

  cm <- merge(cm, p_df, by = by_col, sort = FALSE)
  names(cm)[names(cm) == by_col]         <- "Group"
  names(cm)[names(cm) == "n_missing"]    <- "Excluded"
  names(cm)[names(cm) == "mean_dir_deg"] <- "Direction (°)"
  names(cm)[names(cm) == "resultant_R"]  <- "R"
  cm[["Direction (°)"]] <- round(cm[["Direction (°)"]], 1)
  cm[["R"]]             <- round(cm[["R"]], 3)
  cm
}

plain_error <- function(e) {
  m <- conditionMessage(e)
  if (grepl("id|time|mapping", m, ignore.case = TRUE))
    return(paste(
      "Could not identify the trial ID or time columns.",
      "Try selecting your software from the dropdown."
    ))
  if (grepl("x.*y|position|col", m, ignore.case = TRUE))
    return(paste(
      "Could not find x/y position columns.",
      "Try selecting your software from the dropdown."
    ))
  if (grepl("crossing|circ0|circ1", m, ignore.case = TRUE))
    return(paste(
      "No ring crossings found.",
      "Try adjusting the ring radii or choosing a different method."
    ))
  if (grepl("R\\.matlab|mat file", m, ignore.case = TRUE))
    return(paste(
      "Reading .mat files requires the R.matlab package:",
      "install.packages('R.matlab')"
    ))
  paste("Unexpected error:", m)
}

# ---- ui ----------------------------------------------------------------------

ui <- page_fillable(
  theme   = bs_theme(bootswatch = "flatly"),
  padding = "1.5rem",

  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$style(HTML("
    .pill       { display:inline-block; padding:3px 14px;
                  border-radius:99px; font-size:.78rem;
                  font-weight:600; margin-right:3px; }
    .pill-done  { background:#d4edda; color:#1a5c2a; }
    .pill-here  { background:#2780e3; color:#fff; }
    .pill-later { background:#e9ecef; color:#6c757d; }
    .main-body  { min-height:440px; }
  "))),

  card(
    card_header(
      class = "d-flex justify-content-between align-items-center",
      ui_brand(SITE_URL),
      uiOutput("step_pills")
    ),
    card_body(class = "main-body", uiOutput("wizard")),
    card_footer(
      class = "d-flex justify-content-between align-items-center",
      actionButton(
        "back_btn", "Back",
        class = "btn-sm btn-outline-secondary"
      ),
      tags$span(class = "text-muted small",
                paste0("v", utils::packageVersion("radiatR"))),
      uiOutput("fwd_btn_ui")
    )
  )
)

# ---- server ------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    step      = 1L,
    path      = NULL,
    source    = NULL,
    file_name = NULL,
    dialect   = NULL,
    ts        = NULL,
    cond_col  = NULL,
    hd        = NULL,
    method    = NULL,
    error     = NULL,
    mode      = "trajectories",
    raw_hd    = NULL,
    hd_map    = NULL    # list(col, units, convention, group) for the headings spec
  )

  # Single source of truth for axial mode, resolved from the Directionality control.
  is_axial <- reactive(identical(input$data_model, "axial"))

  # Soft default: choosing an inherently axial heading method pre-selects the
  # Axial data model. Overridable -- the user may switch back afterwards, and a
  # directional method does not force it back.
  observeEvent(input$method, {
    if (!is.null(input$method) && input$method %in% AXIAL_METHODS)
      updateRadioButtons(session, "data_model", selected = "axial")
  }, ignoreInit = TRUE)

  # ---- step pills ------------------------------------------------------------
  output$step_pills <- renderUI({
    labs <- c("1. Upload", "2. Configure", "3. Results")
    tags$span(lapply(seq_along(labs), function(i) {
      cls <- if (i < rv$step) "pill pill-done"
             else if (i == rv$step) "pill pill-here"
             else "pill pill-later"
      tags$span(labs[i], class = cls)
    }))
  })

  # ---- forward button --------------------------------------------------------
  output$fwd_btn_ui <- renderUI({
    if (rv$step == 1L)
      actionButton("go2", "Next", class = "btn-primary")
    else if (rv$step == 2L)
      actionButton("go3", "Analyse", class = "btn-success btn-lg")
    else
      actionButton("restart", "Start over",
                   class = "btn-outline-secondary")
  })

  # ---- navigation ------------------------------------------------------------
  observeEvent(input$back_btn, {
    if (rv$step > 1L) {
      rv$step  <- rv$step - 1L
      rv$error <- NULL
    }
  })

  observeEvent(input$input_type, {
    # Also fires once on initial load (harmless: fields are already NULL).
    rv$mode   <- input$input_type
    rv$ts     <- NULL
    rv$hd     <- NULL
    rv$raw_hd <- NULL
    rv$hd_map <- NULL
    rv$error  <- NULL
  })

  observeEvent(input$restart, {
    rv$step     <- 1L
    rv$path     <- NULL
    rv$dialect  <- NULL
    rv$ts       <- NULL
    rv$cond_col <- NULL
    rv$hd       <- NULL
    rv$error    <- NULL
    rv$mode     <- "trajectories"
    rv$raw_hd   <- NULL
    rv$hd_map   <- NULL
    updateRadioButtons(session, "input_type", selected = "trajectories")
  })

  # Step 1 → 2: load Tracks and detect condition column
  observeEvent(input$go2, {
    if (identical(rv$mode, "headings")) {
      if (is.null(rv$raw_hd)) {
        # Example headings are already loaded (no file to map) -> straight to results.
        if (!is.null(rv$hd)) { rv$step <- 3L; rv$error <- NULL; return() }
        rv$error <- "Please upload a file of headings first."
        return()
      }
      num_cols <- names(rv$raw_hd)[vapply(rv$raw_hd, is.numeric, logical(1))]
      if (!length(num_cols)) {
        rv$error <- "No numeric angle column found; headings input needs a column of angles."
        return()
      }
      rv$step <- 2L; rv$error <- NULL
      return()
    }
    if (is.null(rv$path)) {
      # An example dataset may already be loaded without a file path.
      if (!is.null(rv$ts)) {
        rv$step  <- 2L
        rv$error <- NULL
        return()
      }
      rv$error <- "Please upload a file first."
      return()
    }
    d <- if (!is.null(input$dialect_sel) &&
             input$dialect_sel != "auto") {
      input$dialect_sel
    } else {
      rv$dialect
    }
    ensure_pkgs()   # first point that needs radiatR/ggplot2 for a file upload
    map <- if (identical(d, "generic")) {
      pick <- function(v) if (!is.null(v) && nzchar(v)) v else NULL
      list(x = pick(input$map_x), y = pick(input$map_y),
           time = pick(input$map_time), id = pick(input$map_id))
    } else list()
    ts <- tryCatch(
      suppressMessages(suppressWarnings(
        load_ts(rv$path, d, mapping = map,
                read_opts = delim_read_opts(input$delim_sel)))),
      error = function(e) {
        rv$error <- plain_error(e)
        NULL
      }
    )
    if (!is.null(ts)) {
      rv$ts       <- ts
      rv$dialect  <- d
      rv$cond_col <- detect_cond_col(ts)
      rv$step     <- 2L
      rv$error    <- NULL
    }
  })

  # One-line description of the selected heading method, shown under the dropdown.
  output$method_help <- renderUI({
    m <- if (is.null(input$method)) "distal" else input$method
    txt <- method_help_text[[m]]
    if (is.null(txt)) txt <- ""
    tags$p(class = "text-muted small", txt)
  })

  # Contextual note when the heading method and Directionality are mismatched.
  # Hint (grey) for a directional method under the Axial model (directions are
  # folded to axes -- valid but maybe unintended). Warning (amber) for an axial
  # method under the Directional model (a per-track axis analysed as a direction
  # biases the circular statistics). Nothing for matched combos / "none".
  output$method_model_note <- renderUI({
    m <- input$method
    if (is.null(m) || identical(m, "none")) return(NULL)
    axial_method <- m %in% AXIAL_METHODS
    if (is_axial() && !axial_method) {
      tags$div(class = "alert alert-secondary py-1 px-2 small mb-2",
        "Axial model with a directional method: each track's direction is folded ",
        "to an axis (mod-180°). For a per-track movement axis, pick an Axial method.")
    } else if (!is_axial() && axial_method) {
      tags$div(class = "alert alert-warning py-1 px-2 small mb-2",
        "This method returns a per-track axis (0–180°); analysing it as ",
        "Directional biases the circular statistics — set Directionality to Axial.")
    } else {
      NULL
    }
  })

  output$frame_ui <- renderUI({
    req(rv$ts)
    has_rel <- !is.null(rv$ts@cols$rel_x) && !is.null(rv$ts@cols$rel_y)
    rb <- radioButtons(
      "frame", "Heading frame",
      choices  = c("Relative to landmark" = "relative",
                   "Absolute (control)"   = "absolute"),
      selected = isolate(input$frame) %||% "relative")
    if (has_rel) rb else tagList(
      tags$div(rb, style = "opacity:0.5; pointer-events:none;"),
      tags$p(class = "text-muted small",
             "No landmark frame in this dataset — showing absolute coordinates."))
  })

  # When the circular-boxplot overlay is on but the current headings are not
  # drawable (non-unique median or n < 4) the overlay silently no-ops; surface
  # the reason so the empty result is not mysterious.
  output$boxplot_note <- renderUI({
    if (!isTRUE(input$show_boxplot) || is.null(rv$hd)) return(NULL)
    s <- tryCatch(circ_boxplot_stats(rv$hd, axial = is_axial()),
                  error = function(e) NULL)
    if (is.null(s) || isTRUE(s$drawable)) return(NULL)
    tags$div(class = "alert alert-secondary py-1 px-2 small mb-2",
             paste0("Circular boxplot not drawn: ", s$reason, "."))
  })

  # "By elapsed time" colours by a real time axis, which needs a valid frame
  # rate (the package hard-errors otherwise). When time-mode is selected with an
  # unset/invalid fps the figure falls back to sequence colouring; surface why.
  output$track_time_note <- renderUI({
    if (!identical(input$track_colour, "time") && !identical(input$track_colour, "speed"))
      return(NULL)
    fps <- input$frame_rate
    ok  <- is.numeric(fps) && length(fps) == 1L && is.finite(fps) && fps > 0
    if (ok) return(NULL)
    what <- if (identical(input$track_colour, "speed")) "by speed" else "by elapsed time"
    tags$div(class = "alert alert-secondary py-1 px-2 small mb-2",
             paste0("Enter a positive frame rate to colour ", what,
                    " (showing sequence position instead)."))
  })

  # Live illustrative preview of the selected method on fixed demo tracks.
  output$method_preview <- renderPlot({
    method <- if (is.null(input$method)) "distal" else input$method
    tryCatch(
      build_method_preview(
        demo_tracks(), method,
        num_or(input$circ0, 0.3), num_or(input$circ1, 0.6)
      ),
      error = function(e) {
        message("method_preview render failed: ", conditionMessage(e))
        build_method_preview(demo_tracks(), "none")
      }
    )
  }, res = 110)

  # Step 2 → 3: derive headings, join condition if present
  observeEvent(input$go3, {
    if (identical(rv$mode, "headings")) {
      if (is.null(rv$raw_hd)) {
        # Example headings already built; apply the chosen facet -> show results.
        if (!is.null(rv$hd)) {
          grp <- if (!is.null(input$hd_group) && nzchar(input$hd_group))
            input$hd_group else NULL
          hm <- rv$hd_map; hm$group <- grp; rv$hd_map <- hm
          rv$step <- 3L; rv$error <- NULL; return()
        }
        rv$error <- "Please upload a file of headings first."
        return()
      }
      grp <- if (!is.null(input$hd_group) && nzchar(input$hd_group))
        input$hd_group else NULL
      hd <- tryCatch(
        build_headings_input(rv$raw_hd, col = input$hd_col,
                             units = input$hd_units,
                             convention = input$hd_convention, group = grp),
        error = function(e) { rv$error <- plain_error(e); NULL })
      if (!is.null(hd)) {
        rv$hd     <- hd; rv$method <- NULL; rv$step <- 3L; rv$error <- NULL
        rv$hd_map <- list(col = input$hd_col, units = input$hd_units,
                          convention = input$hd_convention, group = grp)
      }
      return()
    }
    req(rv$ts)
    method <- if (is.null(input$method)) "distal" else input$method
    if (identical(method, "none")) {
      rv$hd     <- NULL
      rv$method <- method
      rv$step   <- 3L
      rv$error  <- NULL
      return()
    }
    c0     <- if (is.null(input$circ0))  0.3 else input$circ0
    c1     <- if (is.null(input$circ1))  0.6 else input$circ1
    hd <- tryCatch(
      derive_hd(rv$ts, method, c0, c1, input$frame %||% "relative"),
      error = function(e) {
        rv$error <- plain_error(e)
        NULL
      }
    )
    if (!is.null(hd)) {
      gc <- if (!is.null(input$cond_col) &&
                nzchar(input$cond_col)) {
        input$cond_col
      } else {
        NULL
      }
      if (!is.null(gc)) {
        id_col   <- rv$ts@cols$id
        df       <- as.data.frame(rv$ts)
        cond_map <- unique(df[, c(id_col, gc), drop = FALSE])
        # derive_headings() names its trial column "id"; join it to the
        # Tracks's own id column (which may be named anything, e.g. trial_id).
        hd       <- merge(hd, cond_map, by.x = "id", by.y = id_col,
                          all.x = TRUE)
      }
      rv$hd     <- hd
      rv$method <- method
      rv$frame  <- input$frame %||% "relative"
      rv$step   <- 3L
      rv$error  <- NULL
    }
  })

  # ---- file upload -----------------------------------------------------------
  observeEvent(input$file, {
    req(input$file)
    rv$path      <- input$file$datapath
    rv$source    <- "file"
    rv$file_name <- input$file$name
    if (identical(rv$mode, "headings")) {
      rv$raw_hd <- tryCatch(
        utils::read.csv(input$file$datapath, stringsAsFactors = FALSE,
                        check.names = TRUE),
        error = function(e) { rv$error <- plain_error(e); NULL })
      rv$ts <- NULL
      return()
    }
    rv$dialect   <- guess_dialect(rv$path)
    rv$ts        <- NULL
    rv$cond_col  <- NULL
    rv$hd        <- NULL
    rv$error     <- NULL
  })

  # ---- example dataset -------------------------------------------------------
  observeEvent(input$load_example, {
    ensure_pkgs()   # materialising the cpunctatus Tracks needs radiatR's S4 class
    ts <- tryCatch(
      example_ts(),
      error = function(e) NULL
    )
    if (is.null(ts)) {
      rv$error <- "Could not load the bundled example dataset."
      return()
    }
    rv$ts        <- ts
    rv$path      <- NULL
    rv$source    <- "example"
    rv$file_name <- NULL
    rv$dialect   <- NULL
    rv$cond_col  <- detect_cond_col(ts)
    rv$hd        <- NULL
    rv$step      <- 2L
    rv$error     <- NULL
  })

  observeEvent(input$load_example_hd, {
    ensure_pkgs()
    ex <- tryCatch(example_ts(), error = function(e) NULL)
    if (is.null(ex)) {
      rv$error <- "Could not load the bundled example dataset."; return()
    }
    # Carry EVERY condition column (e.g. arc, type), not just one, so the facet
    # is selectable in Configure rather than hard-coded.
    cond_cols <- grouping_cols(ex)
    id_col    <- ex@cols$id
    hd <- tryCatch({
      hd0  <- derive_headings(ex, rule = "distal", coords = "relative")
      cond <- unique(as.data.frame(ex)[, c(id_col, cond_cols), drop = FALSE])
      hd0  <- merge(hd0, cond, by.x = "id", by.y = id_col, all.x = TRUE)
      build_headings_input(hd0, col = "heading", units = "radians",
                           convention = "unit_circle")
    }, error = function(e) NULL)
    if (is.null(hd)) { rv$error <- "Could not build the example headings."; return() }
    rv$mode      <- "headings"
    rv$hd        <- hd
    rv$hd_map    <- list(col = "heading", units = "radians",
                         convention = "unit_circle",
                         group = detect_cond_col(ex))   # default facet (e.g. arc)
    rv$ts        <- NULL
    rv$raw_hd    <- NULL
    rv$source    <- "example"
    rv$file_name <- NULL
    rv$method    <- NULL
    rv$step      <- 2L     # land on Configure so the facet can be chosen
    rv$error     <- NULL
  })

  # ---- wizard ----------------------------------------------------------------
  output$wizard <- renderUI({

    err_box <- if (!is.null(rv$error))
      div(class = "alert alert-danger mt-3 mb-0", rv$error)

    # ---- Step 1: upload ----
    if (rv$step == 1L) {
      tagList(
        ui_upload_logo(),
        h5("Upload your tracking data"),
        p(class = "text-muted",
          "Upload a CSV or text file from your tracking software.",
          " EthoVision, DeepLabCut, SLEAP, TRex, ANY-maze,",
          " TrackMate, idtracker.ai, and others are supported."),
        radioButtons(
          "input_type", "What are you uploading?",
          choices = c("Trajectories (track coordinates)" = "trajectories",
                      "Headings (one angle per trial)"    = "headings"),
          # Bind to rv$mode so a wizard re-render (triggered when rv$mode/rv$error
          # change) preserves the user's choice instead of resetting to the
          # hardcoded default -- otherwise selecting "Headings" snaps back.
          selected = rv$mode %||% "trajectories", inline = TRUE
        ),
        fileInput(
          "file", NULL,
          accept      = c(".csv", ".CSV", ".tsv", ".txt", ".xlsx", ".xls", ".mat"),
          buttonLabel = "Browse…",
          placeholder = "No file selected"
        ),
        if (identical(rv$mode, "headings")) {
          div(class = "text-muted small mb-2",
              "Don't have a file? ",
              actionLink("load_example_hd",
                         "Load example headings (from the millipede dataset)"),
              ".")
        } else {
          div(class = "text-muted small mb-2",
              "Don't have a file handy? ",
              actionLink("load_example",
                         "Load the example millipede dataset"),
              " (Cylindroiulus punctatus, 235 trials).")
        },
        uiOutput("format_box"),
        uiOutput("delim_box"),
        uiOutput("mapping_box"),
        uiOutput("preview_section"),
        err_box
      )

    # ---- Step 2: configure (headings) ----
    } else if (rv$step == 2L && identical(rv$mode, "headings")) {
      if (is.null(rv$raw_hd)) {
        choices <- .hd_group_choices(rv$hd)
        cur     <- (rv$hd_map %||% list(group = NULL))$group %||% ""
        tagList(
          h5("Example headings"),
          p(class = "text-muted",
            "These headings come from the bundled millipede dataset — no column ",
            "mapping is needed. Choose how to facet the plot, then click Analyse."),
          radioButtons(
            "data_model", "Directionality",
            choices  = c("Directional" = "directional", "Axial" = "axial"),
            selected = "directional", inline = TRUE
          ),
          selectInput("hd_group", "Facet by (optional)",
                      choices  = c("(none)" = "", stats::setNames(choices, choices)),
                      selected = cur),
          err_box
        )
      } else {
        num_cols <- names(rv$raw_hd)[vapply(rv$raw_hd, is.numeric, logical(1))]
        all_cols <- names(rv$raw_hd)
        tagList(
          h5("Describe your headings"),
          radioButtons(
            "data_model", "Directionality",
            choices  = c("Directional" = "directional", "Axial" = "axial"),
            selected = "directional", inline = TRUE
          ),
          selectInput("hd_col", "Angle column", choices = num_cols,
                      selected = num_cols[1]),
          selectInput("hd_units", "Units",
                      choices = c("Degrees" = "degrees", "Radians" = "radians"),
                      selected = "degrees"),
          selectInput("hd_convention", "Angle convention",
                      choices = c("Unit circle (0° = East, counter-clockwise)" = "unit_circle",
                                  "Compass (0° = North, clockwise)"            = "clock"),
                      selected = "unit_circle"),
          selectInput("hd_group", "Group column (optional)",
                      choices = c("(none)" = "", stats::setNames(all_cols, all_cols)),
                      selected = ""),
          err_box
        )
      }

    # ---- Step 2: configure ----
    } else if (rv$step == 2L) {
      tagList(
        layout_columns(
          col_widths = c(5, 7),
          # ---- left: controls ----
          tagList(
            # Directionality is the more general question (does head==tail?), so it
            # comes before the heading-measurement method.
            radioButtons(
              "data_model", "Directionality",
              choices  = c("Directional" = "directional", "Axial" = "axial"),
              selected = "directional", inline = TRUE
            ),
            h5("How should headings be measured?"),
            selectInput(
              "method", NULL,
              choices = list(
                "None (no headings)" = "none",
                "Directional methods" = c(
                  "Direction at furthest point"       = "distal",
                  "Net displacement direction"        = "net",
                  "Exit direction (ring crossing)"    = "crossing",
                  "Longest straight segment"          = "straight",
                  "Smoothed (windowed) net direction" = "window_net",
                  "Mean direction from centre"        = "origin_mean",
                  "Mean velocity direction"           = "velocity_mean",
                  "Direction at peak speed"           = "maxspeed_window",
                  "Von Mises fit of step directions"  = "vm_fit",
                  "Goal-biased direction"             = "goal_bias"
                ),
                "Axial methods" = c(
                  "Movement axis (velocity, axial)"   = "velocity_axis",
                  "Principal axis (PCA)"              = "pca_axis",
                  "Robust straight-line fit (RANSAC)" = "ransac_straight"
                )
              ),
              selected = "distal"
            ),
            uiOutput("method_help"),
            uiOutput("method_model_note"),
            uiOutput("frame_ui"),
            conditionalPanel(
              "input.method == 'crossing'",
              tags$hr(),
              p(class = "text-muted small",
                "Radii as fractions of the unit circle radius",
                " (0 = centre, 1 = edge)."),
              layout_columns(
                col_widths = c(6, 6),
                sliderInput(
                  "circ0", "Inner ring",
                  min = 0.05, max = 0.9, value = 0.3, step = 0.05
                ),
                sliderInput(
                  "circ1", "Outer ring",
                  min = 0.1, max = 1.0, value = 0.6, step = 0.05
                )
              )
            ),
            uiOutput("cond_ui"),
            err_box
          ),
          # ---- right: live method preview ----
          card(
            card_header("How this method finds the heading"),
            card_body(
              padding = 0,
              plotOutput("method_preview", height = "360px")
            )
          )
        )
      )

    # ---- Step 3: results ----
    } else {
      tagList(
        uiOutput("attrition_banner"),
        navset_card_tab(
          id = "results_tab",
          # ---- Circular plots -----------------------------------------------
          nav_panel(
            "Circular plots",
            layout_sidebar(
              sidebar = sidebar(
                width = 360, position = "right", open = TRUE,
                accordion(
                  open = "Display",
                  accordion_panel(
                    "Display",
                    selectInput(
                      "plot_theme", "Theme",
                      choices = c("Void"          = "void",
                                  "Minimal"       = "minimal",
                                  "Classic"       = "classic",
                                  "Black & white" = "bw",
                                  "Grey"          = "grey",
                                  "Light"         = "light",
                                  "Dark"          = "dark",
                                  "Line draw"     = "linedraw"),
                      selected = "void"
                    ),
                    selectInput(
                      "angle_labels", "Angle labels",
                      choices = c("Degrees (45°)" = "degrees",
                                  "None"          = "none",
                                  "Radians (π/4)" = "radians"),
                      selected = "degrees"
                    ),
                    selectInput(
                      "heading_display", "Heading display",
                      choices = c("Points (overlapping)"  = "points",
                                  "Stacked dots (inward)" = "stacked",
                                  "None"                  = "none"),
                      selected = "points"
                    ),
                    selectInput(
                      "track_colour", "Track colour",
                      choices = c("By trajectory"                = "trajectory",
                                  "By sequence (start → finish)" = "sequence",
                                  "By elapsed time"              = "time",
                                  "By speed"                     = "speed"),
                      selected = "trajectory"
                    ),
                    conditionalPanel(
                      condition = "input.track_colour == 'time' || input.track_colour == 'speed'",
                      numericInput("frame_rate", "Frame rate (fps)", value = 30, min = 0, step = 1),
                      uiOutput("track_time_note")
                    ),
                    uiOutput("colour_by_ui"),
                    tags$hr(class = "my-2"),
                    div(
                      style = paste("display: grid;",
                                    "grid-template-columns: repeat(auto-fit, minmax(8.5rem, 1fr));",
                                    "column-gap: 0.75rem;"),
                      .layer_switch("show_tracks",    "Trajectories",       TRUE),
                      .layer_switch("show_oob",       "Allow points beyond the circumference", FALSE),
                      .layer_switch("show_arrow",     "Directedness arrow", TRUE),
                      .layer_switch("show_ci",        "Mean-direction CI",  FALSE),
                      .layer_switch("show_vectors",   "Heading vectors",    FALSE),
                      .layer_switch("show_rayleigh",  "Rayleigh circle",    FALSE),
                      .layer_switch("show_vtest",     "V-test line",        FALSE),
                      .layer_switch("show_boxplot",   "Circular boxplot",   FALSE),
                      .layer_switch("show_quadrants", "Quadrant lines",     FALSE),
                      .layer_switch("show_rings",     "Guide rings",        FALSE)
                    ),
                    uiOutput("boxplot_note"),
                    tags$hr(class = "my-2"),
                    sliderInput(
                      "preview_px", "Preview size (px)",
                      min = 480, max = 1000, value = 840, step = 20
                    ),
                    tags$span(
                      class = "text-muted small",
                      "On-screen only; export size is set under Image export."
                    )
                  ),
                  ui_image_export(""),
                  ui_code_section(
                    "figure_code", "dl_code",
                    paste0("radiatR code that reproduces this figure. Edit the ",
                           "data path to point at your own file."))
                )
              ),
              card(
                card_header("Tracks and headings"),
                # fillable = FALSE so the plot honours the explicit pixel height
                # from the Preview-size slider instead of being stretched to fill
                # the fillable card (which pinned it to the viewport height and
                # made the slider appear to do nothing).
                card_body(
                  padding = 0,
                  fillable = FALSE,
                  uiOutput("track_plot_ui")
                )
              )
            )
          ),
          # ---- Summary & stats ----------------------------------------------
          nav_panel(
            "Summary & stats",
            layout_sidebar(
              sidebar = sidebar(
                width = 360, position = "right", open = TRUE,
                selectInput("omnibus_test", "Omnibus test",
                            choices  = c("Rao spacing" = "rao",
                                         "Hermans-Rasson" = "hermans_rasson"),
                            selected = "rao"),
                tags$hr(class = "my-2"),
                uiOutput("dl_csv_ui"),             # stats CSV download (moved from Download)
                tags$hr(class = "my-2"),
                tags$strong("R code"),
                tags$p(class = "text-muted small",
                       "radiatR code reproducing this analysis. Edit the data path",
                       " to point at your own file."),
                tags$div(style = "max-height:320px; overflow:auto;",
                         verbatimTextOutput("stats_code")),
                tags$button(
                  class   = "btn btn-sm btn-outline-primary w-100 mb-2",
                  onclick = paste0(
                    "navigator.clipboard.writeText(",
                    "document.getElementById('stats_code').innerText);",
                    "this.innerText='Copied';",
                    "setTimeout(()=>this.innerText='Copy R code',1200);"),
                  "Copy R code"),
                downloadButton("dl_stats_code", "Download .R",
                               class = "btn-sm btn-outline-primary w-100")
              ),
              card(
                card_header("Summary"),
                card_body(
                  tableOutput("summary_tbl"),
                  tags$hr(class = "my-2"),
                  tags$strong("Model selection (AICc)"),
                  tableOutput("model_sel_tbl")
                )
              )
            )
          )
          ,
          # ---- Track metrics ------------------------------------------------
          nav_panel(
            "Track metrics",
            layout_sidebar(
              sidebar = sidebar(
                width = 360, position = "right", open = TRUE,
                accordion(
                  open = TRUE,
                  accordion_panel(
                    "Profile",
                    radioButtons("kin_metric", "Metric",
                                 choices = c("Speed" = "speed",
                                             "Turning rate" = "turning"),
                                 selected = "speed"),
                    conditionalPanel(
                      condition = "input.kin_metric == 'turning'",
                      selectInput("kin_units", "Turning units",
                                  choices = c("Radians" = "radians",
                                              "Degrees" = "degrees"),
                                  selected = "radians")
                    ),
                    uiOutput("kin_track_ui"),
                    uiOutput("kin_colour_ui"),
                    sliderInput("kin_smooth", "Smoothing (window, points)",
                                min = 1, max = 25, value = 1, step = 1),
                    conditionalPanel(
                      condition = "input.kin_smooth > 1",
                      checkboxInput("kin_show_raw", "Show raw (faint)",
                                    value = FALSE)
                    ),
                    numericInput("kin_frame_rate", "Frame rate (fps)",
                                 value = 30, min = 0, step = 1),
                    uiOutput("kin_note")
                  ),
                  ui_image_export("kin_"),
                  ui_code_section(
                    "kinematics_code", "dl_kinematics_code",
                    paste0("radiatR code that reproduces this profile. Edit the ",
                           "data path to point at your own file."))
                )
              ),
              card(
                card_header("Per-track metrics"),
                card_body(
                  uiOutput("track_metric_scope_ui"),
                  uiOutput("track_metrics_summary"),
                  div(style = "max-height:300px; overflow:auto;",
                      tableOutput("track_metrics_table")),
                  downloadButton("dl_track_metrics", "Download (CSV)",
                                 class = "btn-sm btn-outline-primary")
                )
              ),
              card(
                card_header("Kinematics profile"),
                card_body(
                  padding = 0,
                  fillable = FALSE,
                  plotOutput("kinematics_plot", height = "460px")
                )
              )
            )
          )
        )
      )
    }
  })

  # ---- step 1 sub-outputs ----------------------------------------------------
  output$format_box <- renderUI({
    req(rv$path)
    d   <- rv$dialect
    lbl <- names(DIALECT_CHOICES)[DIALECT_CHOICES == d]
    lbl <- if (length(lbl)) lbl[1L] else NULL

    tagList(
      if (!is.null(lbl) && d != "generic")
        div(
          class = "alert alert-info py-1 px-2 small mb-2",
          "Format detected: ", tags$b(lbl)
        )
      else
        div(
          class = "alert alert-secondary py-1 px-2 small mb-2",
          "Format not recognised — please select your software:"
        ),
      selectInput(
        "dialect_sel", "Tracking software",
        choices  = DIALECT_CHOICES,
        selected = if (!is.null(d)) d else "auto"
      )
    )
  })

  # Delimiter override for delimited (non-Excel) uploads. Own renderUI so a
  # wizard re-render does not reset the choice.
  output$delim_box <- renderUI({
    req(rv$path)
    ext <- tolower(sub(".*\\.", "", rv$path))
    if (ext %in% c("xlsx", "xls")) return(NULL)
    selectInput("delim_sel", "Delimiter",
                choices = c("Auto-detect" = "auto", "Comma" = ",",
                            "Semicolon" = ";", "Tab" = "\t"),
                selected = "auto")
  })

  # Column mapping for a Generic CSV upload. Kept in its OWN renderUI (not in
  # format_box) so that reading input$dialect_sel here does not re-render and
  # reset the dialect selector.
  output$mapping_box <- renderUI({
    req(rv$path)
    d <- if (!is.null(input$dialect_sel) && input$dialect_sel != "auto")
      input$dialect_sel else rv$dialect
    if (!identical(d, "generic")) return(NULL)
    df <- tryCatch(
      radiatR:::.read_any(rv$path, delim = delim_read_opts(input$delim_sel)$delim),
      error = function(e) NULL)
    if (is.null(df) || !ncol(df)) return(NULL)
    df   <- utils::head(as.data.frame(df), 200L)
    cols <- names(df)
    g    <- tryCatch(radiatR:::guess_columns(df), error = function(e) list())
    sel  <- function(v) if (is.null(v)) "" else v
    xy   <- stats::setNames(cols, cols)
    tagList(
      tags$hr(),
      div(class = "small text-muted mb-1", "Map columns:"),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("map_x", "X", choices = xy, selected = sel(g$x)),
        selectInput("map_y", "Y", choices = xy, selected = sel(g$y))),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("map_time", "Time",
                    choices = c("(row order)" = "", xy), selected = sel(g$time)),
        selectInput("map_id", "ID",
                    choices = c("(single track)" = "", xy), selected = sel(g$id)))
    )
  })

  output$preview_section <- renderUI({
    req(rv$path)
    tagList(tags$hr(), tableOutput("preview_tbl"))
  })

  output$preview_tbl <- renderTable({
    req(rv$path)
    df <- tryCatch(
      radiatR:::.read_any(rv$path, delim = delim_read_opts(input$delim_sel)$delim),
      error = function(e) NULL)
    if (is.null(df))
      return(data.frame(Note = "Could not preview this file"))
    utils::head(as.data.frame(df), 5L)
  }, striped = TRUE, caption = "First 5 rows",
     caption.placement = "top")

  # ---- step 2 condition selector ---------------------------------------------
  output$cond_ui <- renderUI({
    req(rv$ts)
    col <- rv$cond_col
    if (is.null(col)) return(NULL)

    other <- grouping_cols(rv$ts)
    if (!length(other)) return(NULL)
    choices <- c("None" = "", stats::setNames(other, other))

    tagList(
      tags$hr(),
      selectInput(
        "cond_col",
        "Facet by (optional)",
        choices  = choices,
        selected = col
      )
    )
  })

  # Colour-by selector (Display panel). Defaults to per-trajectory colour; any
  # grouping column can instead drive the track + marker colour. Independent of
  # the Facet-by selector.
  output$colour_by_ui <- renderUI({
    if (identical(rv$mode, "headings")) {
      grp <- (rv$hd_map %||% list(group = NULL))$group
      choices <- if (is.null(grp)) c("Single colour" = SPEC_TRAJ_KEY)
                 else stats::setNames(c(SPEC_TRAJ_KEY, grp), c("Single colour", grp))
      sel <- if (!is.null(input$colour_by) && input$colour_by %in% choices)
        input$colour_by else (grp %||% SPEC_TRAJ_KEY)
      return(selectInput("colour_by", "Colour by", choices = choices, selected = sel))
    }
    req(rv$ts)
    cats    <- colour_cols(rv$ts)
    choices <- c("Trajectory" = TRAJ_COLOUR_KEY, stats::setNames(cats, cats))
    sel <- if (!is.null(input$colour_by) && input$colour_by %in% choices)
      input$colour_by else TRAJ_COLOUR_KEY
    selectInput("colour_by", "Colour by", choices = choices, selected = sel)
  })

  # ---- step 3 outputs --------------------------------------------------------

  # Treat an as-yet-unrendered toggle (NULL) as its declared default.
  tog <- function(v, default) if (is.null(v)) default else isTRUE(v)

  # Single app-wide capture frame rate. Both the Circular time/speed colouring
  # and the Kinematics tab read this one value; the per-sidebar numeric inputs
  # (frame_rate, kin_frame_rate) mirror it. Equality guards prevent feedback loops.
  fps_rv <- reactiveVal(30)
  # When a loaded Tracks carries its own capture rate, adopt it as the app fps.
  observeEvent(rv$ts, {
    fr <- if (is.null(rv$ts)) NULL else frame_rate(rv$ts)
    if (!is.null(fr) && is.finite(fr) && fr > 0 &&
        !isTRUE(all.equal(fr, fps_rv()))) fps_rv(fr)
  }, ignoreNULL = FALSE)
  observeEvent(input$frame_rate, {
    if (!isTRUE(all.equal(input$frame_rate, fps_rv()))) fps_rv(input$frame_rate)
  }, ignoreInit = TRUE)
  observeEvent(fps_rv(), {
    v <- fps_rv()
    if (!isTRUE(all.equal(input$frame_rate, v)))
      updateNumericInput(session, "frame_rate", value = v)
    if (!isTRUE(all.equal(input$kin_frame_rate, v)))   # no-op until Task 3 adds the input
      updateNumericInput(session, "kin_frame_rate", value = v)
  })

  # The plot spec resolved from the current inputs (shared by the figure and the
  # code export so they cannot drift).
  current_spec <- function() {
    if (identical(rv$mode, "headings")) {
      # Read the mapping from rv$hd_map (set by Step 2 OR the example button) so
      # the spec is correct even when Step 2 was skipped (example path).
      map <- rv$hd_map %||% list(col = "heading", units = "radians",
                                 convention = "unit_circle", group = NULL)
      grp <- map$group
      return(build_plot_spec(
        ts = NULL, hd = rv$hd, method = NULL,
        data = list(
          source = if (identical(rv$source, "example")) "example" else "file",
          mode = "headings", path = rv$file_name %||% "your_headings.csv",
          col = map$col, units = map$units,
          convention = map$convention, group = grp),
        inputs = list(
          cond_col = grp, colour_by = input$colour_by,
          omnibus_test = input$omnibus_test,
          plot_theme = input$plot_theme, angle_labels = input$angle_labels,
          heading_display = input$heading_display,
          track_colour = input$track_colour %||% "trajectory",
          frame_rate = fps_rv(),
          show_arrow = tog(input$show_arrow, TRUE),
          show_vectors = tog(input$show_vectors, FALSE),
          show_rayleigh = tog(input$show_rayleigh, FALSE),
          show_ci = tog(input$show_ci, FALSE),
          axial = is_axial(),
          show_vtest = tog(input$show_vtest, FALSE),
          show_boxplot = tog(input$show_boxplot, FALSE),
          show_quadrants = tog(input$show_quadrants, FALSE),
          show_rings = tog(input$show_rings, FALSE))))
    }
    gc <- if (!is.null(input$cond_col) && nzchar(input$cond_col))
      input$cond_col else NULL
    build_plot_spec(
      ts = rv$ts, hd = rv$hd, method = rv$method,
      data = list(
        source  = if (identical(rv$source, "example")) "example" else "file",
        mode    = "trajectories",
        path    = rv$file_name %||% "your_tracks.csv",
        dialect = if (is.null(rv$dialect) || rv$dialect %in% c("auto", "generic"))
          NULL else rv$dialect),
      inputs = list(
        cond_col = input$cond_col, colour_by = input$colour_by,
        omnibus_test = input$omnibus_test,
        circ0 = input$circ0, circ1 = input$circ1,
        plot_theme = input$plot_theme, angle_labels = input$angle_labels,
        heading_display = input$heading_display,
        track_colour = input$track_colour %||% "trajectory",
        frame = rv$frame %||% "relative",
        frame_rate = fps_rv(),
        # Path-metrics caption, none mode only (no headings). spec_to_plot and
        # spec_to_code render and reproduce it.
        caption  = if (is.null(rv$hd)) straightness_caption(rv$ts, gc) else NULL,
        show_tracks = tog(input$show_tracks, TRUE),
        show_oob    = tog(input$show_oob,    FALSE),
        show_arrow  = tog(input$show_arrow,  TRUE),
        show_vectors = tog(input$show_vectors, FALSE),
        show_rayleigh = tog(input$show_rayleigh, FALSE),
        show_ci    = tog(input$show_ci,    FALSE),
        axial      = is_axial(),
        show_vtest = tog(input$show_vtest, FALSE),
        show_boxplot = tog(input$show_boxplot, FALSE),
        show_quadrants = tog(input$show_quadrants, FALSE),
        show_rings     = tog(input$show_rings,     FALSE)))
  }

  # The Results figure -- tracks, every statistical overlay (CI, Rayleigh,
  # V-test), and the none-mode caption -- now comes entirely from the shared
  # spec, so the code export reproduces exactly what is on screen.
  build_results_plot <- function() spec_to_plot(current_spec(), rv$ts, rv$hd)

  # Preview canvas height tracks the chosen export aspect ratio so the on-screen
  # plot reflects the width/height the user will download. The Preview size
  # slider scales the canvas without affecting the exported file. Width fills
  # the card.
  output$track_plot_ui <- renderUI({
    w    <- num_or(input$plot_w, 7)
    h    <- num_or(input$plot_h, 7)
    base <- num_or(input$preview_px, 840)
    px   <- max(160, min(1000, round(base * (h / w))))
    plotOutput("track_plot", height = paste0(px, "px"))
  })

  output$track_plot <- renderPlot({
    req(rv$ts %||% rv$hd)
    p <- tryCatch(
      build_results_plot(),
      error = function(e) {
        msg <- conditionMessage(e)
        message("track_plot render failed: ", msg)
        ggplot() +
          annotate(
            "text", x = 0, y = 0,
            label = paste0("Plot error:\n", msg),
            colour = "grey40", size = 3.5
          ) +
          theme_void()
      }
    )
    print(p)
  }, res = 120)

  # Attrition banner: shown only when some trials produced no heading. Derived
  # headings get a loud bias caveat; provided headings get a neutral note.
  output$attrition_banner <- renderUI({
    if (is.null(rv$hd)) return(NULL)
    derived <- !identical(rv$mode, "headings") &&
               !is.null(rv$method) && !identical(rv$method, "none")
    if (derived) {
      n_total   <- attr(rv$hd, "n_total")   %||% nrow(rv$hd)
      n_missing <- attr(rv$hd, "n_missing") %||% sum(is.na(rv$hd$heading))
      rule      <- rv$method
    } else {
      n_total   <- nrow(rv$hd)
      n_missing <- sum(is.na(rv$hd$heading))
      rule      <- NULL
    }
    note <- attrition_note(n_total, n_missing, derived = derived, rule = rule)
    if (is.null(note)) return(NULL)
    div(class = if (derived) "alert alert-warning mt-0 mb-3"
                else "alert alert-info mt-0 mb-3",
        note)
  })

  # Single source of truth for the summary's headings frame + group column, used
  # by output$summary_tbl, model_sel(), and output$model_sel_tbl so they cannot
  # drift. has_hd = FALSE means no headings (none-mode / not yet loaded).
  summary_ctx <- reactive({
    if (identical(rv$mode, "headings")) {
      if (is.null(rv$hd)) return(list(has_hd = FALSE, mode = "none"))
      grp    <- (rv$hd_map %||% list(group = NULL))$group
      hd     <- rv$hd
      by_col <- grp
      pooled <- is.null(by_col)
      if (pooled) { hd[[".all"]] <- "All"; by_col <- ".all" }
      return(list(has_hd = TRUE, mode = "headings", hd = hd, by_col = by_col,
                  pooled = pooled, gc = NULL))
    }
    if (is.null(rv$ts)) return(list(has_hd = FALSE, mode = "none"))
    gc <- if (!is.null(input$cond_col) && nzchar(input$cond_col)) input$cond_col else NULL
    if (is.null(rv$hd)) return(list(has_hd = FALSE, mode = "none_traj", gc = gc))
    by_col <- if (!is.null(gc)) gc else "id"
    list(has_hd = TRUE, mode = "traj", hd = rv$hd, by_col = by_col,
         pooled = FALSE, gc = gc)
  })

  # Candidate-model ranking (detection): all three models, independent of the
  # Directionality control. NULL when there are no headings.
  model_sel <- reactive({
    ctx <- summary_ctx()
    if (!isTRUE(ctx$has_hd)) return(NULL)
    tryCatch(circ_model_select(ctx$hd, group_col = ctx$by_col),
             error = function(e) NULL)
  })

  output$summary_tbl <- renderTable({
    ctx <- summary_ctx()

    # No headings frame: trajectory Straightness-only, or nothing yet.
    if (!isTRUE(ctx$has_hd)) {
      if (identical(ctx$mode, "none_traj")) {
        req(rv$ts)
        st  <- straightness_index(rv$ts); idc <- rv$ts@cols$id; gc <- ctx$gc
        if (!is.null(gc)) {
          cond_map <- unique(as.data.frame(rv$ts)[, c(idc, gc), drop = FALSE])
          st  <- merge(st, cond_map, by = idc)
          agg <- tapply(st$straightness, as.character(st[[gc]]),
                        function(v) mean(v, na.rm = TRUE))
          return(data.frame(Group = names(agg),
                            Straightness = round(as.numeric(agg), 3),
                            stringsAsFactors = FALSE))
        }
        return(data.frame(Group = as.character(st[[idc]]),
                          Straightness = round(st$straightness, 3),
                          stringsAsFactors = FALSE))
      }
      req(rv$hd %||% rv$ts)
      return(data.frame(Note = "Summary not available"))
    }

    hd <- ctx$hd; by_col <- ctx$by_col
    tryCatch({
      cm <- circ_summary_table(hd, by_col, axial = is_axial(),
                               omnibus = input$omnibus_test %||% "rao",
                               model_sel = model_sel())
      if (identical(ctx$mode, "traj")) {
        st  <- straightness_index(rv$ts); idc <- rv$ts@cols$id; gc <- ctx$gc
        if (!is.null(gc)) {
          cond_map <- unique(as.data.frame(rv$ts)[, c(idc, gc), drop = FALSE])
          st  <- merge(st, cond_map, by = idc)
          agg <- tapply(st$straightness, as.character(st[[gc]]),
                        function(v) mean(v, na.rm = TRUE))
        } else {
          agg <- stats::setNames(st$straightness, as.character(st[[idc]]))
        }
        cm[["Straightness"]] <- round(as.numeric(agg[as.character(cm[["Group"]])]), 3)
      }
      if (isTRUE(ctx$pooled)) cm[["Group"]] <- NULL
      cm
    }, error = function(e) data.frame(Note = "Summary not available"))
  }, striped = TRUE, hover = TRUE, align = "c")

  output$model_sel_tbl <- renderTable({
    ms <- model_sel()
    if (is.null(ms) || nrow(ms) == 0L)
      return(data.frame(Note = "Model selection needs headings"))
    ctx <- summary_ctx()
    ms$AICc   <- round(ms$AICc, 1)
    ms$dAICc  <- round(ms$dAICc, 1)
    ms$weight <- round(ms$weight, 3)
    keep <- c(ctx$by_col, "model", "k", "AICc", "dAICc", "weight")
    ms <- ms[, intersect(keep, names(ms)), drop = FALSE]
    names(ms)[names(ms) == ctx$by_col] <- "Group"
    if (isTRUE(ctx$pooled)) ms[["Group"]] <- NULL
    ms
  }, striped = TRUE, hover = TRUE, align = "c")

  # The summary tables live in the "Summary & stats" sub-tab, which is not the
  # default Results tab. Without this, Shiny suspends their outputs (returning
  # NULL) until the user first clicks onto that tab. Keep them live so the
  # summary/model tables — and the CSV download that reads them — are populated
  # as soon as Results is reached, regardless of which sub-tab is in front.
  outputOptions(output, "summary_tbl",   suspendWhenHidden = FALSE)
  outputOptions(output, "model_sel_tbl", suspendWhenHidden = FALSE)

  # The radiatR script reproducing the current figure (shown, copied, downloaded).
  figure_code_text <- reactive({
    req(rv$ts %||% rv$hd)
    spec_to_code(current_spec())
  })
  output$figure_code <- renderText(figure_code_text())
  output$dl_code <- downloadHandler(
    filename = function() paste0("radiatR_figure_", Sys.Date(), ".R"),
    content  = function(file) writeLines(figure_code_text(), file)
  )

  # The radiatR script reproducing the Summary & stats analysis.
  stats_code_text <- reactive({
    req(rv$hd %||% rv$ts)
    spec_to_stats_code(current_spec())
  })
  output$stats_code <- renderText(stats_code_text())
  output$dl_stats_code <- downloadHandler(
    filename = function() paste0("radiatR_stats_", Sys.Date(), ".R"),
    content  = function(file) writeLines(stats_code_text(), file)
  )

  output$dl_plot <- downloadHandler(
    filename = function() {
      fmt <- if (is.null(input$plot_fmt)) "pdf" else input$plot_fmt
      paste0("radiatR_plot_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(rv$ts %||% rv$hd)
      fmt <- if (is.null(input$plot_fmt)) "pdf" else input$plot_fmt
      # Transparent background applies to every format except JPG (no alpha).
      transparent <- isTRUE(input$plot_transparent) && fmt != "jpg"
      p <- build_results_plot()
      if (transparent) p <- p + .transparent_theme()
      ggsave(
        file, p,
        device = .plot_device(fmt),
        width  = num_or(input$plot_w, 7),
        height = num_or(input$plot_h, 7),
        units  = "in",
        dpi    = num_or(input$plot_dpi, 300),
        bg     = if (transparent) "transparent" else NULL
      )
    }
  )

  # Data-download button: relabel for None mode and use a clearer (non-ghosted)
  # outline-primary style matching the plot-download button.
  output$dl_csv_ui <- renderUI({
    lab <- if (is.null(rv$hd)) "Metrics (CSV)" else "Headings (CSV)"
    downloadButton("dl_csv", lab, class = "btn-sm btn-outline-primary w-100")
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      stem <- if (is.null(rv$hd)) "radiatR_metrics_" else "radiatR_headings_"
      paste0(stem, Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$ts %||% rv$hd)
      dat <- if (is.null(rv$hd)) path_metrics_table(rv$ts) else rv$hd
      utils::write.csv(dat, file, row.names = FALSE)
    }
  )

  # ---- Kinematics sub-tab ----------------------------------------------------

  # Ghost (hide) the Track metrics tab when only headings are loaded, since
  # track metrics require trajectory data. Show it once track data is present.
  observe({
    req(rv$step == 3L)
    if (is.null(rv$ts)) {
      nav_hide("results_tab", "Track metrics", session = session)
    } else {
      nav_show("results_tab", "Track metrics", select = FALSE, session = session)
    }
  })

  # Sync the Kinematics frame-rate widget into the single shared fps value.
  observeEvent(input$kin_frame_rate, {
    if (!isTRUE(all.equal(input$kin_frame_rate, fps_rv())))
      fps_rv(input$kin_frame_rate)
  }, ignoreInit = TRUE)

  output$kin_track_ui <- renderUI({
    if (is.null(rv$ts)) return(NULL)
    tid <- as.character(ids(rv$ts))
    selectInput("kin_track", "Track",
                choices = c(stats::setNames(tid, tid),
                            "All tracks (overlay)" = ""),
                selected = tid[1])
  })

  # Colour-by selector: categorical columns of the loaded Tracks ("None" default).
  output$kin_colour_ui <- renderUI({
    cols <- if (is.null(rv$ts)) character(0) else colour_cols(rv$ts)
    selectInput("kin_colour_by", "Colour lines by",
                choices = c("None" = "", stats::setNames(cols, cols)),
                selected = "")
  })

  kinematics_spec <- reactive({
    req(rv$ts)
    build_kinematics_spec(rv$ts, list(
      kin_metric    = input$kin_metric,
      kin_units     = input$kin_units,
      kin_colour_by = input$kin_colour_by,
      kin_track     = input$kin_track,
      kin_smooth    = input$kin_smooth,
      kin_show_raw  = input$kin_show_raw,
      fps           = fps_rv(),
      data = list(
        source  = if (identical(rv$source, "example")) "example" else "file",
        mode    = "trajectories",
        path    = rv$file_name %||% "your_tracks.csv",
        dialect = if (is.null(rv$dialect) || rv$dialect %in% c("auto", "generic"))
          NULL else rv$dialect)))
  })

  # fps is required only for numeric-frame Tracks; POSIXct time needs none.
  kin_fps_ok <- reactive({
    if (is.null(rv$ts)) return(FALSE)
    if (inherits(as.data.frame(rv$ts)[[rv$ts@cols$time]], "POSIXct")) return(TRUE)
    f <- fps_rv(); is.numeric(f) && length(f) == 1L && is.finite(f) && f > 0
  })

  output$kin_note <- renderUI({
    if (is.null(rv$ts))
      return(div(class = "alert alert-secondary py-1 px-2 small mb-2",
                 "Kinematics needs trajectory data (load tracks, not headings)."))
    if (!isTRUE(kin_fps_ok()))
      return(div(class = "alert alert-warning py-1 px-2 small mb-2",
                 "Set a positive frame rate to convert frames to seconds."))
    NULL
  })

  output$kinematics_plot <- renderPlot({
    req(rv$ts)
    validate(need(isTRUE(kin_fps_ok()),
                  "Set a positive frame rate to plot kinematics."))
    kinematics_spec_to_plot(kinematics_spec(), rv$ts)
  })

  kinematics_code_text <- reactive({
    req(rv$ts)
    spec_to_kinematics_code(kinematics_spec())
  })
  output$kinematics_code <- renderText(kinematics_code_text())
  output$dl_kinematics_code <- downloadHandler(
    filename = function() paste0("radiatR_kinematics_", Sys.Date(), ".R"),
    content  = function(file) writeLines(kinematics_code_text(), file)
  )

  # Image export for the kinematics profile, mirroring dl_plot for the figure.
  output$dl_kin_plot <- downloadHandler(
    filename = function() {
      fmt <- if (is.null(input$kin_plot_fmt)) "pdf" else input$kin_plot_fmt
      paste0("radiatR_kinematics_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(rv$ts, isTRUE(kin_fps_ok()))
      fmt <- if (is.null(input$kin_plot_fmt)) "pdf" else input$kin_plot_fmt
      transparent <- isTRUE(input$kin_plot_transparent) && fmt != "jpg"
      p <- kinematics_spec_to_plot(kinematics_spec(), rv$ts)
      if (transparent) p <- p + .transparent_theme()
      ggsave(
        file, p,
        device = .plot_device(fmt),
        width  = num_or(input$kin_plot_w, 7),
        height = num_or(input$kin_plot_h, 7),
        units  = "in",
        dpi    = num_or(input$kin_plot_dpi, 300),
        bg     = if (transparent) "transparent" else NULL
      )
    }
  )

  # ---- Per-track metrics (length / straightness / tortuosity / sinuosity) -----
  # Geometric metrics for every loaded track (no frame rate needed). When a
  # heading method was used, some tracks may not have produced a heading, so the
  # scope control lets the user view all tracks or only the heading-producing
  # subset. The same metrics are downloadable.

  # ids of the tracks that produced a heading (NULL in no-headings mode).
  heading_ids <- reactive(if (is.null(rv$hd)) NULL else unique(as.character(rv$hd$id)))

  output$track_metric_scope_ui <- renderUI({
    req(rv$ts, !is.null(rv$hd))            # only relevant when headings exist
    radioButtons("track_metric_scope", NULL, inline = TRUE,
                 choices  = c("All loaded tracks"      = "all",
                              "Tracks with a heading"  = "heading"),
                 selected = "all")
  })

  track_metrics_tbl <- reactive({
    req(rv$ts)
    m   <- path_metrics_table(rv$ts)
    idc <- rv$ts@cols$id
    hid <- heading_ids()
    if (identical(input$track_metric_scope, "heading") && !is.null(hid))
      m <- m[as.character(m[[idc]]) %in% hid, , drop = FALSE]
    m
  })

  output$track_metrics_summary <- renderUI({
    req(rv$ts)
    m   <- track_metrics_tbl()
    hid <- heading_ids()
    fmean <- function(v) mean(v[is.finite(v)], na.rm = TRUE)
    counts <- if (is.null(hid))
      sprintf("%d tracks", nrow(path_metrics_table(rv$ts)))
    else
      sprintf("%d tracks loaded · %d produced a heading",
              nrow(path_metrics_table(rv$ts)), length(hid))
    div(class = "text-muted small mb-2",
        counts, br(),
        sprintf("Means (shown) — straightness %.2f · tortuosity %.2f · sinuosity %.2f",
                fmean(m$straightness), fmean(m$tortuosity), fmean(m$sinuosity)))
  })

  output$track_metrics_table <- renderTable(track_metrics_tbl(), digits = 3)

  output$dl_track_metrics <- downloadHandler(
    filename = function() paste0("radiatR_track_metrics_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$ts)
      utils::write.csv(track_metrics_tbl(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
