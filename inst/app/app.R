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

# Identify a candidate condition column in a loaded TrajSet.
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

load_ts <- function(path, dialect) {
  if (is.null(dialect) || dialect %in% c("auto", "generic"))
    return(TrajSet_read(path))
  TrajSet_read(path, dialect = dialect)
}

# The bundled Cylindroiulus punctatus millipede example, as a TrajSet, so
# users can try the app without supplying their own tracking file.
example_ts <- function() {
  e <- new.env()
  utils::data("cpunctatus", package = "radiatR", envir = e)
  e$cpunctatus
}

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
  maxspeed_window = "Direction over the window of the subject's fastest movement.",
  vm_fit          = "Von Mises fit to the step directions (peak of the fitted distribution).",
  pca_axis        = "Principal (long) axis of the visited positions.",
  ransac_straight = "Robust straight-line fit that ignores outlying points (RANSAC).",
  goal_bias       = "Net step direction weighted toward outward (away-from-centre) movement."
)

# One-line path-metrics caption for the no-headings ("none") mode. Reads the
# same straightness_index() table the summary uses so numbers match. Extend by
# appending more "name: value" clauses as metrics are added.
straightness_caption <- function(ts, gc = NULL) {
  st  <- straightness_index(ts)
  idc <- ts@cols$id
  if (is.null(gc)) {
    m <- mean(st$straightness, na.rm = TRUE)
    if (!is.finite(m)) return("")
    sprintf("Mean straightness: %.2f", m)
  } else {
    cond_map <- unique(as.data.frame(ts)[, c(idc, gc), drop = FALSE])
    st  <- merge(st, cond_map, by = idc)
    agg <- tapply(st$straightness, as.character(st[[gc]]),
                  function(v) mean(v, na.rm = TRUE))
    parts <- sprintf("%s: %.2f", names(agg), as.numeric(agg))
    paste0("Straightness - ", paste(parts, collapse = ", "))
  }
}

derive_hd <- function(ts, method, circ0, circ1) {
  # Use relative coords when available so headings are in the same frame as
  # the rel_x/rel_y display (stimulus fixed at East). Fall back to absolute
  # for datasets without a normalised relative coordinate system.
  has_rel <- !is.null(ts@cols$rel_x) && !is.null(ts@cols$rel_y)
  args <- list(x = ts, coords = if (has_rel) "relative" else "absolute")
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

rayleigh_p_fmt <- function(angles) {
  tryCatch({
    a <- circular::circular(
      angles[is.finite(angles)],
      units = "radians", type = "angles"
    )
    p <- circular::rayleigh.test(a)$p.value
    if (is.na(p))   return("—")
    if (p < 0.001)  return("< 0.001")
    sprintf("%.3f", p)
  }, error = function(e) "—")
}

# Display-ready circular summary (n, mean direction, resultant R, Rayleigh p) for
# a headings frame, grouped by `by_col`. Returns a data frame with the group
# column renamed to "Group"; callers may append trajectory-only columns
# (e.g. Straightness) keyed on the returned "Group" values.
circ_summary_table <- function(hd, by_col) {
  cm <- circ_summarise(
    hd, "heading", units = "radians", .by = by_col,
    stats = c("n", "n_missing", "mean_dir_deg", "resultant_R"),
    display = circ_display(zero = 0)
  )
  groups <- unique(hd[[by_col]])
  p_vals <- vapply(groups, function(g)
    rayleigh_p_fmt(hd$heading[hd[[by_col]] == g]), character(1L))
  p_df <- stats::setNames(
    data.frame(groups, p_vals, stringsAsFactors = FALSE),
    c(by_col, "Rayleigh p"))
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

  tags$head(tags$style(HTML("
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
      tags$b("radiatR — Circular Track Analysis"),
      uiOutput("step_pills")
    ),
    card_body(class = "main-body", uiOutput("wizard")),
    card_footer(
      class = "d-flex justify-content-between align-items-center",
      actionButton(
        "back_btn", "Back",
        class = "btn-sm btn-outline-secondary"
      ),
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

  # Step 1 → 2: load TrajSet and detect condition column
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
    ts <- tryCatch(
      suppressMessages(suppressWarnings(load_ts(rv$path, d))),
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
        # Example headings already built; nothing to map -> show results.
        if (!is.null(rv$hd)) { rv$step <- 3L; rv$error <- NULL; return() }
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
      derive_hd(rv$ts, method, c0, c1),
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
        # TrajSet's own id column (which may be named anything, e.g. trial_id).
        hd       <- merge(hd, cond_map, by.x = "id", by.y = id_col,
                          all.x = TRUE)
      }
      rv$hd     <- hd
      rv$method <- method
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
    ensure_pkgs()   # materialising the cpunctatus TrajSet needs radiatR's S4 class
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
    hd <- tryCatch({
      ex   <- example_ts()
      hd0  <- derive_headings(ex, rule = "distal", coords = "relative")
      cond <- unique(as.data.frame(ex)[, c("trial_id", "type")])
      hd0  <- merge(hd0, cond, by.x = "id", by.y = "trial_id", all.x = TRUE)
      build_headings_input(hd0, col = "heading", units = "radians",
                           convention = "unit_circle", group = "type")
    }, error = function(e) NULL)
    if (is.null(hd)) { rv$error <- "Could not build the example headings."; return() }
    rv$mode      <- "headings"
    rv$hd        <- hd
    rv$hd_map    <- list(col = "heading", units = "radians",
                         convention = "unit_circle", group = "type")
    rv$ts        <- NULL
    rv$raw_hd    <- NULL
    rv$source    <- "example"
    rv$file_name <- NULL
    rv$method    <- NULL
    rv$step      <- 3L
    rv$error     <- NULL
  })

  # ---- wizard ----------------------------------------------------------------
  output$wizard <- renderUI({

    err_box <- if (!is.null(rv$error))
      div(class = "alert alert-danger mt-3 mb-0", rv$error)

    # ---- Step 1: upload ----
    if (rv$step == 1L) {
      tagList(
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
          accept      = c(".csv", ".txt", ".tsv", ".mat"),
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
        uiOutput("preview_section"),
        err_box
      )

    # ---- Step 2: configure (headings) ----
    } else if (rv$step == 2L && identical(rv$mode, "headings")) {
      if (is.null(rv$raw_hd)) {
        tagList(
          h5("Example headings"),
          p(class = "text-muted",
            "These headings come from the bundled example dataset — no column ",
            "mapping is needed. Click Analyse to view the results, or go back ",
            "and upload your own file of angles."),
          err_box
        )
      } else {
        num_cols <- names(rv$raw_hd)[vapply(rv$raw_hd, is.numeric, logical(1))]
        all_cols <- names(rv$raw_hd)
        tagList(
          h5("Describe your headings"),
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
            h5("How should headings be measured?"),
            selectInput(
              "method", NULL,
              choices = c(
                "None (no headings)"                = "none",
                "Direction at furthest point"       = "distal",
                "Net displacement direction"        = "net",
                "Exit direction (ring crossing)"    = "crossing",
                "Longest straight segment"          = "straight",
                "Smoothed (windowed) net direction" = "window_net",
                "Mean direction from centre"        = "origin_mean",
                "Mean velocity direction"           = "velocity_mean",
                "Direction at peak speed"           = "maxspeed_window",
                "Von Mises fit of step directions"  = "vm_fit",
                "Principal axis (PCA)"              = "pca_axis",
                "Robust straight-line fit (RANSAC)" = "ransac_straight",
                "Goal-biased direction"             = "goal_bias"
              ),
              selected = "distal"
            ),
            uiOutput("method_help"),
            conditionalPanel(
              "input.method == 'crossing'",
              tags$hr(),
              p(class = "text-muted small",
                "Radii as fractions of the arena radius",
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
        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header("Tracks and headings"),
            card_body(
              padding = 0,
              uiOutput("track_plot_ui")
            )
          ),
          accordion(
            id = "results_options",
            # Collapsible option groups so the right column can hold more
            # controls without crowding the plot. Display + Summary start open;
            # Download starts collapsed. multiple = TRUE lets several stay open.
            open = c("Display", "Summary"),
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
              uiOutput("colour_by_ui"),
              tags$hr(class = "my-2"),
              .layer_switch("show_tracks",   "Trajectories",       TRUE),
              .layer_switch("show_arrow",   "Directedness arrow", TRUE),
              .layer_switch("show_ci",      "Mean-direction CI",  FALSE),
              .layer_switch("show_vectors", "Heading vectors",    FALSE),
              .layer_switch("show_rayleigh", "Rayleigh circle",    FALSE),
              .layer_switch("show_vtest",   "V-test line",        FALSE),
              .layer_switch("show_quadrants", "Quadrant lines",   FALSE),
              .layer_switch("show_rings",   "Guide rings",        FALSE),
              tags$hr(class = "my-2"),
              sliderInput(
                "preview_px", "Preview size (px)",
                min = 480, max = 1000, value = 720, step = 20
              ),
              tags$span(
                class = "text-muted small",
                "On-screen only; export size is set under Download."
              )
            ),
            accordion_panel(
              "Summary",
              tableOutput("summary_tbl")
            ),
            accordion_panel(
              "Download",
              layout_columns(
                col_widths = c(6, 6),
                numericInput("plot_w", "Width (in)", value = 7,
                             min = 1, max = 30, step = 0.5),
                numericInput("plot_h", "Height (in)", value = 7,
                             min = 1, max = 30, step = 0.5)
              ),
              selectInput(
                "plot_fmt", "Format",
                choices = c("PDF (vector)"  = "pdf",
                            "SVG (vector)"  = "svg",
                            "PNG (raster)"  = "png",
                            "JPG (raster)"  = "jpg")
              ),
              conditionalPanel(
                "input.plot_fmt == 'png' || input.plot_fmt == 'jpg'",
                numericInput("plot_dpi", "Resolution (dpi)", value = 300,
                             min = 72, max = 600, step = 1)
              ),
              # JPEG has no alpha channel, so transparency only applies to the
              # other formats; hide the option when JPG is selected.
              conditionalPanel(
                "input.plot_fmt != 'jpg'",
                checkboxInput("plot_transparent",
                              "Transparent background", value = FALSE)
              ),
              downloadButton(
                "dl_plot", "Download plot",
                class = "btn-sm btn-outline-primary w-100 mb-2"
              ),
              uiOutput("dl_csv_ui")
            ),
            accordion_panel(
              "R code",
              tags$p(
                class = "text-muted small",
                "radiatR code that reproduces this figure. Edit the data path",
                " to point at your own file."
              ),
              tags$div(
                style = "max-height:320px; overflow:auto;",
                verbatimTextOutput("figure_code")
              ),
              tags$button(
                class   = "btn btn-sm btn-outline-primary w-100 mb-2",
                onclick = paste0(
                  "navigator.clipboard.writeText(",
                  "document.getElementById('figure_code').innerText);",
                  "this.innerText='Copied';",
                  "setTimeout(()=>this.innerText='Copy R code',1200);"
                ),
                "Copy R code"
              ),
              downloadButton("dl_code", "Download .R",
                             class = "btn-sm btn-outline-primary w-100")
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

  output$preview_section <- renderUI({
    req(rv$path)
    tagList(tags$hr(), tableOutput("preview_tbl"))
  })

  output$preview_tbl <- renderTable({
    req(rv$path)
    df <- tryCatch(
      utils::read.csv(
        rv$path, nrows = 5L,
        check.names = FALSE, stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(df))
      return(data.frame(Note = "Could not preview this file"))
    df
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
          plot_theme = input$plot_theme, angle_labels = input$angle_labels,
          heading_display = input$heading_display,
          show_arrow = tog(input$show_arrow, TRUE),
          show_vectors = tog(input$show_vectors, FALSE),
          show_rayleigh = tog(input$show_rayleigh, FALSE),
          show_ci = tog(input$show_ci, FALSE),
          show_vtest = tog(input$show_vtest, FALSE),
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
        circ0 = input$circ0, circ1 = input$circ1,
        plot_theme = input$plot_theme, angle_labels = input$angle_labels,
        heading_display = input$heading_display,
        # Path-metrics caption, none mode only (no headings). spec_to_plot and
        # spec_to_code render and reproduce it.
        caption  = if (is.null(rv$hd)) straightness_caption(rv$ts, gc) else NULL,
        show_tracks = tog(input$show_tracks, TRUE),
        show_arrow  = tog(input$show_arrow,  TRUE),
        show_vectors = tog(input$show_vectors, FALSE),
        show_rayleigh = tog(input$show_rayleigh, FALSE),
        show_ci    = tog(input$show_ci,    FALSE),
        show_vtest = tog(input$show_vtest, FALSE),
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
    base <- num_or(input$preview_px, 720)
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

  output$summary_tbl <- renderTable({
    if (identical(rv$mode, "headings")) {
      req(rv$hd)
      grp    <- (rv$hd_map %||% list(group = NULL))$group
      hd     <- rv$hd
      by_col <- grp
      pooled <- is.null(by_col)
      # No group column: inject a dummy single-level column so circ_summarise /
      # the per-group rayleigh loop run uniformly; the dummy is dropped below.
      if (pooled) { hd[[".all"]] <- "All"; by_col <- ".all" }
      out <- tryCatch({
        cm <- circ_summary_table(hd, by_col)
        if (pooled) cm[["Group"]] <- NULL    # single pooled row: drop the dummy group col
        cm
      }, error = function(e) data.frame(Note = "Summary not available"))
      return(out)
    }
    req(rv$ts)
    gc <- if (!is.null(input$cond_col) && nzchar(input$cond_col))
      input$cond_col else NULL

    # None mode: no headings -> show Group + Straightness only.
    if (is.null(rv$hd)) {
      st  <- straightness_index(rv$ts)
      idc <- rv$ts@cols$id
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

    # rv$hd is a headings frame whose trial column is always "id".
    by_col <- if (!is.null(gc)) gc else "id"

    tryCatch({
      cm <- circ_summary_table(rv$hd, by_col)

      # Mean path straightness per group: net displacement / path length per
      # trial (0 = convoluted, 1 = straight), averaged over the group's trials.
      # Join via a character-keyed lookup to avoid factor/character merge
      # mismatches on the (possibly ordered-factor) condition column.
      st  <- straightness_index(rv$ts)
      idc <- rv$ts@cols$id
      if (!is.null(gc)) {
        cond_map <- unique(as.data.frame(rv$ts)[, c(idc, gc), drop = FALSE])
        st  <- merge(st, cond_map, by = idc)
        agg <- tapply(st$straightness, as.character(st[[gc]]),
                      function(v) mean(v, na.rm = TRUE))
      } else {
        # No condition column: each group is a single trial.
        agg <- stats::setNames(st$straightness, as.character(st[[idc]]))
      }
      cm[["Straightness"]] <-
        round(as.numeric(agg[as.character(cm[["Group"]])]), 3)
      cm
    }, error = function(e) {
      data.frame(Note = "Summary not available")
    })
  }, striped = TRUE, hover = TRUE, align = "c")

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
      dat <- if (is.null(rv$hd)) straightness_index(rv$ts) else rv$hd
      utils::write.csv(dat, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
