# radiatR — Shiny companion app
# Launch with: radiatR::launch_app()
# To deploy: rsconnect::deployApp(system.file("app", package = "radiatR"))

library(shiny)
library(bslib)
library(ggplot2)
library(radiatR)

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

# A finite scalar from a possibly-NULL/NA numeric input, else a default.
num_or <- function(v, default) {
  if (is.null(v) || length(v) != 1L || is.na(v) || !is.finite(v)) default else v
}

# Resolve a download format to a graphics device for ggsave(). Vector formats
# prefer svglite/cairo so the output is editable in vector tools; PNG uses the
# default raster device.
.plot_device <- function(fmt) {
  switch(fmt,
    png = "png",
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

derive_hd <- function(ts, method, circ0, circ1) {
  # Use relative coords when available so headings are in the same frame as
  # the rel_x/rel_y display (stimulus fixed at East). Fall back to absolute
  # for datasets without a normalised relative coordinate system.
  has_rel <- !is.null(ts@cols$rel_x) && !is.null(ts@cols$rel_y)
  args <- list(x = ts, coords = if (has_rel) "relative" else "absolute")
  if (method == "crossing") {
    args$rule  <- "crossing"
    args$circ0 <- circ0
    args$circ1 <- circ1
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
    step     = 1L,
    path     = NULL,
    dialect  = NULL,
    ts       = NULL,
    cond_col = NULL,
    hd       = NULL,
    error    = NULL
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

  observeEvent(input$restart, {
    rv$step     <- 1L
    rv$path     <- NULL
    rv$dialect  <- NULL
    rv$ts       <- NULL
    rv$cond_col <- NULL
    rv$hd       <- NULL
    rv$error    <- NULL
  })

  # Step 1 → 2: load TrajSet and detect condition column
  observeEvent(input$go2, {
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

  # Step 2 → 3: derive headings, join condition if present
  observeEvent(input$go3, {
    req(rv$ts)
    method <- if (is.null(input$method)) "distal" else input$method
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
      rv$hd    <- hd
      rv$step  <- 3L
      rv$error <- NULL
    }
  })

  # ---- file upload -----------------------------------------------------------
  observeEvent(input$file, {
    req(input$file)
    rv$path     <- input$file$datapath
    rv$dialect  <- guess_dialect(rv$path)
    rv$ts       <- NULL
    rv$cond_col <- NULL
    rv$hd       <- NULL
    rv$error    <- NULL
  })

  # ---- example dataset -------------------------------------------------------
  observeEvent(input$load_example, {
    ts <- tryCatch(
      example_ts(),
      error = function(e) NULL
    )
    if (is.null(ts)) {
      rv$error <- "Could not load the bundled example dataset."
      return()
    }
    rv$ts       <- ts
    rv$path     <- NULL
    rv$dialect  <- NULL
    rv$cond_col <- detect_cond_col(ts)
    rv$hd       <- NULL
    rv$step     <- 2L
    rv$error    <- NULL
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
        fileInput(
          "file", NULL,
          accept      = c(".csv", ".txt", ".tsv", ".mat"),
          buttonLabel = "Browse…",
          placeholder = "No file selected"
        ),
        div(
          class = "text-muted small mb-2",
          "Don't have a file handy? ",
          actionLink(
            "load_example",
            "Load the example millipede dataset"
          ),
          " (Cylindroiulus punctatus, 235 trials)."
        ),
        uiOutput("format_box"),
        uiOutput("preview_section"),
        err_box
      )

    # ---- Step 2: configure ----
    } else if (rv$step == 2L) {
      tagList(
        h5("How should headings be measured?"),
        radioButtons(
          "method", NULL,
          choiceValues = c("distal", "net", "crossing"),
          choiceNames  = list(
            tagList(
              tags$b("Direction at furthest point"),
              tags$br(),
              tags$span(
                class = "text-muted small",
                "Heading when the animal was furthest from the",
                " centre. Recommended — no setup needed."
              )
            ),
            tagList(
              tags$b("Net displacement direction"),
              tags$br(),
              tags$span(
                class = "text-muted small",
                "Straight-line direction from start to end.",
                " Simple and always applicable."
              )
            ),
            tagList(
              tags$b("Exit direction (ring crossing)"),
              tags$br(),
              tags$span(
                class = "text-muted small",
                "Heading as the animal crosses a detection ring.",
                " Set the ring radii below."
              )
            )
          ),
          selected = "distal"
        ),
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
              min = 0, max = 0.9, value = 0.3, step = 0.05
            ),
            sliderInput(
              "circ1", "Outer ring",
              min = 0.1, max = 1.0, value = 0.6, step = 0.05
            )
          )
        ),
        uiOutput("cond_ui"),
        err_box
      )

    # ---- Step 3: results ----
    } else {
      tagList(
        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header("Tracks and headings"),
            card_body(
              padding = 0,
              uiOutput("track_plot_ui")
            )
          ),
          tagList(
            card(
              card_header("Display"),
              card_body(
                .layer_switch("show_tracks", "Trajectories", TRUE),
                .layer_switch("show_points", "Heading points", TRUE),
                .layer_switch("show_arrow",  "Directedness arrow", TRUE),
                .layer_switch("show_ci",     "Mean-direction CI", FALSE),
                tags$hr(class = "my-2"),
                sliderInput(
                  "preview_px", "Preview size (px)",
                  min = 240, max = 900, value = 460, step = 20
                ),
                tags$span(
                  class = "text-muted small",
                  "On-screen only; export size is set under Download."
                )
              )
            ),
            card(
              card_header("Summary"),
              card_body(tableOutput("summary_tbl"))
            ),
            card(
              card_header("Download"),
              card_body(
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
                              "PNG (raster)"  = "png")
                ),
                conditionalPanel(
                  "input.plot_fmt == 'png'",
                  numericInput("plot_dpi", "Resolution (dpi)", value = 300,
                               min = 72, max = 600, step = 1)
                ),
                downloadButton(
                  "dl_plot", "Download plot",
                  class = "btn-sm btn-outline-primary w-100 mb-2"
                ),
                downloadButton(
                  "dl_csv", "Headings (CSV)",
                  class = "btn-sm btn-outline-secondary w-100"
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

    df    <- as.data.frame(rv$ts)
    id_c  <- rv$ts@cols$id
    tc    <- rv$ts@cols$time
    other <- names(df)[vapply(df, function(v) {
      nu <- length(unique(stats::na.omit(v)))
      (is.character(v) || is.factor(v)) && nu >= 2L && nu <= 12L
    }, logical(1))]
    other <- setdiff(other, c(id_c, tc))

    if (!length(other)) return(NULL)
    choices <- c("None" = "", stats::setNames(other, other))

    tagList(
      tags$hr(),
      selectInput(
        "cond_col",
        "Group by condition column (optional)",
        choices  = choices,
        selected = col
      )
    )
  })

  # ---- step 3 outputs --------------------------------------------------------

  # Treat an as-yet-unrendered toggle (NULL) as its declared default.
  tog <- function(v, default) if (is.null(v)) default else isTRUE(v)

  # Build the results plot honouring the layer toggles. Shared by the
  # on-screen plot and the PNG download so they always match.
  build_results_plot <- function() {
    id_col <- rv$ts@cols$id
    gc <- if (!is.null(input$cond_col) && nzchar(input$cond_col))
      input$cond_col else NULL

    # Display convention: reference direction (East in UC / rel coords) at top,
    # clockwise-positive. This matches the old clock display and keeps tracks,
    # heading overlays, and the arrow all in the same orientation.
    disp <- circ_display(zero = 0)

    # Drive the directedness arrow from the chosen heading method (rv$hd), so it
    # summarises the SAME angles as the heading points, CI bar, and summary
    # table. radiate's default arrow instead summarises the per-frame position
    # angle (ts@cols$angle), which has no relation to the selected rule. We
    # broadcast each trial's heading (circular mean across its rows) onto every
    # frame of that trial; radiate then takes the per-trial mean (a no-op here)
    # and the per-panel resultant, matching circ_summarise's R and direction.
    ts_arrow <- rv$ts
    hd_map   <- stats::aggregate(
      rv$hd[["heading"]],
      by  = list(id = rv$hd[["id"]]),
      FUN = function(a) {
        a <- a[is.finite(a)]
        if (!length(a)) NA_real_ else atan2(mean(sin(a)), mean(cos(a)))
      }
    )
    d <- ts_arrow@data
    d[[".arrow_heading"]] <- hd_map$x[match(d[[id_col]], hd_map$id)]
    ts_arrow@data <- d

    p <- radiate(
      ts_arrow,
      group_col       = id_col,
      colour_col      = gc,
      panel_by        = gc,
      # colour_cycle and colour_col are mutually exclusive; only cycle
      # colours when no condition column is driving the colour scale.
      colour_cycle    = if (is.null(gc)) 20 else NULL,
      show_tracks     = tog(input$show_tracks, TRUE),
      show_arrow      = tog(input$show_arrow,  TRUE),
      arrow_angle_col = ".arrow_heading",
      show_labels     = FALSE,
      display         = disp
    )

    # Propagate display to the headings df so overlay functions rotate to match.
    hd_disp <- rv$hd
    attr(hd_disp, "display") <- disp

    if (tog(input$show_points, TRUE)) {
      p <- p + add_heading_points(hd_disp, size = 2.5, alpha = 0.8)
    }
    if (tog(input$show_ci, FALSE)) {
      p <- p + add_heading_interval(
        hd_disp, colour_col = gc, stat = "bootstrap_ci"
      )
    }
    p
  }

  # Preview canvas height tracks the chosen export aspect ratio so the on-screen
  # plot reflects the width/height the user will download. The Preview size
  # slider scales the canvas without affecting the exported file. Width fills
  # the card.
  output$track_plot_ui <- renderUI({
    w    <- num_or(input$plot_w, 7)
    h    <- num_or(input$plot_h, 7)
    base <- num_or(input$preview_px, 460)
    px   <- max(160, min(1000, round(base * (h / w))))
    plotOutput("track_plot", height = paste0(px, "px"))
  })

  output$track_plot <- renderPlot({
    req(rv$ts, rv$hd)
    p <- tryCatch(
      build_results_plot(),
      error = function(e) {
        message("track_plot render failed: ", conditionMessage(e))
        ggplot() +
          annotate(
            "text", x = 0, y = 0,
            label = "Plot unavailable", colour = "grey50"
          ) +
          theme_void()
      }
    )
    print(p)
  }, res = 120)

  output$summary_tbl <- renderTable({
    req(rv$ts, rv$hd)
    gc <- if (!is.null(input$cond_col) && nzchar(input$cond_col))
      input$cond_col else NULL
    # rv$hd is a headings frame whose trial column is always "id".
    by_col <- if (!is.null(gc)) gc else "id"

    tryCatch({
      cm <- circ_summarise(
        rv$hd, "heading",
        units   = "radians",
        .by     = by_col,
        stats   = c("n", "mean_dir_deg", "resultant_R"),
        display = circ_display(zero = 0)
      )
      # Rayleigh test p-value per group
      groups <- unique(rv$hd[[by_col]])
      p_vals <- vapply(groups, function(g) {
        rayleigh_p_fmt(rv$hd$heading[rv$hd[[by_col]] == g])
      }, character(1L))
      p_df <- stats::setNames(
        data.frame(groups, p_vals, stringsAsFactors = FALSE),
        c(by_col, "Rayleigh p")
      )
      cm <- merge(cm, p_df, by = by_col, sort = FALSE)

      names(cm)[names(cm) == by_col]         <- "Group"
      names(cm)[names(cm) == "mean_dir_deg"] <- "Direction (°)"
      names(cm)[names(cm) == "resultant_R"]  <- "R"
      cm[["Direction (°)"]] <-
        round(cm[["Direction (°)"]], 1)
      cm[["R"]] <- round(cm[["R"]], 3)
      cm
    }, error = function(e) {
      data.frame(Note = "Summary not available")
    })
  }, striped = TRUE, hover = TRUE, align = "c")

  output$dl_plot <- downloadHandler(
    filename = function() {
      fmt <- if (is.null(input$plot_fmt)) "pdf" else input$plot_fmt
      paste0("radiatR_plot_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(rv$ts, rv$hd)
      fmt <- if (is.null(input$plot_fmt)) "pdf" else input$plot_fmt
      ggsave(
        file, build_results_plot(),
        device = .plot_device(fmt),
        width  = num_or(input$plot_w, 7),
        height = num_or(input$plot_h, 7),
        units  = "in",
        dpi    = num_or(input$plot_dpi, 300)
      )
    }
  )

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("radiatR_headings_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$hd)
      utils::write.csv(rv$hd, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
