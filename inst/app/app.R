# radiatR — Shiny companion app
# Launch with: radiatR::launch_app()
# Deploy with: rsconnect::deployApp(system.file("app", package = "radiatR"))

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
  hdr <- tryCatch(readLines(path, n = 3L), error = function(e) character(0L))
  if (length(hdr) >= 2L &&
      grepl("scorer",    tolower(hdr[1L]), fixed = TRUE) &&
      grepl("bodyparts", tolower(hdr[2L]), fixed = TRUE))
    return("deeplabcut_multiheader")

  df <- tryCatch(
    utils::read.csv(path, nrows = 3L, check.names = FALSE,
                    stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(df)) return("generic")
  nm <- tolower(gsub("[^a-z0-9]+", "_", names(df)))

  if (any(grepl("_likelihood$", nm)) && any(grepl("_x$", nm))) return("deeplabcut")
  if ("frame_idx" %in% nm && any(grepl("_score$", nm)))         return("sleap")
  if ("track_id"  %in% nm && "position_x" %in% nm)             return("trackmate")
  if ("x_center"  %in% nm || "x_centre"   %in% nm)             return("ethovision")
  if ("x_1"       %in% nm && "y_1"        %in% nm)             return("idtrackerai_wide")
  if ("trial_time" %in% nm || any(grepl("^x_centr", nm)))       return("anymaze")
  if ("trackid"   %in% nm)                                       return("toxtrac")
  "generic"
}

load_ts <- function(path, dialect) {
  if (is.null(dialect) || dialect %in% c("auto", "generic"))
    return(TrajSet_read(path))
  TrajSet_read(path, dialect = dialect)
}

derive_hd <- function(ts, method, circ0, circ1) {
  args <- list(x = ts, coords = "absolute", angle_convention = "unit_circle")
  if (method == "crossing") {
    args$rule  <- "crossing"
    args$circ0 <- circ0
    args$circ1 <- circ1
  } else {
    args$rule <- method
  }
  do.call(derive_headings, args)
}

plain_error <- function(e) {
  m <- conditionMessage(e)
  if (grepl("id|time|mapping",      m, ignore.case = TRUE))
    return("Could not identify the trial ID or time columns. Try selecting your software from the dropdown.")
  if (grepl("x.*y|position|col",    m, ignore.case = TRUE))
    return("Could not find x/y position columns. Try selecting your software from the dropdown.")
  if (grepl("crossing|circ0|circ1", m, ignore.case = TRUE))
    return("No ring crossings found. Try adjusting the ring radii or choosing a different method.")
  if (grepl("R\\.matlab|mat file",  m, ignore.case = TRUE))
    return("Reading .mat files requires the R.matlab package: install.packages('R.matlab')")
  paste("Unexpected error:", m)
}

# ---- ui ----------------------------------------------------------------------

ui <- page_fillable(
  theme   = bs_theme(bootswatch = "flatly"),
  padding = "1.5rem",

  tags$head(tags$style(HTML("
    .pill       { display:inline-block; padding:3px 14px; border-radius:99px;
                  font-size:.78rem; font-weight:600; margin-right:3px; }
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
      actionButton("back_btn", "Back", class = "btn-sm btn-outline-secondary"),
      uiOutput("fwd_btn_ui")
    )
  )
)

# ---- server ------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    step    = 1L,
    path    = NULL,
    dialect = NULL,
    ts      = NULL,
    hd      = NULL,
    error   = NULL
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
      actionButton("restart", "Start over", class = "btn-outline-secondary")
  })

  # ---- navigation ------------------------------------------------------------
  observeEvent(input$back_btn, {
    if (rv$step > 1L) { rv$step <- rv$step - 1L; rv$error <- NULL }
  })
  observeEvent(input$restart, {
    rv$step <- 1L; rv$path <- NULL; rv$dialect <- NULL
    rv$ts   <- NULL; rv$hd  <- NULL; rv$error  <- NULL
  })

  # Step 1 → 2: load the TrajSet
  observeEvent(input$go2, {
    if (is.null(rv$path)) {
      rv$error <- "Please upload a file first."
      return()
    }
    d  <- if (!is.null(input$dialect_sel) && input$dialect_sel != "auto")
            input$dialect_sel else rv$dialect
    ts <- tryCatch(
      suppressMessages(suppressWarnings(load_ts(rv$path, d))),
      error = function(e) { rv$error <- plain_error(e); NULL }
    )
    if (!is.null(ts)) {
      rv$ts <- ts; rv$dialect <- d; rv$step <- 2L; rv$error <- NULL
    }
  })

  # Step 2 → 3: derive headings
  observeEvent(input$go3, {
    req(rv$ts)
    method <- if (is.null(input$method)) "distal" else input$method
    c0     <- if (is.null(input$circ0))  0.3 else input$circ0
    c1     <- if (is.null(input$circ1))  0.6 else input$circ1
    hd <- tryCatch(
      derive_hd(rv$ts, method, c0, c1),
      error = function(e) { rv$error <- plain_error(e); NULL }
    )
    if (!is.null(hd)) { rv$hd <- hd; rv$step <- 3L; rv$error <- NULL }
  })

  # ---- file upload -----------------------------------------------------------
  observeEvent(input$file, {
    req(input$file)
    rv$path    <- input$file$datapath
    rv$dialect <- guess_dialect(rv$path)
    rv$ts      <- NULL; rv$hd <- NULL; rv$error <- NULL
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
          "Upload a CSV or text file exported from your tracking software.",
          " EthoVision, DeepLabCut, SLEAP, TRex, ANY-maze, TrackMate, idtracker.ai,",
          " and others are supported automatically."),
        fileInput("file", NULL,
                  accept      = c(".csv", ".txt", ".tsv", ".mat"),
                  buttonLabel = "Browse…",
                  placeholder = "No file selected"),
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
              tags$span(class = "text-muted small",
                "The heading at the moment the animal was furthest from the",
                " arena centre. Recommended — no setup needed.")
            ),
            tagList(
              tags$b("Net displacement direction"),
              tags$br(),
              tags$span(class = "text-muted small",
                "The straight-line direction from the start to the end of the",
                " trial. Simple and always applicable.")
            ),
            tagList(
              tags$b("Exit direction (ring crossing)"),
              tags$br(),
              tags$span(class = "text-muted small",
                "The heading as the animal crosses a detection ring.",
                " Set the inner and outer ring radii below.")
            )
          ),
          selected = "distal"
        ),
        conditionalPanel(
          "input.method == 'crossing'",
          tags$hr(),
          p(class = "text-muted small",
            "Ring radii as fractions of the arena radius (0 = centre,",
            " 1 = edge)."),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput("circ0", "Inner ring", min = 0,   max = 0.9,
                        value = 0.3, step = 0.05),
            sliderInput("circ1", "Outer ring", min = 0.1, max = 1.0,
                        value = 0.6, step = 0.05)
          )
        ),
        err_box
      )

    # ---- Step 3: results ----
    } else {
      tagList(
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header("Tracks and headings"),
            card_body(padding = 0,
              plotOutput("track_plot", height = "380px")
            )
          ),
          tagList(
            card(
              card_header("Summary per trial"),
              card_body(tableOutput("summary_tbl"))
            ),
            card(
              card_header("Download"),
              card_body(
                downloadButton("dl_plot", "Plot (PNG)",
                               class = "btn-sm btn-outline-primary w-100 mb-2"),
                downloadButton("dl_csv",  "Headings (CSV)",
                               class = "btn-sm btn-outline-secondary w-100")
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
    lbl <- if (length(lbl)) lbl[1] else NULL

    tagList(
      if (!is.null(lbl) && d != "generic")
        div(class = "alert alert-info py-1 px-2 small mb-2",
            "Format detected: ", tags$b(lbl))
      else
        div(class = "alert alert-secondary py-1 px-2 small mb-2",
            "Format not recognised — please select your software:"),
      selectInput("dialect_sel", "Tracking software",
                  choices  = DIALECT_CHOICES,
                  selected = if (!is.null(d)) d else "auto")
    )
  })

  output$preview_section <- renderUI({
    req(rv$path)
    tagList(tags$hr(), tableOutput("preview_tbl"))
  })

  output$preview_tbl <- renderTable({
    req(rv$path)
    df <- tryCatch(
      utils::read.csv(rv$path, nrows = 5L, check.names = FALSE,
                      stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(df)) return(data.frame(Note = "Could not preview this file"))
    df
  }, striped = TRUE, caption = "First 5 rows",
     caption.placement = "top")

  # ---- step 3 outputs --------------------------------------------------------
  output$track_plot <- renderPlot({
    req(rv$ts, rv$hd)
    id_col <- rv$ts@cols$id
    p <- tryCatch({
      radiate(rv$ts,
              group_col    = id_col,
              colour_cycle = 20,
              show_arrow   = TRUE,
              show_labels  = FALSE) +
        add_heading_points(rv$hd, size = 2.5, alpha = 0.8)
    }, error = function(e)
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Plot unavailable",
                 colour = "grey50") +
        theme_void()
    )
    print(p)
  }, res = 120)

  output$summary_tbl <- renderTable({
    req(rv$ts, rv$hd)
    id_col <- rv$ts@cols$id
    tryCatch({
      cm <- circ_summarise(rv$hd, heading, units = "radians",
                           .by    = id_col,
                           stats  = c("n", "mean_dir_deg", "resultant_R"))
      names(cm)[names(cm) == id_col]          <- "Trial"
      names(cm)[names(cm) == "mean_dir_deg"]  <- "Direction (°)"
      names(cm)[names(cm) == "resultant_R"]   <- "R"
      cm$`Direction (°)` <- round(cm$`Direction (°)`, 1)
      cm$R                    <- round(cm$R, 3)
      cm
    }, error = function(e) data.frame(Note = "Summary not available"))
  }, striped = TRUE, hover = TRUE, align = "c")

  output$dl_plot <- downloadHandler(
    filename = function() paste0("radiatR_plot_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$ts, rv$hd)
      p <- radiate(rv$ts, group_col = rv$ts@cols$id,
                   show_arrow = TRUE, show_labels = FALSE) +
           add_heading_points(rv$hd, size = 2.5, alpha = 0.8)
      ggsave(file, p, width = 7, height = 7, dpi = 180)
    }
  )

  output$dl_csv <- downloadHandler(
    filename = function() paste0("radiatR_headings_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$hd)
      utils::write.csv(rv$hd, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
