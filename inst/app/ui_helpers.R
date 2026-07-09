# Shared Results-sidebar UI builders, so the Circular plots and Kinematics
# subtabs offer the same "Image export" and "R code" sections. Pure UI (shiny +
# bslib only); sourced by app.R and unit-tested standalone. Keeping these here
# (not inline in app.R) lets both subtabs reuse one component and lets tests
# assert the wiring without launching the app.

# An "Image export" accordion panel: plot size, format, dpi, transparency, and a
# Download-plot button. `prefix` namespaces the input ids so each subtab gets its
# own settings; prefix = "" reproduces the original Circular-plots ids (plot_w,
# plot_fmt, dl_plot, ...) so the existing handlers need no change.
ui_image_export <- function(prefix = "") {
  id  <- function(x) paste0(prefix, x)
  fmt <- id("plot_fmt")
  bslib::accordion_panel(
    "Image export",
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::numericInput(id("plot_w"), "Width (in)", value = 7,
                          min = 1, max = 30, step = 0.5),
      shiny::numericInput(id("plot_h"), "Height (in)", value = 7,
                          min = 1, max = 30, step = 0.5)
    ),
    shiny::selectInput(
      fmt, "Format",
      choices = c("PDF (vector)" = "pdf",
                  "SVG (vector)" = "svg",
                  "PNG (raster)" = "png",
                  "JPG (raster)" = "jpg")
    ),
    shiny::conditionalPanel(
      sprintf("input.%s == 'png' || input.%s == 'jpg'", fmt, fmt),
      shiny::numericInput(id("plot_dpi"), "Resolution (dpi)", value = 300,
                          min = 72, max = 600, step = 1)
    ),
    # JPEG has no alpha channel, so transparency only applies to the other
    # formats; hide the option when JPG is selected.
    shiny::conditionalPanel(
      sprintf("input.%s != 'jpg'", fmt),
      shiny::checkboxInput(id("plot_transparent"),
                           "Transparent background", value = FALSE)
    ),
    shiny::downloadButton(
      paste0("dl_", prefix, "plot"), "Download plot",
      class = "btn-sm btn-outline-primary w-100 mb-2"
    )
  )
}

# An "R code" accordion panel: a description, the emitted code, a Copy button,
# and a Download .R button. `code_id` is the verbatimTextOutput id, `dl_id` the
# download button id, `desc` the muted helper line.
ui_code_section <- function(code_id, dl_id, desc) {
  bslib::accordion_panel(
    "R code",
    shiny::tags$p(class = "text-muted small", desc),
    shiny::tags$div(
      style = "max-height:320px; overflow:auto;",
      shiny::verbatimTextOutput(code_id)
    ),
    shiny::tags$button(
      class   = "btn btn-sm btn-outline-primary w-100 mb-2",
      onclick = paste0(
        "navigator.clipboard.writeText(",
        "document.getElementById('", code_id, "').innerText);",
        "this.innerText='Copied';",
        "setTimeout(()=>this.innerText='Copy R code',1200);"
      ),
      "Copy R code"
    ),
    shiny::downloadButton(dl_id, "Download .R",
                          class = "btn-sm btn-outline-primary w-100")
  )
}

# The header brand cluster: the radiatR logo + title, linked to the package
# website (logo-as-home), plus a small explicit Documentation link. `site_url`
# is the pkgdown site. Pure UI; unit-tested standalone.
ui_brand <- function(site_url) {
  shiny::tags$span(
    shiny::tags$a(
      href = site_url, target = "_blank", rel = "noopener",
      style = "text-decoration:none; color:inherit;",
      shiny::tags$img(src = "logo.png", height = "30px", alt = "radiatR",
                      class = "me-2"),
      shiny::tags$b("radiatR — Circular Track Analysis")
    ),
    shiny::tags$a(
      href = site_url, target = "_blank", rel = "noopener",
      class = "ms-3 small text-muted",
      "Documentation ↗"
    )
  )
}

# A larger centered radiatR logo for the Upload step's landing screen.
# Decorative (not linked).
ui_upload_logo <- function() {
  shiny::tags$img(src = "logo.png", height = "120px",
                  class = "d-block mx-auto mb-3", alt = "radiatR")
}
