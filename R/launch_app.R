#' Launch the radiatR Shiny companion app
#'
#' Opens a browser-based graphical interface for uploading tracking data,
#' selecting a heading method, and viewing circular track plots and summary
#' statistics.  No R coding is required to use the app.
#'
#' The same app directory can be deployed to
#' \url{https://www.shinyapps.io} or any Shiny-compatible server without
#' modification:
#'
#' \preformatted{
#'   rsconnect::deployApp(system.file("app", package = "radiatR"))
#' }
#'
#' @param port Integer; TCP port to listen on.  \code{NULL} (default) lets
#'   Shiny pick a free port automatically.
#' @param launch_browser Logical; open the system browser automatically
#'   (default \code{TRUE}).  Set to \code{FALSE} when running headless.
#' @return Called for its side-effect; returns the result of
#'   \code{\link[shiny]{runApp}} invisibly.
#' @export
launch_app <- function(port = NULL, launch_browser = TRUE) {
  rlang::check_installed("shiny", reason = "to run the radiatR app")
  rlang::check_installed("bslib", reason = "to run the radiatR app")
  app_dir <- system.file("app", package = "radiatR")
  if (!nzchar(app_dir))
    stop("radiatR app directory not found — ",
         "is the package installed correctly?")
  shiny::runApp(
    app_dir,
    port           = port,
    launch.browser = launch_browser
  )
}
