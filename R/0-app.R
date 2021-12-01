#' Run the Shiny Application
#'
#' @param install_dependencies logical-install package/app dependencies?
#'   Defaults to FALSE.
#' @param launch_browser,shiny_port,shiny_host Shiny options
#'
#' @keywords app
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom openmetrics register_shiny_metrics
run_app <- function(install_dependencies = FALSE, launch_browser = TRUE, shiny_port = 8080, shiny_host = "0.0.0.0") {

  if (install_dependencies) install_app_dependencies()
  
  app <- shiny::shinyApp(ui = app_ui(), server = app_server, options = list(launch.browser = launch_browser, shiny.port = shiny_port, shiny.host = shiny_host))
  openmetrics::register_shiny_metrics(app)
}

#' @keywords app
#'
#' @export
run_app.local <- function() {
  appDir <- system.file("app", package = "propalloc")

  if (appDir == "") {
    stop("Could not find app folder. Try re-installing `propalloc`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#' Install App Dependencies
#'
#' @return invisible
#' @export
#' @keywords app
#' @importFrom fs path_package
install_app_dependencies <- function() {

  source(fs::path_package("propalloc/scripts", "dependencies.R"))

  return(invisible(0))

}

#' Add external resources to UI head
#'
#' Adds external resources to shiny application from \code{propalloc}'s www
#' directory.
#'
#' @details # Adds the following:
#' 1. [shinyjs::useShinyjs()]
#' 1. [shinyWidgets::useSweetAlert()]
#' 1. [rintrojs::introjsUI()]
#' 1. Custom CSS - flipBox, DT styles, etc.
#' 1. Custom JavaScript - SOV Module JS functionality, flipBox, etc.
#'
#' @return HTML head tag
#' @export
#'
#' @keywords app
#'
#' @importFrom shiny addResourcePath
#' @importFrom htmltools tags
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useSweetAlert
add_external_resources <- function(){

  shiny::addResourcePath(
    'www', fs::path_package("propalloc", "app/www")
  )

  # load data before server call to only load once instead of multiple times per user
  load_demo_data()

  # highcharter options
  hc_opts()

  htmltools::tags$head(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    rintrojs::introjsUI(),
    htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  )

}
