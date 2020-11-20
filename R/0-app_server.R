#' App Server
#'
#' @param input shiny server input
#' @param output shiny server output
#' @param session shiny server session
#'
#' @return shiny app server
#' @export
#' @importFrom shiny callModule observe
app_server <- function(input, output, session) {

  shiny::showModal(
    shiny::modalDialog(
      title = icon_text("comments", "Welcome!"),
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        shiny::actionButton("tour", "Tour the App", icon = shiny::icon("bus")),
        shiny::actionButton("docs2", "View Docs", icon = shiny::icon("file")),
        shiny::modalButton("Dismiss", icon("door-open"))
      ),
      tags$iframe(src = "www/welcome.html", width = "100%", height = "600px",
                  scrolling = "auto", frameborder = 0)
    )
  )

  shiny::observeEvent(input$docs, {
    propalloc::open_docs()
  })

  header_tour_steps <- list(
    list(
      element = "#tour_sidebar",
      intro = "Use the sidebar to navigate between various tabs.<br/><br/>You
      can collapse the sidebar by clicking the button to the right of the
      Oliver Wyman logo in the header to increase screen display.",
      position = "right"
    )
  )

  # first level modules

  # header buttons
  shiny::callModule(
    header_buttons,
    "header",
    tour_steps = header_tour_steps,
    parent_session = session
  )

  # inputs menu
  inputs_menu_list <- shiny::callModule(inputs_menu, "input_tables")

  allocation_data <- shiny::callModule(
    allocation_menu, "allocation_tables",
    inputs_menu_list
  )

  analysis_items <- shiny::callModule(
    location_summary_menu, "analysis_tables",
    allocation_data
  )

  shiny::callModule(
    insights_menu, "insights_tables", allocation_data = allocation_data
  )

  observe({
    print(list(names(allocation_data$allocation_data_full())))
  })

}






