#' App UI
#'
#' @return HTML tagList containing [shinydashboard::dashboardPage()]
#' @export
#' @importFrom htmltools tagList
#' @importFrom shinydashboard dashboardPage
app_ui <- function() {

  htmltools::tagList(

    # adding external resources
    add_external_resources(),

    # shinydashboardPage
    shinydashboard::dashboardPage(
      header = app_header_ui(),
      sidebar = app_sidebar_ui(),
      body = app_body_ui(),
      title = "Property-Allocation-App",
      skin = "black"
    )
  )

}

#' App Header UI Function
#'
#' Adds header buttons and contacts.
#'
#' @return HTML for a [dashboardHeader][shinydashboard::dashboardHeader()]
#' @export
#'
#' @importFrom shinydashboard dashboardHeader
#' @importFrom htmltools tags
app_header_ui <- function() {

  shinydashboard::dashboardHeader(

    title = htmltools::tags$a(
      htmltools::tags$img(
        src = "www/resized.png",
        width = 200
      ),
      href = "#"
    ),

    .list = header_buttons_ui("header", include_tour = FALSE, contacts = contacts())
  )

}


#' App Sidebar UI Function
#'
#' @return HTML for a [dashboardSidebar][shinydashboard::dashboardSidebar()]
#' @export
#'
#' @importFrom htmltools tags hr h5 div
#' @importFrom shiny icon
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
app_sidebar_ui <- function() {

  shinydashboard::dashboardSidebar(

    # app information
    htmltools::tags$div(
      htmltools::hr(),
      htmltools::h5(
        "Property Allocation\nShiny Application\n",
        htmltools::br(),
        shiny::actionLink("docs", "Documentation", icon = shiny::icon("file"))
      ),
      htmltools::hr(),
      align = "center",
      style = "font-weight: bold; color: #ffffff;"
    ),

    htmltools::div(
      id = "tour_sidebar",
      shinydashboard::sidebarMenu(
        id = "sidebar_menus",
        shinydashboard::menuItem(
          "Inputs",
          tabName = "inputs",
          icon = shiny::icon("inbox"),
          selected = TRUE
        ),
        shinydashboard::menuItem(
          "Allocation",
          tabName = "allocation",
          icon = shiny::icon("bezier-curve")
        ),
        shinydashboard::menuItem(
          "Location Summary",
          tabName = "location_summary",
          icon = shiny::icon("city")
        ),
        shinydashboard::menuItem(
          "Insights",
          tabName = "insights",
          icon = shiny::icon("lightbulb")
        )
      )
    )
  )

}

#' App Body UI Function
#'
#' @return HTML for a [dashboardBody][shinydashboard::dashboardBody()]
#' @export
#'
#' @importFrom shinydashboard dashboardBody tabItem tabBox
app_body_ui <- function() {

  shinydashboard::dashboardBody(

    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "inputs",
        inputs_menu_ui("input_tables"),
      ),
      shinydashboard::tabItem(
        tabName = "allocation",
      allocation_menu_ui("allocation_tables")
      ),
      shinydashboard::tabItem(
        tabName = "location_summary",
        location_summary_menu_ui("analysis_tables", sov = sov)
      ),
      shinydashboard::tabItem(
        tabName = "insights",
        insights_menu_ui("insights_tables")
      )
    )
  )
}




