#' Header Buttons UI Module
#'
#' @param id namespace ID
#' @param include_tour logical - include a 'tour app' button?
#' @param include_refresh logical - include a 'refresh' button?
#' @param include_help logical - include a 'help' button?
#' @param include_disclaimer logical - include a 'disclaimer' button?
#' @param include_contact logical - include a 'contact' button?
#' @param include_logout logical - include a 'logout' button? Note this will replace the
#'   default shiny logout.
#' @param contacts contacts list from [contacts()].
#'
#' @return \code{htmltools::tagList()}
#' @export
#' @importFrom shiny NS actionLink icon
#' @importFrom htmltools tags div a
#' @importFrom purrr compact
header_buttons_ui <- function(id,
                              include_tour = TRUE,
                              include_refresh = TRUE,
                              include_help = TRUE,
                              include_disclaimer = TRUE,
                              include_contact = TRUE,
                              include_logout = TRUE,
                              contacts = NULL) {

  ns <- shiny::NS(id)

  if (include_tour) {

    tour <- htmltools::tags$li(
      shiny::actionLink(
        ns("tour"),
        "Tour App",
        shiny::icon("bus")
      ),
      class = "dropdown"
    )

  } else {

    tour <- NULL

  }

  if (include_refresh) {

    refresh <- htmltools::tags$li(
      shiny::actionLink(
        ns("refresh"),
        "Refresh",
        shiny::icon("refresh")
      ),
      class = "dropdown"
    )

  } else {

    refresh <- NULL

  }

  if (include_disclaimer) {

    disclaimer <- htmltools::tags$li(
      shiny::actionLink(ns("disclaimer"),
                        label = "Disclaimer",
                        icon = shiny::icon("warning")),
      class = "dropdown"
    )

  } else {

    disclaimer <- NULL

  }

  if (include_help) {

    help <- htmltools::tags$li(
      shiny::actionLink(ns("help"),
                        label = "Help",
                        icon = shiny::icon("info-circle")),
      class = "dropdown"
    )

  } else {

    help <- NULL

  }

  docs <- htmltools::tags$li(
    shiny::actionLink(ns("docs"),
                      label = "Allocation Help",
                      icon = shiny::icon("file")),
    class = "dropdown"
  )

  if (include_contact) {

    if (is.null(contacts)) contacts <- contacts()

    contact <- contact_menu(contacts)

  } else {

    contact <- NULL

  }


  if (include_logout) {

    logout <- htmltools::tags$li(
      class = "dropdown",
      htmltools::tags$a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "dropdown",
        htmltools::div(
          htmltools::tags$i(
            class = "fa fa-door-open"
          ),
          "Logout",
          style = "display: inline"
        )
      ),
      htmltools::tags$ul(
        class = "dropdown-menu",
        htmltools::tags$li(
          htmltools::a(
            shiny::icon("door-open"),
            "Logout",
            href = "__logout__"
          )
        )
      )
    )


  } else {

    logout <- NULL

  }

  out <- list(
    docs,
    tour,
    refresh,
    help,
    disclaimer,
    contact,
    logout
  ) %>% purrr::compact()

  return(out)

}

#' Header Buttons Server Module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param parent_session shiny session of the parent environment where this
#'   module is called from. Only used if \code{include_tour} is TRUE.
#' @param tour_steps list of 'steps' for tour. Passed to [rintrojs::introjs()].
#' @inheritParams header_buttons_ui
#' @param help_path character - if \code{include_help} is TRUE;
#'  specify the path to the 'help' R Markdown document to utilize when button
#'  is pressed.
#' @param disclaimer_path character - if \code{include_disclaimer} is TRUE;
#'  specify the path to the 'disclaimer' R Markdown document to utilize when
#'  button is pressed.
#'
#' @return server
#' @export
#' @importFrom htmltools div
#' @importFrom rintrojs introjs
#' @importFrom shiny observeEvent showModal modalDialog includeMarkdown icon
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyWidgets confirmSweetAlert
header_buttons <- function(
  input, output, session,
  parent_session = NULL,
  include_tour = TRUE,
  tour_steps = NULL,
  include_refresh = TRUE,
  include_help = TRUE,
  include_disclaimer = TRUE,
  help_path = fs::path_package("propalloc", "reports/help.Rmd"),
  disclaimer_path = fs::path_package("propalloc", "reports/disclaimer.Rmd")
) {

  shiny::observeEvent(input$docs, {
    shiny::showModal(
      shiny::modalDialog(title = icon_text("file", "Allocation Documentation"),
                         easyClose = TRUE,
                         fade = TRUE,
                         size = "l",
                         htmltools::tags$iframe(
                           src = "www/bb/allocation.html",
                           width = "100%",
                           height = "630px",
                           scrolling = "auto",
                           frameborder = 0
                         )
      )
    )
  })

if (include_refresh) {

  shiny::observeEvent(input$refresh, {

    shinyWidgets::confirmSweetAlert(
      session = session,
      inputId = session$ns("confirmrefresh"),
      title = "Confirm Application Refresh?",
      text = "All progress will be lost.",
      type = "question",
      btn_labels = c("Cancel", "Confirm"),
      closeOnClickOutside = TRUE
    )

  })

  shiny::observeEvent(input$confirmrefresh, {

    if (isTRUE(input$confirmrefresh)) session$reload()

  })


}

if (include_help) {

  shiny::observeEvent(input$help, {
    shiny::showModal(
      shiny::modalDialog(title = icon_text("info-circle", "Help"),
                         easyClose = TRUE,
                         fade = TRUE,
                         size = "l",
                         shiny::includeMarkdown(help_path)
      )
    )
  })

}

if (include_disclaimer) {

  shiny::observeEvent(input$disclaimer, {
    shiny::showModal(
      shiny::modalDialog(title = icon_text("warning", "Disclaimer"),
                         easyClose = TRUE,
                         fade = TRUE,
                         size = "l",
                         shiny::includeMarkdown(disclaimer_path)
      )
    )
  })

}

if (include_tour && !is.null(tour_steps)) {

  shiny::observeEvent(input$tour, {

    shinydashboard::updateTabItems(
      session = parent_session, "sidebar_menus", "inputs"
    )

    rintrojs::introjs(
      session,
      options = list(
        steps = tour_steps
      )
    )
  })

}


}
