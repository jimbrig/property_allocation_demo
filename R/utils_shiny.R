#' Creates a dropdown menu specific for contacts
#'
#' @param ... contact items to put into dropdown
#'
#' @return menu
#' @export
#' @importFrom htmltools tags div
contact_menu <- function(...){

  items <- c(list(...))

  htmltools::tags$li(
    class = "dropdown",
    htmltools::tags$a(
      href = "#",
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      htmltools::div(
        htmltools::tags$i(
          class = "fa fa-phone"
        ),
        "Contact",
        style = "display: inline"
      ),
      htmltools::tags$ul(
        class = "dropdown-menu",
        items)
    )
  )
}

#' Contact Item
#'
#' Creates an item to be placed in a contact dropdownmenu.
#'
#' @param name Name
#' @param role Role
#' @param phone Phone
#' @param email Email
#'
#' @return contact menu item
#' @export
#' @importFrom htmltools tagList tags a h4 h5
#' @importFrom shiny icon
contact_item <- function(name = "First Name, Last Name",
                         role = "Role",
                         phone = "###-###-####",
                         email = "first.last@oliverwyman.com"){

  htmltools::tagList(
    htmltools::tags$li(
      htmltools::a(
        href = "#",
        htmltools::h4(htmltools::tags$b(name)),
        htmltools::h5(htmltools::tags$i(role))
      )
    ),
    htmltools::tags$li(
      htmltools::a(
        shiny::icon("envelope"), href = paste0("mailto:", email),
        email
      )
    ),
    htmltools::tags$li(
      htmltools::a(shiny::icon("phone"), href = "#", phone)
    ),
    htmltools::tags$hr()
  )

}

#' Fluid Column - Shiny fluidRow + Column
#'
#' @param ... elements to include within the flucol
#' @param width width
#' @param offset offset
#'
#' @return A column wrapped in fluidRow
#' @export
#'
#' @importFrom shiny fluidRow column
flucol <- function(..., width = 12, offset = 0) {

  if (!is.numeric(width) || (width < 1) || (width > 12))
    stop("column width must be between 1 and 12")

  shiny::fluidRow(
    shiny::column(
      width = width,
      offset = offset,
      ...
    )
  )
}

#' Insert Logo
#'
#' @param file file
#' @param style style
#' @param width width
#' @param ref ref
#'
#' @return tag
#' @export
#' @importFrom shiny tags img
insert_logo <- function(file,
                        style = "background-color: #FFF; width: 100%; height: 100%;",
                        width = NULL,
                        ref = "#"){

  shiny::tags$div(
    style = style,
    shiny::tags$a(
      shiny::img(
        src = file,
        width = width
      ),
      href = ref
    )
  )

}

#' Icon Text
#'
#' Creates an HTML div containing the icon and text.
#'
#' @param icon fontawesome icon
#' @param text text
#'
#' @return HTML div
#' @export
#'
#' @examples
#' icon_text("table", "Table")
#'
#' @importFrom htmltools tagList
#' @importFrom shiny icon
icon_text <- function(icon, text) {

  if (is.character(icon)) i <- shiny::icon(icon) else i <- icon
  t <- paste0(" ", text)
  htmltools::tagList(div(i, t))

}

#' Download Button without opening up new window in browser when downloaded
#'
#' @param outputId output ID
#' @param label label
#' @param class class
#' @param icon icon
#' @param ... dots
#'
#' @return download bttn
#' @export
downloadButtonEdit <- function (outputId, label = "Download", class = NULL, icon = NULL, ...)
{
  aTag <-
    tags$a(
      id = outputId,
      class = paste("btn btn-default shiny-download-link",
                    class),
      href = "",
      target = NA, # NA here instead of _blank
      download = NA,
      icon(icon),
      label,
      ...
    )
}

#' Picker Input
#'
#' @param id id
#' @param label label
#' @param choices choices
#' @param selected selected
#' @param multiple multiple
#' @param count_threshold count threshold
#' @param choice_options choce opts
#' @param ... passed to [shinyWidgets::pickerInput()]
#'
#' @return HTML
#' @export
picker <- function(id, label, choices, selected = choices, multiple = TRUE,
                   count_threshold = 3, choice_options = NULL, ...) {

  shinyWidgets::pickerInput(
    id,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    choicesOpt = choice_options,
    options = shinyWidgets::pickerOptions(
      actionsBox = TRUE,
      liveSearch = TRUE,
      selectedTextFormat = paste0("count > ", count_threshold),
      countSelectedText = "{0} / {1} Selected",
      liveSearchPlaceholder = "Search...",
      virtualScroll = 100,
      dropupAuto = FALSE,
      dropdownAlignRight = TRUE
    ),
    ...
  )

}

