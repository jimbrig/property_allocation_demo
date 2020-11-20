#' flip_box
#'
#' creates a rotating card with fron and back sides for various UI content.
#'
#' @param id id for the card - will be passed to [flip_button_front()]
#'   and [flip_button_back()] as well.
#' @param front_content UI for front of card - preferably wrapped in a div
#' @param back_content UI for back of card - preferably wrapped in a div
#' @param front_text text for button on front (default is "More Info.")
#' @param back_text text for button on back (default is "Back to Front")
#'
#' @return an [htmltools::tagList()].
#'
#' @keywords utils shiny UI
#'
#' @details This function utilizes external CSS and JS included as external
#'   dependencies contained within the
#'   \code{system.file("app", "www", package = "propalloc")} resources
#'   folder.
#'
#' @export
#'
#' @importFrom htmltools tagList div tags
#' @importFrom shiny singleton
#'
#' @examples
#' library(shiny)
#' library(shinydashboard)
#' library(propalloc)
#' shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'       flip_box(
#'         id = "id",
#'         front_content = div(
#'           class = "text-center",
#'           img(
#'             src = "https://image.flaticon.com/icons/svg/149/149076.svg",
#'             height = "300px",
#'             width = "100%"
#'           )
#'         ),
#'         back_content = div(
#'           class = "text-center",
#'           height = "300px",
#'           width = "100%",
#'           h1("Details...."),
#'           p("More information....")
#'         )
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'   }
#' )

flip_box <- function(id = "id",
                     ns = NULL,
                     front_content,
                     back_content,
                     front_text = "More Info",
                     back_text = "Back to Front") {

  if (is.null(id)) stop("card id cannot be null and must be unique")

  if (!is.null(ns)) ns <- shiny::NS

  front_content_ <- htmltools::tagList(
    front_content,
    htmltools::div(
      class = "text-center",
      id = "go_to_back",
      flip_button_front("id", front_text)
    )
  )

  back_content_ <- htmltools::tagList(
    back_content,
    div(
      class = "text-center",
      id = "go_to_front",
      flip_button_back("id", back_text)
    )
  )

  htmltools::tagList(
    htmltools::div(
      class = "rotate-container",
      id = id,
      htmltools::tags$div(
        class = paste0("card-front-", id),
        style = "background-color: white;",
        front_content_
      ),
      htmltools::tags$div(
        class = paste0("card-back-", id),
        style = "background-color: white;",
        back_content_
      ),
      htmltools::tagList(
        shiny::singleton(
          htmltools::tags$head(
            htmltools::tags$style(
              flip_box_style()
            ),
            htmltools::tags$script(
              flip_box_script_1(id)
            ),
            htmltools::tags$script(
              flip_box_script_2()
            )
          )
        )
      )
    )
  )
}


# helpers -----------------------------------------------------------------

flip_box_script_1 <- function(id) {

  paste0(
    "$(function() {

    // For card rotation
    $('#btn-flip-front-", id, "').click(function(){
    $('.card-front-", id, "').addClass(' rotate-card-front-", id, "');
    $('.card-back-", id, "').addClass(' rotate-card-back-", id, "');
    });

    $('#btn-flip-back-", id, "').click(function(){

    $('.card-front-", id, "').removeClass(' rotate-card-front-", id, "');
    $('.card-back-", id, "').removeClass(' rotate-card-back-", id, "');
    });

  });"
  )
}

flip_box_script_2 <- function(id) {
  "$(function() {
    $('#go_to_back').click(function(){
    $('#go_to_back').hide()
    $('#go_to_front').show()
    });

    $('#go_to_front').click(function(){
    $('#go_to_front').hide()
    $('#go_to_back').show()
  });
 });"
}

flip_box_style <- function() {
  ".rotate-container {
  position: relative;
}

.rotate-container .card-front-id, .rotate-container .card-back-id {
  width: 100%;
  height: 100%;
  -webkit-transform: perspective(600px) rotateY(0deg);
  transform: perspective(600px) rotateY(0deg);
  -webkit-backface-visibility: hidden;
  backface-visibility: hidden;
  transition: all 0.5s linear 0s;
}

.card .card-background-id {
  height: 1em
}

.rotate-container .card-back-id {
  -webkit-transform: perspective(1600px) rotateY(180deg);
  transform: perspective(1600px) rotateY(180deg);
  position: absolute;
  top: 0; left: 0; right: 0;
}

.rotate-container .rotate-card-front-id {
  -webkit-transform: perspective(1600px) rotateY(-180deg);
  transform: perspective(1600px) rotateY(-180deg);
}

.rotate-container .rotate-card-back-id {
  -webkit-transform: perspective(1600px) rotateY(0deg);
  transform: perspective(1600px) rotateY(0deg);
}
"
}


#' flip_button_front
#'
#' @param id the flipbox id
#' @param text the button text
#'
#' @rdname flip_box
#'
#' @export
#' @importFrom htmltools tags
flip_button_front <- function(id, text) {
  htmltools::tags$button(
    id = paste0("btn-flip-front-", id),
    class = "btn btn-primary btn-rotate",
    text
  )
}

#' flip_button_back
#'
#' @param id the flipbox id
#' @param text the button text
#'
#' @rdname flip_box
#'
#' @importFrom htmltools tags
#'
#' @export
flip_button_back <- function(id, text) {
  htmltools::tags$button(
    id = paste0("btn-flip-back-", id),
    class = "btn btn-primary btn-rotate",
    text
  )
}


#' #' flip_box_demo
#' #'
#' #' runs an internal flip_box_demo application.
#' #'
#' #' @return shinyApp
#' #' @export
#' #'
#' #' @importFrom shiny runApp
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(shinydashboardPlus)
#' #' flip_box_demo()
#' #' }
#' #' @importFrom shiny runApp
#' flip_box_demo <- function() {
#'
#'   shiny::runApp(appDir = system.file("flip_box_demo", "app.R", package = "shinydashboardPlus"))
#'
#' }
