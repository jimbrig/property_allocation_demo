#' Relativities tab module - UI
#'
#' @param id shiny namespace ID
#'
#' @return HTML
#' @export
#'
#' @importFrom DT DTOutput
#' @importFrom htmltools br div tags hr
#' @importFrom shiny NS tabPanel fluidRow column
#' @importFrom shinycustomloader withLoader
rels_tab_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = icon_text("sliders-h", " Relativities"),
    width = 12,
    flucol(
      width = 12,
      htmltools::div(
        style = "display: inline; align: pull-left",
        shiny::actionButton(ns("tab_tour"),
                            "Walkthrough",
                            icon = shiny::icon("shoe-prints")),
        actionButton(ns("table_features"),
                     label = "View Table Features",
                     icon = icon("info"))
      )
    ),
    htmltools::hr(),
    htmltools::div(
      id = "tour_all_tables",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          htmltools::br(),
          htmltools::div(
            class = "text-center",
            htmltools::tags$h4("Business Unit Relativity:")
          ),
          htmltools::div(
            id = "tour_bu_rels_table",
            DT::DTOutput(ns("bu_rels_table")) %>%
              shinycustomloader::withLoader()
          ),
          htmltools::br(),
          htmltools::hr(),
          htmltools::br(),
          htmltools::div(
            class = "text-center",
            htmltools::tags$h4("All Other Peril Relativity:")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          htmltools::div(
            class = "text-center",
            htmltools::tags$h5("Sprinkler Tiers")
          ),
          htmltools::div(
            id = "tour_sprinkler_tier_rels_table",
            DT::DTOutput(ns("sprinkler_tier_rels_table")) %>%
              shinycustomloader::withLoader()
          )
        ),
        shiny::column(
          6,
          htmltools::div(
            class = "text-center",
            htmltools::tags$h5("Combustible")
          ),
          htmltools::div(
            id = "tour_combustible_rels_table",
            DT::DTOutput(ns("combustible_rels_table")) %>%
              shinycustomloader::withLoader()
          )
        )
      )
    )
  )
}

#' Relativities tab module - Server
#'
#' @param input input
#' @param output output
#' @param session session
#' @param initial_rels bu_rels, sprinkler_tier_rels, combustible_rels
#' @param dictionary dictionary
#' @param output_id output id corresponding to the table generated.
#'
#' @return reactive relativity data
#' @export
#'
#' @importFrom DT renderDT datatable formatRound dataTableProxy editData
#' @importFrom shiny reactiveValues reactive req observeEvent
#' @importFrom utils str
#' @importFrom dplyr mutate_if
rels_tab <- function(input, output, session, initial_rels, dictionary,
                     output_id = c("bu_rels_table",
                                   "sprinkler_tier_rels_table",
                                   "combustible_rels_table")) {

  rels_data <- shiny::reactiveValues()
  rels_data$r_data <- initial_rels
  display_data <- initial_rels %>%
    apply_labels(dict = dictionary, dataset_name = "rels")

  fields_to_format <- names(display_data)[-1]

  rels_data$display_data <- display_data

  table_prep <- shiny::reactive({
    rels_data$display_data %>%
      dplyr::mutate_if(is.numeric, round, 3)
  })

  output[[output_id]] <- DT::renderDT({

    shiny::req(table_prep())

    out <- table_prep()

    cap <- "Relativities"

    DT::datatable(
      out,
      rownames = FALSE,
      caption = cap,
      editable = TRUE,
      selection = "none",
      class = "compact stripe row-border nowrap",
      style = "bootstrap",
      extensions = c("Buttons"),
      # filter = "top",

      options = list(
        scrollX = TRUE,
        dom = 'Brt',
        # pageLength = 50,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        ),
        buttons = list(
          'copy', 'print',
          list(
            extend = "collection",
            buttons = c('excel', 'pdf'),
            text = "Download",
            title = paste0("Relativities-", Sys.Date())
          )
        )
      )
    ) %>%
      DT::formatRound(fields_to_format, digits = 3)

  })

  rels_table_proxy <- DT::dataTableProxy(output_id)

  shiny::observeEvent(input$rels_table_cell_edit, {

    info <- input$rels_table_cell_edit

    cli::cli_alert_info(cli::col_cyan("Change detected on Relativity Table: {output_id}:"))
    utils::str(info)
    cli::cli_rule()

    new_display_data <- DT::editData(
      table_prep(),
      info,
      proxy = rels_table_proxy,
      rownames = FALSE,
      resetPaging = FALSE
    )

    new_r_data <- new_display_data %>% reverse_labels(dict = dictionary, dataset_name = "rels")

    rels_data$r_data <- new_r_data
    rels_data$display_data <- new_display_data

  })

  shiny::observeEvent(input$tab_tour, {
    rintrojs::introjs(
      session,
      options = list(
        steps = list(
          list(
            element = "#tour_all_tables",
            intro = "In this tab, you can change the relativities that are applied during the allocation process.<br/><br/>
            A relativity gives more or less weight to an entity during the allocation depending on the attributes of the entity.
            For example, if an entity has a sprinkler system installed, then they may be less prone to significant fire damage.
            The entity may then have a lower sprinkler tier than other entities, and such a tier may attract a smaller relativity,
            ultimately leading to less allocated premium than the entity would otherwise be allocated if they do not have sprinklers installed.<br/><br/>
            The three relativity types that can be set include: Business Unit, Sprinkler Tier, and Combustible relativies.<br/><br/>
            Click next to walk through each of them."
          ),

          list(
            element = "#tour_bu_rels_table",
            intro = "Business Unit relativies refer to a relativity applied to different business units.<br/><br/>
            In this specialized table, each row represents a different business unit, while each column represents the type of coverage to which the relativity applies."
          ),

          list(
            element = "#tour_sprinkler_tier_rels_table",
            intro = "The Sprinkler Tier relativities fall under the All Other Peril (AOP) allocation part of the allocation process.
            This means that these relativities are applied only after Catastraphic premiums have been allocated.
            Note that the Business Unit relativity table also contains an AOP relativity column.<br/><br/>
            The Sprinkler Tier field in this table must be a unique identifier that should cover all possible values of Sprinkler Tiers found in
            the SOV tab and table."
          ),

          list(
            element = "#tour_combustible_rels_table",
            intro = "The Combustible relativities, like Sprinkler Tier relativities, fall under the All Other Peril (AOP) allocation part of the allocation process.<br/><br/>
            The Combustible? field in this table must be a unique identifier that should cover all possible values of 'AOP Combustible' found in
            the SOV tab and table."
          )
        )
      )
    )
  })

  shiny::observeEvent(input$table_features, {

    shiny::showModal(
      shiny::modalDialog(
        title = icon_text("table", "Table Features Overview:"),
        easyClose = TRUE,
        size = "l",
        htmltools::tags$iframe(
          src = "www/table_features.html",
          width = "100%",
          height = "630px",
          scrolling = "auto",
          frameborder = 0
        )
      )
    )

  })

  rels_out <- shiny::reactive({
    rels_data$r_data
  })

  rels_out
}
