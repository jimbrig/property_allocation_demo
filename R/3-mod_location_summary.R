#' Location Summary Menu UI
#'
#' @param id ns id
#'
#' @export
#' @return HTML
#'
#' @importFrom dplyr mutate select
#' @importFrom DT DTOutput
#' @importFrom htmltools tagList div h3 br hr tags
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom shiny NS fluidRow column tabPanel textOutput actionButton
#' @importFrom shinycustomloader withLoader
#' @importFrom shinydashboard tabItem tabBox valueBoxOutput box
#' @importFrom shinyjs extendShinyjs
#' @importFrom stringr str_to_title
location_summary_menu_ui <- function(id, sov) {

  ns <- shiny::NS(id)

  js <- 'shinyjs.winprint = function(){
window.print();
}'

  sov <- sov %>%
    dplyr::mutate(state = ifelse(is.na(.data$state), "Non USA", .data$state))

  filter_inputs <- sov %>%
    dplyr::select(entity_id, bu:department) %>%
    names() %>%
    purrr::map(function(x) {

      if (x == "bu") {
        lab <- "Business Unit"
      } else if (x == "location") {
        lab = "Unit"
      } else if (x == "entity_id") {
        lab = "Location"
      } else {
        lab <- stringr::str_to_title(x)
      }

      choices <- pull_unique(sov, x, names = FALSE) %>%
        sort_numeric() %>%
        rlang::set_names(
          dictionary$value_label[match(., dictionary$value)]
        )

      picker(
        ns(x), label = lab, choices = choices, multiple = TRUE,
        inline = FALSE #options =
      )
    }) %>%
    rlang::set_names(dplyr::select(sov, entity_id, bu:department) %>% names())

  htmltools::tagList(
    shinyjs::extendShinyjs(text = js, functions = c()),

    shinydashboard::tabItem(
      tabName = ns(id),
      shiny::fluidRow(
        shiny::column(
          12,
          shinydashboard::tabBox(
            id = ns("tab"),
            title = icon_text("balance-scale-left", " Location Summary"),
            width = 9,

            shiny::tabPanel(
              title = icon_text("bullseye", " Driver Summary Drilldown"),
              width = 12,
              flucol(
                width = 12,
                htmltools::div(
                  class = "text-center",
                  flucol(
                    htmltools::h3("PREMIUMS")
                  )
                ),
                htmltools::div(
                  shiny::fluidRow(
                    id = ns("valboxes"),
                    shinydashboard::valueBoxOutput(ns("valbox1"), 4),
                    shinydashboard::valueBoxOutput(ns("valbox2"), 4),
                    shinydashboard::valueBoxOutput(ns("valbox3"), 4)
                  )
                ),
                htmltools::div(
                  class = "text-center",
                  flucol(
                    htmltools::h3("RATES")
                  )
                ),
                htmltools::div(
                  shiny::fluidRow(
                    id = ns("valboxes2"),
                    shinydashboard::valueBoxOutput(ns("valbox4"), 4),
                    shinydashboard::valueBoxOutput(ns("valbox5"), 4),
                    shinydashboard::valueBoxOutput(ns("valbox6"), 4)
                  )
                ),
                htmltools::hr(),
                flucol(
                  width = 12,
                  # flucol(width = 12,
                  #        align = "center",
                  #        htmltools::h3(
                  #          htmltools::tags$em(
                  #              "Prior Total Premium + Net Changes = Current Total Premium"
                  #            ), htmltools::br(),
                  #          shiny::textOutput(ns("written_summary"))
                  #        )
                  # ),
                  DT::DTOutput(ns("analysis_table")) %>%
                    shinycustomloader::withLoader()
                ),
                htmltools::hr()
              )
            ),

            shiny::tabPanel(
              title = icon_text("database", " Individual Data"),
              width = 12,
              flucol(
                width = 12,
                DT::DTOutput(ns("individual_analysis_table")) %>%
                  shinycustomloader::withLoader()
              )
            )
          ),

          shinydashboard::box(
            title = icon_text("filter", "Filters"),
            width = 3,
            flucol(
              width = 12,
              htmltools::div(
                class = "text-center",
                filter_inputs[["entity_id"]],
                filter_inputs[["bu"]],
                filter_inputs[["region"]],
                filter_inputs[["country"]],
                # filter_inputs[["state"]],
                # filter_inputs[["division"]],
                filter_inputs[["location"]],
                filter_inputs[["department"]],
                htmltools::hr(),
                shiny::actionButton(ns("print"), "Print Page")
              )
            )
          )
        )
      )
    )
  )

}

#' Location Summary Menu Module - Server
#'
#' @param input input
#' @param output output
#' @param session session
#' @param allocation_data The \code{allocation_data} list of reactive data.frames
#'   returned from [allocation_menu()] 's server module.
#'
#' @return driver summary reactive list
#' @export
#' @importFrom dplyr filter mutate transmute group_by
#' @importFrom DT renderDT datatable formatCurrency
#' @importFrom htmltools tags
#' @importFrom scales dollar
#' @importFrom shiny observeEvent reactiveValues reactive renderText req
#' @importFrom shinydashboard renderValueBox valueBox
#' @importFrom shinyjs js
location_summary_menu <- function(input, output, session,
                                  allocation_data) {

  shiny::observeEvent(input$print, {
    shinyjs::js$winprint()
  })

  analysis_rv <- shiny::reactiveValues()

  shiny::observeEvent(list(
    input$entity_id,
    input$bu,
    input$region,
    input$country,
    input$state,
    input$division,
    input$location,
    input$department,
    allocation_data$allocation_data_full()
  ), {

    # browser()

    analysis_rv$aggregate_summary_data <- allocation_data$allocation_data_full() %>%
      dplyr::filter(
        entity_id %in% input$entity_id,
        bu %in% input$bu,
        region %in% input$region,
        country %in% input$country,
        # state %in% input$state,
        # division %in% input$divison,
        location %in% input$location,
        department %in% input$department
      ) %>%
      prepare_driver_summary(NULL)

    analysis_rv$aggregate_summary_data_display <- analysis_rv$aggregate_summary_data$driver_summary %>%
      dplyr::mutate(total = analysis_rv$aggregate_summary_data$final_premium_change) %>%
      t() %>%
      as.data.frame() %>%
      dplyr::transmute(driver = rownames(.),
                       impact = V1) %>%
      apply_labels(dict = dictionary,
                   dataset_name = "driver_summary")

    analysis_rv$individual_analysis_data <- analysis_rv$aggregate_summary_data$driver_summary_individual

    analysis_rv$individual_analysis_data_display <- analysis_rv$individual_analysis_data %>%
      arrange_by_entity() %>%
      apply_labels(dict = dictionary,
                   dataset_name = "individual_driver")
  })

  aggregate_summary_prep <- shiny::reactive({
    analysis_rv$aggregate_summary_data_display
  })

  # output$written_summary <- shiny::renderText({
  #   summary_df <- c(final_premium_prior = analysis_rv$aggregate_summary_data$final_premium_prior,
  #                   final_premium_change = abs(analysis_rv$aggregate_summary_data$final_premium_change),
  #                   final_premium_current = analysis_rv$aggregate_summary_data$final_premium_current) %>%
  #     scales::dollar()
  #
  #     # htmltools::br(),
  #     paste0(
  #       summary_df[["final_premium_prior"]], " ",
  #       ifelse(analysis_rv$aggregate_summary_data$final_premium_change < 0, "- ", "+ "),
  #       summary_df[["final_premium_change"]], " = ",
  #       summary_df[["final_premium_current"]]
  #     )
  # })

  output$analysis_table <- DT::renderDT({

    shiny::req(aggregate_summary_prep())

    out <- aggregate_summary_prep()

    DT::datatable(
      out,
      rownames = FALSE,
      editable = FALSE,
      selection = "none",
      class = "compact stripe row-border nowrap",
      style = "bootstrap",
      extensions = c("Buttons"),
      filter = "none",
      options = list(
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        dom = 'Brt',
        lengthChange = FALSE,
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
            title = paste0("Rates-", Sys.Date())
          )
        ),
        initComplete = DT::JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '150%',
                          });
                        }
                    ")
      )
    ) %>%
      DT::formatCurrency(c("Change in Premium Attributable to Driver"), digits = 0) %>%
      DT::formatStyle(columns = 1:2, fontSize = '125%')
  })


  individual_analysis_prep <- shiny::reactive({
    analysis_rv$individual_analysis_data_display
  })

  output$individual_analysis_table <- DT::renderDT({

    shiny::req(individual_analysis_prep())

    out <- individual_analysis_prep() %>%
      dplyr::mutate(Location = as.numeric(extract_number(Location))) %>%
      dplyr::arrange(Location) %>%
      dplyr::mutate(Location = as.factor(Location))

    DT::datatable(
      out,
      rownames = FALSE,
      editable = FALSE,
      selection = "none",
      class = "compact stripe row-border nowrap",
      style = "bootstrap",
      extensions = c("Buttons"),
      filter = "top",

      options = list(
        scrollX = TRUE,
        dom = 'BRfrltpi',
        pageLength = 50,
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
            title = paste0("Rates-", Sys.Date())
          )
        )
      )
    ) %>%
      DT::formatCurrency(2:12, digits = 0) %>%
      DT::formatString(c("Location"), prefix = "Location ")
  })

  summary_data <- shiny::reactive({
    allocation_data$allocation_data_full() %>%
      dplyr::filter(
        bu %in% input$bu,
        region %in% input$region,
        country %in% input$country,
        # state %in% input$state,
        # division %in% input$divison,
        location %in% input$location,
        department %in% input$department
      ) %>%

      dplyr::group_by(location)
  })


  output$valbox1 <- shinydashboard::renderValueBox({

    shiny::req(nrow(individual_analysis_prep()) > 0)

    val <- analysis_rv$aggregate_summary_data$final_premium_current %>%
      format_round_dollar()

    shinydashboard::valueBox(
      value = val,
      subtitle = toupper("Current 2021 Final Premium"),
      icon = htmltools::tags$i(
        class = "fa fa-money",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "navy",
      width = 3
    )
  })

  output$valbox2 <- shinydashboard::renderValueBox({

    shiny::req(nrow(individual_analysis_prep()) > 0)

    val <- analysis_rv$aggregate_summary_data$final_premium_prior %>%
      format_round_dollar()

    shinydashboard::valueBox(
      value =  val,
      subtitle = toupper("Prior 2020 Final Premium"),
      icon = htmltools::tags$i(
        class = "fa fa-money",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "navy",
      width = 3
    )
  })

  output$valbox3 <- shinydashboard::renderValueBox({

    shiny::req(nrow(individual_analysis_prep()) > 0)

    # browser()

    val_dol <- analysis_rv$aggregate_summary_data$final_premium_change %>%
      format_round_dollar()

    val_pct <- (analysis_rv$aggregate_summary_data$final_premium_change_pc * 100) %>%
      prettyNum(big.mark = ",", digits = 0)

    if (as.numeric(val_pct) > 0) {
      up_down <- "UP "
    } else {
      up_down <- "DOWN "
    }

    shinydashboard::valueBox(
      value =  val_dol,
      subtitle = paste0(up_down, val_pct, "% SINCE PRIOR"),
      icon = htmltools::tags$i(
        class = "fa fa-exchange",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "navy",
      width = 3
    )
  })

  output$valbox4 <- shinydashboard::renderValueBox({

    shiny::req(nrow(individual_analysis_prep()) > 0)

    val <- analysis_rv$aggregate_summary_data$implied_rate_current %>%
      prettyNum(big.mark = ",", digits = 3)

    shinydashboard::valueBox(
      value =  val,
      subtitle = toupper("Implied Rate 2021"),
      icon = htmltools::tags$i(
        class = "fa fa-heartbeat",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "aqua",
      width = 3
    )
  })

  output$valbox5 <- shinydashboard::renderValueBox({

    shiny::req(nrow(individual_analysis_prep()) > 0)

    val <- analysis_rv$aggregate_summary_data$implied_rate_prior %>%
      prettyNum(big.mark = ",", digits = 3)

    shinydashboard::valueBox(
      value =  val,
      subtitle = toupper("Implied Rate 2020"),
      icon = htmltools::tags$i(
        class = "fa fa-heartbeat",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "aqua",
      width = 3
    )
  })

  output$valbox6 <- shinydashboard::renderValueBox({

    shiny::req(nrow(individual_analysis_prep()) > 0)

    # browser()

    val_pct <- (analysis_rv$aggregate_summary_data$implied_rate_change_pc * 100) %>%
      prettyNum(big.mark = ",", digits = 0)

    if (as.numeric(val_pct) > 0) {
      up_down <- "UP "
    } else {
      up_down <- "DOWN "
    }

    shinydashboard::valueBox(
      value = paste0(up_down, val_pct, "%"),
      subtitle = "CHANGE SINCE PRIOR",
      icon = htmltools::tags$i(
        class = "fa fa-exchange",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "aqua",
      width = 3
    )
  })


  # output$driver_summary <- shiny::renderUI({
  #
  #
  #
  #   browser()
  #
  #   tibble::tibble(
  #     final_premium = ,
  #     prior_premium = analysis_rv$aggregate_summary_data$final_premium_current
  #
  #
  #
  #   merged_data <- analysis_rv$individual_analysis_data %>%
  #     dplyr::left_join(allocation_data$allocation_data_full() %>%
  #                        dplyr::select(location, final_premium, prior_allocated,
  #                                      percent_))
  #
  #   htmltools::tagList(
  #     flucol(
  #       12,
  #
  #
  #     )
  #   )
  # })

  return(list(
    driver_summary = reactive({ analysis_rv$aggregate_summary_data })
  ))
}

