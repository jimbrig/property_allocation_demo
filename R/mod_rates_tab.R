#' Rates Tab Module - UI
#'
#' @param id Namespace ID
#'
#' @return HTML
#' @export
#' @importFrom DT DTOutput
#' @importFrom highcharter highchartOutput
#' @importFrom htmltools div hr
#' @importFrom shiny NS tabPanel actionButton icon selectInput
#' @importFrom shinycustomloader withLoader
#' @importFrom shinydashboard tabBox
rates_tab_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = icon_text("percentage", " Rates"),
    width = 12,

    flucol(
      width = 12,

      htmltools::div(
        style = "display: inline; align: pull-left",
        shiny::actionButton(ns("tab_tour"),
                            "Walkthrough",
                            icon = shiny::icon("shoe-prints")),
        shiny::actionButton(ns("table_features"),
                            label = "View Table Features",
                            icon = icon("info"))
      ),

      htmltools::hr(),

      shinydashboard::tabBox(
        width = 12,
        # side = "right",
        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("table"),
          flucol(
            width = 12,
            htmltools::div(
              align = "center",
              id = "tour_rates_table",
              DT::DTOutput(ns("rates_table"), width = "75%") %>%
                shinycustomloader::withLoader()
            )
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("line-chart"),
          flucol(
            width = 12,
            shiny::selectInput(ns("coverage"), "Coverage:",
                               choices = list(
                                 "All Other Peril" = "aop",
                                 "Catastrophe: Earthquake" = "cat_eq",
                                 "Catastrophe: Wind" = "cat_wind",
                                 "Catastrophe: Flood" = "cat_flood",
                                 "Terrorism" = "terrorism"
                               ),
                               selected = "aop",
                               multiple = FALSE),
            highcharter::highchartOutput(ns("rates_bubble_chart"),
                                         height = "800px") %>%
              shinycustomloader::withLoader()
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("info-circle"),
          flucol(
            width = 12,
            htmltools::tags$h3("Rates Data Summary Report: "),
            htmltools::tags$hr()#,
            # shiny::uiOutput(ns("report")) %>%
              # shinycustomloader::withLoader()
          )
        )
      )
    )
  )

}

#' Rates Tab Module - Server
#'
#' @param input input
#' @param output output
#' @param session session
#' @param initial_rates initial rates
#' @param dictionary dictionary
#'
#' @importFrom cli cli_rule
#' @importFrom dplyr mutate_if filter group_by summarise ungroup rename mutate left_join glimpse
#' @importFrom DT renderDT datatable formatRound dataTableProxy editData
#' @importFrom highcharter renderHighchart highchart hc_chart hc_title hc_subtitle hc_xAxis hc_yAxis_multiples hc_legend hc_tooltip hc_exporting hc_plotOptions hc_add_series
#' @importFrom htmltools tags
#' @importFrom purrr map_dfr
#' @importFrom rintrojs introjs
#' @importFrom rlang set_names
#' @importFrom shiny reactiveValues reactive req observeEvent showModal modalDialog renderUI
#' @importFrom utils str
rates_tab <- function(input, output, session, initial_rates, dictionary) {

  rates_data <- shiny::reactiveValues()
  rates_data$r_data <- initial_rates
  rates_data$display_data <- initial_rates %>%
    apply_labels(dict = dictionary, dataset_name = "rates")

  table_prep <- shiny::reactive({
    rates_data$display_data %>%
      dplyr::mutate_if(is.numeric, round, 3)
  })

  output$rates_table <- DT::renderDT({

    shiny::req(table_prep())

    out <- table_prep()

    cap <- "Prior, Market, and Model Rates for Allocating Risk (Premium / TIV)"

    DT::datatable(
      out,
      rownames = FALSE,
      caption = cap,
      editable = TRUE,
      selection = "none",
      class = "compact stripe row-border nowrap",
      style = "bootstrap",
      extensions = c("Buttons"),
      filter = "top",
      fillContainer = FALSE,
      width = "75%",
      options = list(
        autoWidth = TRUE,
        dom = 'Brt',
        pageLength = nrow(out),
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
          ),
          list(
            extend = 'colvis',
            text = 'Edit Displayed Columns'
          )
        )
      )
    ) %>%
      DT::formatRound(c("Prior Rate", "Market Rate", "Model Rate"), digits = 3)

  })

  rates_table_proxy <- DT::dataTableProxy("rates_table")

  shiny::observeEvent(input$rates_table_cell_edit, {

    info <- input$rates_table_cell_edit
    utils::str(info)

    new_display_data <- DT::editData(
      table_prep(),
      info,
      proxy = rates_table_proxy,
      rownames = FALSE,
      resetPaging = FALSE
    )

    new_r_data <- new_display_data %>% reverse_labels(dict = dictionary, dataset_name = "rates")

    rates_data$r_data <- new_r_data
    rates_data$display_data <- new_display_data

  })

  shiny::observeEvent(input$tab_tour, {
    rintrojs::introjs(
      session,
      options = list(
        steps = list(
          list(
            element = "#tour_rates_table",
            intro = "There's just one input item in this tab - short and sweet!<br/><br/>
            Here, all TIV-to-premium rates are set out. These rates are the core of the allocation model:
            TIVs are multiplied by these rates to produce preliminary premiums.<br/><br/>
            SOME MORE WORDS ABOUT HOW THIS WORKS - PENDING FINALIZATION OF METHOD OF INPUT FOR THIS TABLE.<br/><br/>
            Make sure that you check out all the pages of the table by clicking through them at the bottom of the table."
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

  rates_out <- shiny::reactive({
    rates_data$r_data
  })

  # get TIV from SOV for bubbles:
  chart_prep <- shiny::reactive({

    get_tiv <- function(cov = c("aop", "cat_eq", "cat_wind", "cat_flood", "terrorism"),
                        dat = sov) {

      cov_col <- paste0(cov, "_coverage")
      id_col <- paste0(cov, "_id")

      dat %>%
        dplyr::filter(.data[[cov_col]] == TRUE) %>%
        dplyr::group_by(.data[[id_col]]) %>%
        dplyr::summarise(
          tiv = sum(tiv, na.rm = TRUE), .groups = "drop_last") %>%
        dplyr::ungroup() %>%
        dplyr::rename("id" = id_col) %>%
        dplyr::mutate(coverage = cov)

    }

    covs <- c("aop", "cat_eq", "cat_wind", "cat_flood", "terrorism") %>%
      rlang::set_names()
    tivs <- purrr::map_dfr(covs, get_tiv, dat = sov)

    out <- rates_data$r_data %>%
      dplyr::rename(
        coverage = rate_type,
        id = rate_id
      ) %>%
      dplyr::mutate(coverage = ifelse(coverage == "terror", "terrorism", coverage)) %>%
      dplyr::left_join(tivs, by = c("coverage", "id"))

    out

  })

  output$rates_bubble_chart <- highcharter::renderHighchart({
    # req(chart_prep(), input$coverage)
    # browser()


    hold <- chart_prep() %>%
      dplyr::filter(!is.na(.data$model_rate),
                    .data$model_rate != 0,
                    !is.na(.data$market_rate),
                    .data$market_rate != 0,
                    !is.na(.data$prior_rate),
                    .data$prior_rate != 0,
                    .data$coverage == input$coverage)

    x_cats <- pull_unique(hold, "id")
    names(x_cats) <- NULL
    subtit <- switch(input$coverage,
                     "aop" = "All Other Peril",
                     "cat_eq" = "Catastrophe: Earthquake",
                     "cat_wind" = "Catastrophe: Wind",
                     "cat_flood" = "Catastrophe: Flood",
                     "terrorism" = "Terrorism")

    highcharter::highchart() %>%
      highcharter::hc_chart(
        type = "column"
      ) %>%
      highcharter::hc_title(
        text = "Model, Market, & Prior Rates by ID",
        style = list(fontWeight = "bold")
      ) %>%
      highcharter::hc_subtitle(
        text = paste0("Selected Coverage: ", subtit),
        style = list(fontWeight = "bold")
      ) %>%
      highcharter::hc_xAxis(
        categories = x_cats,
        title = list(
          text = "Rate Identifier",
          style = list(fontWeight = 'bold')
        )
      ) %>%
      highcharter::hc_yAxis_multiples(
        list(
          title = list(
            text = "Rate (Premium / TIV)",
            style = list(fontWeight = 'bold')
          ),
          labels = list(format = '{value:.3f}')
        ),
        list(
          title = list(
            text = "Total Insured Value (TIV)",
            style = list(fontWeight = 'bold')
          ),
          opposite = TRUE,
          labels = list(format = '{value:,.0f}')
        )
      ) %>%
      highcharter::hc_legend(
        enabled = TRUE,
        align = 'center',
        verticalAlign = "top"
      ) %>%
      highcharter::hc_tooltip(
        headerFormat = "<b>Identifier: {point.x}:</b><br/>",
        shared = TRUE,
        crosshairs = TRUE
      ) %>%
      highcharter::hc_exporting(
        enabled = TRUE,
        filename = "Chart",
        formAttributes = list(target = '_blank'),
        buttons = hc_btn_options(),
        sourceWidth = 1000,
        sourceHeight = 600
      ) %>%
      highcharter::hc_plotOptions(
        column = list(
          dataLabels = list(
            enabled = TRUE,
            color = "#000",
            formatter = data_labels_rate_formatter()
          )
        ),
        spline = list(
          marker = list(
            enabled = TRUE
          ),
          dataLabels = list(
            enabled = TRUE,
            formatter = data_labels_dollar_formatter()
          )
        )
      ) %>%
      highcharter::hc_add_series(
        data = hold$model_rate,
        name = "Model Rate",
        tooltip = list(
          pointFormat = tooltip_formatter_rate()
        )
      ) %>%
      highcharter::hc_add_series(
        data = hold$market_rate,
        name = "Market Rate",
        tooltip = list(
          pointFormat = tooltip_formatter_rate()
        )
      ) %>%
      highcharter::hc_add_series(
        data = hold$prior_rate,
        name = "Prior Rate",
        tooltip = list(
          pointFormat = tooltip_formatter_rate()
        )
      ) %>%
      highcharter::hc_add_series(
        data = hold$tiv,
        name = "TIV",
        tooltip = list(
          pointFormat = tooltip_formatter_dollar()
        ),
        type = "spline",
        yAxis = 1
      )

  })

  # output$report <- shiny::renderUI({

    # tmpfile <- tempfile(fileext = ".html")

    # flucol(
    #   width = 12,
    #   print(
    #     summarytools::dfSummary(
    #       rates_data$display_data,
    #       varnumbers = FALSE,
    #       valid.col = FALSE,
    #       justify = "c",
    #       graph.magnif = 1.5
    #     ),
    #     headings = TRUE,
    #     method = "render",
    #     bootstrap.css = FALSE,
    #     report.title = "Rates Data Summary Report:",
    #     footnote = "Created by the Oliver Wyman Actuarial Consulting Property Allocation Application.",
    #     file = tmpfile
    #   )
    # )
  # })



  shiny::observeEvent(rates_out(), {
    disp <- rates_out()
    cli::cli_rule("Rates Table")
    dplyr::glimpse(disp)
    cli::cli_rule()
  })

  rates_out
}

# name = "Model")
# spline = list(
#   marker = list(
#     enabled = TRUE
#   )
#   ) %>%
#   highcharter::hc_add_series(
#     list(
#     data = list(
#       y = hold$model_rate,
#       z = hold$tiv #,
#       # x = hold$id
#     ),
#     name = "Model Rate",
# tooltip =
#   )
#   )
#
# #   data = list(
#     x = hold$,
#     y = rates_data$r_data %>%
#       dplyr::filter(rate_type == "model") %>%
#       dplyr::pull(model_rate),
#     z = 100000
#   ),
#   name = "",
#   tooltip = list(pointFormat = '<span style="color:{point.color};font-weight:bold">\u25CF {series.name}:</span>{point.y:,.3f}<br/>')
# )

# dummy_df <- data.frame(x= 1:5, y = 4:8)
#
# highcharter::hchart(df, "line", hcaes(x = x, y = y))  %>%
#   highcharter::hc_title(text = "Dummy Plot", align = "center") %>%
#   highcharter::hc_add_theme(hc_theme_elementary())

# })



# data_list <- purrr::map(x_cats, function(x) {
#
#   hold <- hold %>%
#     dplyr::filter(.data$id == x)
#
#   y_vals <- list(
#     "model" = hold %>% dplyr::pull(.data$model_rate),
#     "market" = hold %>% dplyr::pull(.data$market_rate),
#     "prior" = hold %>% dplyr::pull(.data$prior_rate)
#   )
#
#   z_vals <- hold %>% dplyr::pull(.data$tiv)
#
#   data <- list(
#     x = rep(x, length(y_vals$model)),
#     y = y_vals$model,
#     z = z_vals
#   )
#
#   out <- list(
#     data = data,
#     name = x,
#     tooltip = list(pointFormat = '<span style="color:{point.color};font-weight:bold">\u25CF {series.name}:</span>{point.y:,.3f}<br/>')
#   )
#
#   return(out)
#
# })
#
# data <- hold %>%
#   split(.$id)

