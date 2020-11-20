
#' Renewal Costs Module UI
#'
#' @param id Namespace ID
#'
#' @return shinydashboard::tabBox HTML
#' @export
#' @importFrom DT dataTableOutput
#' @importFrom highcharter highchartOutput
#' @importFrom htmltools hr div h3 br
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS tabPanel icon actionButton fluidRow column
#' @importFrom shinycustomloader withLoader
#' @importFrom shinydashboard tabBox
renewal_costs_tab_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = icon_text("money-bill-wave", " Renewal Costs"),
    width = 12,
    flucol(
      width = 12,
      htmltools::hr(),
      shinydashboard::tabBox(
        id = ns("tabbox"),
        title = "Renewal Costs",
        width = 12,
        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("table"),
          flucol(
            width = 12,
            htmltools::div(
              style = "display: inline; align: pull-left",
              shiny::actionButton(ns("tab_tour"),
                                  "Walkthrough",
                                  icon = shiny::icon("shoe-prints")),
              shiny::actionButton(ns("table_features"),
                                  label = "View Table Features",
                                  icon = shiny::icon("info")),
              htmltools::div(
                id = "tour_reset_cost", # not documented in tour
                style = "display: inline-block; padding-left: 5px;",
                shiny::actionButton(
                  ns("reset"),
                  label = "Reset to Defaults",
                  icon = shiny::icon("refresh")
                )
              ),
              htmltools::div(
                id = "tour_add_cost", # not documented in tour
                style = "display: inline-block; padding-left: 5px;",
                shiny::actionButton(
                  ns("add_new"),
                  label = "Add New Cost",
                  class = "btn-success",
                  style = "color: #fff;",
                  icon = shiny::icon("plus")
                )
              )
            )
          ),
          flucol(
            width = 12,
            htmltools::h3(icon_text("money", "Costs to be Allocated:"), class = "text-center"),
            htmltools::br(),
            htmltools::div(
              id = "tour_renewal_cost_table",
              rhandsontable::rHandsontableOutput(
                ns("renewal_cost_table"),
                height = "200px"
              ) %>%
                shinycustomloader::withLoader()
            )
          ),

          flucol(
            width = 12,
            htmltools::h3(icon_text("money", "Extracted Costs for Allocation:"), class = "text-center"),
            htmltools::br(),
            shiny::column(
              6,
              htmltools::div(
                id = "tour_extracted_costs",
                htmltools::h5("Click on a row above to view its detailed information here!"),
                DT::dataTableOutput(ns("extracted_renewal_costs")) %>%
                  shinycustomloader::withLoader(),
                htmltools::tags$hr(),
                htmltools::div(
                  id = "tour_extracted_ui",
                  htmltools::tags$h3(
                    shiny::uiOutput(ns("extracted_ui"))
                  )
                )
              )
            ),
            shiny::column(
              width = 6,
              htmltools::h5("Drilldown into the chart by clicking on an individual compenent!"),
              htmltools::div(
                id = "tour_costs_chart",
                highcharter::highchartOutput(ns("costs_chart"), height = "400px") %>%
                  shinycustomloader::withLoader()
              )
            )
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("info-circle") #,
          # ...
        )
      )
    )
  )
}

#' Inputs Tab Module Server
#'
#' @inheritParams app_server
#' @param initial_costs data.frame for initial table representing renewal costs.
#' @param dictionary internal
#'
#' @return List of input data.frames
#' @importFrom cli cli_rule cli_dl
#' @importFrom dplyr rename mutate filter pull
#' @importFrom DT renderDT datatable formatCurrency
#' @importFrom highcharter renderHighchart highchart hc_exporting hc_chart hc_title hc_xAxis hc_legend hc_credits hc_plotOptions hc_add_series hc_drilldown list_parse2
#' @importFrom htmltools tags
#' @importFrom rhandsontable renderRHandsontable hot_to_r
#' @importFrom rintrojs introjs
#' @importFrom shiny reactiveValues observeEvent reactive req showModal modalDialog
#' @importFrom shinyjs onclick show
#' @importFrom tibble as_tibble rownames_to_column tibble
renewal_costs_tab <- function(input, output, session,
                              initial_costs = renewal_costs,
                              dictionary) {

  # initial reactives
  costs_data <- shiny::reactiveValues()
  costs_data$r_data <- initial_costs
  costs_data$display_data <- initial_costs %>%
    apply_labels(dict = dictionary, dataset_name = "renewal_costs")
  costs_data$extracted_costs <- initial_costs %>% extract_costs()

  # rhandsontable main input/output table ------------------------------------

  # output
  output$renewal_cost_table <- rhandsontable::renderRHandsontable({

    req(costs_data$r_data, costs_data$display_data)

    out <- costs_data$r_data %>%
      apply_labels(dict = dictionary, dataset_name = "renewal_costs") %>%
      dplyr::mutate_if(is.numeric, round, 0)

    costs_data$display_data <- out

    centercols <- c(match( c("cost_type", "description"), names(costs_data$r_data)))
    currcols <- c(match(c("prior", "current"), names(costs_data$r_data)))
    numcols <- costs_data$display_data %>% dplyr::select_if(is.numeric) %>% names()

    rhandsontable::rhandsontable(
      data = out,
      rowHeaders = NULL,
      useTypes = TRUE,
      highlightRow = TRUE,
      highlightCol = TRUE,
      digits = 0,
      search = TRUE,
      stretchH = "all",
      height = NULL,
      contextMenu = TRUE,
      manualColumnResize = TRUE
    ) %>%
      rhandsontable::hot_context_menu(
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))),
        allowReadOnly = TRUE,
        allowComments = TRUE,
        allowCustomBorders = TRUE
      ) %>%
      rhandsontable::hot_table(
        stretchH = "all",
        highlightRow = TRUE,
        highlightCol = TRUE,
        enableComments = TRUE
      ) %>%
      rhandsontable::hot_cols(columnSorting = TRUE) %>%
      rhandsontable::hot_cols(cols = currcols, format = "$0,0") %>%
      rhandsontable::hot_cols(cols = centercols, halign = "htCenter") %>%
      rhandsontable::hot_validate_numeric(cols = numcols,
                                          min = 0,
                                          allowInvalid = FALSE) %>%
      rhandsontable::hot_col(1, type = "dropdown",
                             source = c("Premium", "Expense", "Excluded"))
  })

  # extracted costs ---------------------------------------------------------

  # DT selectable table
  output$extracted_renewal_costs <- DT::renderDT({
    shiny::req(costs_data$extracted_costs)

    tibble::as_tibble(costs_data$extracted_costs) %>%
      as.matrix() %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Cost Type") %>%
      dplyr::rename("Dollar Amount" = V1) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(`Cost Type` = toproper(`Cost Type`)) %>%
      DT::datatable(
        rownames = FALSE,
        caption = "Extracted Costs for Allocation:",
        filter = "none",
        extensions = "Buttons",
        autoHideNavigation = TRUE,
        fillContainer = FALSE,
        selection = list(mode = "single", selected = 5, selectable = TRUE, target = "row"),
        style = "bootstrap4",
        options = list(
          dom = "t"
        )
      ) %>%
      DT::formatCurrency(columns = "Dollar Amount", digits = 0)

  })


  # costs chart -------------------------------------------------------------
  output$costs_chart <- highcharter::renderHighchart({

    req(costs_data$r_data, costs_data$extracted_costs)

    renewal_costs <- costs_data$r_data

    display_data <- costs_data$display_data

    extracted_costs <- costs_data$extracted_costs

    df_outer <- tibble::tibble(
      name = c("Premiums", "Expenses"),
      y = c(extracted_costs$risk_transfer, extracted_costs$expenses),
      drilldown = c("premiums", "expenses")
    ) %>%
      mutate_if(is.numeric, round, 0)

    df_premiums <- tibble::tibble(
      name = c("All Risk", "Terrorism"),
      value = c(extracted_costs$all_risk, extracted_costs$terrorism)
    ) %>%
      mutate_if(is.numeric, round, 0)

    df_expenses <- tibble::tibble(
      name = display_data %>%
        dplyr::filter(.data$`Cost Type` == "Expense") %>%
        dplyr::pull(.data$`Cost Description`),
      value = renewal_costs %>% dplyr::filter(cost_type == "expense") %>% pull(current),
    ) %>%
      mutate_if(is.numeric, round, 0)

    highcharter::highchart() %>%
      highcharter::hc_exporting(
        enabled = TRUE,
        filename = "Chart",
        formAttributes = list(target = '_blank'),
        buttons = hc_btn_options(),
        sourceWidth = 1000,
        sourceHeight = 600
      ) %>%
      highcharter::hc_chart(type = "pie") %>%
      highcharter::hc_title(
        text = "Renewal Costs",
        style = list(fontWeight = "bold"),
        align = "center",
        verticalAlign = "middle"
      ) %>%
      highcharter::hc_xAxis(type = "category") %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_credits(
        enabled = TRUE,
        text = "Click on a category to drilldown to its individual components."
      ) %>%
      highcharter::hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            distance = -50,
            startAngle = -90,
            endAngle = 90,
            center = list(
              "50%", "75%"
            )
          )
        )
      ) %>%
      highcharter::hc_add_series(
        data = df_outer,
        name = "Renewal Costs",
        colorByPoint = TRUE,
        innerSize = "50%"
      ) %>%
      highcharter::hc_drilldown(
        allowPointDrilldown = TRUE,
        drillUpButton = list(
          position = list(align = "left"),
          relativeTo = "spacingBox"
        ),
        series = list(
          list(
            name = "Value",
            id = "premiums",
            innerSize = "50%",
            data = highcharter::list_parse2(df_premiums)
          ),
          list(
            name = "Value",
            id = "expenses",
            innerSize = "50%",
            data = highcharter::list_parse2(df_expenses)
          )
        )
      )
  })

  # observers ---------------------------------------------------------------

  # rhandsontable / r and display data
  # observe changes to input and update server side R table
  shiny::observeEvent(input$renewal_cost_table$changes$changes, {

    if (!is.null(input$renewal_cost_table)) {

      display_data <- rhandsontable::hot_to_r(input$renewal_cost_table)

      r_data <- display_data %>%
        reverse_labels(dict = dictionary, dataset_name = "renewal_costs") %>%
        dplyr::filter(!is.na(.data$cost_type))

      extracted_costs <- r_data %>% extract_costs()

      costs_data$r_data <- r_data
      costs_data$display_data <- display_data
      costs_data$extracted_costs <- extracted_costs

    }

  }, ignoreInit = TRUE)

  # add row
  shiny::observeEvent(input$add_new, {

    placeholder_row <- tibble::tibble_row(
      cost_type = "Excluded",
      description = "Add a description here..",
      prior = as.numeric(0),
      current = as.numeric(0)
    )

    r_data <- dplyr::bind_rows(
      costs_data$r_data, placeholder_row
    )

    display_data <- r_data %>%
      apply_labels(dict = dictionary, dataset_name = "renewal_costs")

    costs_data$r_data <- r_data
    costs_data$display_data <- display_data
    costs_data$extracted_costs <- r_data %>% extract_costs()

    shiny::showNotification(
      "Successfully added new cost to renewal costs table.",
      type = "message"
    )

  })

  # reset
  shiny::observeEvent(input$reset, {
    costs_data$r_data <- initial_costs
    shiny::showNotification(
      "Renewal cost table successfully reset to initial defaults.",
      type = "message"
    )
  })

  # rhandsonttable bttn
  shiny::observeEvent(input$table_features, {
    shiny::showModal(
      shiny::modalDialog(
        title = icon_text("table", "Table Features Overview:"),
        easyClose = TRUE,
        size = "l",
        htmltools::tags$iframe(
          src = "www/handsontable_features.html",
          width = "100%",
          height = "700px",
          scrolling = "auto",
          frameborder = 0
        )
      )
    )
  })

  # click on extracted renewal costs to populate text below table:
  shiny::observeEvent(input$extracted_renewal_costs_rows_selected, {

    ns <- session$ns

    output$extracted_ui <- shiny::renderText({

      sel <- input$extracted_renewal_costs_rows_selected
      cost_sel <- paste0("$", prettyNum(costs_data$extracted_costs[[sel]], big.mark = ","))
      name_sel <- toproper(names(costs_data$extracted_costs)[sel])
      pr <- dplyr::select(costs_data$r_data, cost_type, description, current, prior) %>%
        extract_costs()
      pr_cost_sel <- paste0("$", prettyNum(pr[[sel]], big.mark = ","))
      pr_name_sel <- toproper(names(pr)[sel])
      diff <- paste0("$", prettyNum(costs_data$extracted_costs[[sel]] - pr[[sel]], big.mark = ","))
      color <- ifelse(costs_data$extracted_costs[[sel]] - pr[[sel]] >= 0, "<font color=\"green\">", "<font color=\"red\">")

      text <- paste0(
        name_sel,
        " equals ",
        tags$strong(cost_sel),
        " for the current 2021 Policy Year.",
        htmltools::tags$br(),
        "This is a ",
        color,
        tags$strong(diff), "</font>",
        ifelse(costs_data$extracted_costs[[sel]] - pr[[sel]] >= 0, " increase ", " decrease "),
        "compared to the prior at 2020."
      )

      text

    })

  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # tour
  shiny::observeEvent(input$tab_tour, {
    rintrojs::introjs(
      session,
      options = list(
        steps = list(

          list(
            element = "#tour_renewal_cost_table",
            intro = "Here, you input all the costs which should be allocated, one per line.<br/><br/>
            Each cost can be assigned one of three cost types: Premium, Expense, or Excluded.<br/><br/>
            (Only Premium and Expense cost types will be included in the allocation, and this will be reflected<br/>
            in the Extracted Costs table above.)<br/><br/>
            The cost description can be anything you want to help you document the cost.<br/><br/>
            Finally, enter current and future annual costs for the cost type to be allocated."
          ),

          list(
            element = "#tour_extracted_costs",
            intro = "This is a summary table of the information that you have entered in table below.<br/><br/>
            Here, the costs which are to be allocated by entity in subsequent tabs, are set out, but only for the projected year - prior costs are not shown in this table.<br/><br/>
            Costs from the table below fall into one of the following categories: Terrorism, All Risk, Risk Transfer, and Expenses"
          ),

          list(
            element = "#tour_costs_chart",
            intro = "This is a pimple radial/donut chart with the capability to drill down into the costs by type.
            Try clicking on expenses to see the distribution split out amongst the various types of expenses."
          ),

          list(
            element = "#tour_extracted_ui",
            intro = "Try clicking on a row in the extracted costs table above to populate some informative text here showing
            the changes by category since prior."
          )
        )
      )
    )
  })

  observe({

    r_data <- costs_data$r_data
    display_data <- costs_data$display_data
    extracted_costs <- costs_data$extracted_costs

    cli::cli_rule("Extracted Renewal Costs")
    cli::cli_dl(unlist(prettyNum(extracted_costs, big.mark = ",")))
    cli::cli_rule()

  })

  # return ------------------------------------------------------------------

  return(shiny::reactive({ costs_data$extracted_costs } ))

}


# helpers -----------------------------------------------------------------



# commented out -----------------------------------------------------------

# rhandsontable::hot_cols(renderer = "
#          function (instance, td, row, col, prop, value, cellProperties) {
#          Handsontable.renderers.NumericRenderer.apply(this, arguments);
#          if (value==0L) {
#          td.style.background = 'pink';}
#          }") %>%
# rhandsontable::hot_cols(renderer = "
#          function (instance, td, row, col, prop, value, cellProperties) {
#          Handsontable.renderers.TextRenderer.apply(this, arguments);
#          if (value=='Add a description here..') {
#          td.style.background = 'pink';}
#          }")

# shiny::observeEvent(costs_data$r_data, {
#
#   before <- costs_data$display_data %>%
#     reverse_labels(dict = dictionary, dataset_name = "renewal_costs")
#
#   changes <-
#
#     costs_data$display_data <- costs_data$r_data %>%
#     apply_labels(dict = dictionary, dataset_name = "renewal_costs")
# })


