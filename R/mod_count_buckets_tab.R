#' Claim count surcharges tab module - UI
#'
#' @param id shiny namespace ID
#'
#' @return HTML
#' @export
#'
#' @importFrom DT DTOutput
#' @importFrom shiny NS tabPanel
#' @importFrom shinycustomloader withLoader
count_buckets_tab_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = icon_text("angle-double-up", " Claim Count Surcharges"),
    width = 12,
    flucol(
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

      shinydashboard::tabBox(
        id = ns("tabbox"),
        title = "Count Bucket Surcharges",
        width = 12,
        # side = "right",
        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("table"),
          flucol(
            width = 12,
            htmltools::div(
              align = "center",
              id = "tour_count_buckets_table",
              DT::DTOutput(ns("count_buckets_table")) %>%
                shinycustomloader::withLoader()
            )
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("line-chart"),

          htmltools::div(
            align = "center",
            id = "tour_count_buckets_charts",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                highcharter::highchartOutput(ns("count_buckets_losses_chart"),
                                             height = "400px") %>%
                  shinycustomloader::withLoader()
              )
            ),

            htmltools::tags$br(),

            shiny::fluidRow(
              shiny::column(
                width = 12,
                highcharter::highchartOutput(ns("count_buckets_surcharges_chart"),
                                             height = "400px") %>%
                  shinycustomloader::withLoader()
              )
            )
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("info-circle"),
          flucol(
            width = 12,
            htmltools::tags$h3("Count Buckets & Surcharges"),
            htmltools::tags$hr() #,
            # shiny::uiOutput(ns("report")) %>%
            #   shinycustomloader::withLoader()
          )
        )
      )
    )
  )
}

#' Claim count surcharges tab module - Server
#'
#' @param input input
#' @param output output
#' @param session session
#' @param initial_count_buckets count_buckets
#' @param dictionary dictionary
#'
#' @return reactive conut buckets data
#' @export
#'
#' @importFrom DT renderDT datatable formatCurrency dataTableProxy editData
#' @importFrom shiny reactiveValues reactive req observeEvent
#' @importFrom utils str
count_buckets_tab <- function(input, output, session, initial_count_buckets, dictionary, loss_run) {

  count_buckets_data <- shiny::reactiveValues()
  count_buckets_data$r_data <- initial_count_buckets
  count_buckets_data$display_data <- initial_count_buckets %>%
    dplyr::select(-name) %>%
    apply_labels(dict = dictionary, dataset_name = "count_buckets")

  table_prep <- shiny::reactive({
    count_buckets_data$display_data
  })

  charts_prep <- shiny::reactive({
    loss_run() %>%
      dplyr::mutate(
        count_bucket = cut(
          .data$total_incurred,
          breaks = c(0, count_buckets_data$r_data$max),
          labels = count_buckets_data$r_data$bucket,
          include.lowest = TRUE
        ) %>% as.character()
      ) %>%
      dplyr::left_join(count_buckets_data$r_data, by = c("count_bucket" = "bucket")) %>%
      dplyr::group_by(count_bucket) %>%
      dplyr::summarise(count = dplyr::n(),
                       total_incurred = sum(total_incurred),
                       average_incurred = total_incurred / count,
                       dollar_surcharge = sum(dollar_surcharge),
                       percent_surcharge = sum(percent_surcharge)) %>%
      dplyr::mutate(count_bucket = factor(count_bucket, labels = count_buckets_data$display_data$`Count Bucket`))

  })

  output$count_buckets_losses_chart <- highcharter::renderHighchart({
    hold <- charts_prep()

    highcharter::highchart() %>%
      highcharter::hc_chart(
        type = "column"
      ) %>%
      highcharter::hc_title(
        text = "Loss Counts and Totals by Surcharge Bucket",
        style = list(fontWeight = "bold")
      ) %>%
      highcharter::hc_xAxis(
        categories = hold$count_bucket,
        title = list(
          text = "Surcharge Bucket",
          style = list(fontWeight = 'bold')
        )
      ) %>%
      highcharter::hc_yAxis_multiples(
        list(
          title = list(
            text = "Reported Claim Counts",
            style = list(fontWeight = 'bold')
          ),
          labels = list(format = '{value:,.0f}')
        ),
        list(
          title = list(
            text = "Total Incurred",
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
        headerFormat = "<b>Bucket: {point.x}:</b><br/>",
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
            formatter = data_labels_dollar_formatter()
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
        data = hold$count,
        name = "Reported Claim Counts",
        tooltip = list(
          pointFormat = tooltip_formatter_number()
        )
      ) %>%
      highcharter::hc_add_series(
        type = "spline",
        data = hold$total_incurred,
        name = "Total Incurred",
        tooltip = list(
          pointFormat = tooltip_formatter_dollar()
        ),
        yAxis = 1
      )
  })

  output$count_buckets_surcharges_chart <- highcharter::renderHighchart({
    hold <- charts_prep()

    # browser()

    highcharter::highchart() %>%
      highcharter::hc_chart(
        type = "column"
      ) %>%
      highcharter::hc_title(
        text = "Surcharges Due to Losses by Surcharge Bucket",
        style = list(fontWeight = "bold")
      ) %>%
      highcharter::hc_xAxis(
        categories = hold$count_bucket,
        title = list(
          text = "Surcharge Bucket",
          style = list(fontWeight = 'bold')
        )
      ) %>%
      highcharter::hc_yAxis_multiples(
        list(
          title = list(
            text = "Dollar Surcharge ($ Lump Sum)",
            style = list(fontWeight = 'bold')
          ),
          labels = list(format = '{value:,.0f}')
        ),
        list(
          title = list(
            text = "Percent Surcharge (% of Premium)",
            style = list(fontWeight = 'bold')
          ),
          labels = list(format = '{value:,.0f}'),
          opposite = TRUE
        )
      ) %>%
      highcharter::hc_legend(
        enabled = TRUE,
        align = 'center',
        verticalAlign = "top"
      ) %>%
      highcharter::hc_tooltip(
        headerFormat = "<b>Bucket: {point.x}:</b><br/>",
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
            formatter = data_labels_dollar_formatter()
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
        data = hold$dollar_surcharge,
        name = "Surcharge ($ Lump Sum)",
        tooltip = list(
          pointFormat = tooltip_formatter_dollar()
        )
      ) %>%
      highcharter::hc_add_series(
        type = "spline",
        data = hold$percent_surcharge * 100,
        name = "Surcharge (% of Premium)",
        tooltip = list(
          pointFormat = tooltip_formatter_rate()
        ),
        yAxis = 1
      )
  })

  output$count_buckets_table <- DT::renderDT({

    shiny::req(table_prep())

    out <- table_prep()

    cap <- "Buckets"

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
      autoHideNavigation = TRUE,
      options = list(
        autoWidth = TRUE,
        # scrollX = TRUE,
        dom = 'Brt',
        paging = FALSE,
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
            title = paste0("Claim-Count-Buckets-", Sys.Date())
          ),
          list(
            extend = 'colvis',
            text = 'Edit Displayed Columns'
          )
        )
      )
    ) %>%
      DT::formatCurrency(c("Bucket Minimum",
                           "Bucket Maximum",
                           "Dollar Surcharge"), digits = 0) %>%
      DT::formatPercentage(c("Percent Surcharge"), digits = 1)

  })

  count_buckets_table_proxy <- DT::dataTableProxy("count_buckets_table")

  shiny::observeEvent(input$count_buckets_table_cell_edit, {

    info <- input$count_buckets_table_cell_edit
    cli::cli_alert_info(cli::col_cyan("Change detected on count buckets:"))
    utils::str(info)
    cli::cli_rule()

    new_display_data <- DT::editData(
      table_prep(),
      info,
      proxy = count_buckets_table_proxy,
      rownames = FALSE,
      resetPaging = FALSE
    )

    new_r_data <- new_display_data %>% reverse_labels(dict = dictionary, dataset_name = "count_buckets")

    count_buckets_data$r_data <- new_r_data
    count_buckets_data$display_data <- new_display_data

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

  shiny::observeEvent(input$tab_tour, {
    rintrojs::introjs(
      session,
      options = list(
        steps = list(
          list(
            element = "#tour_count_buckets_table",
            intro = "There's just one input item in this tab - short and sweet!<br/><br/>
            Here, the surcharge rule for historical claims made by entities is defined.
            The allocation process will surcharge entities with dollar amounts or a percentage per this table for each claim made in the past.<br/><br/>
            The dollar amount or percentage (or both!) to surcharge entities by can be set by claim size bucket,
            and this functionality appears in fields 'Bucket Minimum' and 'Bucket Maximum'."
          )
        )
      )
    )
  })

  count_buckets_out <- shiny::reactive({
    count_buckets_data$r_data
  })

  observeEvent(count_buckets_out(), {
    disp <- count_buckets_out()
    cli::cli_rule("Count Buckets:")
    dplyr::glimpse(disp)
    cli::cli_rule()
  })

  count_buckets_out
}
