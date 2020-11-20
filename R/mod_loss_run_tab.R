#' Loss run tab module - UI
#'
#' @param id shiny namespace ID
#'
#' @return HTML
#' @export
#'
#' @importFrom DT DTOutput
#' @importFrom shiny NS tabPanel
#' @importFrom shinycustomloader withLoader
loss_run_tab_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = icon_text("exclamation-triangle", " Loss Data"),
    width = 12,

    flucol(
      width = 12,
      htmltools::hr(),

      shinydashboard::tabBox(
        id = ns("tabbox"),
        title = "Loss Data",
        width = 12,
        # side = "right",
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

              actionButton(ns("table_features"),
                           label = "View Table Features",
                           icon = icon("info"))
            )
          ),
          flucol(
            width = 12,
            htmltools::div(
              id = "tour_loss_run_table",
              align = "center",
              DT::DTOutput(ns("loss_run_table")) %>% #, width = "75%") %>%
                shinycustomloader::withLoader()
            )
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("line-chart"),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              highcharter::highchartOutput(ns("loss_chart"), height = "400px") %>%
                shinycustomloader::withLoader()
            )
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("info-circle")

        )
      )
    )
  )
}

#' Loss run tab module - Server
#'
#' @param input input
#' @param output output
#' @param session session
#' @param initial_loss_run loss_run
#' @param dictionary dictionary
#'
#' @return reactive loss data
#' @export
#'
#' @importFrom DT renderDT datatable formatCurrency dataTableProxy editData
#' @importFrom shiny reactiveValues reactive req observeEvent
#' @importFrom utils str
loss_run_tab <- function(input, output, session, initial_loss_run, dictionary) {


  initial_loss_run <- initial_loss_run %>%
    dplyr::select(-location_dud)

  loss_run_data <- shiny::reactiveValues()
  loss_run_data$r_data <- initial_loss_run
  loss_run_data$display_data <- initial_loss_run %>%
    apply_labels(dict = dictionary, dataset_name = "loss_run")

  table_prep <- shiny::reactive({
    loss_run_data$display_data %>%
      dplyr::mutate_if(is.numeric, round, 0) %>%
      dplyr::mutate_at(dplyr::vars(`Location`, `Claim Number`),
                       extract_number, return_as = "numeric") %>%
      dplyr::arrange(`Location`) %>%
      dplyr::mutate_at(dplyr::vars(`Location`, `Claim Number`), as.factor) %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      dplyr::mutate(`Accident City` = as.character(`Accident Description`))
  })

  output$loss_run_table <- DT::renderDT({

    shiny::req(table_prep())

    out <- table_prep()

    cap <- "Loss Run Evaluated as of December 31, 2019"

    DT::datatable(
      out,
      rownames = FALSE,
      caption = cap,
      editable = TRUE,
      selection = "none",
      class = "compact stripe row-border nowrap",
      style = "bootstrap",
      extensions = c("Buttons"), # "Scroller"),
      filter = "top",
      autoHideNavigation = TRUE,
      width = "75%",
      fillContainer = FALSE,
      options = list(
        searching = TRUE,
        autoWidth = TRUE,
        # scroller = TRUE,
        # scrollX = TRUE,
        scrollY = "500px",
        dom = 'Brt',
        pageLength = nrow(out),
        # paging = FALSE,
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
            title = paste0("Loss-Run-", Sys.Date())
          ),
          list(
            extend = 'colvis',
            text = 'Edit Displayed Columns'
          )
        )
      )
    ) %>%
      DT::formatCurrency(c("Total Reported"), digits = 0) %>%
      DT::formatString(c("Location"), prefix = "Location ") %>%
      DT::formatString(c("Claim Number"), prefix = "Claim ")

  })

  loss_run_table_proxy <- DT::dataTableProxy("loss_run_table")

  shiny::observeEvent(input$loss_run_table_cell_edit, {

    info <- input$loss_run_table_cell_edit
    utils::str(info)

    new_display_data <- DT::editData(
      table_prep(),
      info,
      proxy = loss_run_table_proxy,
      rownames = FALSE,
      resetPaging = FALSE
    )

    new_r_data <- new_display_data %>% reverse_labels(dict = dictionary, dataset_name = "loss_run")

    loss_run_data$r_data <- new_r_data
    loss_run_data$display_data <- new_display_data

  })

  shiny::observeEvent(input$tab_tour, {
    rintrojs::introjs(
      session,
      options = list(
        steps = list(
          list(
            element = "#tour_loss_run_table",
            intro = "There's just one input item in this tab - short and sweet!<br/><br/>
            Here, examine claims made by each entity over time. This table is ultimately used by tab Claim Count Surcharges,
            where you are able to set a rule for surcharging entities with more premium if they have made claims in the past.
            The more claims, the more the entity is surcharged, and the more premium and expense the entity is ultimate allocated.<br/><br/>
            You'll want to check that the claim date and year are correct (as well as the size!),
            because tab Claim Count Surcharges will let you filter on claim date."
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

  output$loss_chart <- highcharter::renderHighchart({

    hold <- # loss_run_data$r_data %>%
      loss_run %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        inc = sum(total_incurred, na.rm = TRUE),
        cnt = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(severity = inc / cnt)

    x_cats <- unique(hold$year) %>% sort()

    highcharter::highchart() %>%
      highcharter::hc_chart(
        type = "column"
      ) %>%
      highcharter::hc_title(
        text = "Loss Data: Reported Dollars, Counts, and Average Severity by Year",
        style = list(fontWeight = "bold")
      ) %>%
      # highcharter::hc_subtitle(
      #   text = paste0("Selected Coverage: ", subtit),
      #   style = list(fontWeight = "bold")
      # ) %>%
      highcharter::hc_xAxis(
        categories = x_cats,
        title = list(
          text = "Accident Year",
          style = list(fontWeight = 'bold')
        )
      ) %>%
      highcharter::hc_yAxis(
        title = list(
          text = "Average Severity",
          style = list(fontWeight = 'bold')
        ),
        labels = list(format = '{value:,.0f}')
      ) %>%
      highcharter::hc_legend(
        enabled = TRUE,
        align = 'center',
        verticalAlign = "top"
      ) %>%
      highcharter::hc_tooltip(
        headerFormat = "<b>Year: {point.x}:</b><br/>",
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
        series = list(
          dataLabels = list(
            enabled = TRUE,
            color = "#000",
            formatter = data_labels_dollar_formatter()
          )
        )
      ) %>%
      highcharter::hc_add_series(
        data = hold$severity,
        name = "Average Severity",
        tooltip = list(
          pointFormat = tooltip_formatter_dollar()
        )
      )


  })

  loss_run_out <- shiny::reactive({
    loss_run_data$r_data
  })

  observe({
    lr <- loss_run_out()
    cli::cat_rule("Loss Run")
    dplyr::glimpse(lr)
    cli::cli_rule()
  })

  return(loss_run_out)
}
