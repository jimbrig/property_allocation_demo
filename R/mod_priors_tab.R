#' Prior premiums tab module - UI
#'
#' @param id shiny namespace ID
#'
#' @return HTML
#' @export
#'
#' @importFrom DT DTOutput
#' @importFrom shiny NS tabPanel
#' @importFrom shinycustomloader withLoader
priors_tab_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = icon_text("history", " Prior Premiums"),
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
    flucol(
      width = 12,
      htmltools::div(
        id = "tour_priors_table",
        DT::DTOutput(ns("priors_table")) %>%
          shinycustomloader::withLoader()
      )
    )
  )
}

#' Prior premiums tab module - Server
#'
#' @param input input
#' @param output output
#' @param session session
#' @param initial_priors priors
#' @param dictionary dictionary
#'
#' @return reactive prior premiums data
#' @export
#'
#' @importFrom DT renderDT datatable formatCurrency dataTableProxy editData
#' @importFrom shiny reactiveValues reactive req observeEvent
#' @importFrom utils str
#' @importFrom dplyr mutate_if
priors_tab <- function(input, output, session, initial_priors, dictionary) {

  initial_priors <- initial_priors %>%
    dplyr::select(entity_id,
                  prior_tiv,
                  prior_cat_eq_premium,
                  prior_cat_wind_premium,
                  prior_cat_flood_premium,
                  prior_total_cat_premium,
                  prior_aop_premium,
                  prior_terrorism_premium,
                  prior_all_risk_premium,
                  prior_risk_transfer_premium,
                  prior_expenses,
                  prior_premium_incl_expenses)

  priors_data <- shiny::reactiveValues()

  priors_data$r_data <- initial_priors

  priors_data$display_data <- initial_priors %>%
    apply_labels(dict = dictionary, dataset_name = "priors")

  table_prep <- shiny::reactive({
    priors_data$display_data
  })

  output$priors_table <- DT::renderDT({

    shiny::req(table_prep())

    out <- table_prep() %>%
      dplyr::mutate(Location = extract_number(Location, return_as = "numeric")) %>%
      dplyr::arrange(Location)

    cap <- "Prior Allocated Premiums Evaluated as of December 31, 2018"

    # browser()

    contain <- htmltools::withTags(
      table(
        # class = "compact",
        thead(
          tr(
            th(
              class = "dt-center",
              rowspan = 3,
              "Location"
            ),
            th(
              class = "dt-center",
              rowspan = 3,
              colspan = 1,
              "Total Insured Value (TIV)"
            ),
            th(
              class = "dt-center",
              rowspan = 1,
              colspan = 10,
              "Prior Premiums"
            )
          ),
          tr(
            th(
              class = "dt-center",
              rowspan = 1,
              colspan = 4,
              "Catastrophe (CAT)"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              colspan = 1,
              "AOP"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              colspan = 1,
              "Terrorism"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              colspan = 1,
              "All Risk"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              colspan = 1,
              "Risk Transfer"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              colspan = 1,
              "Total Expenses"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              colspan = 1,
              "Final Premium + Expense"
            )
          ),
          tr(
            th(
              class = "dt-center",
              rowspan = 1,
              colspan = 1,
              "Earthquake"
            ),
            th(
              class = "dt-center",
              rowspan = 1,
              colspan = 1,
              "Wind"
            ),
            th(
              class = "dt-center",
              rowspan = 1,
              colspan = 1,
              "Flood"
            ),
            th(
              class = "dt-center",
              rowspan = 1,
              colspan = 1,
              "Total"
            )
          )
        )
      )
    )

    DT::datatable(
      out,
      rownames = FALSE,
      caption = cap,
      container = contain,
      editable = TRUE,
      selection = "none",
      class = "compact stripe row-border nowrap",
      style = "bootstrap",
      extensions = c("Buttons"),
      filter = "top",
      options = list(
        searching = TRUE,
        autoWidth = TRUE,
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
            title = paste0("Prior-Premiums-", Sys.Date())
          ),
          list(
            extend = 'colvis',
            text = 'Edit Displayed Columns'
          )
        )
      )
    ) %>%
      DT::formatCurrency(c("Prior TIV",
                           "Prior Total Expenses",
                           "Prior AOP Premium",
                           "Prior Premium CAT-EQ",
                           "Prior Premium CAT-Wind",
                           "Prior Premium CAT-Flood",
                           "Prior Premium Terrorism",
                           "Prior Total CAT Premium",
                           "Prior Total All Risk Premium",
                           "Prior Total Premium Excl. Expense",
                           "Prior Total Premium and Expense"), digits = 0) %>%
      DT::formatString(c("Location"), "Location ")

  })

  priors_table_proxy <- DT::dataTableProxy("priors_table")

  shiny::observeEvent(input$priors_table_cell_edit, {

    info <- input$priors_table_cell_edit
    utils::str(info)

    new_display_data <- DT::editData(
      table_prep(),
      info,
      proxy = priors_table_proxy,
      rownames = FALSE,
      resetPaging = FALSE
    )

    new_r_data <- new_display_data %>% reverse_labels(dict = dictionary, dataset_name = "priors")

    priors_data$r_data <- new_r_data
    priors_data$display_data <- new_display_data

  })

  shiny::observeEvent(input$tab_tour, {
    rintrojs::introjs(
      session,
      options = list(
        steps = list(
          list(
            element = "#tour_priors_table",
            intro = "There's just one input item in this tab - short and sweet!<br/><br/>
            Here, examine the prior year's premiums and expenses by entity. This is historical data, so you generally won't want to touch this table.
            But if you feel the need to test something out, or to correct historical wrongs, double click on any entry to modify it.<br/><br/>
            Note that the data of this table may conflict with the data entered in the Renewal Costs tab. If this occurs, the application
            will automatically scale prior results so that they agree with totals set out in the Renewal Costs tab."
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

  priors_out <- shiny::reactive({
    priors_data$r_data
  })

  observeEvent(priors_out(), {
    prs <- priors_out()
    cli::cat_rule("Priors:")
    dplyr::glimpse(prs)
    cli::cli_rule()
  })

  priors_out
}
