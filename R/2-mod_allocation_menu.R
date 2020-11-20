#' Allocation Menu UI
#'
#' @param id ns id
#'
#' @importFrom DT DTOutput
#' @importFrom htmltools div br hr
#' @importFrom shiny NS fluidRow column tabPanel icon sliderInput
#' @importFrom shinycustomloader withLoader
#' @importFrom shinydashboard tabItem tabBox box
#' @importFrom shinyjs hidden
#' @importFrom shinyWidgets sliderTextInput radioGroupButtons checkboxGroupButtons
allocation_menu_ui <- function(id) {

  ns <- shiny::NS(id)

  feature_choices <- c(
    "Budget Guidance" = "bg",
    "Rate Capping Compared to Prior" = "capping",
    "Apply Relativities" = "rels"
  )

  init_exp_period <- c(
    min(loss_run$year):max(loss_run$year)
  )

  shinydashboard::tabItem(
    tabName = ns(id),
    shiny::fluidRow(
      shiny::column(
        12,
        # shiny::actionButton(ns("show_settings"), "Show Settings", icon = shiny::icon("eye")) %>%
        #   shinyjs::hidden(),

        shinydashboard::tabBox(
          id = ns("tab"),
          title = icon_text("balance-scale-left", " Allocation"),
          width = 9,

          shiny::tabPanel(
            title = icon_text("heartbeat", " Allocation"),
            width = 12,
            flucol(
              width = 12,
              shiny::helpText("Hint: Toggle the sidebar on the left to increase screen visibility."),
              DT::DTOutput(ns("allocation_table")) %>%
                shinycustomloader::withLoader()
            )
          ),
          shiny::tabPanel(
            title = icon_text("info", " Details"),
            width = 12,
            flucol(
              width = 12,
              DT::DTOutput(ns("allocation_details")) %>%
                shinycustomloader::withLoader()
            )
          ),
          priors_tab_ui(ns("priors")),
          shiny::tabPanel(
            title = icon_text("balance-scale", "Relativity Adjusted TIVs"),
            width = 12,
            flucol(
              width = 12,
              DT::DTOutput(ns("rel_adj_tiv")) %>%
                shinycustomloader::withLoader()
            )
          )
        ),

        # allocation widgets
        # htmltools::div(
        #   id = ns("settings"),
        shinydashboard::box(
          # collapsible = TRUE,
          # collapsed = TRUE,
          title = icon_text("cogs", "Settings"),
          width = 3,
          flucol(
            width = 12,
            htmltools::div(
              class = "text-center",
              shiny::downloadButton(ns("download_allocation_data"),
                                    "Download Allocation Data",
                                    icon = shiny::icon("download")),
              htmltools::br(),
              htmltools::hr(),

              shinyWidgets::sliderTextInput(
                inputId = ns("experience_period"),
                label = icon_text("business-time", "Experience Period:"),
                choices = init_exp_period,
                selected = init_exp_period[c(1, length(init_exp_period))],
                animate = TRUE,
                grid = TRUE
              ),

              htmltools::br(),
              htmltools::hr(),
              htmltools::br(),

              shinyWidgets::radioGroupButtons(
                inputId = ns("expense_weight_variable"),
                label = icon_text("money-bill-alt", "How Should Expenses be Allocated?"),
                choices = c("TIV" = "tiv", "Premium" = "premium"),
                selected = "tiv",
                justified = TRUE
              ),

              htmltools::br(),
              htmltools::hr(),
              htmltools::br(),

              shinyWidgets::checkboxGroupButtons(
                inputId = ns("features"),
                label = icon_text("check-double", "Select Allocation Features:"),
                choices = feature_choices,
                selected = feature_choices,
                direction = "vertical",
                justified = TRUE,
                width = "100%",
                # individual = TRUE,
                checkIcon = list(
                  yes = shiny::icon("ok",
                                    lib = "glyphicon"),
                  no = shiny::icon("remove",
                                   lib = "glyphicon")
                )
              ),

              htmltools::br(),
              htmltools::hr(),
              htmltools::br(),

              shiny::sliderInput(
                ns("bg_percent"),
                label = icon_text("cash-register" ,"Select Budget Guidance Percent"),
                min = 0,
                max = 100,
                value = 0,
                step = 1,
                round = TRUE,
                animate = TRUE,
                post = "%"
              ) %>%
                shinyjs::hidden(),

              htmltools::br(),

              shiny::sliderInput(
                ns("threshold"),
                label = icon_text("history", "Select Rate Capping Threshold:"),
                min = 0,
                max = 100,
                value = 100,
                step = 1,
                round = TRUE,
                animate = TRUE,
                post = "%"
              ) %>%
                shinyjs::hidden()
            )
          )
        )
      )
    )
  )
}

#' @title Allocation Menu Module - Server
#' @description Allocation menu module's server function.
#' @param input input
#' @param output output
#' @param session session
#' @param inputs_menu_list list of reactives derived in the \code{inputs_menu}
#'   module.
#' @return list of reactives corresponding to the allocation's derived datasets.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{downloadHandler}},\code{\link[shiny]{reactive}},\code{\link[shiny]{callModule}},\code{\link[shiny]{observe}},\code{\link[shiny]{updateSliderInput}},\code{\link[shiny]{showNotification}},\code{\link[shiny]{reactiveValues}},\code{\link[shiny]{observeEvent}},\code{\link[shiny]{req}}
#'  \code{\link[writexl]{write_xlsx}}
#'  \code{\link[shinyjs]{visibilityFuncs}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[DT]{dataTableOutput}},\code{\link[DT]{datatable}},\code{\link[DT]{formatCurrency}},\code{\link[DT]{dataTableProxy}},\code{\link[DT]{editData}}
#'  \code{\link[tidyselect]{starts_with}}
#'  \code{\link[htmltools]{withTags}}
#'  \code{\link[utils]{str}}
#' @rdname allocation_menu
#' @export
#' @importFrom shiny downloadHandler reactive callModule observe updateSliderInput showNotification removeNotification reactiveValues observeEvent req
#' @importFrom writexl write_xlsx
#' @importFrom shinyjs show hide
#' @importFrom dplyr mutate arrange select left_join
#' @importFrom DT renderDataTable datatable formatCurrency formatString renderDT formatRound formatPercentage dataTableProxy editData
#' @importFrom tidyselect starts_with
#' @importFrom htmltools withTags
#' @importFrom utils str
#' @keywords module
allocation_menu <- function(input, output, session,
                            inputs_menu_list
) {

  output$download_allocation_data <- shiny::downloadHandler(
    filename = function() {
      paste0("Allocation-Data-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(allocation_data_full(), path = file)
    }
  )

  observeEvent(input$download_allocation_data, {

    shiny::downloadHandler("Allocation-Data.xlsx")

  })

  # extract reactives from inputs_menu_list
  # entity_data <- shiny::reactive({ inputs_menu_list$entity_data() })
  costs <- shiny::reactive({ inputs_menu_list$extracted_costs() })
  count_buckets <- shiny::reactive({ inputs_menu_list$count_buckets() })
  sov <- shiny::reactive({ inputs_menu_list$sov() })
  relativity_adjusted_tivs <- shiny::reactive({ inputs_menu_list$relativity_adjusted_tivs() })
  rates <- shiny::reactive({ inputs_menu_list$rates() })
  priors <- shiny::callModule(priors_tab,
                              "priors",
                              initial_prior = priors,
                              dictionary = dictionary)
  loss_run <- shiny::reactive({ inputs_menu_list$loss_run() })


  # change UI depending on User Selections
  shiny::observe({

    if ("bg" %in% input$features) {
      shinyjs::show("bg_percent", anim = TRUE)
      shiny::updateSliderInput(
        session = session, "bg_percent", value = 5,
        label = "Budget Guidance Percent",
        min = 0, max = 100, step = 1
      )
    } else {
      shinyjs::hide("bg_percent", anim = TRUE)
      shiny::updateSliderInput(
        session = session, "bg_percent", value = 0,
        label = "Budget Guidance Percent",
        min = 0, max = 100, step = 1
      )
    }
    if ("capping" %in% input$features) {
      shinyjs::show("threshold", anim = TRUE)
      shiny::updateSliderInput(session = session, "threshold", value = 25,
                               min = 0, max = 100, step = 1)
    } else {
      shinyjs::hide("threshold", anim = TRUE)
      shiny::updateSliderInput(session = session, "threshold", value = 100,
                               min = 0, max = 100, step = 1)
    }

  })

  # derive user-inputs
  bg_percent <- shiny::reactive({
    input$bg_percent / 100
  })

  threshold <- shiny::reactive({
    input$threshold / 100
  })

  rel_tivs <- shiny::reactive({

    if (!("rels" %in% input$features)) {
      hold <- relativity_adjusted_tivs() %>%
        dplyr::mutate(aop_adj_tiv = .data$tiv,
                      cat_eq_adj_tiv = .data$tiv,
                      cat_wind_adj_tiv = .data$tiv,
                      cat_flood_adj_tiv = .data$tiv,
                      terrorism_adj_tiv = .data$tiv)
      return(hold)
    } else {
      return(relativity_adjusted_tivs())
    }

  })

  output$rel_adj_tiv <- DT::renderDataTable({

    out <- rel_tivs() %>%
      dplyr::mutate(entity_id = as.numeric(gsub("entity_", "", .data$entity_id))) %>%
      dplyr::arrange(entity_id) %>%
      dplyr::mutate(entity_id = as.factor(entity_id))

    DT::datatable(
      out,
      rownames = FALSE,
      colnames = c(
        "Location",
        "Unadjusted TIV",
        "AOP Adjusted TIV",
        "CAT-EQ Adjusted TIV",
        "CAT-Wind Adjusted TIV",
        "CAT-Flood Adjusted TIV",
        "Terrorism Adjusted TIV"
      ),
      class = "compact stripe row-border nowrap",
      style = "bootstrap",
      extensions = c("Buttons"),
      filter = "top",
      options = list(
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
          ),
          list(
            extend = 'colvis',
            text = 'Edit Displayed Columns'
          )
        )
      )
    ) %>%
      DT::formatCurrency(
        c(2:ncol(out)),
        digits = 0
      ) %>%
      DT::formatString(c("entity_id"), "Location ")

  })

  # derive current and prior overall rates and percent change
  pct_change <- shiny::reactive({

    costs <- costs()
    sov <- sov()
    priors <- priors()

    curr_rate <- costs$risk_transfer / sum(sov$tiv)
    prior_rate <- sum(priors$prior_risk_transfer_premium) /
      sum(priors()$prior_tiv)

    (curr_rate / prior_rate) - 1

  })


  # derive 'entity loss data' from  loss run and count buckets --------------
  entity_loss_data <- shiny::reactive({

    msg <- shiny::showNotification(
      "Deriving Entity Loss Data...",
      duration = NULL,
      closeButton = TRUE
    )

    on.exit(shiny::removeNotification(msg), add = TRUE)

    entity_loss_summary(
      loss_run(), count_buckets(),
      experience_period = as.numeric(input$experience_period[1]:input$experience_period[2])
    )

  })

  # merge results into entity data for progression into allocation model ----
  entity_data <- shiny::reactive({

    msg <- shiny::showNotification(
      "Deriving Merged Entity Data...",
      duration = NULL,
      closeButton = TRUE
    )

    on.exit(shiny::removeNotification(msg), add = TRUE)

    merge_entity_data(
      sov(),
      rel_tivs(),
      entity_loss_data(),
      rates(),
      priors()
    )

  })

  # shiny::observe({
  #   cli::cli_rule("Derived Entity Allocation Data from Input Ingestion")
  #   dplyr::glimpse(entity_data())
  #   cli::cli_rule()
  # })

  # derive initial allocation data reactice and run preliminary allocation
  allocation_data_full <- shiny::reactive({

    hold <- entity_data() %>%
      preliminary_allocation(costs(),
                             bg_pct = bg_percent()) %>%
      # apply surcharges
      apply_surcharges(count_buckets()) %>%
      dplyr::mutate(
        prior_allocated = prior_risk_transfer_premium,
        prior_allocated_rate = prior_allocated / prior_tiv,
        uncapped_allocated = surcharged_premium
      ) %>%
      apply_threshold(
        total_pct_chg = pct_change(),
        threshold = threshold()
      ) %>%
      # final rebalancing and allocate expenses
      allocate_expenses(costs(), weight_variable = input$expense_weight_variable)

    if (!("risk_engineering" %in% colnames(hold))) {
      hold <- dplyr::mutate(hold,
                            risk_engineering = 0)
    }

    if (!("man_adj" %in% colnames(hold))) {
      hold <- dplyr::mutate(hold,
                            man_adj = 0)
    }

    hold %>%
      dplyr::mutate(
        model_cat_rate = model_cat_eq_rate + model_cat_flood_rate + model_cat_wind_rate,
        model_all_risk_rate = model_cat_rate + model_aop_rate,
        claim_surcharges = surcharged_premium - total_model_premium_adj,
        re_allocated = risk_engineering + final_allocated_w_expense,
        final_premium = re_allocated + man_adj
      )
  })

  allocation_data <- reactive({

    allocation_data_full() %>%
      dplyr::select(
        -tidyselect::starts_with("bucket_")
      ) %>%
      dplyr::mutate(
        capped_rate_percent_change_net = capped_rate_percent_change - pct_change(),
      ) %>%
      dplyr::select(
        entity_id,
        # location,
        department,
        tiv,
        # prior_tiv,
        model_aop_rate,
        model_cat_rate,
        model_all_risk_rate,
        model_terrorism_rate,
        initial_model_premium = total_model_premium_adj,
        claim_surcharges,
        surcharged_premium,
        allocated, # Premium after Capping and Rebalancing
        capped_rate_percent_change_net,
        allocated_expenses,
        risk_engineering,
        man_adj,
        final_premium
      )
  })

  allocation_data_rv <- shiny::reactiveValues()

  # observe a change to allocation_data reactive expression
  shiny::observeEvent(allocation_data(), {

    allocation_data_rv$r_data <- allocation_data()
    allocation_data_rv$display_data <- allocation_data() %>%
      apply_labels(dict = dictionary, dataset_name = "allocation")

  })

  table_prep <- shiny::reactive({
    allocation_data_rv$display_data
  })

  output$allocation_table <- DT::renderDT({

    # browser()

    shiny::req(table_prep())

    out <- table_prep() %>%
      dplyr::mutate(`Location` = extract_number(`Location`, return_as = "numeric"),
                    Department = extract_number(Department, return_as = "numeric")) %>%
      dplyr::arrange(`Location`) %>%
      dplyr::mutate(`Location` = as.factor(`Location`))

    # browser()

    cap <- "Allocation Results"

    contain <- htmltools::withTags(
      table(
        # class = "compact",
        thead(
          tr(
            th(
              class = "dt-center",
              rowspan = 2,
              "Location"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              "Department"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              "Total Insured Value (TIV)"
            ),
            th(
              class = "dt-center",
              colspan = 4,
              rowspan = 1,
              "Model Rates"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              "Initial Premium"
            ),
            th(
              class = "dt-center",
              colspan = 2,
              rowspan = 1,
              "Surcharges"
            ),
            lapply(
              colnames(out)[11:ncol(out)],
              th,
              rowspan = 2
            )
          ),
          tr(
            lapply(c("AOP", "CAT", "All Risk", "Terrorism"), th),
            th(
              "Total Surcharges"
            ),
            th(
              "Surcharged Premium"
            )
          )
        )
      )
    )


    DT::datatable(
      out,
      container = contain,
      rownames = FALSE,
      caption = cap,
      editable = TRUE,
      selection = "none",
      # class = "compact stripe row-border nowrap",
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
          ),
          list(
            extend = 'colvis',
            text = 'Edit Displayed Columns'
          )
        )
      )
    ) %>%
      DT::formatRound(
        c("Model AOP Rate",
          "Model CAT Rate",
          "Model All Risk Rate",
          "Model Terrorism Rate"
        ), digits = 3) %>%
      DT::formatCurrency(c(
        "Total Insured Value (TIV)",
        "Initial Model Premium",
        "Claim Surcharges",
        "Surcharged Premium",
        "Premium After Cap/Rebal",
        "Allocated Expenses",
        "Risk Engineering",
        "Man Adj",
        "Final Premium"
      ), digits = 0) %>%
      DT::formatPercentage(c(
        "Net Capped Rate % Chg"
      ), digits = 2) %>%
      DT::formatString(
        "Location", prefix = "Location "
      ) %>%
      DT::formatString("Department", prefix = "Department ")
  })

  allocation_table_proxy <- DT::dataTableProxy("allocation_table")

  # observe event to table edit inline by user
  shiny::observeEvent(input$allocation_table_cell_edit, {

    info <- input$allocation_table_cell_edit
    utils::str(info)

    new_display_data <- DT::editData(
      table_prep(),
      info,
      proxy = allocation_table_proxy,
      rownames = FALSE,
      resetPaging = FALSE
    )

    new_r_data <- new_display_data %>%
      reverse_labels(dict = dictionary, dataset_name = "allocation") %>%
      dplyr::mutate(
        final_premium = .data$risk_engineering + .data$allocated + .data$man_adj +
          .data$allocated_expenses
      )

    # adjust totals:
    new_display_data <- new_display_data %>%
      dplyr::mutate(
        `Final Premium` = `Risk Engineering` + `Man Adj` +
          `Allocated Expenses` + `Premium After Cap/Rebal`
      )

    allocation_data_rv$r_data <- new_r_data
    allocation_data_rv$display_data <- new_display_data

  })

  details_data <- reactive({

    # if (is.null(allocation_data_rv$r_data())) {
    # hold <- allocation_data()
    # } else {
    # hold <- allocation_data_rv$r_data()
    # }

    allocation_data_rv$r_data %>%
      dplyr::left_join(
        allocation_data_full() %>%
          dplyr::select(
            # location,
            entity_id,
            department,
            prior_allocated,
            prior_tiv,
            prior_expenses,
            # capped_rate_percent_change,
            total_market_cat_premium,
            market_aop_premium_adj,
            market_terrorism_premium,
            bg_aop_premium,
            total_bg_cat_premium,
            bg_terrorism_premium
          ), by = c("entity_id", "department")
      ) %>%
      dplyr::mutate(
        prior_allocated = prior_allocated + prior_expenses,
        dollar_change = final_premium - prior_allocated,
        # curr_rate = final_premium / tiv,
        # prior_rate = prior_allocated / prior_tiv,
        # capped_rate_percent_change_net = capped_rate_percent_change - pct_change(), #(curr_rate / prior_rate) - 1,
        market_premium_plus_expense = total_market_cat_premium +
          market_aop_premium_adj + market_terrorism_premium +
          allocated_expenses,
        dollar_change_market = final_premium - market_premium_plus_expense,
        percent_change_market = (final_premium / market_premium_plus_expense) - 1,
        percent_change_market_rate = ((final_premium / tiv) / (market_premium_plus_expense / tiv)) - 1,
        bg_premium_plus_expense = bg_aop_premium + total_bg_cat_premium +
          bg_terrorism_premium + allocated_expenses,
        dollar_change_bg = final_premium - bg_premium_plus_expense,
        percent_change_bg = (final_premium / bg_premium_plus_expense) - 1,
        percent_change_bg_rate = ((final_premium / tiv) / (bg_premium_plus_expense / tiv)) - 1,
        percent_change_rate = ((final_premium / tiv) / (prior_allocated / prior_tiv)) - 1
      ) %>%
      dplyr::select(
        entity_id,
        department,
        final_premium,
        prior_allocated,
        dollar_change,
        percent_change_rate,
        market_premium_plus_expense,
        dollar_change_market,
        percent_change_market_rate,
        bg_premium_plus_expense,
        dollar_change_bg,
        percent_change_bg_rate
      )
  })


  # inputs_menu_list_rv <- reactive({
  #   inputs_menu_list
  # })

  details_data_rv <- shiny::reactiveValues()

  # observe event change to details data
  shiny::observeEvent(details_data(), {
    details_data_rv$r_data <- details_data()

    # browser()

    hold <- details_data()

    if (!("bg" %in% input$features)) {
      hold <- hold %>%
        dplyr::select(
          -bg_premium_plus_expense,
          -dollar_change_bg,
          -percent_change_bg_rate
        )
    }

    details_data_rv$display_data <- hold %>%
      apply_labels(dict = dictionary, dataset_name = "details")
  })

  details_prep <- shiny::reactive({
    details_data_rv$display_data
  })

  output$allocation_details <- DT::renderDT({

    shiny::req(details_prep())

    out <- details_prep() %>%
      dplyr::mutate(Location = extract_number(Location, return_as = "numeric"),
                    Department = extract_number(Department, return_as = "numeric")) %>%
      dplyr::arrange(Location) %>%
      dplyr::mutate(`Location` = as.factor(`Location`))

    cap <- "Allocation Details (Premiums Include Expenses; % Changes Applied to Rates)"

    curr_cols <- c(
      "Final Premium Plus Expense",
      "Prior Allocated Plus Expense",
      "Dollar Change Since Prior",
      "Market Premium Plus Expense",
      "Dollar Change Market",
      "Budget Guidance Premium Plus Expense",
      "Dollar Change Budget Guidance"
    )

    curr_cols <- curr_cols[curr_cols %in% names(out)]

    pct_cols <- c(
      "% Change (Rate)",
      "% Change Market (Rate)",
      "% Change Budget Guidance (Rate)"
    )

    pct_cols <- pct_cols[pct_cols %in% names(out)]

    contain <- htmltools::withTags(
      table(
        # class = "compact stripe row-border",
        thead(
          tr(
            th(
              class = "dt-center",
              rowspan = 2,
              "Location"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              "Department"
            ),
            th(
              class = "dt-center",
              rowspan = 2,
              "Final Premium + Expense"
            ),
            th(
              class = "dt-center",
              colspan = 3,
              rowspan = 1,
              "Priors"
            ),
            th(
              class = "dt-center",
              colspan = 3,
              rowspan = 1,
              "Market"
            ),
            th(
              class = "dt-center",
              colspan = 3,
              rowspan = 1,
              "Budget Guidance"
            )
          ),
          tr(
            lapply(rep(c("Premium", "$ Change", "% Change (Rate)"), 3), th)
          )
        )
      )
    )

    DT::datatable(
      out,
      rownames = FALSE,
      caption = cap,
      container = contain,
      # editable = TRUE,
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
          ),
          list(
            extend = 'colvis',
            text = 'Edit Displayed Columns'
          )
        )
      )
    ) %>%
      DT::formatPercentage(pct_cols, digits = 2) %>%
      DT::formatCurrency(curr_cols, digits = 0) %>%
      DT::formatString("Location", prefix = "Location ") %>%
      DT::formatString("Department", prefix = "Department ")
  })

  details_table_proxy <- DT::dataTableProxy("allocation_details")

  # shiny::observeEvent(list(
  #   details_data(),
  #   input$allocation_table_cell_edit,
  #   input$details_table_cell_edit,
  #   input$threshold,
  #   input$bg_percent,
  #   input$features,
  #   input$experience_period,
  #   input$expense_weight_variable,
  #   inputs_menu_list_rv() #,
  #   # allocation_data_full()
  # ), {
  #
  #   # browser()
  #
  #   info <- input$allocation_details_cell_edit
  #   utils::str(info)
  #
  #   new_display_data <- details_data() %>%
  #     arrange_by_location() %>%
  #     apply_labels(dict = dictionary, dataset_name = "details")
  #
  #   if (!is.null(info)) {
  #     new_display_data <- DT::editData(
  #       details_prep(),
  #       info,
  #       proxy = details_table_proxy,
  #       rownames = FALSE,
  #       resetPaging = FALSE
  #     )
  #   }
  #
  #   hold <- allocation_data_full() %>%
  #     dplyr::mutate(
  #       dollar_change = final_premium - prior_allocated,
  #       percent_change = (final_premium / prior_allocated) - 1,
  #       market_premium_plus_expense = total_market_cat_premium +
  #         market_aop_premium_adj + market_terrorism_premium +
  #         allocated_expenses,
  #       dollar_change_market = final_premium - market_premium_plus_expense,
  #       percent_change_market = (final_premium / market_premium_plus_expense) - 1,
  #       bg_premium_plus_expense = bg_aop_premium + total_bg_cat_premium +
  #         bg_terrorism_premium + allocated_expenses,
  #       dollar_change_bg = final_premium - bg_premium_plus_expense,
  #       percent_change_bg = (final_premium / bg_premium_plus_expense) - 1
  #     ) %>%
  #     dplyr::select(
  #       location,
  #       department,
  #       final_premium,
  #       prior_allocated,
  #       dollar_change,
  #       percent_change,
  #       market_premium_plus_expense,
  #       dollar_change_market,
  #       percent_change_market,
  #       bg_premium_plus_expense,
  #       dollar_change_bg,
  #       percent_change_bg
  #     )
  #
  #   new_r_data <- new_display_data %>%
  #     reverse_labels(dict = dictionary, dataset_name = "details") %>%
  #     coalesce_join(hold, by = c("location", "department"))
  #
  #   new_display_data_out <- new_r_data %>%
  #     arrange_by_location() %>%
  #     apply_labels(dict = dictionary, dataset_name = "details")
  #
  #   details_data_rv$r_data <- new_r_data # details_data()
  #   details_data_rv$display_data <- new_r_data %>% #details_data()
  #     arrange_by_location() %>%
  #     apply_labels(dict = dictionary, dataset_name = "details")
  #
  # }, ignoreInit = TRUE)

  # shiny::observeEvent(input$allocation_table_cell_edit, {
  #
  #   browser()
  #
  #   info <- input$allocation_table_cell_edit
  #   utils::str(info)
  #
  #   new_display_data <- DT::editData(
  #     table_prep(),
  #     info,
  #     proxy = allocation_table_proxy,
  #     rownames = FALSE,
  #     resetPaging = FALSE
  #   )
  #
  #   new_r_data <- new_display_data %>%
  #     reverse_labels(dict = dictionary, dataset_name = "allocation") %>%
  #     dplyr::mutate(
  #       re_allocated = .data$risk_engineering + .data$allocated,
  #       final_premium = .data$re_allocated + .data$man_adj
  #     )
  #
  #   allocation_data_rv$r_data <- new_r_data
  #   allocation_data_rv$display_data <- new_display_data
  # })

  allocation_data_out <- reactive({

    allocation_data_full() %>%
      coalesce_join(allocation_data(), by = c("department", "entity_id"))

  })

  return(list(
    allocation_data = allocation_data,
    allocation_data_full = allocation_data_out,
    details_data = details_data
  ))


}
