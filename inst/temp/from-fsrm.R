observeEvent(list(input$table_features_1, input$table_features_2), {

  shiny::showModal(
    shiny::modalDialog(
      title = icon_text("table", "Table Features Overview:"),
      easyClose = TRUE,
      size = "l",
      tags$iframe(src = "table_features.html",
                  width = "100%",
                  height = "630px",
                  scrolling = "auto",
                  frameborder = 0)
    )
  )

}, ignoreInit = TRUE)

current_filters <- reactive({

  tibble::tibble(
    "Label" = c(
      "Coverage",
      "Evaluation Date",
      "Development Age",
      "Grouping By",
      "Exposure Base",
      "Members",
      "Departments",
      "Incurred Min/Max",
      "Loss Date Min/Max",
      "Cause",
      "Day of Week",
      "Claim Type",
      "Claimant State",
      "Loss State",
      "Claim Size",
      "Report Lag",
      "Driver Age"
    ),
    "Value" = c(
      ifelse(cov == "wc", "Worker's Compensation", "Auto Liability"),
      format(eval(), "%B %d, %Y"),
      paste0(input$devt_age, " Months of Maturity"),
      ifelse(input$use_group_by == TRUE, toproper(input$group_by), "Totals"),
      stringr::str_to_title(exposure_type()),
      paste0(length(input$member), "/", length(pull_unique(loss_data, "member")),
             " Members Selected: ", knitr::combine_words(input$member)),
      paste0(length(input$department), "/", length(pull_unique(loss_data, "department")),
             " Departments Selected: ", knitr::combine_words(input$department)),
      paste0(
        paste0("Min: ",
               prettyNum(
                 input$incurred_limits[1], digits = 0, big.mark = ","
               )
        ),
        paste0("; Max: ",
               prettyNum(
                 input$incurred_limits[2], digits = 0, big.mark = ","
               )
        )
      ),
      paste0("Min: ", format(input$loss_date[1], "%B %d, %Y"),
             "; Max: ", format(input$loss_date[2], "%B %d, %Y")),

      paste0(length(input$cause), "/", length(pull_unique(loss_data, "cause")),
             " Causes Selected: ", knitr::combine_words(input$cause)),
      paste0(length(input$day_of_week), "/", length(pull_unique(loss_data, "day_of_week")),
             " Days of the Week Selected: ", knitr::combine_words(input$day_of_week)),
      paste0(length(input$claim_type), "/", length(pull_unique(loss_data, "claim_type")),
             " Claim Types Selected: ", knitr::combine_words(input$claim_type)),
      paste0(length(input$claimant_state), "/", length(pull_unique(loss_data, "claimant_state")),
             " Claimant States Selected: ",knitr::combine_words(input$claimant_state)),
      paste0(length(input$loss_state), "/", length(pull_unique(loss_data, "loss_state")),
             " Loss States Selected: ", knitr::combine_words(input$loss_state)),
      paste0(length(input$incurred_group), "/", length(pull_unique(loss_data, "incurred_group")),
             " Claim Size Groups Selected: ",knitr::combine_words(input$incurred_group)),
      paste0(length(input$report_lag_group), "/", length(pull_unique(loss_data, "report_lag_group")),
             " Report Lag Groups Selected: ",knitr::combine_words(input$report_lag_group)),
      ifelse(
        tolower(cov) == "wc", "N/A",
        paste0(length(input$driver_age_group), "/", length(pull_unique(loss_data, "driver_age_group")),
               " Driver Age Groups Selected: ", knitr::combine_words(input$driver_age_group))
      )
    )
  )

})

observe({
  print(list("Current Filters" = current_filters()))
})

output$active_filters <- renderDT({
  current_filters() %>%
    datatable(# fillContainer = TRUE,
      rownames = FALSE,
      caption = "Hover over a row to view all contents.",
      options = list(
        # paging = FALSE,
        processing = TRUE,
        # dom = "t",
        pageLength = nrow(current_filters()),
        columnDefs = list(
          list(
            # className = "dt-center",
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 160 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 160) + '...</span>' : data;",
              "}"
            )
          )
        )
      )
    )
})

observeEvent({
  list(input$reset_defaults1,
       input$reset_defaults2)
}, {

  default_expo_type <- switch(tolower(cov),
                              "wc" = "payroll",
                              "al" = "miles")

  shiny::showNotification("All Options and Filters Reset to Defaults.",
                          duration = 5, type = "message")

  updateAirDateInput(session = session, inputId = "eval_date", value = "2019-12-31")
  updateSliderTextInput(session = session, inputId = "devt_age", selected = 12)
  updateRadioGroupButtons(session = session, inputId = "exposure_type",
                          selected = default_expo_type)
  updateAirDateInput(session = session, inputId = "loss_date",
                     value = c(
                       beginning_of_month(min(loss_data$loss_date, na.rm = TRUE)),
                       end_of_month(max(loss_data$loss_date, na.rm = TRUE))
                     ))
  updateNumericRangeInput(session = session, inputId = "incurred_limits",
                          label = icon_text("dollar", "Select Per Claim Incurred Range:"),
                          value = ceiling(loss_data$total_incurred))

  # shinyjs::reset("devt_age")
  shinyjs::reset("metrics")
  shinyjs::reset("use_group_by")
  shinyjs::reset("group_by")
  shinyjs::reset("member")
  shinyjs::reset("department")
  shinyjs::reset("incurred_group")
  shinyjs::reset("report_lag_group")
  shinyjs::reset("driver_age_group")
  # shinyjs::reset("loss_date")
  # shinyjs::reset("incurred_limits")
  shinyjs::reset("cause")
  shinyjs::reset("day_of_week")
  shinyjs::reset("claim_type")
  shinyjs::reset("claimant_state")
  shinyjs::reset("loss_state")
  # shinyjs::reset("expo_type")
  # shinyjs::reset("filters")


}, ignoreInit = TRUE)
