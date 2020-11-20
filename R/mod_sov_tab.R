#' SOV Tab Module - UI
#'
#' This function defines the user-interface behind the Schedule of Values (SOV)
#' input tab within the shiny application.
#'
#' @param id Shiny Namespace ID
#'
#' @return HTML for a [shiny::tabPanel] (for the [shinydashboard::tabBox])
#' @export
#' @importFrom DT DTOutput
#' @importFrom highcharter highchartOutput
#' @importFrom htmltools hr div h3 br tags
#' @importFrom shiny NS tabPanel actionButton icon fluidRow column
#' @importFrom shinycustomloader withLoader
#' @importFrom shinydashboard tabBox valueBoxOutput
#' @importFrom shinyWidgets pickerInput prettyCheckbox
sov_tab_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = icon_text("file-invoice-dollar", " Schedule of Values"),
    width = 12,

    flucol(
      width = 12,

      htmltools::hr(),

      htmltools::div(
        class = "text-center",
        htmltools::h3(icon_text("file-invoice-dollar", "Schedule of Values (SOV):")),
        htmltools::br(),
        shiny::actionButton(ns("tab_tour"),
                            "Walkthrough",
                            icon = shiny::icon("shoe-prints")),
        actionButton(ns("table_features"),
                     label = "View Table Features",
                     icon = icon("info"),
                     style = "margin-left: 5px;"),
        htmltools::div(
          id = "tour_add_entity",
          style = "display: inline-block; padding-left: 5px;",
          shiny::actionButton(
            ns("add_entity"),
            "Add an Entity",
            class = "btn-success",
            style = "color: #fff;",
            icon = shiny::icon("plus")
          )
        )
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
              id = "tour_sov_table",
              DT::DTOutput(ns("sov_table")) %>%
                shinycustomloader::withLoader()
            )
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("sort-amount-down"),
          flucol(
            width = 12,
            shinyWidgets::pickerInput(
              ns("group_by"),
              label = "Group Data by:",
              choices = c("Country" = "country", "Region" = "region", "Division" = "division", "Department" = "department"),
              selected = "country",
              multiple = FALSE
            ),
            # shinyWidgets::prettyCheckbox(
            #   ns("stack_bu"),
            #   label = "Stack by Business Unit?",
            #   value = TRUE,
            # ),
            shinyWidgets::prettyCheckbox(
              ns("separate_bu"),
              label = "Separate Out Business Units?",
              value = FALSE
            ),
            icon_text("info",
                      "NOTE: When the scrollbar is in use, the chart will re-scale as you scroll down."),
            highcharter::highchartOutput(ns("sov_chart"),
                                         height = "1000px") %>%
              shinycustomloader::withLoader()
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("globe"),
          flucol(
            width = 12,
            shinyWidgets::pickerInput(
              ns("map_by"),
              label = "Pick a Map to Display:",
              choices = c(
                "Continents" = "custom/world-continents",
                "Countries" = "custom/world",
                "USA States" = "countries/us/us-all"
              ),
              selected = "custom/world-continents",
              multiple = FALSE
            ),
            highcharter::highchartOutput(ns("world_map"),
                                         height = "800px") %>%
              shinycustomloader::withLoader(),
            htmltools::hr(),
            DT::dataTableOutput(ns("world_map_table"))
          )
        ),

        shiny::tabPanel(
          title = " ",
          icon = shiny::icon("info-circle"),
          flucol(
            width = 12,
            htmltools::div(
              id = ns("valboxes"),
              shiny::fluidRow(
                shiny::column(
                  offset = 2,
                  width = 9,
                  shinydashboard::valueBoxOutput(ns("valbox_tiv"), 4),
                  shinydashboard::valueBoxOutput(ns("valbox_count"), 4),
                  shinydashboard::valueBoxOutput(ns("valbox_avg_tiv"), 4)
                )
              )
            ),

            htmltools::hr(),
            htmltools::tags$h3("SOV Data Information & Summary Report: "),
            htmltools::tags$hr(),
            flucol(
              width = 12 #,
              # shiny::uiOutput(ns("report")) %>%
              #   shinycustomloader::withLoader()
            )
          )
        )
      )
    ),

    htmltools::tags$script(src = "https://code.highcharts.com/mapdata/custom/world-continents.js"),
    htmltools::tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
    htmltools::tags$script(src = "https://code.highcharts.com/mapdata/countries/us/custom/us-all-territories.js"),

    htmltools::tags$script(src = "www/sov_table_module.js"),
    htmltools::tags$script(
      paste0("sov_table_module_js('", ns(''), "')")
    )

  )
}

#' SOV Tab Module - Server
#'
#' @inheritParams app_server
#' @param initial_sov data.frame for initial table representing sov.
#' @param dictionary internal
#'
#' @return sov table
#' @importFrom cli cli_rule
#' @importFrom dplyr select mutate mutate_at vars arrange mutate_if rename filter case_when group_by summarise ungroup group_by_at desc glimpse
#' @importFrom DT renderDT datatable JS formatCurrency formatString
#' @importFrom highcharter renderHighchart hcmap hc_title hc_mapNavigation highchart hc_chart hc_subtitle hc_xAxis hc_yAxis hc_legend hc_tooltip hc_exporting hc_plotOptions hc_add_series
#' @importFrom htmltools tags
#' @importFrom purrr map_chr
#' @importFrom rintrojs introjs
#' @importFrom shiny reactiveValues observeEvent req eventReactive callModule reactive showModal modalDialog
#' @importFrom shinydashboard renderValueBox valueBox
#' @importFrom stringr str_to_title
#' @importFrom tibble tibble
sov_tab <- function(input, output, session,
                    initial_sov,
                    dictionary) {

  # initialize data
  initial_r_data <- initial_sov %>%
    dplyr::select(
      entity_id,
      bu:tiv,
      aop_sprinkler_tier:aop_tiv_size_bucket,
      cat_wind_hurricane,
      aop_id:terrorism_id
    ) %>%
    dplyr::mutate(tiv = round(tiv, 2))

  # highcharter::data_to_hierarchical(sov, group_vars = c(region, country), size_var = tiv) %>% highcharter::hchart(type = "sunburst")

  # initialize reactiveVal's for R and Display datasets
  sov_data <- shiny::reactiveValues()
  sov_data$r_data <- initial_r_data

  shiny::observeEvent(sov_data$r_data, {

    hold <- sov_data$r_data
    ids <- hold$entity_id
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
        <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
        <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
      </div>'
      )
    })

    display_data_init <- hold %>%
      dplyr::mutate(state = ifelse(state == "Non-USA", NA_character_, state)) %>%
      arrange_by_entity() %>%
      apply_labels(dict = dictionary, dataset_name = "sov")

    out <- cbind(tibble::tibble("Actions" = actions), display_data_init)

    sov_data$display_data <- out

  })

  table_prep <- reactive({
    sov_data$display_data %>%
      dplyr::mutate_at(dplyr::vars(`Location`, `Unit`, `Department`),
                       extract_number, return_as = "numeric") %>%
      dplyr::arrange(`Location`) %>%
      dplyr::mutate_at(dplyr::vars(`Location`, `Unit`, `Department`),
                       as.factor) %>%
      dplyr::mutate_if(is.character, as.factor)
  })

  # container <- htmltools::withTags(
  #   table(
  #     thead(
  #       tr(
  #         th(rowspan = 2, "Location ID"),
  #
  #       )
  #     )
  #   )
  # )

  output$sov_table <- DT::renderDT({

    shiny::req(table_prep())

    out <- table_prep()

    n_row <- nrow(out)
    n_col <- length(out)

    id <- session$ns("sov_table")

    js <- paste0(
      "function(settings, json) {
        var filters = $('#", id, " td .form-group');
        // columns that should have visible filters
        var cols = [", paste(2:n_col, collapse = ", "), "];
        // hide certain column filters
        for (var i = 1; i <= filters.length; i++) {
          if (!cols.includes(i))
            filters.eq(i - 1).css('visibility', 'hidden');
          filters.eq(i - 1).css('position', 'static');
        }
      }")

    cap <- c(
      "Schedule of Values Data as of December 31, 2020"
    )

    cols <- out %>% dplyr::rename("TIV" = `Total Insured Value (TIV)`) %>% names()
    hide_cols <- c("AOP Sprinkler Tier",
                   "AOP Combustible",
                   "AOP TIV Size Bucket",
                   "CAT-Wind Hurricane",
                   "State (USA)",
                   "Unit",
                   "Division") %>% match(names(out)) - 1
    center_cols <- names(out) %>% setdiff(c("Total Insured Value (TIV)")) %>%
      match(names(out)) - 1

    DT::datatable(
      out,
      rownames = FALSE,
      colnames = cols,
      caption = cap,
      selection = "none",
      class = "stripe row-border compact", #
      callback = DT::JS('return table'),
      style = "bootstrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      extensions = c("Buttons", "Scroller"), #, "FixedColumns"),
      filter = "top",
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = 500,
        # scrollCollapse = TRUE,
        # deferRender = TRUE,
        # scroller = TRUE,
        dom = '<Bl>tip',
        # pageLength = 25,
        lengthMenu = list(
          c(-1, 25, 50, 100),
          c("All", "25", "50", "100")
        ),
        columnDefs = list(
          list(visible = FALSE, targets = hide_cols),
          list(targets = 0, orderable = FALSE),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = list(
          list(
            extend = "copy",
            text = '<i class="fa fa-paperclip"></i>',
            titleAttr = 'Copy',
            exportOptions = list(columns = 1:(length(out) - 1),
                                 modifier = list(selected = NULL))
          ),
          list(
            extend = "print",
            text = '<i class="fa fa-print"></i>',
            titleAttr = 'Print',
            autoPrint = FALSE,
            exportOptions = list(columns = 1:(length(out) - 1),
                                 modifier = list(selected = NULL))
          ),
          list(
            extend = "excel",
            text = '<i class="fa fa-file-excel-o"></i>',
            titleAttr = "Excel",
            title = paste0("Schedule-of-Values-", Sys.Date()),
            exportOptions = list(columns = 1:(length(out) - 1),
                                 modifier = list(selected = NULL))
          ),
          list(
            extend = "csv",
            text = '<i class="fa fa-file-csv"></i>',
            titleAttr = "CSV",
            title = paste0("Schedule-of-Values-", Sys.Date()),
            exportOptions = list(columns = 1:(length(out) - 1),
                                 modifier = list(selected = NULL))
          ),
          list(
            extend = 'pdf',
            text = '<i class="fa fa-file-pdf-o"></i>',
            titleAttr = 'PDF',
            orientation = 'landscape',
            pageSize = "LEGAL",
            download = 'open',
            title = paste0("Schedule-of-Values-", Sys.Date()),
            exportOptions = list(columns = ":visible"),
            modifier = list(selected = NULL)
          ),
          list(
            extend = "colvis",
            text = '<i class = "fa fa-filter"></i>',
            titleAttr = "Column Visibility"#,
            # postfixButtons = 'colvisRestore',
            # collectionLayout = "three-column" #,
            # columns = ':not(.showalways)'
          ),
          list(
            extend = "pageLength",
            text = '<i class="fa fa-list"></i>',
            titleAttr = "Page Length"
          )
        ),
        initComplete = DT::JS(js)
      )
    ) %>%
      DT::formatCurrency(c("Total Insured Value (TIV)"), digits = 0) %>%
      DT::formatString("Location", prefix = "Location ") %>%
      DT::formatString("Unit", prefix = "Unit ") %>%
      DT::formatString("Department", prefix = "Department ")
  })

  # sov_proxy <- DT::dataTableProxy("sov_table")

  observe({
    print(list("entity_id_to_edit" = input$entity_id_to_edit))
  })

  entity_to_edit <- shiny::eventReactive(input$entity_id_to_edit, {
    sov_data$r_data %>%
      dplyr::filter(entity_id == input$entity_id_to_edit)
  })

  shiny::callModule(
    edit_entity,
    "edit_entity",
    entity_to_edit,
    sov_data
  )

  shiny::callModule(
    edit_entity,
    "add_entity",
    entity_to_edit = function() NULL,
    sov_data,
    trigger = shiny::reactive({ input$add_entity })
  )

  observeEvent(nrow(table_prep()) > nrow(initial_r_data), {

  })




  # sov_table_proxy <- DT::dataTableProxy("sov_table")

  # observeEvent(input$sov_table_cell_edit, {
  #
  #   info <- input$sov_table_cell_edit
  #   str(info)
  #
  #   new_display_data <- DT::editData(
  #     table_prep(),
  #     info,
  #     proxy = sov_table_proxy,
  #     rownames = FALSE,
  #     resetPaging = FALSE
  #   )
  #
  #   new_r_data <- new_display_data %>% reverse_labels(dict = dictionary, dataset_name = "sov")
  #
  #   sov_data$r_data <- new_r_data
  #   sov_data$display_data <- new_display_data
  #
  # })

  shiny::observeEvent(input$tab_tour, {
    rintrojs::introjs(
      session,
      options = list(
        steps = list(
          list(
            element = "#tour_sov_table",
            intro = "Here, the Total Insured Value for each entity, as well as several of their other attributes, such as geography and business unit, can be set.<br/><br/>
            To change a value, double click on the item in the table.<br/><br/>
            Make sure that you check out all the pages of the table by clicking through them at the bottom of the table."
          ),

          list(
            element = "#tour_add_entity",
            intro = "Add new entities to the table below by clicking this button.<br/><br/>
            Entities cannot be removed."
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

  output$valbox_tiv <- shinydashboard::renderValueBox({
    val <- sum(sov_data$r_data$tiv) %>%
      format_round_dollar()

    shinydashboard::valueBox(
      value = val,
      subtitle = toupper("Total Current TIV"),
      icon = htmltools::tags$i(
        class = "fa fa-money",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "navy",
      width = 4
    )
  })

  output$valbox_count <- shinydashboard::renderValueBox({
    val <- nrow(sov_data$r_data) %>%
      prettyNum(big.mark = ",", digits = 0)

    shinydashboard::valueBox(
      value = val,
      subtitle = toupper("Location Count"),
      icon = htmltools::tags$i(
        class = "fa fa-list-ol",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "navy",
      width = 4
    )
  })

  output$valbox_avg_tiv <- shinydashboard::renderValueBox({
    val <- mean(sov_data$r_data$tiv) %>%
      format_round_dollar()

    shinydashboard::valueBox(
      value = val,
      subtitle = toupper("Avergage TIV"),
      icon = htmltools::tags$i(
        class = "fa fa-blance-scale",
        style = "color: #ffffff; padding-right: 30px"
      ),
      color = "navy",
      width = 4
    )
  })

  map_prep <- reactive({

    if (input$map_by == "custom/world-continents") {

      hold <- sov_data$r_data %>%
        dplyr::mutate_if(is.numeric, round, 0) %>%
        dplyr::mutate(
          continent = dplyr::case_when(
            region == "cala" ~ "sa",
            region == "apac" ~ "oc",
            region == "middle_east_africa" ~ "af",
            region == "europe" ~ "eu",
            region == "canada" ~ "na",
            region == "usa" ~ "na",
            TRUE ~ "na"
          )
        ) %>%
        dplyr::select(`hc-key` = continent, tiv) %>%
        dplyr::group_by(
          `hc-key`
        ) %>%
        dplyr::summarise(tiv = sum(tiv, na.rm = TRUE)) %>%
        dplyr::ungroup()

      data <- hold %>%
        dplyr::transmute(
          Continent = `hc-key`,
          Continent = dplyr::case_when(
            Continent == "sa"  ~ "CALA",
            Continent == "oc" ~ "Asia Pacific",
            Continent ==  "af" ~ "Middle East and Africa",
            Continent ==  "eu" ~ "Europe",
            Continent ==  "na" ~ "North America",
            TRUE ~ "na"),
          `Total Insured Value` = tiv
        )

      chart <- highcharter::hcmap(
        map = "custom/world-continents",
        download_map_data = FALSE,
        data = hold,
        value = "tiv",
        joinBy = c("hc-key"),
        name = "Total Insured Value",
        borderWidth = 0,
        nullColor = "#d3d3d3",
        dataLabels = list(enabled = TRUE, format = '{point.name}'),
        states = list(hover = list(color = "#FF0000"))
      ) %>%
        highcharter::hc_title(text = "Total Insured Value by Continent") %>%
        highcharter::hc_mapNavigation(
          enabled = TRUE,
          buttonOptions = list(verticalAlign = "bottom")
        )

    } else if (input$map_by == "custom/world") {

      hold <- sov_data$r_data %>%
        dplyr::mutate_if(is.numeric, round, 0) %>%
        dplyr::mutate(
          name = dplyr::case_when(
            country == "United States" ~ "United States of America",
            country == "Hong Kong" ~ "Japan",
            country == "Aruba" ~ "Netherlands",
            country == "UAE" ~ "United Arab Emirates",
            country == "Trinidad & Tobago" ~ "Trinidad and Tobago",
            TRUE ~ country
          )) %>%
        dplyr::select(name, tiv) %>%
        dplyr::group_by(
          `name`
        ) %>%
        dplyr::summarise(tiv = sum(tiv, na.rm = TRUE)) %>%
        dplyr::ungroup()

      data <- hold %>%
        dplyr::transmute(
          Country = name,
          `Total Insured Value` = tiv
        )

      chart <- highcharter::hcmap(
        map = "custom/world",
        download_map_data = FALSE,
        data = hold,
        value = "tiv",
        joinBy = c("name"),
        name = "Total Insured Value",
        borderWidth = 0,
        nullColor = "#d3d3d3",
        dataLabels = list(enabled = TRUE, format = '{point.name}'),
        states = list(hover = list(color = "#FF0000"))
      ) %>%
        highcharter::hc_title(text = "Total Insured Value by Country") %>%
        highcharter::hc_mapNavigation(
          enabled = TRUE,
          buttonOptions = list(verticalAlign = "bottom")
        )




    } else {

      hold <- sov_data$r_data %>%
        dplyr::mutate_if(is.numeric, round, 0) %>%
        dplyr::filter(!is.na(state)) %>%
        dplyr::mutate(`hc-a2` = state) %>%
        dplyr::select(`hc-a2`, tiv) %>%
        dplyr::group_by(
          `hc-a2`
        ) %>%
        dplyr::summarise(tiv = sum(tiv, na.rm = TRUE)) %>%
        dplyr::ungroup()

      data <- hold %>%
        transmute(
          State = dplyr::case_when(
            `hc-a2` == "DC" ~ "Washington D.C.",
            `hc-a2` == "PR" ~ "Puerto Rico",
            `hc-a2` == "Various" ~ "Various",
            `hc-a2` == "VI" ~ "Virgin Islands",
            TRUE ~ state.name[match(`hc-a2`, state.abb)]
          ),
          `Total Insured Value` = tiv
        )

      chart <- highcharter::hcmap(
        map = "countries/us/custom/us-all-territories",
        download_map_data = FALSE,
        data = hold,
        value = "tiv",
        joinBy = c("hc-a2"),
        name = "Total Insured Value",
        borderWidth = 0,
        nullColor = "#d3d3d3",
        dataLabels = list(enabled = TRUE, format = '{point.name}'),
        states = list(hover = list(color = "#FF0000"))
      ) %>%
        highcharter::hc_title(text = "Total Insured Value by State or Territory (USA)") %>%
        highcharter::hc_mapNavigation(
          enabled = TRUE,
          buttonOptions = list(verticalAlign = "bottom")
        )

    }

    list(
      data = data,
      chart = chart
    )

  })

  output$world_map <- highcharter::renderHighchart({

    map_prep()$chart

  })

  output$world_map_table <- DT::renderDataTable({

    DT::datatable(map_prep()$data,
                  rownames = FALSE,
                  fillContainer = FALSE,
                  class = "stripe row-border compact", #
                  callback = DT::JS('return table'),
                  style = "bootstrap",
                  # Escape the HTML in all except 1st column (which has the buttons)
                  extensions = c("Buttons"), #, "FixedColumns"),
                  options = list(
                    # autoWidth = TRUE,
                    # dom = '<Bl>tip',
                    # lengthMenu = list(
                    #   c(-1, 25, 50, 100),
                    #   c("All", "25", "50", "100")
                    # ),
                    columnDefs = list(
                      list(className = "dt-center dt-col", targets = "_all")
                    ) #,
                  #   buttons = list(
                  #     list(
                  #       extend = "copy",
                  #       text = '<i class="fa fa-paperclip"></i>',
                  #       titleAttr = 'Copy',
                  #       exportOptions = list(columns = 1:(length(out) - 1),
                  #                            modifier = list(selected = NULL))
                  #     ),
                  #     list(
                  #       extend = "print",
                  #       text = '<i class="fa fa-print"></i>',
                  #       titleAttr = 'Print',
                  #       autoPrint = FALSE,
                  #       exportOptions = list(columns = 1:(length(out) - 1),
                  #                            modifier = list(selected = NULL))
                  #     ),
                  #     list(
                  #       extend = "excel",
                  #       text = '<i class="fa fa-file-excel-o"></i>',
                  #       titleAttr = "Excel",
                  #       title = paste0("Schedule-of-Values-", Sys.Date()),
                  #       exportOptions = list(columns = 1:(length(out) - 1),
                  #                            modifier = list(selected = NULL))
                  #     ),
                  #     list(
                  #       extend = "csv",
                  #       text = '<i class="fa fa-file-csv"></i>',
                  #       titleAttr = "CSV",
                  #       title = paste0("Schedule-of-Values-", Sys.Date()),
                  #       exportOptions = list(columns = 1:(length(out) - 1),
                  #                            modifier = list(selected = NULL))
                  #     ),
                  #     list(
                  #       extend = 'pdf',
                  #       text = '<i class="fa fa-file-pdf-o"></i>',
                  #       titleAttr = 'PDF',
                  #       orientation = 'landscape',
                  #       pageSize = "LEGAL",
                  #       download = 'open',
                  #       title = paste0("Schedule-of-Values-", Sys.Date()),
                  #       exportOptions = list(columns = ":visible"),
                  #       modifier = list(selected = NULL)
                  #     ),
                  #     list(
                  #       extend = "pageLength",
                  #       text = '<i class="fa fa-list"></i>',
                  #       titleAttr = "Page Length"
                  #     )
                  #   )
                  )
    ) %>%
      DT::formatCurrency(c("Total Insured Value"), digits = 0)

  })


  # hold <- sov_data$r_data %>%
  #   dplyr::mutate_if(is.numeric, round, 0) %>%
  #   dplyr::mutate(
  #     continent = dplyr::case_when(
  #       region == "cala" ~ "sa",
  #       region == "apac" ~ "oc",
  #       region == "middle_east_africa" ~ "af",
  #       region == "europe" ~ "eu",
  #       region == "canada" ~ "na",
  #       region == "usa" ~ "na",
  #       TRUE ~ "na"
  #     )
  #   ) %>%
  #   dplyr::select(`hc-key` = continent, tiv)
  #
  # highcharter::hcmap(
  #   map = "custom/world-continents",
  #   # download_map_data = FALSE,
  #   data = hold,
  #   value = "tiv",
  #   joinBy = c("hc-key"),
  #   name = "Total Insured Value",
  #   borderWidth = 0,
  #   nullColor = "#d3d3d3",
  #   dataLabels = list(enabled = TRUE, format = '{point.name}'),
  #   states = list(hover = list(color = "#BADA5"))
  # ) %>%
  #   highcharter::hc_title(text = "Total Insured Value by Country") %>%
  #   highcharter::hc_mapNavigation(
  #     enabled = TRUE,
  #     buttonOptions = list(verticalAlign = "bottom")
  #   ) # %>%
  # highcharter::hc_colorAxis(
  # min = 0,
  # type = "logarithmic"
  # stops = highcharter::color_stops(colors = viridisLite::inferno(5)), type = "logarithmic"
  # )

  # highcharter::highchart(type = "map") %>%
  #   highcharter::hc_add_series(
  #     mapData = highcharter::JS('Highcharts.maps["custom/world-continents"]'), #"custom/world-continents", # ,
  #     data = hold, joinBy = c("hc-key"), name = "Total Insured Value by Continent",
  #     value = "tiv", borderWidth = 0, nullColor = "#d3d3d3",
  #   ) %>%

  # highcharter::hc_plotOptions(
  # series = list(
  #

  # )
  # ) %>%

  # highcharter::hc_subtitle(text = 'Source Map: <a href="http://code.highcharts.com/mapdata/custom/world-palestine-highres.js">World with Palestine areas, high resolution</a>') %>%


  output$sov_chart <- highcharter::renderHighchart({

    # hold <- sov_data$r_data %>%
    if (input$separate_bu) {

      hold <- sov_data$r_data %>%
        dplyr::group_by_at(
          c(input$group_by, "bu")
        ) %>%
        dplyr::summarise(
          tiv = sum(.data$tiv, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(tiv)) %>%
        apply_labels(dict = dictionary, dataset_name = "sov")

    } else {

      hold <- sov_data$r_data %>%
        dplyr::group_by_at(
          input$group_by
        ) %>%
        dplyr::summarise(
          tiv = sum(.data$tiv, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(tiv)) %>%
        apply_labels(dict = dictionary, dataset_name = "sov")

    }

    subtit <- paste0("Grouped by ", stringr::str_to_title(input$group_by))

    x_cats <- unique(hold[[1]])

    init <- highcharter::highchart() %>%
      highcharter::hc_chart(type = "bar") %>%
      highcharter::hc_title(
        text = "Total Insured Value",
        style = list(fontWeight = "bold")
      ) %>%
      highcharter::hc_subtitle(
        text = subtit,
        style = list(fontWeight = "bold")
      ) %>%
      highcharter::hc_xAxis(
        min = 0,
        max = ifelse(length(x_cats) > 9, 10, length(x_cats) - 1),
        # type = "category",
        # reversed = TRUE,
        scrollbar = list(enabled = ifelse(length(x_cats) > 9, TRUE, FALSE),
                         liveRedraw = ifelse(length(x_cats) > 9, TRUE, FALSE)),
        categories = x_cats,
        title = list(
          text = stringr::str_to_title(input$group_by),
          style = list(fontWeight = 'bold')
        )
      ) %>%
      highcharter::hc_yAxis(
        min = 0,
        title = list(
          text = "Total Insured Value (TIV)",
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
        headerFormat = paste0("<b>", stringr::str_to_title(input$group_by), ": {point.x}:</b><br/>"),
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
        bar = list(
          dataLabels = list(
            enabled = TRUE,
            formatter = data_labels_dollar_formatter()
          )
        )
      )

    # if (input$stack_bu) {
    #   init <- init %>%
    #     highcharter::hc_plotOptions(
    #       bar = list(
    #         dataLabels = list(
    #           stacking = "normal"
    #         )
    #       )
    #     ) %>%
    #     highcharter::hc_yAxis(
    #       reversedStacks = FALSE
    #     )
    # }

    if (!input$separate_bu) {
      init %>%
        highcharter::hc_add_series(
          x = hold[[stringr::str_to_title(input$group_by)]],
          data = hold$`Total Insured Value (TIV)`,
          name = "TIV",
          tooltip = list(
            pointFormat = tooltip_formatter_dollar()
          )
        )
    } else {

      bus <- unique(hold$`Business Unit`)

      hold_1 <- hold %>% dplyr::filter(`Business Unit` == bus[1])
      hold_2 <- hold %>% dplyr::filter(`Business Unit` == bus[2])

      init %>%
        highcharter::hc_add_series(
          x = hold_1[[stringr::str_to_title(input$group_by)]],
          data = hold_1$`Total Insured Value (TIV)`,
          name = bus[1],
          tooltip = list(
            pointFormat = tooltip_formatter_dollar()
          )
        ) %>%
        highcharter::hc_add_series(
          x = hold_2[[stringr::str_to_title(input$group_by)]],
          data = hold_2$`Total Insured Value (TIV)`,
          name = bus[2],
          tooltip = list(
            pointFormat = tooltip_formatter_dollar()
          )
        )
    }


  })



  # output$valbox_edits <- shinydashboard::renderValueBox({
  #   val <- sum(sov_data$r_data$tiv) %>%
  #     format_round_dollar()
  #
  #   shinydashboard::valueBox(
  #     value = val,
  #     subtitle = toupper("Total Current TIV"),
  #     icon = htmltools::tags$i(
  #       class = "fa fa-money",
  #       style = "color: #ffffff; padding-right: 30px"
  #     ),
  #     color = "navy",
  #     width = 3
  #   )
  # })

  observeEvent(sov_out(), {
    sov <- sov_out()
    cli::cli_rule("SOV")
    dplyr::glimpse(sov)
    cli::cli_rule()
  })

  sov_out <- reactive({
    sov_data$r_data
  })

  sov_out

}


#' @importFrom dplyr right_join bind_rows add_row
#' @importFrom shiny showModal modalDialog fluidRow column textInput selectInput numericInput modalButton actionButton eventReactive showNotification observeEvent removeModal
#' @importFrom tibble tibble
edit_entity <- function(input, output, session, entity_to_edit, sov_data, trigger = entity_to_edit) {

  ns <- session$ns

  shiny::observeEvent(trigger(), {

    hold <- entity_to_edit()
    if (!is.null(hold)) hold <- apply_labels(hold, dict = dictionary, dataset_name = "sov")

    new_id <- paste0("Location ", nrow(sov_data$r_data) + 1)

    shiny::showModal(
      shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(
            width = 6,

            shiny::textInput(
              ns("entity_id"),
              'Location',
              value = ifelse(is.null(hold), new_id, gsub("entity_", "Location ", hold$`Location`))
            ),
            shiny::selectInput(
              ns('bu'),
              'Business Unit',
              choices = pull_unique(sov, "bu", names = FALSE) %>%
                rlang::set_names(dictionary$value_label[match(., dictionary$value)]),
              selected = ifelse(is.null(hold), "", hold$`Business Unit`),
              multiple = FALSE
            ),
            shiny::selectInput(
              ns('region'),
              'Region',
              choices = pull_unique(sov, "region", names = FALSE) %>%
                rlang::set_names(dictionary$value_label[match(., dictionary$value)]),
              selected = ifelse(is.null(hold), "", hold$Region),
              multiple = FALSE
            ),
            shiny::selectInput(
              ns('country'),
              'Country',
              choices = pull_unique(sov, "country", names = FALSE) %>%
                rlang::set_names(dictionary$value_label[match(., dictionary$value)]),
              selected = ifelse(is.null(hold), "", hold$Country),
              multiple = FALSE
            ),
            shiny::selectInput(
              ns('state'),
              'State',
              choices = c("Non-USA" = "Non-USA", pull_unique(sov, "state", names = FALSE) %>%
                            rlang::set_names(dictionary$value_label[match(., dictionary$value)])),
              selected = ifelse(is.null(hold), "", ifelse(is.na(hold$`State (USA)`),
                                                          "Non-USA",
                                                          hold$`State (USA)`)),
              multiple = FALSE
            ),
            shiny::selectInput(
              ns('division'),
              'Division',
              choices = pull_unique(sov, "division", names = FALSE) %>%
                rlang::set_names(dictionary$value_label[match(., dictionary$value)]),
              selected = ifelse(is.null(hold), "", hold$Division),
              multiple = FALSE
            ),
            shiny::selectInput(
              ns('location'),
              'Unit',
              choices = pull_unique(sov, "location", names = FALSE) %>%
                rlang::set_names(dictionary$value_label[match(., dictionary$value)]),
              selected = ifelse(is.null(hold), "", gsub("location_", "Unit ", hold$Unit)),
              multiple = FALSE
            ),
            shiny::selectInput(
              ns('department'),
              'Department',
              choices = pull_unique(sov, "department", names = FALSE) %>%
                rlang::set_names(dictionary$value_label[match(., dictionary$value)]),
              selected = ifelse(is.null(hold), "", hold$Department),
              multiple = FALSE
            ),
            shiny::numericInput(
              ns("tiv"),
              "TIV",
              value = ifelse(is.null(hold), "", hold$`Total Insured Value (TIV)`),
              min = 0,
              step = 100
            )
          ),
          column(
            width = 6,

            shiny::selectInput(
              ns('aop_sprinkler_tier'),
              "AOP Sprinkler Tier",
              choices =  pull_unique(sov, "aop_sprinkler_tier", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`AOP Sprinkler Tier`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('aop_combustible'),
              "AOP Combustible",
              choices =  pull_unique(sov, "aop_combustible", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`AOP Combustible`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('aop_tiv_size_bucket'),
              "AOP TIV Size Bucket",
              choices =  pull_unique(sov, "aop_tiv_size_bucket", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`AOP TIV Size Bucket`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('cat_wind_hurricane'),
              "CAT Wind Hurricane",
              choices =  pull_unique(sov, "cat_wind_hurricane", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`CAT-Wind Hurricane`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('aop_id'),
              "AOP Identifier",
              choices =  pull_unique(sov, "aop_id", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`AOP ID`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('cat_eq_id'),
              "CAT EQ Identifier",
              choices =  pull_unique(sov, "cat_eq_id", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`CAT-EQ ID`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('cat_wind_id'),
              "CAT Wind Identifier",
              choices =  pull_unique(sov, "cat_wind_id", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`CAT-Wind ID`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('cat_flood_id'),
              "CAT Flood ID",
              choices =  pull_unique(sov, "cat_flood_id", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`CAT-Flood ID`),
              multiple = FALSE
            ),

            shiny::selectInput(
              ns('terrorism_id'),
              "Terrorism Identifier",
              choices =  pull_unique(sov, "terrorism_id", names = FALSE),
              selected = ifelse(is.null(hold), "", hold$`Terrorism ID`),
              multiple = FALSE
            )
          )
        ),
        title = "Add/Edit SOV Locations:",
        size = 'm',
        footer = list(
          shiny::modalButton('Cancel'),
          shiny::actionButton(
            ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )
  })

  edit_sov_dat <- shiny::eventReactive(input$submit, {

    # browser()
    if (!is.null(entity_to_edit())) {
      id <- entity_to_edit()$entity_id
      msg <- paste0("Location ", extract_number(id), " successfully edited!")
    } else {
      id <- paste0("entity_", nrow(sov_data$r_data) + 1)
      msg <- paste0("Location ", extract_number(id), " successfully added to the SOV!")
    }

    out <- tibble::tibble(
      entity_id = id,
      bu = input$bu,
      region = input$region,
      country = input$country,
      state = input$state,
      division = input$division,
      location = input$location,
      department = input$department,
      tiv = input$tiv,
      aop_sprinkler_tier = input$aop_sprinkler_tier,
      aop_combustible = input$aop_combustible,
      aop_tiv_size_bucket = input$aop_tiv_size_bucket,
      cat_wind_hurricane = input$cat_wind_hurricane,
      aop_id = input$aop_id,
      cat_eq_id = input$cat_eq_id,
      cat_wind_id = input$cat_wind_id,
      cat_flood_id = input$cat_flood_id,
      terrorism_id = input$terrorism_id
    )

    if (out$entity_id %in% sov_data$r_data$entity_id) {
      r_out <- coalesce_join(sov_data$r_data, out, join = dplyr::right_join, by = "entity_id")
    } else {
      r_out <- dplyr::bind_rows(
        sov_data$r_data,
        out
      )

      dictionary <<- dictionary %>%
        dplyr::add_row(
          dataset = "sov",
          variable = "entity_id",
          variable_label = "Location",
          value = id,
          value_label = paste0("Location ", extract_number(id)),
          value_order = NA
        )
    }

    shiny::showNotification(
      msg,
      type = "message"
    )

    sov_data$r_data <- r_out

  })

  # shiny::observeEvent(input$entity_id, {
  #   if (input$entity_id %in% sov_data$r_data$entity_id) {
  #     shinyFeedback::showFeedback(
  #       "entity_id",
  #       text = "Entity must be unique.",
  #
  #     )
  #   }
  # })

  shiny::observeEvent(edit_sov_dat(), {
    shiny::removeModal()
  })

  # shiny::observeEvent(input$separate_bu == TRUE, {
  #
  #     shinyWidgets::updatePrettyCheckbox(
  #       session = session, inputId = "stack_bu",
  #       label = "Stack by Business Unit?", value = FALSE
  #     )
  #
  # })
  #
  # shiny::observeEvent(input$stack_bu == TRUE, {
  #
  #     shinyWidgets::updatePrettyCheckbox(
  #       session = session, inputId = "separate_bu",
  #       label = "Separate Out Business Units?", value = FALSE
  #     )
  #
  # })


}

# r_data <- shiny::reactiveVal(initial_r_data)
# display_data <- shiny::reactiveVal(initial_display_data)

# setup observe to replace display data when r_data is changed
# shiny::observeEvent(r_data(), {
#   out <- r_data() %>% apply_labels(dict = dictionary, dataset_name = "sov")
#   display_data(out)
# })
#
# shiny::observeEvent(display_data(), {
#   out <- display_data() %>% reverse_labels(dict = dictionary, dataset_name = "sov")
#   r_data(out)
# })

# need to add action buttons to display data - 'sov_table_prep' reactiveVal
# sov_table_prep <- shiny::reactiveVal(NULL)
#
# shiny::observeEvent(list(display_data(), input$sov_table), {
#
#   # browser()
#
#   hold <- r_data()
#
#   ids <- hold$entity_id
#
#   actions <- purrr::map_chr(ids, function(id_) {
#     paste0(
#       '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
#         <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
#         <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
#       </div>'
#     )
#   })
#
#   out <- cbind(tibble::tibble("Actions" = actions), display_data())
#
#   if (is.null(sov_table_prep())) {
#
#     sov_table_prep(out)
#
#     display <- out %>%
#       select(-Actions)
#
#     display_data(display)
#
#   } else {
#
#     DT::replaceData(sov_table_proxy, out,
#                     resetPaging = FALSE, rownames = FALSE)
#
#     display <- out %>%
#       select(-Actions)
#
#     display_data(display)
#
#   }
#
# })

# observeEvent(input$sov_table, {
#   DT::editData(sov_table_prep(), )
# })

# shiny::callModule(
#   entity_edit_module,
#   "add_entity",
#   modal_title = "Add New Entity",
#   entity_to_edit = function() NULL,
#   modal_trigger = shiny::reactive({input$add_entity}),
#   initial_data = r_data
# )

# observe({
#   print(list("entity_id_to_edit" = input$entity_id_to_edit))
# })
#
# entity_to_edit <- shiny::eventReactive(input$entity_id_to_edit, {
#   r_data() %>%
#     dplyr::filter(entity_id == input$entity_id_to_edit)
# })
#
# observe({
#   print(list("entity_to_edit" = entity_to_edit()))
# })

# shiny::callModule(
#   entity_edit_module,
#   "edit_entity",
#   modal_title = "Edit Entity",
#   entity_to_edit = entity_to_edit,
#   modal_trigger = shiny::reactive({input$entity_id_to_edit}),
#   initial_data = r_data
# )

# entity_to_delete <- shiny::eventReactive(input$entity_id_to_delete, {
#   r_data() %>%
#     filter(entity_id == input$entity_id_to_delete) %>%
#     as.list()
# })

# callModule(
#   entity_delete_module,
#   "delete_entity",
#   modal_title = "Delete Entity",
#   entity_to_delete = entity_to_delete,
#   modal_trigger = reactive({input$entity_id_to_delete})
# )

# ret <- shiny::reactive({
#   r_data()
#
# })

# return(ret)

# TODO: return a list of r_data, display_data, and changes made.


# SOV TAB UTILS -----------------------------------------------------------


#' #' Entity Edit Module - Server
#' #'
#' #' This function is used within the SOV tab module.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom dplyr filter select
#' #' @importFrom purrr map_chr map pmap
#' #' @importFrom rlang set_names
#' #' @importFrom shiny reactive observeEvent showModal modalDialog fluidRow column modalButton actionButton eventReactive
#' #' @importFrom shinyFeedback showFeedbackDanger hideFeedback
#' #' @importFrom shinyjs disable enable
#' entity_edit_module <- function(input, output, session,
#'                                modal_title,
#'                                entity_to_edit,
#'                                modal_trigger,
#'                                initial_data) {
#'
#'   ns <- session$ns
#'
#'   initial_data <- reactive({
#'     initial_data()[NULL, ]
#'   })
#'
#'   # entity_to_edit <- entity_to_edit()
#'
#'   # derive choices from dictionary
#'   dict <- dictionary %>% dplyr::filter(dataset == "sov") %>%
#'     dplyr::select(variable, variable_label, value, value_label)
#'
#'   vars <- dict %>% pull_unique("variable")
#'
#'   labs <- vars %>%
#'     purrr::map_chr(function(x) {
#'       dict %>% dplyr::filter(variable == x) %>% pull_unique("variable_label")
#'     })
#'
#'   choices <- purrr::map(vars, get_choices, df = dict) %>%
#'     rlang::set_names(vars)
#'
#'   params <- list(
#'     col = vars,
#'     lab = labs
#'   )
#'
#'   shiny::observeEvent(modal_trigger(), {
#'
#'     if (is.null(entity_to_edit())) hold <- initial_data()
#'     if (!is.null(entity_to_edit())) hold <- entity_to_edit()
#'
#'     edit_inputs <- purrr::pmap(
#'       params, edit_input, choices = choices, dat = hold, ns = ns
#'     ) %>%
#'       rlang::set_names(vars)
#'
#'     shiny::showModal(
#'       shiny::modalDialog(
#'         shiny::fluidRow(
#'           shiny::column(
#'             width = 4,
#'             edit_inputs[1:6]
#'           ),
#'           shiny::column(
#'             width = 4,
#'             edit_inputs[7:12])
#'         ),
#'         shiny::column(
#'           width = 4,
#'           edit_inputs[13:18]
#'         ),
#'         title = modal_title,
#'         size = "m",
#'         footer = list(
#'           shiny::modalButton("Cancel"),
#'           shiny::actionButton(
#'             ns("submit"),
#'             "Submit",
#'             class = "btn btn-primary",
#'             style = "color: white"
#'           )
#'         )
#'       )
#'     )
#'
#'     #   # Observe event for "Modal" text input in Add/Edit sov Modal
#'     #   # `shinyFeedback`
#'     #   shiny::observeEvent(input$entity_id, {
#'     #     if (input$entity_id == "") {
#'     #       shinyFeedback::showFeedbackDanger(
#'     #         "entity_id",
#'     #         text = "Must enter a unique Entity ID!"
#'     #       )
#'     #       shinyjs::disable('submit')
#'     #     } else {
#'     #       shinyFeedback::hideFeedback("entity_id")
#'     #       shinyjs::enable('submit')
#'     #     }
#'     #   })
#'     #
#'   })
#'
#'   edit_entity_dat <- shiny::reactive({
#'     # session$userData$sov_trigger(session$userData$sov_trigger() + 1)
#'     hold <- entity_to_edit()
#'     list(
#'       entity_id = if(is.na(hold[1,1])) NA else hold$entity_id,
#'       data = purrr::map(vars, function(x) input[[x]]) %>% rlang::set_names(vars)
#'     )
#'   })
#'
#'   validate_edit <- shiny::eventReactive(input$submit, {
#'     dat <- edit_entity_dat()
#'
#'     # Logic to validate inputs...
#'
#'     dat
#'   })
#'
#'
#'
#'
#' }
#'
#' #' Get Choices
#' #'
#' #' @param df data
#' #' @param var variable
#' #'
#' #' @export
#' #'
#' #' @return named list
#' #'
#' #' @importFrom dplyr filter
#' #' @importFrom rlang set_names
#' get_choices <- function(df, var) {
#'
#'   hold <- dplyr::filter(df, variable == var)
#'
#'   vals <- pull_unique(hold, "value")
#'   labs <- pull_unique(hold, "value_label")
#'
#'   rlang::set_names(vals, labs)
#'
#' }
#'
#' #' Edit Input
#' #'
#' #' @param ns namespace
#' #' @param col column
#' #' @param lab label
#' #' @param dat data
#' #' @param choices choices list
#' #'
#' #' @return htmlwidget
#' #' @export
#' edit_input <- function(ns, col, lab, dat, choices) {
#'
#'   dat_col <- dat[[col]]
#'
#'   if (col %in% c("entity_id", "loss_run_id")) {
#'     class(dat_col) <- c("id", class(dat_col))
#'   }
#'
#'   UseMethod("edit_input", dat_col)
#'
#' }
#'
#' #' @describeIn edit_input Edit Input - Character
#' #' @export
#' #' @importFrom shiny selectInput
#' edit_input.character <- function(ns, col, lab, dat, choices) {
#'
#'   shiny::selectInput(
#'     ns(col),
#'     lab,
#'     choices = choices[[col]],
#'     selected = ifelse(is.na(dat[1,1]), "", dat[[col]])
#'   )
#'
#' }
#'
#' #' @describeIn edit_input Edit Input - Logical
#' #' @export
#' #' @importFrom shiny checkboxInput
#' edit_input.logical <- function(ns, col, lab, dat, choices) {
#'
#'   shiny::checkboxInput(
#'     ns(col),
#'     lab,
#'     value = ifelse(is.na(dat[1,1]), TRUE, dat[[col]]),
#'   )
#'
#' }
#'
#' #' @describeIn edit_input Edit Input - ID
#' #' @export
#' #' @importFrom shiny textInput
#' edit_input.id <- function(ns, col, lab, dat, choices) {
#'
#'   shiny::textInput(
#'     ns(col),
#'     lab,
#'     value = ifelse(is.na(dat[1,1]), "", dat[[col]])
#'   )
#'
#' }
#'
#' #' @describeIn edit_input Edit Input - Numeric
#' #' @export
#' #' @importFrom shiny numericInput
#' edit_input.numeric <- function(ns, col, lab, dat, choices) {
#'
#'   shiny::numericInput(
#'     ns(col),
#'     lab,
#'     value = ifelse(is.na(dat[1,1]), "", dat[1, col]),
#'     min = 0,
#'     step = 1
#'   )
#'
#' }
#'
