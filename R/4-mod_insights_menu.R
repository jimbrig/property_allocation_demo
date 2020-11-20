#' Insights Tab Module UI
#'
#' @param id namespace ID
#'
#' @return [shinydashboard::tabItem()]
#' @export
#' @importFrom highcharter highchartOutput
#' @importFrom htmltools h2
#' @importFrom shiny NS fluidRow column tabPanel
#' @importFrom shinydashboard tabItem tabBox
insights_menu_ui <- function(id) {

  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = ns(id),
    shiny::fluidRow(
      shiny::column(
        12,
        shinydashboard::tabBox(
          id = ns("tabbox"),
          title = icon_text("lightbulb", " Insights"),
          width = 12,

          shiny::tabPanel(
            title = icon_text("chart-area", " "),
            width = 12,
            flucol(
              width = 12,
              highcharter::highchartOutput(ns("area_chart"), height = "700px") %>%
                shinycustomloader::withLoader()
            )
          ),

          shiny::tabPanel(
            title = icon_text("chart-bar", " "),
            width = 12,
            flucol(
              width = 12,
              htmltools::h2("All Other Peril"),
              highcharter::highchartOutput(ns("aop_chart")) %>%
                shinycustomloader::withLoader(),
              htmltools::h2("Catastrophe"),
              highcharter::highchartOutput(ns("cat_eq_chart")) %>%
                shinycustomloader::withLoader()
            )
          )
        )
      )
    )
  )

}

#' Insights tab Server
#'
#' @inheritParams app_server
#'
#' @return a named list of all input tables
#' @export
#' @importFrom dplyr mutate group_by summarise_at vars rename case_when mutate_if select
#' @importFrom highcharter renderHighchart hchart hcaes
#' @importFrom tidyr gather
insights_menu <- function(input, output, session, allocation_data) {


  output$aop_chart <- highcharter::renderHighchart({

    dat <- allocation_data$allocation_data_full() %>%
      dplyr::mutate(
        model_cat_premium = model_cat_eq_premium_adj + model_cat_flood_premium_adj + model_cat_wind_premium_adj,
        model_cat_rate = model_cat_premium / tiv,
        prior_cat_rate = prior_total_cat_premium / prior_tiv,
        market_cat_rate = total_market_cat_premium / tiv,
        bg_aop_rate = bg_aop_premium / tiv,
        bg_cat_rate = total_bg_cat_premium / tiv,
        bg_terrorism_rate = bg_terrorism_premium / tiv
      ) #%>%
    # dplyr::select(entity_id:tiv,
    #               prior_tiv,
    #               aop_id:terrorism_id,
    #               aop_adj_tiv:terrorism_adj_tiv,
    #               total_counts, total_incurred,
    #               prior_aop_rate:model_aop_rate, bg_aop_rate,
    #               prior_cat_rate, market_cat_rate, model_cat_rate, bg_cat_rate,
    #               prior_terrorism_rate:model_terrorism_rate, bg_terrorism_rate,
    #               final_premium, claim_surcharges, allocated_expenses, total_model_premium_adj)

    aop <- dat %>%
      dplyr::group_by(aop_id) %>%
      dplyr::summarise_at(dplyr::vars(tiv, prior_tiv, prior_aop_premium, bg_aop_premium, market_aop_premium_adj, rebalanced_model_aop_premium_adj),
                          sum, na.rm = TRUE) %>%
      ungroup() %>%
      tidyr::gather(key = "Methodology", value = "Allocated Premium", 4:7) %>%
      dplyr::rename("AOP Identifier" = aop_id, "TIV" = tiv, "Prior TIV" = prior_tiv) %>%
      dplyr::mutate(
        `Methodology` = dplyr::case_when(
          `Methodology` == "prior_aop_premium" ~ "Prior",
          `Methodology` == "market_aop_premium_adj" ~ "Market",
          `Methodology` == "rebalanced_model_aop_premium_adj" ~ "Current",
          TRUE ~ "Budget Guidance"
        )
      ) %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      dplyr::mutate(Methodology = factor(Methodology, levels = c("Current", "Prior", "Market", "Budget Guidance")))
    # total <- dat %>%
    #   summarise()
    #   tidyr::pivot_longer(cols = prior_aop_rate:bg_aop_rate, names_to = "")


    highcharter::hchart(aop, "bar", highcharter::hcaes(x = `AOP Identifier`,
                                                       y = `Allocated Premium`,
                                                       group = `Methodology`))

  })

  output$cat_eq_chart <- highcharter::renderHighchart({

    dat <- allocation_data$allocation_data_full() %>%
      dplyr::mutate(
        model_cat_premium = model_cat_eq_premium_adj + model_cat_flood_premium_adj + model_cat_wind_premium_adj,
        model_cat_rate = model_cat_premium / tiv,
        prior_cat_rate = prior_total_cat_premium / prior_tiv,
        market_cat_rate = total_market_cat_premium / tiv,
        bg_aop_rate = bg_aop_premium / tiv,
        bg_cat_rate = total_bg_cat_premium / tiv,
        bg_terrorism_rate = bg_terrorism_premium / tiv,
        market_cat_premium = market_cat_eq_premium + market_cat_flood_premium + market_cat_wind_premium
      )

    cat_eq <- dat %>%
      dplyr::group_by(cat_eq_id) %>%
      dplyr::summarise_at(dplyr::vars(tiv, prior_tiv,
                                      prior_total_cat_premium,
                                      total_bg_cat_premium,
                                      market_cat_premium,
                                      model_cat_premium),
                          sum, na.rm = TRUE) %>%
      ungroup() %>%
      tidyr::gather(key = "Methodology", value = "Allocated Premium", 4:7) %>%
      dplyr::rename("CAT-EQ Identifier" = cat_eq_id, "TIV" = tiv, "Prior TIV" = prior_tiv) %>%
      dplyr::mutate(
        `Methodology` = dplyr::case_when(
          `Methodology` == "prior_total_cat_premium" ~ "Prior",
          `Methodology` == "total_bg_cat_premium" ~ "Budget Guidance",
          `Methodology` == "market_cat_premium" ~ "Market",
          TRUE ~ "Current"
        )
      ) %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      dplyr::mutate(Methodology = factor(Methodology, levels = c("Current", "Prior", "Market", "Budget Guidance")))


    highcharter::hchart(cat_eq, "bar", highcharter::hcaes(x = `CAT-EQ Identifier`,
                                                          y = `Allocated Premium`,
                                                          group = `Methodology`))

  })


  output$area_chart <- highcharter::renderHighchart({



    dat <- allocation_data$allocation_data_full() %>%
      dplyr::select(Location = entity_id, `Allocated Premium` = final_premium, TIV = tiv) %>%
      dplyr::mutate(
        Location = extract_number(Location)
      )

    highcharter::hchart(dat, "treemap", highcharter::hcaes(x = `Location`,
                                                           value = `Allocated Premium`,
                                                           color = TIV))

  })

  return(allocation_data)

}
