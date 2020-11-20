#' Inputs Tab Module UI
#'
#' @param id namespace ID
#'
#' @return [shinydashboard::tabItem()]
#' @export
#' @importFrom shiny NS fluidRow column
#' @importFrom shinydashboard tabItem tabBox
#' @importFrom shiny NS fluidRow column
#' @importFrom shinydashboard tabItem tabBox
inputs_menu_ui <- function(id) {

  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = ns(id),
    shiny::fluidRow(
      shiny::column(
        12,
        shinydashboard::tabBox(
          id = ns("tabbox"),
          title = icon_text("inbox", " Inputs"),
          width = 12,
          # overview_tab_ui(ns("overview")),
          renewal_costs_tab_ui(ns("renewal_costs")),
          sov_tab_ui(ns("sov")),
          # priors_tab_ui(ns("priors")),
          loss_run_tab_ui(ns("loss_run")),
          rates_tab_ui(ns("rates")),
          rels_tab_ui(ns("rels")),
          count_buckets_tab_ui(ns("count_buckets"))
        )
      )
    )
  )

}

#' Inputs tab Server
#'
#' @inheritParams app_server
#'
#' @return a named list of all input tables
#' @export
#' @importFrom cli cli_rule
#' @importFrom dplyr glimpse
#' @importFrom shiny callModule reactive observe observeEvent
inputs_menu <- function(input, output, session) {

  extracted_costs <- shiny::callModule(renewal_costs_tab,
                                       "renewal_costs",
                                       initial_costs = renewal_costs,
                                       dictionary = dictionary)

  sov <- shiny::callModule(sov_tab,
                           "sov",
                           initial_sov = sov,
                           dictionary = dictionary)

  priors <- reactive({ priors })
  # shiny::callModule(priors_tab,
  #                             "priors",
  #                             initial_prior = priors,
  #                             dictionary = dictionary)

  loss_run <- shiny::callModule(loss_run_tab,
                                "loss_run",
                                initial_loss_run = loss_run,
                                dictionary = dictionary)

  rates <- shiny::callModule(rates_tab,
                             "rates",
                             initial_rates = rates,
                             dictionary = dictionary)

  count_buckets <- shiny::callModule(count_buckets_tab,
                                     "count_buckets",
                                     initial_count_buckets = count_buckets,
                                     dictionary = dictionary,
                                     loss_run = loss_run)

  bu_rels <- shiny::callModule(rels_tab,
                               "rels",
                               initial_rels = bu_rels,
                               dictionary = dictionary,
                               output_id = "bu_rels_table")

  sprinkler_tier_rels <- shiny::callModule(rels_tab,
                                           "rels",
                                           initial_rels = sprinkler_tier_rels,
                                           dictionary = dictionary,
                                           output_id = "sprinkler_tier_rels_table")

  combustible_rels <- shiny::callModule(rels_tab,
                                        "rels",
                                        initial_rels = combustible_rels,
                                        dictionary = dictionary,
                                        output_id = "combustible_rels_table")


  # derive Relativity Adjusted TIVs -----------------------------------------
  relativity_adjusted_tivs <- shiny::reactive({

    apply_rels(
      bu_rels(),
      sprinkler_tier_rels(),
      combustible_rels(),
      sov()
    )

  })

  out <- list(
    sov = sov,
    relativity_adjusted_tivs = relativity_adjusted_tivs,
    rates = rates,
    priors = priors,
    extracted_costs = extracted_costs,
    count_buckets = count_buckets,
    loss_run = loss_run
  )

  return(out)

}
