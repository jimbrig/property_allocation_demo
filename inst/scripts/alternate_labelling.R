

sov_prior <- sov

sov <- sov_prior %>%
  dplyr::rename(id = entity_id, building = location) %>%
  dplyr::mutate(id = as.numeric(stringr::str_replace(id, "entity_", "")),
                building = as.numeric(stringr::str_replace(building, "location_", "")),
                bu = toupper(stringr::str_replace(bu, "bu_", "")),
                division = toupper(stringr::str_replace(division, "div_", "")),
                department = as.numeric(stringr::str_replace(department, "dept_", "")),
                region = dplyr::case_when(
                  region == "cala" ~ "CALA",
                  region == "apac" ~ "APAC",
                  region == "canada" ~ "Canada",
                  region == "europe" ~ "EU",
                  region == "middle_east_africa" ~ "Middle East and Africa",
                  region == "usa" ~ "United States of America"
                )) %>%
  dplyr::select(-tidyselect::contains("_coverage"))

sov_labelled <- sov %>%
  labelled::set_variable_labels(
    .labels = c("Location", "Lossrun ID", "Business Unit", "Region", "Country", "State",
                "Division", "Building", "Department", "TIV", "Sprinkler Tier", "Combustible",
                "TIV Size", "Wind or Hurricane?", "AOP ID", "CAT-EQ ID", "CAT-Wind ID",
                "CAT-Flood ID", "Terrorism ID")
  )

labelled::var_label(sov_labelled)

# labelled:::set_value_labels()

sov_dict <- labelled::generate_dictionary(sov_labelled) %>%
  dplyr::mutate(
    col_type = case_when(
      variable %in% c("id", "loss_run_id", "aop_id", "cat_eq_id",
                      "cat_wind_id", "cat_flood_id", "terrorism_id") ~ "key",
      variable %in% c("bu", "region", "country", "state", "division", "building",
                      "department", "aop_sprinkler_tier", "aop_combustible", "aop_tiv_size_bucket",
                      "cat_wind_hurricane") ~ "character",
      variable %in% c("tiv") ~ "dollar"
    ),
    prefix = c("Location ", "", "Business Unit ", "", "", "", "Division ", "Building ",
               "Department ", "", "", "", "", "", "", "", "", "", ""),
    class = unlist(class)
  ) %>%
  tibble::as_data_frame()

attr(sov_labelled, "dict") <- sov_dict

loss_run_prior <- loss_run

loss_run <- loss_run_prior %>%
  dplyr::rename(
    loss_run_id = location_dud,
    id = entity_id,
    accident_city = accident_location
  ) %>%
  dplyr::mutate(
    claim_number = stringr::str_remove(claim_number, "claim_")
  )

loss_run_labelled <- loss_run %>%
  labelled::set_variable_labels(
    .labels = c(
      "Claim Number",
      "Loss Run ID",
      "Location",
      "Date of Loss",
      "Total Incurred",
      "Accident Description",
      "Accident City",
      "Accident Year"
    )
  )

labelled::var_label(loss_run_labelled)

loss_run_dict <- labelled::generate_dictionary(loss_run_labelled) %>%
  dplyr::mutate(
    col_type = case_when(
      variable %in% c("claim_number", "loss_run_id", "id") ~ "key",
      variable %in% c("date_of_loss") ~ "date",
      variable %in% c("total_incurred") ~ "dollar",
      variable %in% c("year") ~ "numeric",
      variable %in% c("accident_description",
                      "accident_city") ~ "character",
    ),
    prefix = c("", "", "Location ", "", "", "", "", ""),
    class = unlist(class)
  ) %>%
  tibble::as_data_frame()

attr(loss_run_labelled, "dict") <- loss_run_dict



dress <- function(dat) {

  dict <- attr(dat, "dict") %>% tibble::as_tibble()

  out <- dat %>% rlang::set_names(dict$label) %>%
    labelled::remove_labels() %>%
    prefix_df()

}

undress <- function(dat) {

  dict <- attr(dat, "dict") %>% tibble::as_tibble()

  out <- dat %>% rlang::set_names(dict$variable) %>%
    labelled::remove_labels() %>%
    unprefix_df()

}

prefix_df <- function(dat) {

  dict <- attr(dat, "dict")
  prefix <- dict$prefix

  purrr::walk(names(dat), function(x) {

    prefix_ <- prefix[match(x, names(dat))]

    if (prefix_ != "") {
      dat[x] <<- paste0(prefix_, dat[[x]])
    }
  })

  return(dat)

}

unprefix_df <- function(dat) {

  dict <- attr(dat, "dict")
  prefix <- dict$prefix

  purrr::walk(names(dat), function(x) {

    prefix_ <- prefix[match(x, names(dat))]

    if (prefix_ != "") {
      dat[x] <<- stringr::str_remove_all(dat[[x]], prefix_)
    }

  })

  return(set_col_classes(dat))

}

set_col_classes <- function(dat) {

  hold <- dat
  dict <- attr(dat, "dict")
  classes <- sapply(hold, class)
  mismatched <- names(classes[classes != unlist(dict$class)])

  purrr::walk(mismatched, function(x) {

    class_to <- dict$class[match(x, dict$variable)]

    if (class_to == "numeric") {
      hold[[x]] <<- as.numeric(hold[[x]])
    }
    else if (tolower(class_to) == "date") {
     hold[[x]] <<- as.Date(hold)
    } else {
      hold[[x]] <<- as.character(hold[[x]])
    }

  })

  # attr(dat, "dict")[["class"]] %>%
  #   purrr::walk2(classes, names(classes), function(x, y) {
  #     class(hold[[y]]) <- x
  #   })
  #
  return(hold)

}
