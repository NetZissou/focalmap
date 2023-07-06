data_drug_overdose_deaths <- function(agg_by = c("place", "zip", "census_tract")) {

  # ========================== #
  # ---- Latest file path ----
  # ========================== #

  file_name = switch(
    agg_by,
    "place" = "OD_11_21_2010places_Totals_v2.csv",
    "zip" = "OD_11_21_ZIPs_totals_v2.csv",
    "census_tract" = "OD_11_21_tracts_totals_v2.csv"
  )

  latest_data_info <-
    check_files_update_time(
      dir = fs::path(
        opioidDashboard::ROOT_PATH,
        "other",
        "Unintentional Drug Overdose Deaths"
      ),
      regexp = file_name
    ) %>%
    head(1)

  latest_data_path <- latest_data_info$path
  latest_data_modification_time <- latest_data_info$modification_time


  # =================== #
  # ---- Load file ----
  # =================== #
  message("Loading file: ", latest_data_path)
  message("Modified at: ", latest_data_modification_time)


  data <-
    vroom::vroom(
      latest_data_path
    ) %>%
    tidyr::pivot_longer(
      dplyr::all_of(as.character(c(2011:2021))),
      names_to = "year",
      values_to = "n"
    )

  return(data)
}


data_drug_overdose_deaths_zip_5yr_rate <- function() {

  data <- data_drug_overdose_deaths("zip")

  data %>%
    dplyr::mutate(ZIP_CODE = as.character(.data$ZIP_CODE)) %>%
    dplyr::filter(.data$year %in% as.character(c(2017:2021))) %>%
    dplyr::group_by(
      .data$ZIP_CODE
    ) %>%
    dplyr::summarise(n = sum(.data$n)) %>%
    dplyr::left_join(
      get_zipcode_sf(),
      by = c("ZIP_CODE" = "GEOID")
    ) %>%
    dplyr::mutate(
      rate = 100000*.data$n/.data$TOTAL_POP,
      label = glue::glue(
        "
        <b>{label}</b></br>
        Count: {n} </br>
        Rate: {rate} per 100K</br>
        Population: {pop} </br>
        ",
        label = .data$ZIP_CODE,
        n = .data$n,
        rate = round(.data$rate, 0),
        pop = .data$TOTAL_POP
      )
    ) %>%
    sf::st_as_sf()
}

data_drug_overdose_deaths_ct_10yr_rate <- function() {

  data <- data_drug_overdose_deaths("census_tract")

  data %>%
    dplyr::mutate(GEOID10 = as.character(.data$GEOID10)) %>%
    dplyr::filter(.data$year %in% as.character(c(2012:2021))) %>%
    dplyr::group_by(
      .data$GEOID10
    ) %>%
    dplyr::summarise(n = sum(.data$n)) %>%
    dplyr::left_join(
      get_census_tract_sf(),
      by = c("GEOID10" = "GEOID")
    ) %>%
    dplyr::mutate(
      rate = 100000*.data$n/.data$TOTAL_POP,
      label = glue::glue(
        "
        <b>{label}</b></br>
        Count: {n} </br>
        Rate: {rate} per 100K</br>
        Population: {pop} </br>
        ",
        label = .data$GEOID10,
        n = .data$n,
        rate = round(.data$rate, 0),
        pop = .data$TOTAL_POP
      )
    ) %>%
    sf::st_as_sf()
}

