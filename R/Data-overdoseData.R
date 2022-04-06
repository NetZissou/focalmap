#' Load focused indicators  extracted from naloxone intake form
#'
#' @param dir data directory on OSC that contains all of the naloxone intake forms data
#' @param processed load processed opioid overdose data
#'
#' @return focused naloxone intake forms indicators
#' @export
#'
#' @examples
opioid_overdose_data <- function(dir = opioidDashboard::OPIOID_OVERDOSE_DATA_DIRECTORY, processed = TRUE) {

  if (processed) {
    # ========================== #
    # ---- Latest file path ----
    # ========================== #
    latest_data_info <-
      check_files_update_time(
        dir = dir,
        regexp = "primary_table_processed.csv"
      ) %>%
      head(1)

    latest_data_path <- latest_data_info$path
    latest_data_modification_time <- latest_data_info$modification_time

    # =================== #
    # ---- Load file ----
    # =================== #
    message("Loading file: ", latest_data_path)
    message("Modified at: ", latest_data_modification_time)

    suppressWarnings(
      suppressMessages(
        data <-
          vroom::vroom(
            latest_data_path
          ) %>%
          dplyr::mutate(
            zip = ifelse(is.na(.data$zip), .data$zip_sf, .data$zip)
          )
      )
    )
    return(data)
  }

  # ========================== #
  # ---- Latest file path ----
  # ========================== #
  latest_data_info <-
    check_files_update_time(
      dir = dir,
      regexp = "primary_table.csv"
    ) %>%
    head(1)

  latest_data_path <- latest_data_info$path
  latest_data_modification_time <- latest_data_info$modification_time

  # =================== #
  # ---- Load file ----
  # =================== #
  message("Loading file: ", latest_data_path)
  message("Modified at: ", latest_data_modification_time)

  suppressWarnings(
    suppressMessages(
      data <-
        vroom::vroom(
          latest_data_path
        ) %>%
        janitor::clean_names()
    )
  )

  data <-
    data %>%
    #dtplyr::lazy_dt() %>%
    dplyr::mutate(
      # Use regex to extract valid text
      race = stringr::str_extract(
        .data$race, pattern = "[a-zA-Z ]+"
      ),
      # ethnicity 98% missing
      hispanic_or_latino = !is.na(.data$ethnicity),
      # location_type 20% missing
      location_type = ifelse(
        is.na(.data$location_type),
        "Undocumented",
        .data$location_type
      ),
      location_type = ifelse(
        stringr::str_detect(.data$location_type, "Recreation"),
        "Place of Recreation/Sport",
        .data$location_type
      ),
      # destination 14% missing
      destination = ifelse(
        is.na(.data$destination),
        "Undocumented",
        .data$destination
      ),
      #  CAT age
      age_cat = dplyr::case_when(
        .data$age < 15 ~ "0-14",
        .data$age < 25 ~ "15-24",
        .data$age < 35 ~ "25-34",
        .data$age < 45 ~ "35-44",
        .data$age < 55 ~ "45-54",
        .data$age < 65 ~ "55-64",
        .data$age >= 65 ~ "65+",
        TRUE ~ "Prefer not to say"
      )
    ) %>%
    dplyr::select(-.data$ethnicity) %>% # changed into hispanic_or_latino
    # remove data with no date, latitude, longitude
    dplyr::filter(
      !is.na(.data$date),
      !is.na(.data$latitude),
      !is.na(.data$longitude)
    ) %>%
    dplyr::rename(
      lat = .data$latitude,
      lng = .data$longitude
    ) %>%
    # remove one outlier from 1900-01-01
    dplyr::filter(
      .data$date != lubridate::ymd(19000101)
    ) %>%
    tibble::as_tibble()
  return(
    data
  )
}

