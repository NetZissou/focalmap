#' Load focused indicators  extracted from naloxone intake form
#'
#' @param dir data directory on OSC that contains all of the naloxone intake forms data
#'
#' @return focused naloxone intake forms indicators
#' @export
#'
#' @examples
#' project_dawn_focused_indicators()
opioid_overdose_data <- function(dir = opioidDashboard::OPIOID_OVERDOSE_DATA_DIRECTORY) {
  data_name <- "primary_table.csv"
  data_path <- paste0(dir, "/", data_name)

  suppressWarnings(
    suppressMessages(
      data <-
        readr::read_csv(
          data_path
        ) %>%
        janitor::clean_names()
    )
  )

  data <-
    data %>%
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
      )
    ) %>%
    dplyr::select(-.data$ethnicity) %>% # changed into hispanic_or_latino
    # remove data with no date, latitude, longitude
    dplyr::filter(
      !is.na(.data$date),
      !is.na(.data$latitude),
      !is.na(.data$longitude)
    )
  return(
    data
  )
}
