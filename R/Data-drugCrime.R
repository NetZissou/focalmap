#' Loading function for drug crime data
#'
#' @param dir directory on OSC that contains drug crime data
#' @param geocode TRUE will return data with lat/lng geocoding information
#'
#' @return drug crime data
#' @export
#'
#' @examples
#' drug_crime_data()
drug_crime_data <- function(dir = opioidDashboard::DRUG_CRIME_DATA_DIRECTORY, geocode = TRUE) {

  if (!geocode) {
    path <- paste0(
      dir,
      "/",
      "ayaz_drug_crime.csv"
    )

    suppressMessages(
      data <- readr::read_csv(
        file = path
      )
    )

    suppressMessages(
      suppressWarnings(
        data <- data %>%
          purrr::set_names(
            c("city", "agency_id", "id", "date_time", "location_addr_1", "location_addr_2", "location_city", "location_zip")
          ) %>%
          dplyr::filter(!is.na(.data$location_addr_1) | !is.na(.data$location_addr_2)) %>%
          dplyr::mutate(
            location_addr_1 = ifelse(is.na(.data$location_addr_1), "", .data$location_addr_1),
            location_addr_2 = ifelse(is.na(.data$location_addr_2), "", .data$location_addr_2),
            location_zip = ifelse(is.na(.data$location_zip), "", .data$location_zip),
            formatted_addr = stringr::str_c(
              .data$location_addr_1,
              .data$location_addr_2,
              .data$city,
              .data$location_zip,
              sep = " "
            ),
            formatted_addr = stringr::str_trim(.data$formatted_addr, side = "both"),
            formatted_addr = stringr::str_remove(.data$formatted_addr, "[\\s]*[\\w]{1,} County Sheriff's Office")
          ) %>%
          dplyr::select(
            .data$id, .data$date_time, .data$city, .data$formatted_addr, zip = .data$location_zip
          ) %>%
          dplyr::mutate(
            zip = readr::parse_number(.data$zip, na = "NA"),
            date_time = lubridate::mdy_hms(.data$date_time)
          )
      )
    )
  } else {
    path <- paste0(
      dir,
      "/",
      "drug_crime_data_geocoded.csv"
    )

    suppressMessages(
      suppressWarnings(
        data <- readr::read_csv(
          file = path
        )
      )
    )

    data <-
      data %>%
      dplyr::transmute(
        .data$id,
        .data$date_time,
        date = lubridate::as_date(.data$date_time),
        county = .data$County,
        city = .data$City,
        .data$formatted_addr,
        zip = .data$Zip,
        lng = .data$Longitude,
        lat = .data$Latitude
      )
  }

  return(data)
}
