#' Get the business information from Ohio
#'
#' @return business information from Ohio
#' @export
get_ohio_business <- function() {
  # suppressMessages(
  #   suppressWarnings(
  #     vroom::vroom(
  #       file = opioidDashboard::OHIO_BUSINESS_DATA_DIRECTORY,
  #       delim = ",",
  #       col_names = FALSE
  #     ) %>%
  #       dplyr::transmute(
  #         id = dplyr::row_number(),
  #         name = .data$X1,
  #         type = .data$X13,
  #         address = .data$X2,
  #         city = .data$X3,
  #         zipcode = .data$X5,
  #         lat = readr::parse_number(.data$X47),
  #         lng = .data$X48
  #       ) %>%
  #       stats::na.omit()
  #   )
  # )
  suppressMessages(
    suppressWarnings(
      vroom::vroom(
        file = "/fs/ess/PDE0001/focal_data_ingestion/other/Ohio Businesses/ohio_businesses.csv"
      )
    )
  )
}
