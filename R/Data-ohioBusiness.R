#' Get the business information from Ohio
#' @param parquet
#' @return business information from Ohio
#' @export
get_ohio_business <- function(parquet = FALSE) {
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
  if (parquet) {
    arrow::read_parquet(
      fs::path(
        opioidDashboard::ROOT_PATH,
        "other", "Ohio Businesses",
        "ohio_businesses.parquet"
      ),
      as_data_frame = FALSE
    )
  } else {
    suppressMessages(
      suppressWarnings(
        vroom::vroom(
          file = fs::path(
            opioidDashboard::ROOT_PATH,
            "other", "Ohio Businesses",
            "ohio_businesses.csv"
          )
        )
      )
    )
  }
}
