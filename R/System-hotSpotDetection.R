
#' Get the hot spot region from spatial data
#'
#' @param od_data opioid overdose data
#' @param percent_tile hot spot threshold
#' @param bin_width size of the bin, a 2-d (x, y) vector
#'
#' @return a tibble with hot spot spatial info
#' @export
#' @examples
#' # Change the od_data to get different hot spot region
get_hot_spot_region <- function(
  od_data = opioidDashboard::opioid_overdose_data(),
  percent_tile = 0.75,
  bin_width = c(0.01, 0.01)
) {

  zip_code_map <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = opioidDashboard::get_zipcode_sf())

  # ============== #
  # ---- Bins ----
  # ============== #
  bin <-
    zip_code_map +
    ggplot2::geom_bin_2d(
      ggplot2::aes(x = .data$lng, y = .data$lat),
      binwidth = bin_width,
      #bins = 60,
      data = od_data
    ) +
    ggplot2::scale_fill_gradient('Overdose Cases',
                        low = "#ffeda0",
                        high = "#f03b20") +
    ggplot2::labs(title = "Bins")
  bin_data <-
    tibble::as_tibble(
      ggplot2::ggplot_build(bin)$data[[2]]
    )

  # ========================= #
  # ---- Filter hot spot ----
  # ========================= #
  hot_spot_region <-
    bin_data %>%
    dplyr::filter(
      .data$ndensity >= percent_tile
    ) %>%
    dplyr::arrange(-.data$ndensity) %>%
    dplyr::transmute(
      hot_spot = dplyr::row_number(),
      lng = .data$x,
      lat = .data$y,
      n = .data$count,
      quantile = .data$ndensity,
      prop = .data$density,

      lng_min = .data$x - bin_width[1],
      lng_max = .data$x + bin_width[1],

      lat_min = .data$y - bin_width[2],
      lat_max = .data$y + bin_width[2],
    )

  return(hot_spot_region)
}

#' Get the eligible business entities in a hot spot region
#'
#' @param hot_spot_id the id of the hot spot
#' @param lat_min min lat
#' @param lat_max max lat
#' @param lng_min min lng
#' @param lng_max max lng
#' @param business_location table of business location
#' @param eligible_business_type eligible business type and category
#'
#' @return table of eligible business
#' @export
get_hot_spot_business <- function(
  hot_spot_id,
  lat_min, lat_max,
  lng_min, lng_max,
  business_location,
  eligible_business_type
) {
  business_location %>%
    dplyr::filter(
      .data$lat >= lat_min,
      .data$lat <= lat_max,

      .data$lng >= lng_min,
      .data$lng <= lng_max,
    ) %>%
    dplyr::filter(.data$type %in% eligible_business_type) %>%
    dplyr::mutate(
      hot_spot = hot_spot_id
    )
}
