columbus_311_2022 <-
  sf::st_read(
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "311", "Columbus311.gdb"
    )
  ) %>%
  dplyr::filter(
    lubridate::year(.data$REPORTED_DATE) == 2022
  ) %>%
  sf::st_transform(crs = 4326)

columbus_311_2022 <-
  columbus_311_2022 %>%
  dplyr::rename(
    lng = LONGITUDE,
    lat = LATITUDE
  )

# "violation, public health, and street lighting"
bin_width <- 0.01
heatmap_data_311 <-
  ggplot2::layer_data(
    ggplot2::ggplot() +
      ggplot2::geom_bin_2d(
        ggplot2::aes(x = .data$lng, y = .data$lat),
        binwidth = c(bin_width, bin_width),
        alpha = 1,
        data = columbus_311_2022
      ) +
      ggplot2::scale_fill_gradient('311 Request',
                                   low = "#e0ecf4",
                                   high = "#8856a7")
  )

columbus_311_2022 <-
  sf::st_read(
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "311", "Columbus311.gdb"
    )
  ) %>%
  dplyr::filter(
    lubridate::year(.data$REPORTED_DATE) == 2022
  ) %>%
  sf::st_transform(crs = 4326)

columbus_311_2022 <-
  columbus_311_2022 %>%
  dplyr::rename(
    lng = LONGITUDE,
    lat = LATITUDE
  )

# "violation, public health, and street lighting"
bin_width <- 0.01
heatmap_data_311 <-
  ggplot2::layer_data(
    ggplot2::ggplot() +
      ggplot2::geom_bin_2d(
        ggplot2::aes(x = .data$lng, y = .data$lat),
        binwidth = c(bin_width, bin_width),
        alpha = 1,
        data = columbus_311_2022
      ) +
      ggplot2::scale_fill_gradient('311 Request',
                                   low = "#f2f0f7",
                                   high = "#54278f")
  )

heatmap_data_311 %>%
  readr::write_csv(
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "311", "Heatmap", "heatmap_data_columbus_2022.csv"
    )
  )
