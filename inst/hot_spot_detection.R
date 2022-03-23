library(devtools)
load_all()
od_data <- opioidDashboard::opioid_overdose_data()
zip_code_map <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = opioidDashboard::get_zipcode_sf())
# ============= #
# ---- Hex ----
# ============= #
hex <-
  zip_code_map +
  geom_hex(
    aes(x = lng, y = lat),
    #bins = 60,
    binwidth = c(0.01, 0.01),
    data = od_data
  ) +
  scale_fill_gradient('Overdose Cases',
                      low = "#ffeda0",
                      high = "#f03b20") +
  labs(title = "Hex")
hex_data <-
  tibble::as_tibble(
    ggplot2::ggplot_build(hex)$data[[2]]
  )

# ============== #
# ---- Bins ----
# ============== #
bin <-
  zip_code_map +
  geom_bin_2d(
    aes(x = lng, y = lat),
    binwidth = c(0.01, 0.01),
    #bins = 60,
    data = od_data
  ) +
  scale_fill_gradient('Overdose Cases',
                      low = "#ffeda0",
                      high = "#f03b20") +
  labs(title = "Bins")
bin_data <-
  tibble::as_tibble(
    ggplot2::ggplot_build(bin)$data[[2]]
  )
# ================== #
# ---- Contours ----
# ================== #
contour <-
  zip_code_map +
  stat_density_2d(
    geom = "polygon",
    aes(x = lng, y = lat, fill = ..level..),
    #binwidth = c(0.01, 0.01),
    #bins = 60,
    data = od_data
  ) +
  scale_fill_gradient2("Overdose Cases",
                       low = "#ffeda0", mid = "yellow", high = "#f03b20") +
  labs(title = "Contour")

# =============================== #
# ---- OH Business Locations ----
# =============================== #
oh_business <- vroom::vroom(
  file = "/fs/ess/PDE0001/business places data/Business-2020-OH.csv",
  delim = ",",
  col_names = FALSE
) %>%
  dplyr::transmute(
    id = dplyr::row_number(),
    name = .data$X1,
    type = .data$X13,
    address = .data$X2,
    lat = readr::parse_number(.data$X47),
    lng = .data$X48
  ) %>%
  na.omit()

eligible_locations <-
  c(
    # Library
    "LIBRARIES-PUBLIC",
    # Convenience/liquor stores
    "CONVENIENCE STORES", "VARIETY STORES",
    # Fast food places/Restaurants
    "FOOD BANKS", "RESTAURANTS",
    # Check cashing places
    # Gas station
    "SERVICE STATIONS-GASOLINE & OIL",
    # Motels
    # Pawn shops
    "PAWNBROKERS"
    # Hardware stores

  )


suggested_business <-
  get_hot_spot_region(
    od_data = opioidDashboard::opioid_overdose_data()
  ) %>%
  dplyr::mutate(
    business = purrr::pmap(
      .l = list(
        .data$hot_spot,
        .data$lat_min, .data$lat_max,
        .data$lng_min, .data$lng_max
      ),
      .f = get_hot_spot_business,
      business_location = oh_business,
      eligible_location = eligible_locations
    )
  )

suggested_business %>%
  dplyr::select(business) %>%
  tidyr::unnest(cols = "business") %>%
  dplyr::select(
    hot_spot, everything()
  ) %>%
  knitr::kable()
































