## code to prepare `FCPH_LOCATION_OF_INTEREST` dataset goes here
addr_list <-
  c(
    "368 West Park Ave, Columbus, Ohio, 43223",
    "390 South Yearling Road, Whitehall, Ohio, 43213"
  )

FCPH_LOCATION_OF_INTEREST <-
  purrr::map_dfr(
    addr_list,
    .f = geocoding
  ) %>%
  sf::st_as_sf(
    coords = c("lng", "lat"),
    crs = 4326
  ) %>%
  dplyr::mutate(
    name = c("SAFER Station", "Whitehall SAFE Station"),
    popup = glue::glue(
      "<b>Location of Interest</b></br><b>{name}</b></br>{address}",
      name = .data$name,
      address = .data$address
    )
  )

usethis::use_data(FCPH_LOCATION_OF_INTEREST, overwrite = TRUE)
