## code to prepare `PUBLIC_PLACES_ICON_LIST` dataset goes here
public_places <-
  data_public_places()

type_color_tbl <-
  public_places %>%
  tibble::as_tibble() %>%
  dplyr::count(.data$type, .data$color)

PUBLIC_PLACES_ICON_LIST <-
  purrr::map2(
  .x = type_color_tbl$color, .y = type_color_tbl$n,
  .f = function(color, n) {
    leaflet::awesomeIcons(
      icon = 'circle',
      iconColor = 'white',
      library = 'fa',
      markerColor = rep(color, n)
    )
  }
) %>%
  purrr::set_names(type_color_tbl$type)


usethis::use_data(PUBLIC_PLACES_ICON_LIST, overwrite = TRUE)
