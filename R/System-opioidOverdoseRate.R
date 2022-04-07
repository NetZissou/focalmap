#' Get the overdose rate of the regions in a sf object
#'
#' @param region "zip", "school_district", "EMS", "census_tract"
#' @param od_data overdose data
#' @param region_sf sf object
#' @param pop_colname population column name
#' @param label_colname region label column name
#' @param id_colname unique identifier column name
#'
#' @return sf object with `n` and `rate`
#' @export
get_region_od_rate <- function(
  region = c("zip", "school_district", "EMS", "census_tract", "fire_district"),
  region_sf,
  id_colname,
  label_colname,
  pop_colname,
  od_data = opioidDashboard::opioid_overdose_data()
) {

  od_freq <-
    od_data %>%
    dplyr::count(.data[[region]]) %>%
    dplyr::transmute(
      region = as.character(.data[[region]]),
      n = .data$n
    ) %>%
    purrr::set_names(c(id_colname, "n"))

  map_sf <-
    region_sf %>%
    dplyr::left_join(
      od_freq,
      by = id_colname
    ) %>%
    dplyr::mutate(
      n = ifelse(is.na(.data$n), 0, .data$n),
      rate = (.data$n/.data[[pop_colname]])*1e5,
      label = purrr::pmap_chr(
        .l = list(.data[[label_colname]], .data$n, .data$rate, .data[[pop_colname]]),
        .f = function(label, n, rate, pop) {
          if (is.na(n)) {
            n <- 0
            rate <- 0
          }

          #round_rate_label <-  paste0(round(rate, 4)*1e2, "%")

          text <-
            paste0(
              "<b>", label, "</b>",
              "<br/>",
              "Count: ", n,
              "<br/>",
              "Rate: ", scales::number(rate, big.mark = ","), " per 100,000",
              "<br/>",
              "Population: ", scales::number(pop, big.mark = ",")
            )

          if (n < 11) {
            text <-
              paste0(
                text,
                "<br/",
                "<b>",
                "Interpret with caution due to <11 cases for selected time period",
                "</b>"
              )
          }
          return(text)
        }
      ),
      rate_pal = purrr::map_dbl(
        .x = .data$rate,
        .f = function(x) {
          if (!is.na(x)) {
            round_x <- round(x, digits = 4)
            if (round_x == 0) {
              x <- NA
            }
          }
          return(x)
        }
      )
    )

  return(map_sf)
}


# region_sf <-
#   get_region_od_rate(
#     "zip",
#     zipcode_sf,
#     "GEOID",
#     "GEOID",
#     "TOTAL_POP"
#   )


addRegionPolygons <- function(leaflet_object, group_label, region_sf) {

  if (is.null(leaflet_object)) {
    leaflet_object <-
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
  }

  pal <- leaflet::colorQuantile(
    palette = "YlOrRd",
    domain = region_sf$rate_pal,
    n = 5
  )

  leaflet_object %>%
    leaflet::clearShapes() %>%
    leaflet::addPolygons(
      data = region_sf,
      group = group_label,
      stroke = TRUE,
      color = ~pal(rate_pal),
      weight = 1,
      #opacity = 0.8,
      dashArray = "3",
      #fillOpacity = 0.1,

      label = ~ label %>% lapply(htmltools::HTML),

      labelOptions = leaflet::labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      ),

      highlight = leaflet::highlightOptions(
        weight = 3,
        fillOpacity = 0.1,
        color = "black",
        dashArray = "",
        opacity = 0.5,
        bringToFront = TRUE,
        sendToBack = TRUE
      )
    )

}








