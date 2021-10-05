opioidOverdoseMapUI <- function(id) {

  # shinydashboard::box(
  #   width = 12, solidHeader = TRUE,
  #   title = "Opioid Overdose Map",
  #   leaflet::leafletOutput(
  #     outputId = shiny::NS(id, "overdose_map"),
  #     height = 700
  #   )
  # )
  leaflet::leafletOutput(
    outputId = shiny::NS(id, "overdose_map"),
    height = 600
  )
}

opioidOverdoseMapServer <- function(id, filtered_overdose_data) {
  shiny::req(shiny::is.reactivevalues(filtered_overdose_data))
  shiny::moduleServer(id, function(input, output, session){

    filtered_data <-
      shiny::reactive({
        return(
          filtered_overdose_data$data
        )
      })
    output$overdose_map <- leaflet::renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      opioid_overdose_map_init_bounds <- opioidDashboard::opioid_overdose_map_init_bounds

      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::fitBounds(
          lng1 = opioid_overdose_map_init_bounds$min_lng,
          lat1 = opioid_overdose_map_init_bounds$min_lat,
          lng2 = opioid_overdose_map_init_bounds$max_lng,
          lat2 = opioid_overdose_map_init_bounds$max_lat
        )
    })

    shiny::observe({

      leaflet::leafletProxy("overdose_map", data = filtered_data()) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(
          lng = ~lng, lat = ~lat,
          stroke = FALSE,
          fillColor = "darkred",
          fillOpacity = 0.3
        )
    })
  })
}



