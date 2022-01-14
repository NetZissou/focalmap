naloxoneKitDistributionUI <- function(id) {
  shiny::fluidRow(
    shiny::column(
      width = 12,
      leaflet::leafletOutput(
        outputId = shiny::NS(id, "od_hot_spot_map")
      )
    )
  )
}

naloxoneKitDistributionServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session){

    franklin_county_school_district_sf <-
      get_franklin_county_school_district_sf()

    fire_districts_sf <-
      get_fire_district_sf()

    census_tract_sf <-
      get_census_tract_sf()

    zipcode_sf <-
      get_zipcode_sf()

    output$od_hot_spot_map <- leaflet::renderLeaflet({
      opioid_overdose_map_init_bounds <- opioidDashboard::opioid_overdose_map_init_bounds

      leaflet::leaflet() %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        # ========================= #
        # ---- Map init Bounds ----
      # ========================= #
      leaflet::fitBounds(
        lng1 = opioid_overdose_map_init_bounds$min_lng,
        lat1 = opioid_overdose_map_init_bounds$min_lat,
        lng2 = opioid_overdose_map_init_bounds$max_lng,
        lat2 = opioid_overdose_map_init_bounds$max_lat
      ) %>%
        leaflet::addMapPane("County_districts_polyline", zIndex = 420) %>%
        leaflet::addRectangles(
          data = opioidDashboard::get_hot_spot_region(),
          lng1=~lng_min, lat1=~lat_min,
          lng2=~lng_max, lat2=~lat_max,
          color = "red",
          opacity = 0.5,
          fillColor = "transparent"
        ) %>%
        # ================================================ #
        # ---- Shapefile outline: FC School Districts ----
      # ================================================ #
      leaflet::addPolygons(
        data = franklin_county_school_district_sf,
        group = "FC School Districts",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", NAME, "</b>"
        ) %>% lapply(htmltools::HTML),

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
      ) %>%
        # =========================================== #
        # ---- Shapefile outline: Fire Districts ----
      # =========================================== #
      leaflet::addPolygons(
        data = fire_districts_sf,
        group = "Fire Districts",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", DEPARTMENT, "</b>"
        ) %>% lapply(htmltools::HTML),

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
      ) %>%
        # ========================================= #
        # ---- Shapefile outline: Census Tract ----
      # ========================================= #
      leaflet::addPolygons(
        data = census_tract_sf,
        group = "Census Tract",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>"
        ) %>% lapply(htmltools::HTML),

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
      ) %>%
        # ===================================== #
        # ---- Shapefile outline: Zip Code ----
      # ===================================== #
      leaflet::addPolygons(
        data = zipcode_sf,
        group = "Zip Code",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>"
        ) %>% lapply(htmltools::HTML),

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
      ) %>%

        # ======================== #
        # ---- Layers Control ----
      # ======================== #
      leaflet::addLayersControl(
        position = "topright",
        # baseGroups = c(
        #   "Opioid Overdose Cases",
        #   "Drug Crime Cases"
        # ),
        overlayGroups = c(
          "FC School Districts",
          "Fire Districts",
          "Census Tract",
          "Zip Code"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
        leaflet::hideGroup(
          c(
            "FC School Districts",
            "Fire Districts",
            "Census Tract",
            "Zip Code"
          )
        ) %>%
        # --- Full Screen Control --- #
        leaflet.extras::addFullscreenControl(
          position = "topleft",
          pseudoFullscreen = FALSE
        ) %>%
        leaflet.extras::addResetMapButton()
    })
  })
}
