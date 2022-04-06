hotSpotDetectionUI <- function(id) {

  shiny::tagList(
    # ============================ #
    # ---- A. Hyperparameters ----
    # ============================ #

    shiny::fluidRow(

      # ============================== #
      # ---- Overdose Source Data ----
      # ============================== #
      shiny::column(
        width = 3,
        shiny::selectInput(
          inputId = shiny::NS(id, "od_data_source"),
          label = "Choose overdose cases data source",
          choices = c(
            "Raw Overdose Case Data",
            "Filtered Overdose Case Data"
          ),
          selected = NULL
        )
      ),

      # ===================================== #
      # ---- Algo Param: Bucket Bin Size ----
      # ===================================== #
      shiny::column(
        width = 3,
        shiny::numericInput(
          inputId = shiny::NS(id, "bin_width"),
          label = "Bin Size",
          min = 0,
          value = 0.01,
          step = 0.001
        )
      ),

      # ======================================= #
      # ---- Algo Param: Hot Spot Quantile ----
      # ======================================= #
      shiny::column(
        width = 3,
        shiny::numericInput(
          inputId = shiny::NS(id, "quantile"),
          label = "Hot Spot Quantile",
          min = 0,
          max = 0.99,
          value = 0.75,
          step = 0.01
        )
      ),

      # ======================== #
      # ---- Update Control ----
      # ======================== #
      shiny::column(
        width = 3,
        shiny::actionButton(
          inputId = shiny::NS(id, "update_hyper_param"),
          label = "Update Hyperparameters"
        )
      )
    ),

    # ============================= #
    # ---- B. Algorithm Result ----
    # ============================= #

    shiny::fluidRow(

      # ================================= #
      # ---- Hot Spot Map (Overview) ----
      # ================================= #
      shiny::column(
        width = 4,
        align="left",
        shiny::plotOutput(
          outputId = shiny::NS(id, "hot_spot_map_mini"),
          height = "500px",
          width = "100%"
        )
      ),

      # =============================== #
      # ---- Business Table Filter ----
      # =============================== #
      shiny::column(
        width = 8,
        reactable::reactableOutput(
          outputId = shiny::NS(id, "business_table")
        )
      )
    ),

    # ======================================= #
    # ---- C. Hot Spot Map (Interactive) ----
    # ======================================= #

    shiny::fluidRow(
      shiny::column(
        width = 12,
        leaflet::leafletOutput(
          outputId = shiny::NS(id, "hot_spot_map")
        )
      )
    )

  )
}


hotSpotDetectionServer <- function(id, filtered_overdose_data, od_data_all) {

  shiny::moduleServer(id, function(input, output, session){
    shiny::req(
      shiny::is.reactivevalues(filtered_overdose_data)
    )

    # ============================ #
    # ---- A. Hyperparameters ----
    # ============================ #


    hyper_params <- shiny::reactiveValues(
      data_source = od_data_all,
      bin_width = 0.01,
      quantile = 0.75
    )

    shiny::observeEvent(input$update_hyper_param, {

      # ===================================== #
      # ---- Handle overdose data source ----
      # ===================================== #
      od_data_source <- input$od_data_source

      if (od_data_source == "Raw Overdose Case Data") {
        hyper_params$data_source <- od_data_all
      } else {
        hyper_params$data_source <- filtered_overdose_data$data
      }

      # ========================== #
      # ---- Handle bin width ----
      # ========================== #
      hyper_params$bin_width <- input$bin_width

      # ========================= #
      # ---- Handle quantile ----
      # ========================= #
      hyper_params$quantile <- input$quantile

    })

    oh_business <- opioidDashboard::get_ohio_business()

    # ============================= #
    # ---- B. Algorithm Result ----
    # ============================= #

    # ---- Hot Spot Region Data for heatmap ---- #
    hot_spot_region <- shiny::reactiveValues(
      value = opioidDashboard::get_hot_spot_region(
        od_data = od_data_all,
        percent_tile = 0.75,
        bin_width = c(0.01, 0.01)
      )
    )

    # ---- Hot Spot Business Data ----- #
    hot_spot_business <- shiny::reactiveValues(
      value = opioidDashboard::get_hot_spot_region(
        od_data = od_data_all,
        percent_tile = 0.75,
        bin_width = c(0.01, 0.01)
      ) %>%
        dplyr::mutate(
          business = purrr::pmap(
            .l = list(
              .data$hot_spot,
              .data$lat_min, .data$lat_max,
              .data$lng_min, .data$lng_max
            ),
            .f = opioidDashboard::get_hot_spot_business,
            business_location = oh_business,
            eligible_business_type = opioidDashboard::ELIGIBLE_BUSINESS_TYPE
          )
        ) %>%
        dplyr::select(.data$business) %>%
        tidyr::unnest(cols = "business")
    )

    shiny::observeEvent(input$update_hyper_param, {
      hot_spot_region$value <-
        opioidDashboard::get_hot_spot_region(
          od_data = hyper_params$data_source,
          percent_tile = hyper_params$quantile,
          bin_width = c(hyper_params$bin_width, hyper_params$bin_width)
        )

      hot_spot_business$value <-
        opioidDashboard::get_hot_spot_region(
          od_data = hyper_params$data_source,
          percent_tile = hyper_params$quantile,
          bin_width = c(hyper_params$bin_width, hyper_params$bin_width)
        ) %>%
        dplyr::mutate(
          business = purrr::pmap(
            .l = list(
              .data$hot_spot,
              .data$lat_min, .data$lat_max,
              .data$lng_min, .data$lng_max
            ),
            .f = opioidDashboard::get_hot_spot_business,
            business_location = oh_business,
            eligible_business_type = opioidDashboard::ELIGIBLE_BUSINESS_TYPE
          )
        ) %>%
        dplyr::select(.data$business) %>%
        tidyr::unnest(cols = "business")
    })

    # ================================= #
    # ---- Hot Spot Map (Overview) ----
    # ================================= #

    output$hot_spot_map_mini <- shiny::renderPlot({

      data_source <- hyper_params$data_source
      bin_width <- hyper_params$bin_width
      quantile <- hyper_params$quantile


      ggmap::ggmap(opioidDashboard::COLUMBUS_STATIC_ROADMAP) +
        ggplot2::geom_bin_2d(
          ggplot2::aes(x = .data$lng, y = .data$lat),
          binwidth = c(bin_width, bin_width),
          data = data_source
        ) +
        ggplot2::geom_text(
          ggplot2::aes(x = .data$lng, y = .data$lat, label = .data$hot_spot),
          data =  opioidDashboard::get_hot_spot_region(
            od_data = data_source,
            percent_tile = quantile,
            bin_width = c(bin_width, bin_width)
          ),
          alpha = 0.75
        ) +
        ggplot2::scale_fill_gradient('Overdose Cases',
                                     low = "#ffeda0",
                                     high = "#f03b20") +
        ggplot2::coord_quickmap(
          xlim = c(
            opioidDashboard::opioid_overdose_map_init_bounds$min_lng-0.1,
            opioidDashboard::opioid_overdose_map_init_bounds$max_lng+0.15
          ),
          ylim = c(
            opioidDashboard::opioid_overdose_map_init_bounds$min_lat-0.1,
            opioidDashboard::opioid_overdose_map_init_bounds$max_lat+0.1
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme(
          legend.position = "top",
          plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        )
    })

    # =============================== #
    # ---- Business Table Filter ----
    # =============================== #

    output$business_table <- reactable::renderReactable({

      hot_spot_business$value %>%
        dplyr::select(
          `Hot Spot` = .data$hot_spot,
          `Business Name` = .data$name,
          `Type` = .data$type,
          `Address` = .data$address,
          `City` = .data$city,
          `Zip` = .data$zipcode
        ) %>%
        reactable::reactable(
          # Table Format
          filterable = TRUE,
          outlined = TRUE,
          # Selection
          selection = "multiple", onClick = "select",
          highlight = TRUE,
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          # Table Size
          defaultPageSize = 8, minRows = 8
        )
    })

    selected_business_index <- shiny::reactive(
      reactable::getReactableState("business_table", "selected")
    )

    selected_hot_spot_business <- shiny::reactiveValues(
      value = NULL
    )

    shiny::observe({
      shiny::req(!is.null(selected_business_index()))
      if (!is.null(selected_business_index())) {
        selected_hot_spot_business$value <-
          hot_spot_business$value[selected_business_index(),] %>%
          dplyr::select(-.data$hot_spot) %>%
          dplyr::distinct() %>%
          dplyr::mutate(
            popup_content = purrr::pmap_chr(
              .l = list(.data$name, .data$type, .data$address, .data$zipcode),
              .f = function(name, type, address, zipcode) {
                # Clean missing values
                if (is.na(name)) {
                  name <- ""
                }

                if (is.na(type)) {
                  type <- ""
                }

                if (is.na(address)) {
                  address <- ""
                }

                if (is.na(zipcode)) {
                  zipcode <- ""
                }

                name_html <-
                  sprintf(
                    "Name: %s",
                    name
                  )

                type_html <-
                  sprintf(
                    "Type: %s",
                    type
                  )

                address_html <-
                  sprintf(
                    "Address: %s",
                    address
                  )

                zipcode_html <-
                  sprintf(
                    "Zip: %s",
                    zipcode
                  )

                content <-
                  paste(
                    name_html,
                    type_html,
                    address_html,
                    zipcode_html,
                    sep = "<br/>"
                  )
              }
            )
          )
      } else {
        selected_hot_spot_business$value <- NULL
      }
    })


    # ================================ #
    # ---- Hot Spot Map (Leaflet) ----
    # ================================ #

    franklin_county_school_district_sf <-
      get_franklin_county_school_district_sf()

    fire_districts_sf <-
      get_fire_district_sf()

    census_tract_sf <-
      get_census_tract_sf()

    zipcode_sf <-
      get_zipcode_sf()

    output$hot_spot_map <- leaflet::renderLeaflet({

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
        # ================= #
        # ---- Heatmap ----
        # ================= #
      # TODO: Heatmap
      # leaflet.extras::addHeatmap(
      #   data = hyper_params$data_source,
      #   lng = ~lng,
      #   lat = ~lat,
      #   blur = 20, max = 0.05, radius = 15
      # ) %>%

        # ======================== #
        # ---- Layers Control ----
      # ======================== #
      leaflet::addLayersControl(
        position = "topright",
        # baseGroups = c(
        #
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
        leaflet.extras::addResetMapButton() # reset
    })

    # =============================== #
    # ---- Add Business Location ----
    # =============================== #
    shiny::observe({

      if (!is.null(selected_business_index())) {
        leaflet::leafletProxy("hot_spot_map") %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(
            data = selected_hot_spot_business$value,
            lng = ~lng, lat = ~lat,
            popup = ~popup_content,
            #label = ~label,
            #group = "",
            labelOptions = leaflet::labelOptions(
              style = list(
                "font-size" = "15px",
                "font-style" = "bold",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          )
      } else {
        leaflet::leafletProxy("hot_spot_map") %>%
          leaflet::clearMarkers()
      }
    })
  })
}
