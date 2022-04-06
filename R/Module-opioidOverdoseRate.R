opioidOverdoseRateUI <- function(id) {

  shiny::tagList(
    # ============================ #
    # ---- A. Hyperparameters ----
    # ============================ #

    shinydashboard::box(
      title = "Global Filters",
      width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,

      shiny::fluidRow(

        # ===================== #
        # ---- Data Source ---- #
        # ===================== #

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

        # ====================== #
        # ---- Update Param ---- #
        # ====================== #
        shiny::column(
          width = 3,
          shiny::actionButton(
            inputId = shiny::NS(id, "update_hyper_param"),
            label = "Update Hyperparameters"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shinyWidgets::sliderTextInput(
            inputId = shiny::NS(id, "date_range"),
            label = "Date Range",
            choices = purrr::map_chr(
              .x = seq(
                from = as.Date("2019-01-01"),
                to = Sys.Date(),
                by = "1 month"),
              .f = function(date) {
                year <- lubridate::year(date)
                month_num <- lubridate::month(date)
                return(paste0(year, "-", month_num))
              }
            ),
            selected = purrr::map_chr(
              .x = c(as.Date("2019-01-01"), Sys.Date()),
              .f = function(date) {
                year <- lubridate::year(date)
                month_num <- lubridate::month(date)
                return(paste0(year, "-", month_num))
              }
            )
          )
        )
      )
    ),


    # ======================= #
    # ---- Case Rate Map ----
    # ======================= #

    shiny::fluidRow(
      shiny::column(
        width = 12,
        leaflet::leafletOutput(
          outputId = shiny::NS(id, "od_case_rate_map")
        )
      )
    )
  )
}

opioidOverdoseRateServer <- function(id, filtered_overdose_data) {

  shiny::moduleServer(id, function(input, output, session){
    shiny::req(
      shiny::is.reactivevalues(filtered_overdose_data)
    )

    # ======================== #
    # ---- A. HyperParams ----
    # ======================== #
    hyper_params <- shiny::reactiveValues(
      data_source = opioidDashboard::opioid_overdose_data()
    )


    shiny::observeEvent(input$update_hyper_param, {

      # ===================================== #
      # ---- Handle overdose data source ----
      # ===================================== #
      od_data_source <- input$od_data_source

      if (od_data_source == "Raw Overdose Case Data") {
        hyper_params$data_source <- opioidDashboard::opioid_overdose_data()
      } else {
        hyper_params$data_source <- filtered_overdose_data$data
      }

    })


    # ============================= #
    # ---- B. Case Rate Map () ----
    # ============================= #

    school_district_sf <-
      get_ohio_school_district_sf()

    fire_districts_sf <-
      get_fire_district_sf()

    census_tract_sf <-
      get_census_tract_sf()

    zipcode_sf <-
      get_zipcode_sf()

    output$od_case_rate_map <-  leaflet::renderLeaflet({

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
      )  %>%
        # ======================== #
        # ---- Layers Control ----
      # ======================== #
      leaflet::addLayersControl(
        position = "topright",
        baseGroups = c(
          "FC School Districts",
          "Census Tract",
          "Zip Code"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
        # --- Full Screen Control --- #
        leaflet.extras::addFullscreenControl(
          position = "topleft",
          pseudoFullscreen = FALSE
        ) %>%
        leaflet.extras::addResetMapButton() # reset

    })

    shiny::observe({

      od_zip_freq <-
        hyper_params$data_source %>%
        dplyr::count(.data$zip_sf) %>%
        dplyr::transmute(
          zip_sf = as.character(.data$zip_sf),
          n = .data$n
        )

      map_zipcode_sf <-
        zipcode_sf %>%
        dplyr::left_join(
          od_zip_freq,
          by = c("GEOID" = "zip_sf")
        ) %>%
        dplyr::mutate(
          n = ifelse(is.na(.data$n), 0, .data$n),
          rate = .data$n/.data$TOTAL_POP
        )

      pal <- leaflet::colorQuantile(
        palette = "YlOrRd",
        domain = map_zipcode_sf$rate,
        n = 3
      )



      leaflet::leafletProxy("od_case_rate_map") %>%
        # leaflet::leaflet() %>%
        # leaflet::addTiles() %>%
        # leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::clearShapes() %>%
        leaflet::addPolygons(
          data = map_zipcode_sf,
          group = "Zip Code",
          stroke = TRUE,
          color = ~pal(rate),
          weight = 1,
          #opacity = 0.8,
          dashArray = "3",
          #fillOpacity = 0.1,

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
        )
    })


  })
}
