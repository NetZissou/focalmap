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
            label = "Update Map"
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
                from = as.Date("2008-01-01"),
                to = Sys.Date(),
                by = "1 month"),
              .f = function(date) {
                year <- lubridate::year(date)
                month_num <- lubridate::month(date)
                return(paste0(year, "-", month_num))
              }
            ),
            selected = purrr::map_chr(
              .x = c(as.Date("2008-01-01"), Sys.Date()),
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
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::selectInput(
          inputId = shiny::NS(id, "download_layer"),
          label = "Specify layers to download",
          choices = c(
            "Zip Code" = "zip",
            "FC School District" = "school_district",
            "Census Tract" = "census_tract"
          ),
          selected = NULL
        )
      ),

      shiny::column(
        width = 3,
        shiny::downloadButton(
          outputId = shiny::NS(id, "download_case_rate"),
          label = "Download Case Rate"
        )
      )
    )
  )
}

opioidOverdoseRateServer <- function(id, filtered_overdose_data,  od_data_all) {

  shiny::moduleServer(id, function(input, output, session){
    shiny::req(
      shiny::is.reactivevalues(filtered_overdose_data)
    )

    # ======================== #
    # ---- A. HyperParams ----
    # ======================== #

    hyper_params <- shiny::reactiveValues(
      data_source = od_data_all,
      data_min = as.Date("2008-01-01"),
      data_max = Sys.Date()
    )

    school_district_sf <-
      get_ohio_school_district_sf()

    fire_districts_sf <-
      get_fire_district_sf()

    census_tract_sf <-
      get_census_tract_sf()

    zipcode_sf <-
      get_zipcode_sf()

    EMS_sf <-
      get_EMS_sf()

    sf_map_data_list <-
      shiny::reactiveValues(
        zip = list(
          region_sf = get_region_od_rate(
            "zip",
            zipcode_sf,
            "GEOID",
            "GEOID",
            "TOTAL_POP",
            od_data = od_data_all
          ),
          pal = opioidDashboard::od_case_rate_init_pal_list$zip
        ),
        census_tract = list(
          region_sf = get_region_od_rate(
            "census_tract",
            census_tract_sf,
            "GEOID",
            "GEOID",
            "TOTAL_POP",
            od_data = od_data_all
          ),
          pal = opioidDashboard::od_case_rate_init_pal_list$census_tract
        ),
        school_district = list(
          region_sf = get_region_od_rate(
            "school_district",
            school_district_sf %>%
              dplyr::filter(.data$NAME %in% opioidDashboard::franklin_county_school_districts),
            "NAME",
            "NAME",
            "district_pop",
            od_data = od_data_all
          ),
          pal = opioidDashboard::od_case_rate_init_pal_list$school_district
        )
        # EMS = list(
        #   region_sf = get_region_od_rate(
        #     "EMS",
        #     EMS_sf,
        #     "DEPARTMENT",
        #     "DEPARTMENT",
        #     #"TOTAL_POP",
        #     od_data = hyper_params$data_source
        #   )
        # )
      )


    shiny::observe({

      od_data_source <- input$od_data_source


      if (od_data_source == "Raw Overdose Case Data") {
        date_min <- min(od_data_all$date)
        date_max <- max(od_data_all$date)
      } else {
        date_min <- min(filtered_overdose_data$data$date)
        date_max <- max(filtered_overdose_data$data$date)
      }


      shinyWidgets::updateSliderTextInput(
        session = session,
        inputId = "date_range",
        selected = purrr::map_chr(
          .x = c(date_min, date_max),
          .f = function(date) {
            year <- lubridate::year(date)
            month_num <- lubridate::month(date)
            return(paste0(year, "-", month_num))
          }
        ),
        choices = purrr::map_chr(
          .x = seq(
            from = date_min,
            to = date_max,
            by = "1 month"),
          .f = function(date) {
            year <- lubridate::year(date)
            month_num <- lubridate::month(date)
            return(paste0(year, "-", month_num))
          }
        )
      )

    })

    shiny::observeEvent(input$update_hyper_param, {

      # ===================================== #
      # ---- Handle overdose data source ----
      # ===================================== #
      od_data_source <- input$od_data_source


      data_min <- input$date_range[1]
      data_max <- input$date_range[2]

      if (od_data_source == "Raw Overdose Case Data") {
        hyper_params$data_source <- od_data_all %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      } else {
        hyper_params$data_source <- filtered_overdose_data$data %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      }


      sf_map_data_list$zip$region_sf <-
        get_region_od_rate(
          "zip",
          zipcode_sf,
          "GEOID",
          "GEOID",
          "TOTAL_POP",
          od_data = hyper_params$data_source
        )

      sf_map_data_list$census_tract$region_sf <-
        get_region_od_rate(
          "census_tract",
          census_tract_sf,
          "GEOID",
          "GEOID",
          "TOTAL_POP",
          od_data = hyper_params$data_source
        )


      sf_map_data_list$school_district$region_sf <-
        get_region_od_rate(
          "school_district",
          school_district_sf %>%
            dplyr::filter(.data$NAME %in% opioidDashboard::franklin_county_school_districts),
          "NAME",
          "NAME",
          "district_pop",
          od_data = hyper_params$data_source
        )


      sf_map_data_list$zip$pal <-
        leaflet::colorQuantile(
          palette = "YlOrRd",
          domain = sf_map_data_list$zip$region_sf$rate_pal,
          n = 5
        )

      sf_map_data_list$census_tract$pal <-
        leaflet::colorQuantile(
          palette = "YlOrRd",
          domain = sf_map_data_list$census_tract$region_sf$rate_pal,
          n = 5
        )

      sf_map_data_list$school_district$pal <-
        leaflet::colorQuantile(
          palette = "YlOrRd",
          domain = sf_map_data_list$school_district$region_sf$rate_pal,
          n = 5
        )

    })


    # ============================= #
    # ---- B. Case Rate Map () ----
    # ============================= #

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
        # =================== #
        # ---- Case Rate ----
      # ==================== #
      leaflet::addPolygons(
        data = sf_map_data_list$zip$region_sf,
        group = "Zip Code",
        stroke = TRUE,
        color = ~sf_map_data_list$zip$pal(rate_pal),
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
      ) %>%
        # leaflet::addLegend(
        #   data = sf_map_data_list$zip$region_sf,
        #   position = "bottomright",
        #   pal = od_case_rate_init_pal_list$zip,
        #   values = ~rate_pal,
        #   group = "Zip Code"
        # ) %>%

        # ============================ #
        # ---- FC School District ----
      # ============================ #
      leaflet::addPolygons(
        data = sf_map_data_list$school_district$region_sf,
        group = "FC school district",
        stroke = TRUE,
        color = ~sf_map_data_list$school_district$pal(rate_pal),
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
      ) %>%
        # ====================== #
        # ---- Census Tract ----
      # ====================== #
      leaflet::addPolygons(
        data = sf_map_data_list$census_tract$region_sf,
        group = "Census Tract",
        stroke = TRUE,
        color = ~sf_map_data_list$census_tract$pal(rate_pal),
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
      ) %>%
        # ======================== #
        # ---- Layers Control ----
      # ======================== #
      leaflet::addLayersControl(
        position = "topright",
        baseGroups = c(
          "Zip Code",
          "Census Tract",
          "FC school district"
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

    shiny::observeEvent(input$update_hyper_param, {


      leaflet::leafletProxy("od_case_rate_map") %>%
        leaflet::clearShapes() %>%
        # ================== #
        # ---- Zip Code ----
      # ================== #

      leaflet::addPolygons(
        data = sf_map_data_list$zip$region_sf,
        group = "Zip Code",
        stroke = TRUE,
        color = ~sf_map_data_list$zip$pal(rate_pal),
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
      ) %>%

        # ============================ #
        # ---- FC School District ----
      # ============================ #
      leaflet::addPolygons(
        data = sf_map_data_list$school_district$region_sf,
        group = "FC school district",
        stroke = TRUE,
        color = ~sf_map_data_list$school_district$pal(rate_pal),
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
      ) %>%
        # ====================== #
        # ---- Census Tract ----
      # ====================== #
      leaflet::addPolygons(
        data = sf_map_data_list$census_tract$region_sf,
        group = "Census Tract",
        stroke = TRUE,
        color = ~sf_map_data_list$census_tract$pal(rate_pal),
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
    })

    output$download_case_rate <- shiny::downloadHandler(
      filename = function() {
        paste("case_rate_", input$download_layer, "_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {

        switch (input$download_layer,
          zip = {
            case_rate_data <-
              sf_map_data_list$zip$region_sf %>%
              tibble::as_tibble() %>%
              dplyr::select(
                name = .data$GEOID,
                population = .data$TOTAL_POP,
                n = .data$n,
                rate = .data$rate
              )
          },
          school_district = {
            case_rate_data <-
              sf_map_data_list$school_district$region_sf %>%
              tibble::as_tibble() %>%
              dplyr::select(
                name = .data$school_district,
                population = .data$district_pop,
                population_school = .data$district_school_pop,
                population_proverty_children = .data$district_poverty_children_pop,
                n = .data$n,
                rate = .data$rate
              )
          },
          census_tract = {
            case_rate_data <-
              sf_map_data_list$census_tract$region_sf %>%
              tibble::as_tibble() %>%
              dplyr::select(
                name = .data$GEOID,
                population = .data$TOTAL_POP,
                n = .data$n,
                rate = .data$rate
              )
          }
        )
        readr::write_csv(case_rate_data, file)
      }
    )


  })
}
