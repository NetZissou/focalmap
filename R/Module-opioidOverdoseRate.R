opioidOverdoseRateUI <- function(id) {
  shiny::tagList(
    shiny::div(
      class = "outer_map",

      shiny::tags$head(
        # Include our custom CSS
        shiny::tags$style(shiny::HTML(
          "
          div.outer_map {
            position: fixed;
            top: 85px;
            left: 0;
            right: 0;
            bottom: 0;
            overflow: hidden;
            padding: 0;
          }

          #controls {
            /* Appearance */
            background-color: white;
            padding: 0 20px 20px 20px;
            cursor: move;
            /* Fade out while not hovering */
            opacity: 0.65;
            zoom: 0.9;
            transition: opacity 500ms 1s;
          }
          #controls:hover {
            /* Fade in while hovering */
            opacity: 0.95;
            transition-delay: 0;
          }"
        ))
      ),

      # ======================= #
      # ---- Case Rate Map ----
      # ======================= #
      leaflet::leafletOutput(
        outputId = shiny::NS(id, "od_case_rate_map"),
        width="100%", height="100%"
      ),

      # ========================== #
      # ---- Parameters Panel ----
      # ========================== #
      shiny::absolutePanel(
        id = "controls",
        fixed = TRUE,
        draggable = TRUE,
        top = 100, left = 20,
        right = "auto", bottom = "auto",
        width = 450, height = "auto",

        shiny::h2("Controls"),
        # > Param: Data Source ----
        shiny::selectInput(
          inputId = shiny::NS(id, "od_data_source"),
          label = "Choose overdose cases data source",
          choices = c(
            "Raw Overdose Case Data",
            "Filtered Overdose Case Data"
          ),
          selected = NULL
        ),

        # > Param: Date Range ----
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
        ),

        # > Update Parameters ----
        shiny::actionButton(
          inputId = shiny::NS(id, "update_hyper_param"),
          label = "Update Map"
        ),

        # > Download: Source ----

        shiny::h2("Download Case Rate"),

        shiny::fluidRow(
          shiny::column(
            width = 6,
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
            width = 6,
            shiny::downloadButton(
              outputId = shiny::NS(id, "download_case_rate"),
              label = "Download Case Rate"
            )
          )
        ),

        # > Case Rate Table
        reactable::reactableOutput(
          outputId = shiny::NS(id, "case_rate_table")
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

      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomControl = FALSE
        )
      ) %>%
        leaflet::setView(
          lat = 39.9612,
          lng = -82.9988,
          zoom = 10
        ) %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        # ========================= #
        # ---- Map init Bounds ----
      # ========================= #
      # leaflet::fitBounds(
      #   lng1 = opioid_overdose_map_init_bounds$min_lng,
      #   lat1 = opioid_overdose_map_init_bounds$min_lat,
      #   lng2 = opioid_overdose_map_init_bounds$max_lng,
      #   lat2 = opioid_overdose_map_init_bounds$max_lat
      # )  %>%

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
        position = "bottomleft",
        baseGroups = c(
          "Zip Code",
          "Census Tract",
          "FC school district"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
      # --- Full Reset Control --- #
      # leaflet.extras::addFullscreenControl(
      #   position = "bottomleft",
      #   pseudoFullscreen = FALSE
      # ) %>%
      #leaflet.extras::addResetMapButton() # reset

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

    # ========================= #
    # ---- Case Rate Table ----
    # ========================= #
    case_rate_table_data <- shiny::reactiveValues(
      value = get_region_od_rate(
        "zip",
        zipcode_sf,
        "GEOID",
        "GEOID",
        "TOTAL_POP",
        od_data = od_data_all
      ) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          name = .data$GEOID,
          population = .data$TOTAL_POP,
          n = .data$n,
          rate = .data$rate
        )
    )

    shiny::observe({

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

      case_rate_table_data$value <- case_rate_data
    })

    output$case_rate_table <- reactable::renderReactable({

      MIN_ROW <- 5
      if (input$download_layer == "school_district") {
        MIN_ROW <- 3
      }

      case_rate_table_data$value %>%
        dplyr::select(
          Name = .data$name,
          Population = .data$population,
          `Case Count` = .data$n,
          `Rate per 100K` = .data$rate
        ) %>%
        dplyr::arrange(dplyr::desc(.data$`Rate per 100K`)) %>%
        dplyr::mutate(
          `Rate per 100K` = floor(.data$`Rate per 100K`),
          Name = stringr::str_remove(.data$Name, "City School District"),
          Name = stringr::str_remove(.data$Name, "Local School District"),
          Name = stringr::str_remove(.data$Name, "School District")
        ) %>%
        reactable::reactable(
          # Table Format
          filterable = TRUE,
          outlined = TRUE,
          fullWidth = FALSE,
          #defaultColDef = reactable::colDef(minWidth = 50),
          #height = 400,
          #bordered = TRUE,
          # Selection
          #selection = "multiple", onClick = "select",
          highlight = TRUE,
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          # Table Size
          defaultPageSize = MIN_ROW, minRows = MIN_ROW
        )
    })

    output$download_case_rate <- shiny::downloadHandler(
      filename = function() {
        paste("case_rate_", input$download_layer, "_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        readr::write_csv(case_rate_table_data$data, file)
      }
    )


  })
}
