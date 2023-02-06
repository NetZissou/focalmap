fcphUI <- function(id) {
  shiny::tagList(
    shiny::div(
      class = "fcph_map",

      # ==================== #
      # ---- CCS Format ----
      # ==================== #
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(
          "
          div.fcph_map {
            position: fixed;
            top: 85px;
            left: 0;
            right: 0;
            bottom: 0;
            overflow: hidden;
            padding: 0;
          }

          #controlBox {
            opacity: 0.95;
          }

          #businessTable {
            opacity: 0.95;
          }
          "
        ))
      ),


      # ============================================ #
      # ---- Result: Hot Spot Map (Interactive) ----
      # ============================================ #

      leaflet::leafletOutput(
        outputId = shiny::NS(id, "fcph_map"),
        width="100%", height="100%"
      ),

      shiny::absolutePanel(
        id = "controls",
        fixed = TRUE,
        draggable = TRUE,
        top = 100, left = 20,
        right = "auto", bottom = "auto",
        width = 700, height = "auto",

        shinydashboard::box(
          id = "controlBox",
          title = shiny::tagList(shiny::icon("gear"), "Toolkit"),
          collapsible = TRUE, width = 12, solidHeader = TRUE, collapsed = TRUE,

          shiny::tabsetPanel(

            # ================================== #
            # ---- Tab: Base Layer Controls ----
            # ================================== #
            shiny::tabPanel(
              "Base Layer",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = shiny::NS(id, "base_layer_data_source"),
                    label = "Source",
                    choices = c(
                      "Raw Overdose Case Data",
                      "Filtered Overdose Case Data",
                      "Drug-related Crime Data"
                      # TODO: Make 311 Data available
                      #"311 Request Data"
                    ),
                    selected = NULL
                  )
                ),

                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = shiny::NS(id, "base_layer_display_option"),
                    label = "Display Option",
                    choices = c(
                      "nested cluster",
                      "spatial rate"
                    ),
                    selected = NULL
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
              ),



              shiny::fluidRow(

                shiny::column(
                  width = 12,
                  # ======================= #
                  # ---- Param: Update ---- #
                  # ======================= #
                  shiny::actionButton(
                    inputId = shiny::NS(id, "update_base_layer"),
                    label = "Update Base Layer"
                  )
                )
              )
            ),

            # ============================= #
            # ---- Tab: Hot Spot Layer ----
            # ============================= #

            shiny::tabPanel(
              "Hot Spot Layer",

              shiny::fluidRow(

                shiny::column(
                  width = 6,
                  # =========================== #
                  # ---- Param: Bin Width ----  #
                  # =========================== #

                  shiny::numericInput(
                    inputId = shiny::NS(id, "hot_spot_bin_width"),
                    label = "Bin Size",
                    min = 0,
                    value = 0.01,
                    step = 0.001
                  )
                ),
                shiny::column(
                  width = 6,
                  # ========================= #
                  # ---- Param: Quantile ---- #
                  # ========================= #
                  shiny::numericInput(
                    inputId = shiny::NS(id, "hot_spot_quantile"),
                    label = "Hot Spot Quantile",
                    min = 0,
                    max = 0.99,
                    value = 0.75,
                    step = 0.01
                  )
                )

              ),

              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  # ===================================== #
                  # ---- Param: Overdose Data Source ---- #
                  # ===================================== #
                  shiny::selectInput(
                    inputId = shiny::NS(id, "hot_spot_od_data_source"),
                    label = "Choose overdose cases data source",
                    choices = c(
                      "Raw Overdose Case Data",
                      "Filtered Overdose Case Data"
                    ),
                    selected = NULL
                  )
                ),

                shiny::column(
                  width = 6,
                  # ======================= #
                  # ---- Param: Update ---- #
                  # ======================= #
                  shiny::actionButton(
                    inputId = shiny::NS(id, "update_param_hot_spot"),
                    label = "Update Hyperparameters"
                  )
                )
              ),

              shiny::plotOutput(
                outputId = shiny::NS(id, "hot_spot_map_mini"),
                height = "500px",
                width = "100%"
              )
            ),

            # ================================= #
            # ---- Tab: Region of Interest ----
            # ================================= #
            shiny::tabPanel(
              "Region Analysis",

              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = shiny::NS(id, "region_type"),
                    label = "Region Type",
                    multiple = FALSE,
                    choices = list(
                      `Zip` = "zip",
                      `Census Tract` = "census_tract",
                      `School District` = "school_district",
                      `EMS` = "EMS"
                    ),
                    selected = ""
                  ),
                ),
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = shiny::NS(id, "region_select"),
                    label = "Choices",
                    multiple = TRUE,
                    choices = c(),
                    selected = ""
                  )
                )
              ),


              reactable::reactableOutput(
                outputId = shiny::NS(id, "od_count_by_agency_table")
              )


              # EWMA Chart

              # Resource List

              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_school_district"),
              #   label = "School District",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # ),
              #
              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_fire_district"),
              #   label = "Fire District",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # ),
              #
              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_census_tract"),
              #   label = "Census Tract",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # ),
              #
              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_zip"),
              #   label = "Zip",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # )
            ),


          )
        )
      )

    )
  )

}


fcphSERVER <- function(id, filtered_overdose_data, od_data_all, drug_crime_data_all) {

  shiny::moduleServer(id, function(input, output, session){

    shiny::req(
      shiny::is.reactivevalues(filtered_overdose_data)
    )

    # ================================ #
    # ---- Load Data & Shapefiles ----
    # ================================ #

    franklin_county_school_district_sf <-
      get_franklin_county_school_district_sf()

    fire_districts_sf <-
      get_fire_district_sf()

    census_tract_sf <-
      get_census_tract_sf()

    zipcode_sf <-
      get_zipcode_sf()

    # > Food Pantries
    food_pantries <-
      data_food_pantries()

    food_pantries_icon <-
      leaflet::makeIcon(
        iconUrl  = "inst/icons/food.png",
        iconWidth = 36,
        iconHeight = 36
      )

    # > HIV Testing Locations
    hiv_testing_locations <-
      data_hiv_testing_locations()

    hiv_testing_locations_icon <-
      leaflet::makeAwesomeIcon(
        icon = 'health',
        library = 'ion',
        iconColor = 'white',
        markerColor = 'red'
      )

    # > Treatment Providers
    treatment_providers_data_all <- opioidDashboard::treatment_providers_data(tier_2_only = TRUE)

    # > Hep C Treatment
    hepc_treatment <- data_hepc_treatment()
    hepc_treatmetn_facilities <-
      hepc_treatment %>%
      dplyr::count(
        .data$adrs_id,
        .data$org_nm,
        .data$adr_geocode,
        .data$lat,
        .data$lng,
        .data$school_district,
        .data$EMS,
        .data$census_tract
      ) %>%
      dplyr::mutate(
        popup = paste(sep = "<br/>",
                      "<b>Hep C Treatment Resource<b>",
                      glue::glue(
                        "<b>{org_name}</b>", org_name = .data$org_nm
                      ),
                      glue::glue(
                        "<b>Address: </b>{org_addr}", org_addr = .data$adr_geocode
                      ),
                      glue::glue(
                        "<b>Number of Doctors: </b>{n_doc}", n_doc = .data$n
                      )
        )
      )

    # > COTA stops
    sf_cota_stops <-
      get_cota_bus_stops_sf()

    # > COTA lines
    sf_cota_lines <-
      get_cota_bus_lines_sf()

    # > 311 Heatmap
    heatmap_data_311 <-
      data_columbus_311_heatmap()

    # ========================= #
    # ---- Hyperparameters ----
    # ========================= #


    hyper_params <- shiny::reactiveValues(
      base_layer_data_source = od_data_all,
      base_layer_display_option = "nested cluster",
      hot_spot_data_source = od_data_all,
      hot_spot_bin_width = 0.01,
      hot_spot_quantile = 0.75
    )

    hyper_params_sf_rate <-
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
            franklin_county_school_district_sf,
            "NAME",
            "NAME",
            "district_pop",
            od_data = od_data_all
          ),
          pal = opioidDashboard::od_case_rate_init_pal_list$school_district
        )
        # TODO: No population
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

    # ======================================= #
    # > Update param: hot spot detection ----

    shiny::observeEvent(input$update_param_hot_spot, {

      # ===================================== #
      # ---- Handle overdose data source ---- #
      # ===================================== #
      od_data_source <- input$hot_spot_od_data_source

      if (od_data_source == "Raw Overdose Case Data") {
        hyper_params$data_source <- od_data_all
      } else {
        hyper_params$data_source <- filtered_overdose_data$data
      }

      # ========================== #
      # ---- Handle bin width ---- #
      # ========================== #
      hyper_params$hot_spot_bin_width <- input$hot_spot_bin_width

      # ========================= #
      # ---- Handle quantile ---- #
      # ========================= #
      hyper_params$hot_spot_quantile <- input$hot_spot_quantile

    })

    # ===================================== #
    # > Update param: date range input ----
    shiny::observe({

      base_layer_data_source <- input$base_layer_data_source

      if (base_layer_data_source == "Raw Overdose Case Data") {
        date_min <- min(od_data_all$date)
        date_max <- max(od_data_all$date)
      } else if (base_layer_data_source == "Filtered Overdose Case Data") {
        date_min <- min(filtered_overdose_data$data$date)
        date_max <- max(filtered_overdose_data$data$date)
      } else if (base_layer_data_source == "Drug-related Crime Data") {
        date_min <- min(drug_crime_data_all$date, na.rm = T)
        date_max <- max(drug_crime_data_all$date, na.rm = T)
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

    # =========================================== #
    # > Update param: base layer data source ----

    shiny::observeEvent(input$update_base_layer, {

      base_layer_data_source <- input$base_layer_data_source

      data_min <- input$date_range[1]
      data_max <- input$date_range[2]

      if (base_layer_data_source == "Raw Overdose Case Data") {
        hyper_params$base_layer_data_source <- od_data_all %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      } else if (base_layer_data_source == "Filtered Overdose Case Data") {
        hyper_params$base_layer_data_source <- filtered_overdose_data$data %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      } else if (base_layer_data_source == "Drug-related Crime Data") {
        hyper_params$base_layer_data_source <- drug_crime_data_all %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      }


      hyper_params$base_layer_display_option <-
        input$base_layer_display_option

    })

    # ===================================== #
    # > Update param: Region Selection ----

    shiny::observeEvent(input$region_type, {

      region_type <- input$region_type

      if (region_type == "zip") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = zipcode_sf$GEOID
        )
      } else if (region_type == "census_tract") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = census_tract_sf$GEOID
        )
      } else if (region_type == "school_district") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = franklin_county_school_district_sf$NAME
        )
      } else if (region_type == "EMS") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = opioidDashboard::AGENCY
        )
      }

    })


    # =================================== #
    # ---- Hot Spot Algorithm Result ----
    # =================================== #

    # ---- Hot Spot Region Data for heatmap ---- #
    hot_spot_region <- shiny::reactiveValues(
      value = opioidDashboard::get_hot_spot_region(
        od_data = od_data_all,
        percent_tile = 0.75,
        bin_width = c(0.01, 0.01)
      )
    )



    shiny::observeEvent(input$update_param_hot_spot, {
      hot_spot_region$value <-
        opioidDashboard::get_hot_spot_region(
          od_data = hyper_params$hot_spot_data_source,
          percent_tile = hyper_params$hot_spot_quantile,
          bin_width = c(hyper_params$hot_spot_bin_width, hyper_params$hot_spot_bin_width)
        )
    })


    # ---- Hot Spot Map (mini) ---- #

    output$hot_spot_map_mini <- shiny::renderPlot({

      hot_spot_data_source <- hyper_params$hot_spot_data_source
      hot_spot_bin_width <- hyper_params$hot_spot_bin_width
      hot_spot_quantile <- hyper_params$hot_spot_quantile


      ggmap::ggmap(opioidDashboard::COLUMBUS_STATIC_ROADMAP) +
        ggplot2::geom_bin_2d(
          ggplot2::aes(x = .data$lng, y = .data$lat),
          binwidth = c(hot_spot_bin_width, hot_spot_bin_width),
          alpha = 0.75,
          data = hot_spot_data_source
        ) +
        ggplot2::geom_text(
          ggplot2::aes(x = .data$lng, y = .data$lat, label = .data$hot_spot),
          data =  opioidDashboard::get_hot_spot_region(
            od_data = hot_spot_data_source,
            percent_tile = hot_spot_quantile,
            bin_width = c(hot_spot_bin_width, hot_spot_bin_width)
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
        ggplot2::labs(x = "", y = "", title = NULL) +
        ggplot2::theme(
          legend.position = "top",
          plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        )
    })

    # ================== #
    # ---- Map Init ----
    # ================== #

    output$fcph_map <- leaflet::renderLeaflet({

      opioid_overdose_map_init_bounds <- opioidDashboard::opioid_overdose_map_init_bounds

      hot_spot_data_source <- hyper_params$hot_spot_data_source
      hot_spot_bin_width <- hyper_params$hot_spot_bin_width
      hot_spot_quantile <- hyper_params$hot_spot_quantile

      heatmap_data <-
        ggplot2::layer_data(
          ggplot2::ggplot() +
            ggplot2::geom_bin_2d(
              ggplot2::aes(x = .data$lng, y = .data$lat),
              binwidth = c(hot_spot_bin_width, hot_spot_bin_width),
              data = hot_spot_data_source
            ) +
            ggplot2::scale_fill_gradient('Overdose Cases',
                                         low = "#ffeda0",
                                         high = "#f03b20")
        )

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

        # ==================== #
        # ---- Base Layer ----
      # ===================== #
      leaflet::addCircleMarkers(
        data = hyper_params$base_layer_data_source,
        lng = ~lng, lat = ~lat,
        stroke = FALSE,
        fillColor = "#de2d26",
        fillOpacity = 0.3,
        clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
        group = "Base Layer",
        clusterId = "baseCluster"
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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        # =========================== #
        # ---- Hot Spot Heat Map ----
      # ============================ #
      leaflet::addRectangles(
        data = heatmap_data,
        group = "Heatmap",
        lng1 = ~xmin, lng2 = ~xmax,
        lat1 = ~ymin, lat2 = ~ymax,
        color = ~fill,
        dashArray = "3",
        fillColor = ~fill,
        fillOpacity = 0.40,
        opacity = 0
      ) %>%
        # ====================== #
        # ---- Food Pantries ----
      # ======================= #
      leaflet::addMarkers(
        data = food_pantries,
        lat = ~lat,
        lng = ~lng,
        popup = ~popup_label,
        icon = food_pantries_icon,
        group = "Food Pantries"
      ) %>%
        # =============================== #
        # ---- HIV Testing Locations ----
      # ================================ #
      leaflet::addAwesomeMarkers(
        data = hiv_testing_locations,
        lat = ~lat,
        lng = ~lng,
        popup = ~popup_label,
        icon = hiv_testing_locations_icon,
        group = "HIV Testing Sites"
      ) %>%
        # ============================= #
        # ---- Treatment Providers ----
      # ============================== #
      leaflet::addMarkers(
        data = treatment_providers_data_all,
        lng = ~lng, lat = ~lat,
        popup = ~popup,
        #label = ~label,
        group = "Treatment Providers",
        labelOptions = leaflet::labelOptions(
          style = list(
            "font-size" = "15px",
            "font-style" = "bold",
            "border-color" = "rgba(0,0,0,0.5)"
          )
        )
      ) %>%
        # =================================== #
        # ---- HepC Treatment Facilities ----
      # ==================================== #
      leaflet::addMarkers(
        data = hepc_treatmetn_facilities,
        lng = ~lng, lat = ~lat,
        popup = ~popup,
        #label = ~label,
        group = "Hep C Treatment Facilities"
        # labelOptions = leaflet::labelOptions(
        #   style = list(
        #     "font-size" = "15px",
        #     "font-style" = "bold",
        #     "border-color" = "rgba(0,0,0,0.5)"
        #   )
        # )
      ) %>%

        # ======================= #
        # ---- COTA BUS Info ----
      # ======================== #
      leaflet::addPolylines(
        data = sf_cota_lines,
        color = "black",
        label = ~LineName,
        highlight = leaflet::highlightOptions(
          weight = 8,
          #fillOpacity = 0.1,
          color = "blue",
          #dashArray = "",
          #opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        group = "COTA Lines"
      ) %>%
        leaflet::addCircles(
          data = sf_cota_stops,
          color = "red",
          popup = ~StopName,
          group = "COTA Stops"
        ) %>%

        # ============================== #
        # ---- Columbus 311 Heatmap ----
      # =============================== #
      leaflet::addRectangles(
        data = heatmap_data_311,
        group = "Columbus 311 Request Heatmap (2022)",
        lng1 = ~xmin, lng2 = ~xmax,
        lat1 = ~ymin, lat2 = ~ymax,
        color = ~fill,
        dashArray = "3",
        fillColor = ~fill,
        fillOpacity = 0.60,
        opacity = 0
      ) %>%
        # ======================== #
        # ---- Layers Control ----
      # ======================== #
      leaflet::addLayersControl(
        position = "topright",
        baseGroups = c(
          "Base Layer"
        ),
        overlayGroups = c(
          "Heatmap",
          "FC School Districts",
          "Fire Districts",
          "Census Tract",
          "Zip Code",
          "Food Pantries",
          "HIV Testing Sites",
          "Treatment Providers",
          "Hep C Treatment Facilities",
          "COTA Lines",
          "COTA Stops",
          "Columbus 311 Request Heatmap (2022)"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
        leaflet::hideGroup(
          c(
            "Heatmap",
            "FC School Districts",
            "Fire Districts",
            "Census Tract",
            "Zip Code",
            "Food Pantries",
            "HIV Testing Sites",
            "Treatment Providers",
            "Hep C Treatment Facilities",
            "COTA Lines",
            "COTA Stops",
            "Columbus 311 Request Heatmap (2022)"
          )
        ) %>%

        # ========================= #
        # ---- Cluster Control ----
      # ========================= #

      leaflet::addEasyButton(leaflet::easyButton(
        position = "topright",
        states = list(
          leaflet::easyButtonState(
            stateName="unfrozen-markers",
            icon="ion-toggle",
            title="Freeze Clusters",
            onClick = leaflet::JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'baseCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
          ),
          leaflet::easyButtonState(
            stateName="frozen-markers",
            icon="ion-toggle-filled",
            title="UnFreeze Clusters",
            onClick = leaflet::JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'baseCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
          )
        )
      ))
      # --- Full Screen Control --- #
      # leaflet.extras::addFullscreenControl(
      #   position = "topleft",
      #   pseudoFullscreen = FALSE
      # ) %>%
      # leaflet.extras::addResetMapButton() # reset
    })


    # ==================================== #
    # ---- Map Update (click observe) ----
    # ==================================== #

    map_select_region <- shiny::reactiveValues(
      value = NULL,
      group = NULL,
      click = NULL
    )

    shiny::observe({

      click <- input$fcph_map_shape_click

      selected_region <- NULL
      group<- NULL

      if (!is.null(click)) {

        if (click$group == "FC School Districts") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = franklin_county_school_district_sf, id = "NAME")

          group <- "school_district"

        } else if (click$group == "Fire Districts") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = fire_districts_sf, id = "DEPARTMENT")

          group <- "EMS"

        } else if (click$group == "Census Tract") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = census_tract_sf, id = "GEOID")

          group <- "census_tract"

        } else if (click$group == "Zip Code") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = zipcode_sf, id = "GEOID")

          group <- "zip"

        }

      }

      print(click)

      #print(selected_region)
      map_select_region$value <- selected_region
      map_select_region$group <- group
      #map_select_region$click <- click

    })

    shiny::observe({

      print(map_select_region$value)
      print(map_select_region$group)

      # if (!is.null(map_select_region$value)) {
      #   print("not null")
      #   region <- map_select_region$value
      #   attr <- map_select_region$group
      #   if (region %in% map_select_stack$value$name) {
      #     map_select_stack$value <-
      #       map_select_stack$value %>%
      #       dplyr::filter(
      #         !name %in% region
      #       )
      #   } else {
      #     map_select_stack$value <-
      #       map_select_stack$value %>%
      #       rbind(
      #         c(region, attr)
      #       )
      #   }
      # }
    })

    # shiny::observe({
    #   print(map_select_stack$value)
    # })


    shiny::observeEvent(input$update_base_layer, {

      if (hyper_params$base_layer_display_option == "spatial rate") {

        print("spatial rate")

        leaflet::leafletProxy("fcph_map") %>%
          leaflet::clearGroup(
            c(
              "Base Layer",
              "FC School Districts",
              "Census Tract",
              "Zip Code"
            )
          ) %>%
          # ================== #
          # ---- Zip Code ----
        # ================== #
        leaflet::addPolygons(
          data = hyper_params_sf_rate$zip$region_sf,
          group = "Zip Code",
          stroke = TRUE,
          color = ~hyper_params_sf_rate$zip$pal(rate_pal),
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
          data = hyper_params_sf_rate$school_district$region_sf,
          group = "FC School Districts",
          stroke = TRUE,
          color = ~hyper_params_sf_rate$school_district$pal(rate_pal),
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
          data = hyper_params_sf_rate$census_tract$region_sf,
          group = "Census Tract",
          stroke = TRUE,
          color = ~hyper_params_sf_rate$census_tract$pal(rate_pal),
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
            "FC School Districts",
            "Census Tract"
          ),
          overlayGroups = c(
            "Heatmap",
            "Fire Districts",
            "Food Pantries",
            "HIV Testing Sites",
            "Treatment Providers",
            "Hep C Treatment Facilities",
            "COTA Lines",
            "COTA Stops",
            "Columbus 311 Request Heatmap (2022)"
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
          leaflet::showGroup("Zip Code") %>%
          leaflet::hideGroup(
            c(
              "Heatmap",
              "Fire Districts",
              "FC School Districts",
              "Census Tract",
              "Food Pantries",
              "HIV Testing Sites",
              "Treatment Providers",
              "Hep C Treatment Facilities",
              "COTA Lines",
              "COTA Stops",
              "Columbus 311 Request Heatmap (2022)"
            )
          )


      }

    })


    # ========================= #
    # ---- Region Analysis ----
    # ========================= #

    # ====================== #
    # > Region Od Data ----- #
    region_od_data <- shiny::reactiveValues(
      value = NULL
    )

    shiny::observe({
      region_type <- input$region_type
      region_select <- input$region_select

      if (!rlang::is_empty(region_select)) {
        region_od_data$value <-
          od_data_all %>%
          dplyr::filter(
            .data[[region_type]] %in% region_select
          )
      } else {
        region_od_data$value <- NULL
      }

    })


    # ===============================
    # > OD count by agency table ----
    output$od_count_by_agency_table <- reactable::renderReactable({

      od_count_by_agency <-
        tibble::tibble(
          Agency = character(),
          Count = numeric()
        )


      if (!rlang::is_empty(region_od_data$value)) {
        od_count_by_agency <-
          od_count_by_agency %>%
          dplyr::bind_rows(
            region_od_data$value %>%
              dplyr::count(.data$agency, sort = TRUE) %>%
              purrr::set_names(c("Agency", "Count"))
          )
      }

      od_count_by_agency %>%
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
          defaultPageSize = 5, minRows = 5
        )

    })




  })

}

