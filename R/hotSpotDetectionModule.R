hotSpotDetectionUI <- function(id) {


  # shiny::selectInput(
  #   inputId = shiny::NS(id, "eligible_business_type"),
  #   label = "Eligible Business Type",
  #   choices = c(
  #     # Library
  #     "LIBRARIES-PUBLIC",
  #     # Convenience/liquor stores
  #     "CONVENIENCE STORES", "VARIETY STORES",
  #     # Fast food places/Restaurants
  #     "FOOD BANKS", "RESTAURANTS",
  #     # Check cashing places
  #     # Gas station
  #     "SERVICE STATIONS-GASOLINE & OIL",
  #     # Motels
  #     # Pawn shops
  #     "PAWNBROKERS"
  #     # Hardware stores
  #   ),
  #   multiple = TRUE,
  #   selected = c(
  #     # Library
  #     "LIBRARIES-PUBLIC",
  #     # Convenience/liquor stores
  #     "CONVENIENCE STORES", "VARIETY STORES",
  #     # Fast food places/Restaurants
  #     "FOOD BANKS", "RESTAURANTS",
  #     # Check cashing places
  #     # Gas station
  #     "SERVICE STATIONS-GASOLINE & OIL",
  #     # Motels
  #     # Pawn shops
  #     "PAWNBROKERS"
  #     # Hardware stores
  #   )
  # )
  shiny::tagList(
    shiny::fluidRow(
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
        width = 4,
        align="left",
        shiny::plotOutput(
          outputId = shiny::NS(id, "hot_spot_map"),
          height = "600px",
          width = "100%"
        )
      ),
      shiny::column(
        width = 8,
        reactable::reactableOutput(
          outputId = shiny::NS(id, "business_table")
        )
      )
    )
  )

}

hotSpotDetectionServer <- function(id, filtered_overdose_data) {

  shiny::moduleServer(id, function(input, output, session){
    shiny::req(
      shiny::is.reactivevalues(filtered_overdose_data)
    )

    hyper_params <- reactiveValues(
      data_source = opioidDashboard::opioid_overdose_data(),
      bin_width = 0.01,
      quantile = 0.75
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

      # ========================== #
      # ---- Handle bin width ----
      # ========================== #
      hyper_params$bin_width <- input$bin_width

      # ========================= #
      # ---- Handle quantile ----
      # ========================= #
      hyper_params$quantile <- input$quantile

    })

    zip_code_map <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = opioidDashboard::get_zipcode_sf())

    suppressMessages(
      oh_business <- vroom::vroom(
        file = "/fs/ess/PDE0001/business places data/Business-2020-OH.csv",
        delim = ",",
        col_names = FALSE
      ) %>%
        dplyr::transmute(
          id = dplyr::row_number(),
          name = .data$X1,
          type = .data$X13,
          address = .data$X2,
          city = .data$X3,
          zipcode = .data$X5,
          lat = readr::parse_number(.data$X47),
          lng = .data$X48
        ) %>%
        na.omit()
    )

    eligible_business_type <-
      c(
        # Library
        "LIBRARIES-PUBLIC",
        # Convenience/liquor stores
        "CONVENIENCE STORES", "VARIETY STORES",
        # Fast food places/Restaurants
        "FOOD BANKS", "RESTAURANTS",
        # Check cashing places
        # Gas station
        "SERVICE STATIONS-GASOLINE & OIL",
        # Motels
        # Pawn shops
        "PAWNBROKERS"
        # Hardware stores

      )


    output$hot_spot_map <- shiny::renderPlot({

      data_source <- hyper_params$data_source
      bin_width <- hyper_params$bin_width
      quantile <- hyper_params$quantile


      zip_code_map +
        ggplot2::geom_bin_2d(
          ggplot2::aes(x = lng, y = lat),
          binwidth = c(bin_width, bin_width),
          data = data_source
        ) +
        ggplot2::geom_text(
          ggplot2::aes(x = lng, y = lat, label = hot_spot),
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
        ggplot2::coord_sf(
          xlim = c(
            opioidDashboard::opioid_overdose_map_init_bounds$min_lng-0.1,
            opioidDashboard::opioid_overdose_map_init_bounds$max_lng+0.1
          ),
          ylim = c(
            opioidDashboard::opioid_overdose_map_init_bounds$min_lat-0.1,
            opioidDashboard::opioid_overdose_map_init_bounds$max_lat+0.1
          )
        ) +
        ggplot2::theme(
          legend.position = "bottom",
          plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        )
    })

    output$business_table <- reactable::renderReactable({

      data_source <- hyper_params$data_source
      bin_width <- hyper_params$bin_width
      quantile <- hyper_params$quantile



      opioidDashboard::get_hot_spot_region(
        od_data = data_source,
        percent_tile = quantile,
        bin_width = c(bin_width, bin_width)
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
            eligible_business_type = eligible_business_type
          )
        ) %>%
        dplyr::select(business) %>%
        tidyr::unnest(cols = "business") %>%
        dplyr::select(
          hot_spot, dplyr::everything()
        ) %>%
        reactable::reactable(
          filterable = TRUE,
          outlined = TRUE,
          defaultPageSize = 5, minRows = 5
        )


    })
  })
}
