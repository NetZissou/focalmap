opioidOverdoseFiltersUI <- function(id) {

  shinydashboardPlus::box(
    title = "Filters", solidHeader = TRUE, width = 12,
    collapsed = FALSE, collapsible = TRUE,
    id = shiny::NS(id, "filter_box"),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h4("Demographic"),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::selectizeInput(
              inputId = shiny::NS(id, "gender"),
              label = "Gender",
              multiple = TRUE,
              choices = c("", "Female", "Male"),
              selected = ""
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectizeInput(
              inputId = shiny::NS(id, "ethnicity"),
              label = "Race",
              multiple = TRUE,
              choices = c("", "White", "Black or African American", "Asian",
                          "Other"),
              options = list(`multiple-separator` = " | "),
              selected = ""
            )
          )
        ),

        shiny::h4("Temporal"),
        shinyWidgets::sliderTextInput(
          inputId = shiny::NS(id, "date_range_dummy"),
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
        ),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::dateRangeInput(
              inputId = shiny::NS(id, "date_range"),
              label = "Date Range",
              min = "2008-01-01",
              start = "2008-01-01",
              end = "2022-01-01"
            )
          )
        ),

        shiny::h4("Spatial"),
        shiny::fluidRow(

          shiny::column(
            width = 3,
            # Spatial - zip code
            shiny::selectizeInput(
              inputId = shiny::NS(id, "zip"),
              label = "Zip code",
              multiple = TRUE,
              selected = "",
              choices = opioidDashboard::filter_selection_zip
            )
          ),
          shiny::column(
            width = 3,
            # Spatial - agency
            shiny::selectizeInput(
              inputId = shiny::NS(id, "agency"),
              label = "Agency",
              multiple = TRUE,
              selected = "",
              choices = opioidDashboard::filter_selection_agency
            )
          )
        ),

        shiny::fluidRow(

          shiny::column(
            width = 3,
            # Spatial - location type
            shiny::selectizeInput(
              inputId = shiny::NS(id, "location_type"),
              label = "Location Type",
              multiple = TRUE,
              selected = "",
              choices = opioidDashboard::filter_selection_location_type
            )
          ),
          shiny::column(
            width = 3,
            # Spatial - destination
            shiny::selectizeInput(
              inputId = shiny::NS(id, "destination"),
              label = "Destination",
              multiple = TRUE,
              selected = "",
              choices = opioidDashboard::filter_selection_destination
              # options = list(
              #   `selected-text-format`= "count",
              #   `count-selected-text` = "{0} destination choosed (on a total of {1})"
              # )
            )
          )
        ),

        shiny::h4("Treatment Providers"),
        shiny::textOutput(
          outputId = shiny::NS(id, "available_treatment_providers")
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::selectizeInput(
              inputId = shiny::NS(id, "treatment_providers_service"),
              label = "Service Type",
              multiple = TRUE,
              selected = "",
              choices = opioidDashboard::filter_selection_treatment_providers_service
            ),

            shiny::selectizeInput(
              inputId = shiny::NS(id, "treatment_providers_spec"),
              label = "Specifications",
              multiple = TRUE,
              selected = "",
              choices = opioidDashboard::filter_selection_treatment_providers_spec
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::helpText("Make a subset of the opioid overdose data using the filters above. "),
            shiny::helpText("When you finish tuning, click the apply button below to activate those filter criteria. "),
            shiny::helpText("To reset all filters, click the reset button below and follow the apply button to receive all data available. "),
            shiny::br()
          )
        ),

        shiny::fluidRow(

          shiny::column(
            width = 2,
            shiny::actionButton(shiny::NS(id, "apply_filters"), label = "Apply filters")
          ),
          shiny::column(
            width = 2,
            shiny::actionButton(shiny::NS(id, "reset_filters"), label = "Reset filters")
          ),
          shiny::column(
            width = 2,
            shiny::actionButton(shiny::NS(id, "collapse_filter_box"), label = "Hide filters")
          )
        ),

        shiny::tags$br(),

        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::downloadButton(shiny::NS(id, "download_filtered_data"), label = "Download filtered data", icon = NULL)
          ),
          shiny::column(
            width = 3,
            snapper::download_button(
              ui = paste0("#", shiny::NS(shiny::NS(id, "od_map"), "overdose_map")),
              label = "Capture map screenshot",
              filename = "opioid_overdose_map.png",
              opts = snapper::config(
                allowTaint = TRUE,
                useCORS = TRUE
              )
            )
          )
        )
      ),

      shiny::column(
        width = 6,
        shinyWidgets::addSpinner(
          apexcharter::apexchartOutput(outputId = shiny::NS(id, "ts_box"), height = "180px"),
          spin = "fading-circle"
        ),

        opioidOverdoseMapUI(shiny::NS(id, "od_map"))
      )
    )



  )
}

opioidOverdoseFiltersServer <- function(id, od_data_all) {

  shiny::moduleServer(id, function(input, output, session){
    # ================================ #
    # ---- Initialize source data ----
    # ================================ #

    # Source data: opioid overdose data
    filtered_overdose_data <- shiny::reactiveValues(
      data = od_data_all
    )

    # Source data: drug crime data
    drug_crime_data_all <- opioidDashboard::drug_crime_data()
    filtered_drug_crime_data <- shiny::reactiveValues(
      data = drug_crime_data_all
    )

    # Source data: treatment providers
    treatment_providers_data_all <- opioidDashboard::treatment_providers_data(tier_2_only = TRUE)
    filtered_treatment_providers_data <- shiny::reactiveValues(
      data = treatment_providers_data_all
    )

    n_treatment_providers <- shiny::reactiveValues(
      value = nrow(treatment_providers_data_all)
    )

    output$available_treatment_providers <- shiny::renderText({

      text <- paste0(
        "Number of avaiable treatment providers: ",
        n_treatment_providers$value
      )
      return(text)
    })

    shiny::observe({

      df <- treatment_providers_data_all

      n <- nrow(df)

      if (!nothing_selected(input$treatment_providers_spec)) {

        filtered_providers_id <-
          df  %>%
          tidyr::pivot_longer(
            cols = as.character(opioidDashboard::filter_selection_treatment_providers_spec),
            names_to = "spec",
            values_to = "status"
          ) %>%
          dplyr::filter(
            .data$spec %in% input$treatment_providers_spec,
            .data$status == TRUE
          ) %>%
          dplyr::distinct(.data$id) %>%
          dplyr::pull(.data$id)

        df <-
          df %>%
          dplyr::filter(
            .data$id %in% filtered_providers_id
          )
        n <- nrow(df)
      }

      if (!nothing_selected(input$treatment_providers_service)) {
        df <-
          df %>%
          dplyr::mutate(
            service_flag = purrr::pmap_lgl(
              .l = list(.data$service_list),
              .f = function(service_list, selection) {
                n <- selection %in% service_list
                return(
                  sum(n) == length(n)
                )
              },
              selection = input$treatment_providers_service
            )
          ) %>%
          dplyr::filter(.data$service_flag)

        n <- nrow(df)
      }

      n_treatment_providers$value <- n
    })

    # ================================= #
    # ---- Update Date Input Range ----
    # ================================= #
    shiny::observeEvent(filtered_overdose_data, {

      date_range_min <-
        as.character(
          as.Date(
            min(filtered_overdose_data$data$date)
          )
        )

      date_range_max <-
        as.character(
          as.Date(
            max(filtered_overdose_data$data$date)
          )
        )


      shiny::updateDateRangeInput(
        inputId = "date_range",
        label = "Date Range",
        start = date_range_min,
        end = date_range_max,
        min = date_range_min,
        max = date_range_max
      )
    })
    # ================================ #
    # ---- Action Button Controls ----
    # ================================ #

    # Action button: toggle filter box
    shiny::observeEvent(input$collapse_filter_box, {

      shinydashboardPlus::updateBox(
        id = "filter_box",
        action = "toggle"
      )
    })

    # Action button: reset all filters
    shiny::observeEvent(input$reset_filters,{
      # clear spatial
      spatial_inputs <- c("zip", "agency", "location_type", "destination")
      for (spatial_input in spatial_inputs) {

        shiny::updateSelectizeInput(
          inputId = spatial_input,
          selected = ""
        )
      }


      # clear demographics
      demographic_inputs <- c("gender", "ethnicity")
      for (demographic_input in demographic_inputs) {

        shinyWidgets::updatePickerInput(
          inputId = demographic_input,
          session = session,
          selected = ""
        )
      }

      # clear temporal
      shiny::updateDateRangeInput(
        inputId = "date_range",
        min = "2008-01-01",
        max = "2021-07-17",
        start = "2008-01-01",
        end = "2021-07-17"
      )

    })

    # Action button: download filtered data
    output$download_filtered_data <- shiny::downloadHandler(
      filename = function() {
        paste("opioid_overdose_filtered_data", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        readr::write_csv(filtered_overdose_data$data, file)
      }
    )

    # =================== #
    # ---- Filtering ----
    # =================== #

    shiny::observeEvent(input$apply_filters, {

      # Re-initialize the data to make sure that
      # we are not shrinking the data everytime the users update the filters
      filtered_overdose_data$data <- od_data_all
      filtered_drug_crime_data$data <- drug_crime_data_all
      filtered_treatment_providers_data$data <- treatment_providers_data_all

      # Filters: Gender ====
      if (!nothing_selected(input$gender)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$sex %in% input$gender
          )
      }

      # Filters: Ethnicity ====
      if (!nothing_selected(input$ethnicity)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$race %in% input$ethnicity
          )
      }

      # Filters: Date Range ====
      if (!nothing_selected(input$date_range)) {
        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$date >= lubridate::ymd(input$date_range[1]),
            .data$date <= lubridate::ymd(input$date_range[2])
          )

        filtered_drug_crime_data$data <-
          filtered_drug_crime_data$data %>%
          dplyr::filter(
            .data$date >= lubridate::ymd(input$date_range[1]),
            .data$date <= lubridate::ymd(input$date_range[2])
          )
      }

      # Filters: Zip Code ====
      if (!nothing_selected(input$zip)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$zip %in% as.numeric(input$zip)
          )
      }

      # Filters: Agency ====
      if (!nothing_selected(input$agency)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$agency %in% input$agency
          )
      }

      # Filters: Location Type ====
      if (!nothing_selected(input$location_type)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$location_type %in% input$location_type
          )
      }

      # Filters: Destination ====
      if (!nothing_selected(input$destination)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$destination %in% input$destination
          )
      }

      # Filters: Treatment Providers Specification ====
      if (!nothing_selected(input$treatment_providers_spec)) {

        filtered_providers_id <-
          filtered_treatment_providers_data$data  %>%
          tidyr::pivot_longer(
            cols = as.character(opioidDashboard::filter_selection_treatment_providers_spec),
            names_to = "spec",
            values_to = "status"
          ) %>%
          dplyr::filter(
            .data$spec %in% input$treatment_providers_spec,
            .data$status == TRUE
          ) %>%
          dplyr::distinct(.data$id) %>%
          dplyr::pull(.data$id)

        filtered_treatment_providers_data$data <-
          filtered_treatment_providers_data$data %>%
          dplyr::filter(
            .data$id %in% filtered_providers_id
          )
      }

      if (!nothing_selected(input$treatment_providers_service)) {
        filtered_treatment_providers_data$data <-
          filtered_treatment_providers_data$data %>%
          dplyr::mutate(
            service_flag = purrr::pmap_lgl(
              .l = list(.data$service_list),
              .f = function(service_list, selection) {
                n <- selection %in% service_list
                return(
                  sum(n) == length(n)
                )
              },
              selection = input$treatment_providers_service
            )
          ) %>%
          dplyr::filter(.data$service_flag)
      }


      shinyalert::shinyalert(
        title = "Success",
        text = "All of your filters have been applied",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        animation = TRUE
      )

    })


    # ============================== #
    # ---- Time Series Viz Deck ----
    # ============================== #

    output$ts_box <- apexcharter::renderApexchart({

      color <- "#2E93fA"
      background <- "#FFF"

      od_data <- filtered_overdose_data$data
      drug_crime_data <- filtered_drug_crime_data$data

      overdose_daily_data <-
        od_data %>%
        dplyr::mutate(
          date = lubridate::as_date(lubridate::ymd_hms(.data$date))
        ) %>%
        dplyr::group_by(.data$date) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(
          type = "Opioid Overdose Daily Cases"
        )

      overdose_monthly_data <-
        od_data %>%
        dplyr::mutate(
          yearmonth = paste0(lubridate::year(.data$date), "/", lubridate::month(.data$date))
        ) %>%
        dplyr::group_by(.data$yearmonth) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::transmute(
          date = lubridate::ym(.data$yearmonth),
          n = .data$n
          #n = .data$n/30
        ) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(
          type = "Opioid Overdose Monthly Cases"
        )

      # drug_crime_monthly_data <-
      #   drug_crime_data %>%
      #   dplyr::mutate(
      #     yearmonth = paste0(lubridate::year(.data$date), "/", lubridate::month(.data$date))
      #   ) %>%
      #   dplyr::group_by(.data$yearmonth) %>%
      #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      #   dplyr::transmute(
      #     date = lubridate::ym(.data$yearmonth),
      #     n = .data$n/30
      #   ) %>%
      #   dplyr::arrange(.data$date) %>%
      #   dplyr::mutate(
      #     type = "Drug Crime Monthly Average Cases"
      #   )

      plot_data <-
        overdose_daily_data %>%
        dplyr::bind_rows(
          overdose_monthly_data
          #drug_crime_monthly_data
        ) %>%
        purrr::set_names(
          c("date", "Case count", "type")
        )



      spark <-
        plot_data %>%
        apexcharter::apex(type = "area-spline",
                          apexcharter::aes(x = .data$date,
                                           y = .data[["Case count"]],
                                           group = .data$type),
                          auto_update = FALSE) %>%
        apexcharter::ax_colors(c("#fee0d2", "#de2d26", "#756bb1")) %>%
        apexcharter::ax_title(
          text = "Opioid Overdose Case Time Series",
          align = "left",
          style = list(fontSize = "22px", fontWeight = 700)
        ) %>%
        apexcharter::ax_subtitle(
          text = "",
          align = "left"
        ) %>%
        apexcharter::ax_xaxis(
          show = TRUE,
          tooltip = list(
            enabled = TRUE
          )
        ) %>%
        apexcharter::ax_yaxis(
          decimalsInFloat = 0,
          show = TRUE,
          title = list(text = "Daily Cases"),
          axisBorder = list(
            show = TRUE,
            color = "#feb24c"
          ),
          labels = list(
            formatter = apexcharter::format_num(".0f"),
            style = list(
              colors = "#feb24c"
            )
          ),
          tooltip = list(
            enabled = TRUE
          )
        ) %>%
        apexcharter::ax_yaxis2(
          decimalsInFloat = 0,
          opposite = TRUE,
          show = TRUE,
          title = list(text = "Monthly Cases"),
          forceNiceScale = TRUE,
          axisBorder = list(
            show = TRUE,
            color = "#de2d26"
          ),
          labels = list(
            formatter = apexcharter::format_num(".0f"),
            style = list(
              colors = "#de2d26"
            )
          ),
          tooltip = list(
            enabled = TRUE
          )
        ) %>%
        apexcharter::ax_grid(yaxis = list(lines = list(show = FALSE))) %>%
        apexcharter::ax_nodata(
          text = "No data found for this filter",
          fontSize = "30px"
        )

      spark$x$sparkbox <- list(
        color = color, background = background
      )
      spark$sizingPolicy <- htmlwidgets::sizingPolicy(
        defaultWidth = "100%",
        defaultHeight = "180px",
        viewer.defaultHeight = "180px",
        viewer.defaultWidth = "100%",
        viewer.fill = FALSE,
        knitr.figure = FALSE,
        knitr.defaultWidth = "100%",
        knitr.defaultHeight = "180px",
        browser.fill = FALSE,
        viewer.suppress = FALSE,
        browser.external = TRUE,
        padding = 15
      )

      return(spark)
    })

    # ====================== #
    # ---- Overdose Map ----
    # ====================== #
    opioidOverdoseMapServer(
      "od_map",
      filtered_overdose_data,
      filtered_drug_crime_data,
      filtered_treatment_providers_data
    )

    return(
      filtered_overdose_data
    )

  })
}
