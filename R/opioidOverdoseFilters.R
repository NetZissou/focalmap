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
        shiny::fluidRow(

          shiny::column(
            width = 4,
            shiny::dateRangeInput(
              inputId = shiny::NS(id, "date_range"),
              label = "Date Range (Jan/1st/2008 - July/17th/2021)",
              min = "2008-01-01",
              max = "2021-07-17",
              start = "2008-01-01",
              end = "2021-07-17"
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
          apexcharter::apexchartOutput(outputId = shiny::NS(id, "od_ts_monthly"), height = "180px"),
          spin = "fading-circle"
        ),

        opioidOverdoseMapUI(shiny::NS(id, "od_map"))
      )
    )



  )
}

opioidOverdoseFiltersServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session){

    # Initialize source data
    opioid_overdose_data_all <- opioidDashboard::opioid_overdose_data()
    filtered_overdose_data <- shiny::reactiveValues(
      data = opioid_overdose_data_all
    )

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

    shiny::observeEvent(input$apply_filters, {

      filtered_overdose_data$data <- opioid_overdose_data_all


      if (!nothing_selected(input$gender)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$sex %in% input$gender
          )
      }

      if (!nothing_selected(input$ethnicity)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$race %in% input$ethnicity
          )
      }

      if (!nothing_selected(input$date_range)) {
        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$date >= lubridate::ymd(input$date_range[1]),
            .data$date <= lubridate::ymd(input$date_range[2])
          )
      }

      if (!nothing_selected(input$zip)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$zip %in% as.numeric(input$zip)
          )
      }

      if (!nothing_selected(input$agency)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$agency %in% input$agency
          )
      }

      if (!nothing_selected(input$location_type)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$location_type %in% input$location_type
          )
      }

      if (!nothing_selected(input$destination)) {

        filtered_overdose_data$data <-
          filtered_overdose_data$data %>%
          dplyr::filter(
            .data$destination %in% input$destination
          )
      }

      # print(filtered_overdose_data$data)

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

    output$od_ts_monthly <- apexcharter::renderApexchart({

      color <- "#2E93fA"
      background <- "#FFF"

      od_data <- filtered_overdose_data$data

      daily_data <-
        od_data %>%
        dplyr::mutate(
          date = lubridate::as_date(lubridate::ymd_hms(.data$date))
        ) %>%
        dplyr::group_by(.data$date) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(
          type = "Daily Case Count"
        )

      monthly_data <-
        od_data %>%
        dplyr::mutate(
          yearmonth = paste0(lubridate::year(.data$date), "/", lubridate::month(.data$date))
        ) %>%
        dplyr::group_by(.data$yearmonth) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::transmute(
          date = lubridate::ym(.data$yearmonth),
          n = .data$n/30
        ) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(
          type = "Monthly moving average"
        )

      plot_data <-
        daily_data %>%
        dplyr::bind_rows(
          monthly_data
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
        apexcharter::ax_colors(c("#fee0d2", "#de2d26")) %>%
        apexcharter::ax_title(
          text = "Opioid Overdose Case Time Series",
          align = "left",
          style = list(fontSize = "22px", fontWeight = 700)
        ) %>%
        apexcharter::ax_subtitle(
          text = "",
          align = "left"
        ) %>%
        apexcharter::ax_yaxis(
          decimalsInFloat = 0,
          labels = list(
            formatter = apexcharter::format_num(".2")
          )
        ) %>%
        apexcharter::ax_yaxis(
          show = FALSE
        ) %>%
        apexcharter::ax_grid(yaxis = list(lines = list(show = FALSE)))

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

    opioidOverdoseMapServer("od_map", filtered_overdose_data)

    return(
      filtered_overdose_data
    )

  })
}
