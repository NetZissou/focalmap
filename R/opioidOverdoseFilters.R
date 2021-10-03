opioidOverdoseFiltersUI <- function(id) {

  shinydashboardPlus::box(
    title = "Filters", solidHeader = TRUE, width = 12,
    collapsed = FALSE, collapsible = TRUE,
    id = shiny::NS(id, "filter_box"),

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
          label = "Ethnicity",
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
          label = "Date Range",
          min = "2008-01-01",
          max = "2021-07-17"
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
      ),
      shiny::column(
        width = 2,
        shiny::downloadButton(shiny::NS(id, "download_filtered_data"), label = "Store filtered data")
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

    return(
      filtered_overdose_data
    )

  })
}
