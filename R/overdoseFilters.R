overdoseFiltersUI <- function(id) {

  shinydashboardPlus::box(
    title = "Filter", solidHeader = TRUE, width = 12,
    collapsed = FALSE, collapsible = TRUE,
    id = shiny::NS(id, "filter_box"),

    shiny::h4("Demographic"),
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shinyWidgets::pickerInput(
          inputId = shiny::NS(id, "gender"),
          label = "Gender",
          choices = c("", "Female", "Male")
        )
      ),
      shiny::column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = shiny::NS(id, "ethnicity"),
          label = "Ethnicity",
          multiple = TRUE,
          choices = c("", "White", "Black or African American", "Asian",
                      "Other"),
          options = list(`multiple-separator` = " | ")
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
        width = 4,
        # Spatial - zip code
        shiny::selectizeInput(
          inputId = shiny::NS(id, "zip"),
          label = "Zip code",
          multiple = TRUE,
          choices = opioidDashboard::filter_selection_zip
        )
      ),
      shiny::column(
        width = 4,
        # Spatial - agency
        shiny::selectizeInput(
          inputId = shiny::NS(id, "agency"),
          label = "Agency",
          multiple = TRUE,
          choices = opioidDashboard::filter_selection_agency
        )
      )
    ),

    shiny::fluidRow(

      shiny::column(
        width = 4,
        # Spatial - location type
        shiny::selectizeInput(
          inputId = shiny::NS(id, "location_type"),
          label = "Location Type",
          multiple = TRUE,
          choices = opioidDashboard::filter_selection_location_type
        )
      ),
      shiny::column(
        width = 4,
        # Spatial - destination
        shiny::selectizeInput(
          inputId = shiny::NS(id, "destination"),
          label = "Destination",
          multiple = TRUE,
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
        width = 3,
        shiny::actionButton(shiny::NS(id, "reset_filters"), label = "Reset filters")
      ),
      shiny::column(
        width = 3,
        shiny::actionButton(shiny::NS(id, "collapse_filter_box"), label = "Hide filters")
      )
    )

  )
}

overdoseFiltersServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session){

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

  })
}
