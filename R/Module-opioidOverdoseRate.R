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





  })
}
