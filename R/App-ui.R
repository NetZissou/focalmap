#' User Interface for opioid application
#'
#' @return shiny ui
#' @export
ui <- function() {
  # snapper::load_snapper(),
  shiny::bootstrapPage(

    snapper::load_snapper(),
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useShinydashboardPlus(),

    theme = bslib::bs_theme(bootswatch = "materia", font_scale = 1.1),
    shiny::navbarPage(
      title = "FOCAL MAP SECURED",
      selected = "Opioid Overdose Map", id = "navbar",

      #theme = bslib::bs_theme(bootswatch = "materia", font_scale = 1.1),

      # ================================== #
      # ---- Tab: Opioid Overdose Map ----
      # ================================== #
      shiny::tabPanel(title = "Opioid Overdose Map",

                      shiny::fluidRow(

                        # column
                      ),

                      opioidOverdoseFiltersUI("overdose_filters"),
                      #opioidOverdoseMapUI("opioid_overdose_map")

      ),

      # =========================== #
      # ---- Tab: Project DAWN ----
      # =========================== #
      # shiny::tabPanel(title = "Project DAWN (Previous)",
      #                 shiny::fluidRow(
      #                   naloxoneDistributionSeriesUI("kit_ts")
      #                 ),
      #                 shiny::fluidRow(
      #                   naloxoneDemographicsUI("demo")
      #                 )
      # ),

      shiny::tabPanel(title = "Project DAWN",
                      shiny::fluidRow(
                        projectDAWNFilterUI("naloxone_filter")
                      ),
                      shiny::fluidRow(
                        projectDAWNTimeSeriesUI("naloxone_ts")
                      )
      ),

      # ================================= #
      # ---- Tab: Hot Spot Detection ----
      # ================================= #
      shiny::tabPanel(title = "Hot Spot Detection",
                      hotSpotDetectionUI("hot_spot")
      ),

      # ======================================== #
      # ---- Tab: Opioid Overdose Case Rate ----
      # ======================================== #
      shiny::tabPanel(title = "Case Rate Map",
                      opioidOverdoseRateUI("od_case_rate")
      )


    )
  )
}

