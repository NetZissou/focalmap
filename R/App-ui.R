#' User Interface for opioid application
#'
#' @return shiny ui
#' @export
ui <- function() {
  shiny::bootstrapPage(

    snapper::load_snapper(),
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    #shinyWidgets::useShinydashboardPlus(),


    theme = bslib::bs_theme(bootswatch = "materia", font_scale = 1.1),
    shiny::navbarPage(
      title = "FOCAL MAP SECURED",
      selected = "tab_data_filters", id = "navbar",

      #theme = bslib::bs_theme(bootswatch = "materia", font_scale = 1.1),

      # ================================== #
      # ---- Tab: Opioid Overdose Map ----
      # ================================== #
      shiny::tabPanel(value = "tab_data_filters",
                      title = "Data Filters",

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

      shiny::tabPanel(value = "tab_project_dawn",
                      title = "Project DAWN",
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
      shiny::tabPanel(value = "tab_hot_spot_detection",
                      title = "Hot Spot Detection",
                      hotSpotDetectionUI("hot_spot")
      ),

      # ======================================== #
      # ---- Tab: Opioid Overdose Case Rate ----
      # ======================================== #
      shiny::tabPanel(value = "tab_case_rate_map",
                      title = "Case Rate Map",
                      opioidOverdoseRateUI("od_case_rate")
      ),

      # =================== #
      # ---- Tab: FCPH ----
      # =================== #

      shiny::tabPanel(value = "tab_fcph",
                      title = "FCPH",
                      fcphUI("fcph")
      ),

      # ============================================================= #
      # ---- Tab: Application Development Information (DEV ONLY) ----
      # ============================================================= #
      # shiny::tabPanel(value = "tab_dev",
      #                 title = "Dev",
      #                 devInfoUI("dev_info")
      # )

    )
  )
}

