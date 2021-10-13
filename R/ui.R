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
    shinyalert::useShinyalert(),

    theme = bslib::bs_theme(bootswatch = "materia", font_scale = 1.1),
    shiny::navbarPage(
      title = "FOCAL MAP SECURED",
      selected = "Opioid Overdose Map", id = "navbar",

      #theme = bslib::bs_theme(bootswatch = "materia", font_scale = 1.1),

      # ========================================================= #
      # ------------ Tab: Naloxone Intake Forms -----------------
      # ========================================================= #
      shiny::tabPanel(title = "Opioid Overdose Map",

                      shiny::fluidRow(

                        # column
                      ),

                      opioidOverdoseFiltersUI("overdose_filters"),
                      #opioidOverdoseMapUI("opioid_overdose_map")

      ),

      # ========================================================= #
      # ------------ Tab: Naloxone Intake Forms -----------------
      # ========================================================= #
      shiny::tabPanel(title = "Naloxone Intake Forms",
                      shiny::fluidRow(
                        naloxoneDistributionSeriesUI("kit_ts")
                      ),
                      shiny::fluidRow(
                        naloxoneDemographicsUI("demo")
                      )
      )


    )
  )
}

