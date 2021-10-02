#' User Interface for opioid application
#'
#' @return shiny ui
#' @export
ui <- function() {
  shiny::navbarPage(
    title = "FOCAL MAP SECURED",
    selected = "Overdose Map", id = "navbar",

    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useShinydashboardPlus(),
    shinyalert::useShinyalert(),

    theme = bslib::bs_theme(bootswatch = "materia", font_scale = 1.1),

    # ========================================================= #
    # ------------ Tab: Naloxone Intake Forms -----------------
    # ========================================================= #
    shiny::tabPanel(title = "Overdose Map",

                    overdoseFiltersUI("overdose_filters")

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
}

