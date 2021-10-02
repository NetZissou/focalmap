#' User Interface for opioid application
#'
#' @return shiny ui
#' @export
ui <- function() {
  shiny::navbarPage(
    title = "Opioid Tool",
    selected = "Overdose Map", id = "navbar",

    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useShinydashboardPlus(),
    shinyalert::useShinyalert(),

    theme = bslib::bs_theme() %>%
      bslib::bs_theme_update(bootswatch = "yeti"),

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

