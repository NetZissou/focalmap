#' User Interface for opioid application
#'
#' @return shiny ui
#' @export
ui <- function() {
  shiny::navbarPage(
    title = "Opioid Tool",
    selected = "Naloxone Intake Forms", id = "navbar",

    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),

    theme = bslib::bs_theme() %>%
      bslib::bs_theme_update(bootswatch = "yeti"),

    # ========================================================= #
    # ------------ Tab: Naloxone Intake Forms -----------------
    # ========================================================= #
    shiny::tabPanel(title = "Naloxone Intake Forms",
                    shiny::fluidRow(
                      kit_distribution_ts_ui("kit_ts")
                    ),
                    shiny::fluidRow(
                      demographicsUI("demo")
                    )
    ),
    # ========================================================= #
    # ------------ Tab: Naloxone Intake Forms -----------------
    # ========================================================= #
    shiny::tabPanel(title = "Overdose Map",

    )


  )
}

