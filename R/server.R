#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {
  # bslib::bs_themer()
  personal_data <- opioidAction::personal_data()
  kit_data <- opioidAction::kit_data()
  naloxone_usage_data <- opioidAction::naloxone_usage_data()
  program_data <- opioidAction::program_data()

  kit_join_personal <-
    kit_data %>%
    dplyr::left_join(
      personal_data,
      by = "obs_key"
    )

  kit_distribution_ts_server("kit_ts", kit_join_personal = kit_join_personal)
  demographicsServer("demographics")
}
