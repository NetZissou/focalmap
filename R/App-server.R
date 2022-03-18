#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {
  # bslib::bs_themer()
  personal_data <- personal_data()
  kit_data <- kit_data()
  naloxone_usage_data <- naloxone_usage_data()
  program_data <- program_data()

  kit_join_personal <-
    kit_data %>%
    dplyr::left_join(
      personal_data,
      by = "obs_key"
    )

  naloxoneDistributionSeriesServer("kit_ts", kit_join_personal = kit_join_personal)
  naloxoneDemographicsServer("demo", kit_join_personal = kit_join_personal)

  # data type: reactiveValues{data}
  opioid_overdose_data_filtered <-
    opioidOverdoseFiltersServer("overdose_filters")

  hotSpotDetectionServer("hot_spot", filtered_overdose_data = opioid_overdose_data_filtered)
  #opioidOverdoseMapServer("opioid_overdose_map", opioid_overdose_data_filtered)
}
