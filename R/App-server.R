#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {
  # bslib::bs_themer()

  od_data_all <- opioidDashboard::data_opioid_overdose()

  drug_crime_data_all <- opioidDashboard::data_drug_crime()

  project_DAWN_data <- projectDAWNFilterServer(
    "naloxone_filter",
    od_data_all = od_data_all
  )
  projectDAWNTimeSeriesServer(
    "naloxone_ts",
    project_DAWN_data = project_DAWN_data
  )

  # data type: reactiveValues{data}
  opioid_overdose_data_filtered <-
    opioidOverdoseFiltersServer(
      "overdose_filters",
      od_data_all,
      drug_crime_data_all
    )

  hotSpotDetectionServer(
    "hot_spot",
    filtered_overdose_data = opioid_overdose_data_filtered,
    od_data_all = od_data_all
  )
  #opioidOverdoseMapServer("opioid_overdose_map", opioid_overdose_data_filtered)

  opioidOverdoseRateServer(
    "od_case_rate",
    filtered_overdose_data = opioid_overdose_data_filtered,
    od_data_all = od_data_all
  )


  fcphSERVER(
    "fcph",
    filtered_overdose_data = opioid_overdose_data_filtered,
    od_data_all = od_data_all,
    drug_crime_data_all = drug_crime_data_all
  )

  devInfoServer("dev_info", parent_session = session)


}
