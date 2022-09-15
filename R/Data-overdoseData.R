#' Load opioid overdose primary table
#'
#' @return opioid overdose data in primary table format
#' @export
data_opioid_overdose <- function() {

  # ========================== #
  # ---- Latest file path ----
  # ========================== #

  latest_data_info <-
    check_files_update_time(
      dir = fs::path(opioidDashboard::ROOT_PATH, "primary"),
      regexp = "primary_table.csv"
    ) %>%
    head(1)

  latest_data_path <- latest_data_info$path
  latest_data_modification_time <- latest_data_info$modification_time

  # =================== #
  # ---- Load file ----
  # =================== #
  message("Loading file: ", latest_data_path)
  message("Modified at: ", latest_data_modification_time)


  data <-
    vroom::vroom(
      latest_data_path,
      col_types = opioidDashboard::schema_primary
    )

  return(data)
}
