#' Loading function for drug crime data
#'
#' @return drug crime data
#' @export
data_drug_crime <- function() {


  # ========================== #
  # ---- Latest file path ----
  # ========================== #

  latest_data_info <-
    check_files_update_time(
      dir = fs::path(
        opioidDashboard::ROOT_PATH,
        "other",
        "Drug Crime"
      ),
      regexp = "drug_crime.csv"
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
      latest_data_path
    )

  return(data)
}
