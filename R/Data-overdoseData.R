#' Load opioid overdose primary table
#' @param parquet parquet format
#' @return opioid overdose data in primary table format
#' @export
data_opioid_overdose <- function(parquet = FALSE) {

  # ========================== #
  # ---- Latest file path ----
  # ========================== #
  file_format <- "csv"
  if (parquet) {
    file_format <- "parquet"
  }

  latest_data_info <-
    check_files_update_time(
      dir = fs::path(opioidDashboard::ROOT_PATH, "primary"),
      regexp = paste0("primary_table.", file_format)
    ) %>%
    head(1)

  latest_data_path <- latest_data_info$path
  latest_data_modification_time <- latest_data_info$modification_time

  # =================== #
  # ---- Load file ----
  # =================== #
  message("Loading file: ", latest_data_path)
  message("Modified at: ", latest_data_modification_time)


  if (parquet) {
    data <-
      arrow::read_parquet(
        latest_data_path,
        as_data_frame = FALSE
      )
  } else {
    data <-
      vroom::vroom(
        latest_data_path,
        col_types = opioidDashboard::schema_primary
      )
  }

  return(data)
}
