#' Load focused indicators  extracted from naloxone intake form
#'
#' @param dir data directory on OSC that contains all of the naloxone intake forms data
#'
#' @return focused naloxone intake forms indicators
#' @export
#'
#' @examples
#' project_dawn_focused_indicators()
project_dawn_focused_indicators <- function(dir = opioidDashboard::NALOXONE_DATA_DIRECTORY) {
  data_name <- "project_dawn_focused_indicators.RData"
  data_path <- paste0(dir, "/", data_name)
  data <- load(data_path)
  return(
    get(data) %>%
      tibble::as_tibble()
  )
}
#' Load personal data extracted from naloxone intake form
#'
#' @param dir data directory on OSC that contains all of the naloxone intake forms data
#'
#' @return personal data
#' @export
#'
#' @examples
#' personal_data()
personal_data <- function(dir = opioidDashboard::NALOXONE_DATA_DIRECTORY) {
  data_name <- "personal_data.RData"
  data_path <- paste0(dir, "/", data_name)
  data <- load(data_path)
  return(
    get(data) %>%
      tibble::as_tibble()
  )
}


#' Load kit data extracted from naloxone intake form
#'
#' @param dir data directory on OSC that contains all of the naloxone intake forms data
#'
#' @return kit data
#' @export
#'
#' @examples
#' kit_data()
kit_data <- function(dir = opioidDashboard::NALOXONE_DATA_DIRECTORY) {
  data_name <- "kit_data.RData"
  data_path <- paste0(dir, "/", data_name)
  data <- load(data_path)
  return(
    get(data) %>%
      tibble::as_tibble()
  )
}


#' Load naloxone usage data extracted from naloxone intake form
#'
#' @param dir data directory on OSC that contains all of the naloxone intake forms data
#'
#' @return naloxone usage data
#' @export
#'
#' @examples
#' naloxone_usage_data()
naloxone_usage_data <- function(dir = opioidDashboard::NALOXONE_DATA_DIRECTORY) {
  data_name <- "naloxone_usage_data.RData"
  data_path <- paste0(dir, "/", data_name)
  data <- load(data_path)
  return(
    get(data) %>%
      tibble::as_tibble()
  )
}


#' Load program data extracted from naloxone intake form
#'
#' @param dir data directory on OSC that contains all of the naloxone intake forms data
#'
#' @return program data
#' @export
#'
#' @examples
#' program_data()
program_data <- function(dir = opioidDashboard::NALOXONE_DATA_DIRECTORY) {
  data_name <- "naloxone_usage_data.RData"
  data_path <- paste0(dir, "/", data_name)
  data <- load(data_path)
  return(
    get(data) %>%
      tibble::as_tibble()
  )
}
