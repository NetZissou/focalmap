app_dev <- function(run_check = FALSE) {

  if (run_check) {
    devtools::check()
    app()
  } else {
    devtools::load_all()
    app()
  }
}

#' @importFrom rlang ".data"
check_na <- function(tbl, missing_only = TRUE) {
  missing_result <-
    tbl %>%
    dplyr::summarise_all(
      .funs = list(
        ~mean(is.na(.))
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "var",
      values_to = "missing_prop"
    )
  if (missing_only) {
    missing_result <- missing_result %>%
      dplyr::filter(.data$missing_prop > 0)
  }
  return(missing_result %>% dplyr::arrange(-.data$missing_prop))
}

check_na_io <- function(tbl, missing_only = TRUE) {
  missing_info <- check_na(tbl, missing_only = missing_only)

  message("Data missing info: ")
  if (nrow(missing_info) == 0) {
    cat("No missing values detected\n")
  } else {
    print(missing_info)
  }
}

nothing_selected <- function(x) {
  return(
    length(is.na(x)) == 0
  )
}

#' Load Map Shapefile Function
#' @param file sf file path
#' @param crs crs code default to be 4326
load_map_shapefile <- function(file, crs = 4326) {

  if (crs != 4326) {
    stop("'crs' must be 4326 at this moment.")
  }

  map_data <- sf::read_sf(file, quiet = TRUE)
  map_data <- sf::st_transform(map_data, crs = 4326)

  return(map_data)
}
