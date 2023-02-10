where <- tidyselect:::where

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



#' Check the updated time of files in a specified folder
#'
#' @param dir file directory
#' @param regexp 	A regular expression to filter paths
#'
#' @return return the a tibble with the first column `path` contains all of the
#' matched files path and second column `modification_time` contains the modification time of the file
#' @export
#' @importFrom rlang ".data"
#' @importFrom utils "head"
check_files_update_time <- function(
  dir,
  regexp
) {
  fs::dir_info(
    dir,
    regexp = regexp
  ) %>%
    dplyr::select(
      .data$path, .data$modification_time
    ) %>%
    dplyr::arrange(dplyr::desc(.data$modification_time))
}






#' Geolocate a point among the provided sf objects
#'
#' @param lat lat
#' @param lng lng
#' @param sf a sf tibble that contains many sf objects
#' @param id the identifier column of the sf object
#'
#' @return the name of the object that the point falls into; return NA if none
#' @export
geocoding_sf <- function(lat, lng, sf, id = "name") {

  point <- sf::st_point(x = c(lng, lat), dim = "XYZ")

  map_function <- purrr::pmap_lgl
  # if (parallel) {
  #   map_function <- furrr::future_pmap_lgl
  # }

  result <-
    sf %>%
    dplyr::mutate(
      result = map_function(
        .l = list(.data$geometry),
        .f = function(geom, point) {
          result <-
            sf::st_intersects(
              geom,
              point
            )[[1]]

          if (length(result) == 0) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        },
        point = point
      )
    ) %>%
    dplyr::filter(.data$result) %>%
    utils::head(1) %>% # Only keep one if there are multiple result
    dplyr::pull(.data[[id]])

  if (length(result) == 0) {
    result <- NA_character_
  }
  return(result)
}


parse_month <- function(file_path) {

  file_path_contain_month <- stringr::str_detect(
    file_path,
    "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
  )

  if (file_path_contain_month) {

    file_path <-
      dplyr::case_when(
        stringr::str_detect(file_path, " Jan") ~ stringr::str_replace(file_path, " Jan", "_01"),
        stringr::str_detect(file_path, " Feb") ~ stringr::str_replace(file_path, " Feb", "_02"),
        stringr::str_detect(file_path, " Mar") ~ stringr::str_replace(file_path, " Mar", "_03"),
        stringr::str_detect(file_path, " Apr") ~ stringr::str_replace(file_path, " Apr", "_04"),
        stringr::str_detect(file_path, " May") ~ stringr::str_replace(file_path, " May", "_05"),
        stringr::str_detect(file_path, " Jun") ~ stringr::str_replace(file_path, " Jun", "_06"),
        stringr::str_detect(file_path, " Jul") ~ stringr::str_replace(file_path, " Jul", "_07"),
        stringr::str_detect(file_path, " Aug") ~ stringr::str_replace(file_path, " Aug", "_08"),
        stringr::str_detect(file_path, " Sep") ~ stringr::str_replace(file_path, " Sep", "_09"),
        stringr::str_detect(file_path, " Oct") ~ stringr::str_replace(file_path, " Oct", "_10"),
        stringr::str_detect(file_path, " Nov") ~ stringr::str_replace(file_path, " Nov", "_11"),
        stringr::str_detect(file_path, " Dec") ~ stringr::str_replace(file_path, " Dec", "_12")
      )

  }

  return(file_path)
}

# make_icon <- function(color) {
#
#   leaflet::awesomeIcons(
#     icon = "circle",
#     library = "fa",
#     markerColor = color,
#     iconColor = "white"
#   )
# }

