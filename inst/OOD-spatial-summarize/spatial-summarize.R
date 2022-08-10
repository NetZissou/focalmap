library(tidyverse)
library(sf)
library(lubridate)
library(furrr)
library(progressr)
library(devtools)
load_all()
future::plan(multisession)

# ========================== #
# ---- Assist Functions ----
# ========================== #
#' locate the centroid in a sf object
#'
#' @param sf a sf object that contains multiple geometry objects
#' @param centroid a sf point object
#'
#' @return the name of where this centroid falls into
locate_centroid <- function(sf, centroid) {
  result <-
    sf %>%
    dplyr::mutate(
      result = furrr::future_pmap_lgl(
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
        point = centroid
      )
    ) %>%
    dplyr::filter(.data$result) %>%
    head(1) %>% # Only keep one if there are multiple result
    dplyr::pull(.data$name)
  if (length(result) == 0) {
    result <- NA
  }
  return(result)
}

#' This is a wrap-up function for `locate_centroid` to safely handle error and
#' still proceed the program when error happens
locate_centroid_safely <- purrr::safely(
  .f = locate_centroid,
  otherwise = NULL
)

#' locate the centroid in multiple sf objects
#'
#' @param centroid a sf point object
#' @param sf_list a list of sf object
#' @param p progress object to track progress, see `progressr` for more details
#'
#' @return a data frame with one row and `length(sf_list)` number of columns
locate_centroid_sf_list <- function(centroid, sf_list, p) {
  p()

  tibble::tibble(
    name = c("zip", "census_tract", "school_district", "EMS"),
    value = furrr::future_pmap_chr(
      .l = list(sf_list),
      .f = function(sf, centroid) {
        locate_centroid_safely(sf, centroid)$result
      },
      centroid = centroid
    )
  ) %>%
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    )
}

# ====================== #
# ---- Data Loading ----
# ====================== #


opioid_overdose_data <-
  opioidDashboard::opioid_overdose_data(processed = FALSE) %>%
  dplyr::arrange(date)

sf_list <-
  list(
    # TODO: EMS jurisdiction
    #county = tibble::as_tibble(CEdata::sf_ohio_county) %>% dplyr::transmute(name = County, geometry),
    zip = tibble::as_tibble(CEdata::sf_ohio_zip) %>% dplyr::transmute(name = ZCTA5CE10, geometry),
    census_tract = tibble::as_tibble(CEdata::sf_ohio_census_tract) %>% dplyr::transmute(name = GEOID, geometry),
    school_district = tibble::as_tibble(CEdata::sf_ohio_school_district) %>% dplyr::transmute(name = NAME, geometry),
    EMS = tibble::as_tibble(CEdata::sf_EMS) %>% dplyr::transmute(name = DEPARTMENT, geometry)
  )

#  Generate sf point using lat & lng
data_point_level <-
  opioid_overdose_data %>%
  dplyr::transmute(
    id = id,
    date = date,
    centroid = furrr::future_pmap(
      .l = list(.data$lng, .data$lat),
      .f = function(lng, lat) {
        sf::st_point(x = c(lng, lat))
      }
    )
  )

#sample <- data_point_level %>% head(30)

# ================================================ #
# ---- Step: Split the entire dataset by year ----
# ================================================ #
data_point_level_list <-
  data_point_level %>%
  dplyr::mutate(
    year = lubridate::year(.data$date)
  ) %>%
  # dplyr::filter(
  #   !(.data$year %in% c(2022, 2021, 2019, 2014, 2011, 2008))
  # ) %>%
  dplyr::group_split(.data$year)
rm(data_point_level) # Remove this object to save space


# ========================================= #
# ---- Step: Process data year by year ----
# ========================================= #

furrr::future_walk(
  .x = data_point_level_list,
  .f = function(data_point_level) {

    data_year <- unique(data_point_level$year)
    filename <- paste0("/fs/ess/PDE0001/opioid overdose data/opioid_overdose_sf_", data_year, ".csv")

    start_time <- Sys.time()
    progressr::with_progress({
      p <- progressr::progressor(steps = length(data_point_level$centroid))

      result <-
        furrr::future_map_dfr(
          .x = data_point_level$centroid,
          .f = locate_centroid_sf_list,
          sf_list = sf_list,
          p = p
        ) %>%
        dplyr::mutate(
          id = data_point_level$id
        )
      readr::write_csv(
        result,
        file = filename
      )
    })
    finish_time <- Sys.time()
    total_time <- finish_time - start_time

    cat(paste0("Writing file: ", filename))
    cat(paste0("Total time cost: ", total_time))
  }
)

# ===================== #
# ---- Aggregating ----
# ===================== #


file_list <-
  fs::dir_info(path = "/fs/ess/PDE0001/opioid overdose data/", regexp = "opioid_overdose_sf_") %>%
  dplyr::pull(.data$path)

opioid_overdose_data_sf <-
  furrr::future_map_dfr(
    .x = file_list,
    .f = ~vroom::vroom(file = .x, show_col_types = FALSE)
  )

opioid_overdose_data <-
  opioid_overdose_data %>%
  dplyr::select(-.data$zip_sf, -.data$census_tract, -.data$school_district, -.data$EMS) %>%
  dplyr::left_join(
    opioid_overdose_data_sf %>% dplyr::rename(zip_sf = zip),
    by = "id"
  )

opioid_overdose_data %>%
  readr::write_csv("/fs/ess/PDE0001/opioid overdose data/primary_table_processed.csv")


