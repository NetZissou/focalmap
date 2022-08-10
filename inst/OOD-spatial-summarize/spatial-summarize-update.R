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

# primary_table_processed.csv
opioid_overdose_data_processed <-
  opioidDashboard::opioid_overdose_data(processed = TRUE) %>%
  dplyr::arrange(date)

# primary_table.csv
opioid_overdose_data_raw_updated <-
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

# Filter newly updated data
#  Generate sf point using lat & lng
new_OOD <-
  opioid_overdose_data_raw_updated %>%
  dplyr::filter(
    !.data$id %in% opioid_overdose_data_processed$id
  )

data_point_level <-
  new_OOD %>%
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


# =================================== #
# ---- Process newly updated OOD ----
# =================================== #
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
})

# Bind previous processed OOD with new processed records
opioid_overdose_data_processed %>%
  dplyr::mutate(census_tract = as.character(census_tract)) %>%
  dplyr::bind_rows(
    new_OOD %>%
      dplyr::left_join(
        result,
        by = "id"
      )
  ) %>%
  dplyr::select(-dplyr::all_of(c("zip.x", "zip.y"))) %>%
  readr::write_csv("/fs/ess/PDE0001/opioid overdose data/primary_table_processed.csv")





