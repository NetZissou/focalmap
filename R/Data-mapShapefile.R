
#' Load Ohio school district shapefiles
#'
#' @return sf object
#' @export
#'
#' @examples
#' get_ohio_school_district_sf()
get_ohio_school_district_sf <- function() {

  data <-
    sf::read_sf(opioidDashboard::OHIO_SCHOOL_DISTRICT_SF_PATH) %>%
    purrr::set_names(
      c(
        "NAME", "district_id", "old_district_id",
        "center_long", "center_lat", "school_district",
        "district_pop", "district_school_pop",
        "district_poverty_children_pop", "geometry"
      )
    )
  return(
    data
  )
}


#' Load Franklin county school district shapefiles
#'
#' @return sf object
#' @export
#'
#' @examples
#' get_franklin_county_school_district_sf()
get_franklin_county_school_district_sf <- function() {

  data <-
    get_ohio_school_district_sf() %>%
    dplyr::filter(.data$NAME %in% opioidDashboard::franklin_county_school_districts)
}

#' Load fire district shapefiles
#'
#' @return sf object
#' @export
#'
#' @examples
#' get_fire_district_sf()
get_fire_district_sf <- function() {

  path = paste0("/fs/ess/PDE0001/geographies/", "fire_districts")

  data <- sf::read_sf(path)
  return(
    data
  )
}

#' Load census tract shapefiles
#'
#' @return sf object
#' @export
#'
#' @examples
#' get_census_tract_sf()
get_census_tract_sf <- function() {

  path = paste0("/fs/ess/PDE0001/geographies/", "tracts")

  data <- sf::read_sf(path)
  return(
    data
  )
}

#' Load zip code level shapefiles
#'
#' @return sf object
#' @export
#'
#' @examples
#' get_zipcode_sf()
get_zipcode_sf <- function() {

  path = paste0("/fs/ess/PDE0001/geographies/", "zipcodes")

  data <- sf::read_sf(path)
  return(
    data
  )
}


#' Load EMS shapefile
#'
#' @return sf object
#' @export
get_EMS_sf <- function() {

  path = paste0("/fs/ess/PDE0001/geographies/", "EMS")

  data <- sf::read_sf(path)
  return(
    data
  )
}

#' Load response area shapefiles
#'
#' @return sf object
#' @export
#'
#' @examples
#' get_response_area_sf()
get_response_area_sf <- function() {

  path = paste0("/fs/ess/PDE0001/geographies/", "response_area")

  data <- sf::read_sf(path)
  return(
    data
  )
}

#' Load naloxone box shapefiles
#'
#' @return sf object
#' @export
#'
get_naloxone_box_sf <- function() {
  # TODO: Fix this path
  path <- paste0("/fs/ess/PDE0001/naloxone box location/naloxone_box_shapefiles")

  data <- sf::read_sf(path)
  return(
    data
  )
}





