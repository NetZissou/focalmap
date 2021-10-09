
#' Load Ohio school district shapefiles
#'
#' @return sf object
#' @export
#'
#' @examples
#' get_ohio_school_district_sf()
get_ohio_school_district_sf <- function() {

  data <- load(opioidDashboard::OHIO_SCHOOL_DISTRICT_SF_PATH)
  return(
    get(data)
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

  data <- load(opioidDashboard::FRANKLIN_COUNTY_SCHOOL_DISTRICT_SF_PATH)
  return(
    get(data)
  )
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






