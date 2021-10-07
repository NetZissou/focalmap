
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
#' get_ohio_school_district_sf()
get_franklin_county_school_district_sf <- function() {

  data <- load(opioidDashboard::FRANKLIN_COUNTY_SCHOOL_DISTRICT_SF_PATH)
  return(
    get(data)
  )
}
