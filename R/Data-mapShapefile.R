
#' Load Ohio school district shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
#' @examples
#' get_ohio_school_district_sf()
get_ohio_school_district_sf <- function(parquet = TRUE) {

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_oh_school_district.parquet"
        )
      )
  } else {
    data <-
      sf::read_sf(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_oh_school_district.geojson"
        )
      )
      # purrr::set_names(
      #   c(
      #     "NAME", "district_id", "old_district_id",
      #     "center_long", "center_lat", "school_district",
      #     "district_pop", "district_school_pop",
      #     "district_poverty_children_pop", "geometry"
      #   )
      # )
  }
  return(
    data
  )
}



#' Load Franklin county school district shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
#' @examples
#' get_franklin_county_school_district_sf()
get_franklin_county_school_district_sf <- function(parquet = TRUE) {

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
      fs::path(
        opioidDashboard::ROOT_PATH,
        "other", "Shapefile",
        "sf_fc_school_district.parquet"
      )
    )
  } else {
    data <-
      sf::st_read(
      fs::path(
        opioidDashboard::ROOT_PATH,
        "other", "Shapefile",
        "sf_fc_school_district.geojson"
      )
    )
  }

  return(data)
}




#' Load fire district shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
#' @examples
#' get_fire_district_sf()
get_fire_district_sf <- function(parquet = TRUE) {

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_columbus_fire_districts.parquet"
        )
      )
  } else {
    data <-
      sf::st_read(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_columbus_fire_districts.geojson"
        )
      )
  }

  return(
    data
  )
}

#' Load census tract shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
#' @examples
#' get_census_tract_sf()
get_census_tract_sf <- function(parquet = TRUE) {

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_columbus_census_tract.parquet"
        )
      )
  } else {
    data <-
      sf::st_read(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_columbus_census_tract.geojson"
        )
      )
  }
  return(
    data
  )
}

#' Load zip code level shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
#' @examples
#' get_zipcode_sf()
get_zipcode_sf <- function(parquet = TRUE) {

  # path = paste0("/fs/ess/PDE0001/geographies/", "zipcodes")
  #
  # data <- sf::read_sf(path)

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_columbus_zip.parquet"
        )
      )
  } else {
    data <-
      sf::st_read(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_columbus_zip.geojson"
        )
      )
  }

  return(
    data
  )
}


#' Load EMS shapefile
#' @param parquet parquet format
#' @return sf object
#' @export
get_EMS_sf <- function(parquet = TRUE) {

  # path = paste0("/fs/ess/PDE0001/geographies/", "EMS")
  #
  # data <- sf::read_sf(path)

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_EMS.parquet"
        )
      )
  } else {
    data <-
      sf::st_read(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "sf_EMS.geojson"
        )
      )
  }

  return(
    data
  )
}


#' Load naloxone box shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
get_naloxone_box_sf <- function(parquet = TRUE) {
  # TODO: Fix this path
  path <- paste0("/fs/ess/PDE0001/naloxone box location/naloxone_box_shapefiles")

  data <- sf::read_sf(path)
  return(
    data
  )
}

#' Load COTA bus lines shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
get_cota_bus_lines_sf <- function(parquet = TRUE) {

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "cota_lines_2022_09.parquet"
        )
      )
  } else {
    data <-
      sf::st_read(
      fs::path(
        opioidDashboard::ROOT_PATH,
        "other", "COTA",
        "cota_lines_2022_09.shp"
      )
    ) %>%
      sf::st_transform(crs = 4326)
  }
  return(
    data
  )
}


#' Load COTA bus stops shapefiles
#' @param parquet parquet format
#' @return sf object
#' @export
#'
get_cota_bus_stops_sf <- function(parquet = TRUE) {

  if (parquet) {
    data <-
      sfarrow::st_read_parquet(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "Shapefile",
          "cota_stops_2022_09.parquet"
        )
      )
  } else {
    data <-
      sf::st_read(
        fs::path(
          opioidDashboard::ROOT_PATH,
          "other", "COTA",
          "cota_stops_2022_09"
        )
      ) %>%
      sf::st_transform(crs = 4326)
  }

  return(
    data
  )
}


#' Load Jurisdictions
#' @param parquet parquet format
#' @return sf object
#' @export
#'
get_jurisdictions_sf <-
  function(parquet = TRUE) {

    if (parquet) {
      data <-
        sfarrow::st_read_parquet(
          fs::path(
            opioidDashboard::ROOT_PATH,
            "other", "Shapefile",
            "sf_jurisdictions.parquet"
          )
        )
    } else {
      data <-
        sf::st_read(
          fs::path(
            opioidDashboard::ROOT_PATH,
            "other", "Shapefile",
            "sf_jurisdictions.geojson"
          )
        )
    }

    return(data)

  }


#' Load City, Village, and Township outlines
#' @param parquet parquet format
#' @return sf object
#' @export
#'
get_city_sf <-
  function(parquet = TRUE) {

    if (parquet) {
      data <-
        sfarrow::st_read_parquet(
          fs::path(
            opioidDashboard::ROOT_PATH,
            "other", "Shapefile",
            "sf_city_village_township.parquet"
          )
        )
    } else {
      data <-
        sf::st_read(
          fs::path(
            opioidDashboard::ROOT_PATH,
            "other", "Shapefile",
            "sf_city_village_township.geojson"
          )
        )
    }

    return(data)
  }


