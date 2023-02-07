#' Geocoding a list of address using Nominatim, US Census.
#'
#' @param address_list a list of address that should be geocoded
#' @param threshold importance threshold to identify invalid geocoded observation
#' @param max_attempt max attempt to fetch the data from US census API
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding <- function(address_list, threshold = 0.5, max_attempt = 3) {

  # =================== #
  # ---- Nominatim ----
  # =================== #
  geocoding_result <-
    purrr::map_dfr(
      .x = address_list,
      .f = geocoding_nominatim,
      threshold = threshold
    ) %>%
    dplyr::mutate(
      index = dplyr::row_number(),
      # If the zip in the address does not align with
      # the zip from the Nominatim geocoding result
      zip_match = purrr::pmap_lgl(
        .l = list(.data$address, .data$zip_geocode),
        .f = function(address, zip_geocode) {

          zip_address <- stringr::str_extract(address, "\\d{5}")
          has_zip_geocode <- !is.na(zip_geocode)
          has_zip_address <- !is.na(zip_address)

          if (has_zip_geocode && has_zip_address) {
            return(zip_address == zip_geocode)
          } else {
            return(FALSE)
          }
        }
      ),
      status_geocode = ifelse(
        .data$zip_match,
        .data$status_geocode,
        FALSE
      )
    ) %>%
    dplyr::select(-.data$zip_match,) %>%
    dplyr::select(.data$index, dplyr::everything())

  status_geocode_all_valid <- all(geocoding_result$status_geocode)

  # ==================== #
  # ---- US Census -----
  # ==================== #
  MAX_ATTEMPT <- max_attempt
  census_API_attempt <- 0
  while (!status_geocode_all_valid && (census_API_attempt < MAX_ATTEMPT)) {

    geocoding_result_invalid <-
      geocoding_result %>%
      dplyr::filter(!.data$status_geocode)

    geocoding_result_rework <-
      geocoding_result_invalid %>%
      dplyr::select(.data$index) %>%
      dplyr::bind_cols(
        purrr::map_dfr(
          .x = geocoding_result_invalid$address,
          .f = geocoding_census
        )
      )

    geocoding_result <-
      geocoding_result %>%
      dplyr::filter(.data$status_geocode) %>%
      dplyr::bind_rows(geocoding_result_rework) %>%
      dplyr::arrange(.data$index)

    status_geocode_all_valid <- all(geocoding_result$status_geocode)
    census_API_attempt <- census_API_attempt + 1
  }

  # ============== #
  # ---- Tidy ----
  # ============== #

  geocoding_result <-
    geocoding_result %>%
    dplyr::arrange(.data$index) %>%
    dplyr::select(-.data$index) %>%
    dplyr::mutate(
      city = stringr::str_to_title(.data$city),
      county = stringr::str_to_title(stringr::str_remove(.data$city, " County")),
      contains_na = is.na(.data$lat) | is.na(.data$lng) |is.na(.data$city) | is.na(.data$county) | is.na(.data$zip_geocode),
      status_geocode = ifelse(.data$contains_na, FALSE, .data$status_geocode)
    ) %>%
    dplyr::select(-.data$contains_na)

  # ============= #
  # ---- I/O ----
  # ============= #
  geocoding_result_all_valid <- all(geocoding_result$status_geocode)

  if (!geocoding_result_all_valid) {
    geocoding_result_invalid <-
      geocoding_result %>%
      dplyr::filter(!.data$status_geocode)
    n_invalid <- nrow(geocoding_result_invalid)
    cat(crayon::red(
      glue::glue(
        "Failed to geolocate {n_invalid} addresses",
        n_invalid = n_invalid
      )
    ),sep = "\n")
  }

  return(geocoding_result)
}


#' Geocoding data using Nominatim
#'
#' @param address address is free-form but it is best form your address like {street}, {city}, {county}, {state}, {postalcode}
#' @param threshold importance threshold to identify invalid geocoded observation
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding_nominatim <- function(address, threshold = 0.5) {

  tryCatch(
    expr = {
      # ============================ #
      # ---- Format Request URL ----
      # ============================ #
      url <- "https://nominatim.osc.edu/"
      # ================================= #
      # ---- Get data from Nominatim ----
      # ================================= #

      result <-
        tibble::tibble(
          address = address,
          lat = NA_real_,
          lng = NA_real_,
          city = NA_character_,
          county = NA_character_,
          zip_geocode = NA_character_,
          status_geocode = FALSE,
          importance = NA_real_,
          service = "nominatim"
        )

      res <- httr::GET(
        url,
        query = list(
          q = address,
          addressdetails = 1,
          format = "json",
          limit = 1
        )
      )
      res_content <- httr::content(res)

      # ======================== #
      # ---- Error Handling ----
      # ======================== #
      contain_cross_street <- stringr::str_detect(address, "&|/")
      contain_seperators <- stringr::str_detect(address, ",")

      has_records <- (length(res_content) != 0)
      above_threshold <- (has_records && res_content[[1]]$importance >= threshold)
      status_geocode_valid <- has_records && above_threshold

      if (!status_geocode_valid & contain_cross_street) {

        # > Split Cross Street ---- #
        return(
          geocoding_nominatim_safely_cross_street(address = address, threshold = threshold)
        )
      } else if (!status_geocode_valid & contain_seperators) {

        # > Remove Seperators ---- #
        return(
          geocoding_nominatim_safely_remove_seperator(address = address, threshold = threshold)
        )
      } else if (!status_geocode_valid) {

        # > No geocoding result ---- #
        return(result)
      }


      # ==================== #
      # ---- Store Data ----
      # ==================== #
      best_match <- res_content[[1]]
      address_attr <- names(best_match$address)

      # > Geocoding Score ----
      result$importance <- best_match$importance
      # > Geocoding Status ----
      result$status_geocode <- status_geocode_valid
      # > Address: Lat & Lng ----
      result$lat <- as.double(best_match$lat)
      result$lng <- as.double(best_match$lon)

      # > Address: City ----
      if ("city" %in% address_attr) {
        result$city <- as.character(best_match$address$city)
      } else if ("town" %in% address_attr) {
        result$city <- as.character(best_match$address$town)
      }

      # > Address: County ----
      if ("county" %in% address_attr) {
        result$county <- as.character(best_match$address$county)
      }

      # > Address: Zipcode ----
      if ("postcode" %in% address_attr) {
        result$zip_geocode <- as.character(best_match$address$postcode)
      }
    },

    error = function(error) {
      cat(crayon::red(
        glue::glue("Failed geocoding address: {address}", address = address)
      ), sep = "\n")
      print(error)
    }
  )



  return(result)
}


#' Support function for `geocoding_nominatim` when there address failed to geocode but contains cross road
#'
#' @inheritParams geocoding_nominatim
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
geocoding_nominatim_safely_cross_street <- function(address, threshold) {

  # ============================================= #
  # ---- Split Cross Street into Two Address ----
  # ============================================= #
  address_split <-
    stringr::str_split(address, pattern = "&|/")[[1]] %>%
    stringr::str_trim(side = "both")

  street_1 <- address_split[1]
  street_2 <- address_split[2]


  address_street_1 <- stringr::str_c(c(street_1, address_split[-c(1,2)]), collapse  = " ")
  address_street_2 <- stringr::str_c(c(street_2, address_split[-c(1,2)]), collapse  = " ")

  #result_street_1 <- geocoding_nominatim(address_street_1)
  result_street_2 <- geocoding_nominatim(address_street_2)


  return(geocoding_nominatim(address = address, threshold = threshold))
  # result <-
  #   dplyr::bind_rows(
  #     result_street_1,
  #     result_street_2
  #   ) %>%
  #   dplyr::mutate(
  #     address = address
  #   )
  #
  # if (all(result$status_geocode)) {
  #   # Obtained result from both street
  #   if (result_street_1$importance > result_street_2$importance) {
  #     return(result[1,])
  #   } else {
  #     return(result[2,])
  #   }
  # } else if (!all(result$status_geocode)) {
  #   # Obtained no result from either street
  #   result <-
  #     tibble::tibble(
  #       address = address,
  #       lat = NA_real_,
  #       lng = NA_real_,
  #       city = NA_character_,
  #       county = NA_character_,
  #       zip_geocode = NA_character_,
  #       status_geocode = FALSE,
  #       importance = NA_real_,
  #       service = "nominatim"
  #     )
  #   return(result)
  # } else  {
  #   # One street has result
  #   return(
  #     result %>% dplyr::filter(.data$status_geocode)
  #   )
  # }
}


#' Support function for `geocoding_nominatim` when there address failed to geocode because of the seperators between attributes
#'
#' @inheritParams geocoding_nominatim
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
geocoding_nominatim_safely_remove_seperator <- function(address, threshold) {

  address_clean <- stringr::str_replace_all(address, ", ", " ")
  return(
    geocoding_nominatim(address = address_clean, threshold = threshold)
  )
}


#' Reverse geocoding with Nominatim
#'
#' @param lat lat
#' @param lng lng
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding_nominatim_reverse <- function(lat, lng) {

  # ============================ #
  # ---- Format Request URL ----
  # ============================ #
  url <- "https://nominatim.osc.edu/reverse"

  # ================================= #
  # ---- Get data from Nominatim ----
  # ================================= #

  result <-
    tibble::tibble(
      address = NA_character_,
      lat = lat,
      lng = lng,
      city = NA_character_,
      county = NA_character_,
      zip_geocode = NA_character_,
      status_geocode = FALSE,
      importance = NA_real_,
      service = "nominatim"
    )

  res <- httr::GET(
    url,
    query = list(
      lat = lat,
      lon = lng,
      addressdetails = 1,
      format = "json",
      limit = 1
    )
  )

  res_content <- httr::content(res)

  has_records <- (length(res_content) != 0)

  # ==================== #
  # ---- Store Data ----
  # ==================== #
  if (has_records) {

    address_attr <- names(res_content$address)

    # > Geocoding Score ----
    result$importance <- 1
    # > Geocoding Status
    result$status_geocode <- TRUE
    # > Address: Full Address
    result$address <- res_content$display_name

    # > Address: City
    if ("city" %in% address_attr) {
      result$city <- as.character(res_content$address$city)
    } else if ("town" %in% address_attr) {
      result$city <- as.character(res_content$address$town)
    } else if ("municipality" %in% address_attr) {
      result$city <- as.character(res_content$address$municipality)
    }
    # > Address: County ----
    if ("county" %in% address_attr) {
      result$county <- as.character(res_content$address$county)
    }
    # > Address: Zipcode ----
    if ("postcode" %in% address_attr) {
      result$zip_geocode <- as.character(res_content$address$postcode)
    }
  }

  return(result)
}


#' Geocoding data using United States Census
#'
#' @description documentation https://geocoding.geo.census.gov/geocoder/
#' @param address address is free-form but it is best form your address like {street}, {city}, {county}, {state}, {postalcode}
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding_census <- function(address) {

  url_base <- "https://geocoding.geo.census.gov/geocoder/{returntype}/{searchtype}"

  url_geo <- glue::glue(url_base, returntype = "geographies", searchtype = "onelineaddress")

  res <- httr::GET(
    url_geo,
    query = list(
      benchmark = "Public_AR_Current",
      vintage= "Current_Current",
      address = address,
      format = "json"
    )
  )

  res_content <- httr::content(res)

  result <-
    tibble::tibble(
      address = address,
      lat = NA_real_,
      lng = NA_real_,
      city = NA_character_,
      county = NA_character_,
      zip_geocode = NA_character_,
      status_geocode = FALSE,
      importance = NA_real_,
      service = "US Census"
    )

  request_valid <- (httr::status_code(res) == 200)
  has_records <- request_valid && (length(res_content$result$addressMatches) != 0)
  status_geocode_valid <- (request_valid && has_records)

  if (!status_geocode_valid) {
    return(result)
  }

  # ==================== #
  # ---- Store Data ----
  # ==================== #
  best_match <- res_content$result$addressMatches[[1]]
  address_attr <- names(best_match$addressComponents)

  # > Geocoding Score ----
  result$importance <- NA_real_ # US Census service has no importance score
  # > Geocoding Status ----
  result$status_geocode <- status_geocode_valid
  # > Address: Lat & Lng ----
  result$lat <- as.double(best_match$coordinates$y)
  result$lng <- as.double(best_match$coordinates$x)

  # > Address: City ----
  if ("city" %in% address_attr) {
    result$city <- as.character(best_match$addressComponents$city)
  }

  # > Address: County ----
  result$county <- as.character(best_match$geographies$Counties[[1]]$BASENAME)

  # > Address: Zipcode ----
  if ("zip" %in% address_attr) {
    result$zip_geocode <- as.character((best_match$addressComponents$zip))
  }

  # ============== #
  # ---- Tidy ----
  # ============== #
  result <-
    result %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~dplyr::na_if(., "")
      )
    )


  return(result)
}


#' Reverse geocoding with United States Census
#'
#' @param lat lat
#' @param lng lng
#'
#' @return raw request content
#' @export
geocoding_census_reverse <- function(lat, lng) {

  url_base <- "https://geocoding.geo.census.gov/geocoder/{returntype}/{searchtype}"

  url_geo <- glue::glue(url_base, returntype = "geographies", searchtype = "coordinates")

  res <- httr::GET(
    url_geo,
    query = list(
      benchmark = "Public_AR_Current",
      vintage= "Current_Current",
      x = lng,
      y = lat,
      format = "json"
    )
  )

  res_content <- httr::content(res)

  return(res_content)
}

