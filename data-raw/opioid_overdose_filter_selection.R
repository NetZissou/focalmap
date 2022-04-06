## code to prepare `opioid_overdose_filter_selection` dataset goes here
library(dplyr)
library(tidyr)
library(purrr)
library(opioidDashboard)
library(scales)

od_data <-
  opioidDashboard::opioid_overdose_data()

# ================ #
# ---- agency ----
# ================ #
agency_tbl <-
  od_data %>%
  dplyr::count(.data$agency) %>%
  dplyr::arrange(-.data$n) %>%
  dplyr::mutate(
    pct = scales::percent(n/sum(n), accuracy = 0.01),
    selection = paste0(.data$agency, " - ", .data$pct)
  )

filter_selection_agency <-
  c(
    "",
    purrr::set_names(
      agency_tbl$agency,
      agency_tbl$selection
    )
  )

# ===================== #
# ---- destination ----
# ===================== #
destination_tbl <-
  od_data %>%
  dplyr::count(.data$destination) %>%
  dplyr::arrange(-.data$n) %>%
  dplyr::mutate(
    pct = scales::percent(n/sum(n), accuracy = 0.01),
    selection = paste0(.data$destination, " - ", .data$pct)
  )

filter_selection_destination <-
  c(
    "",
    purrr::set_names(
      destination_tbl$destination,
      destination_tbl$selection
    )
  )

# ================== #
# ---- zip code ----
# ================== #

zip_tbl <-
  od_data %>%
  dplyr::count(.data$zip) %>%
  dplyr::arrange(-.data$n) %>%
  dplyr::mutate(
    pct = scales::percent(n/sum(n), accuracy = 0.01),
    selection = paste0(.data$zip, " - ", .data$pct)
  ) %>%
  dplyr::filter(.data$n > 10)

filter_selection_zip <- c(
  "",
  zip_tbl$zip
)

# ======================= #
# ---- location type ----
# ======================= #

location_type_tbl <-
  od_data %>%
  dplyr::count(.data$location_type) %>%
  dplyr::arrange(-.data$n) %>%
  dplyr::mutate(
    pct = scales::percent(n/sum(n), accuracy = 0.01),
    selection = paste0(.data$location_type, " - ", .data$pct)
  )

filter_selection_location_type <-
  c(
    "",
    purrr::set_names(
      location_type_tbl$location_type,
      location_type_tbl$selection
    )
  )

usethis::use_data(filter_selection_agency, overwrite = TRUE)
usethis::use_data(filter_selection_destination, overwrite = TRUE)
usethis::use_data(filter_selection_zip, overwrite = TRUE)
usethis::use_data(filter_selection_location_type, overwrite = TRUE)
