## code to prepare `AGENCY` dataset goes here
AGENCY <-
  opioidDashboard::data_opioid_overdose() %>%
  dplyr::pull(.data$agency) %>%
  unique()
usethis::use_data(AGENCY, overwrite = TRUE)
