# ================================= #
# ----Treatment Providers Spec ----
# ================================= #
filter_selection_treatment_providers_spec <-
  c(
    "gender_all",
    "gender_male",
    "gender_female",
    "pregnancy",
    "veteran",
    "ins_private",
    "ins_medicaid",
    "ins_self",
    "ins_slide",
    "ins_finance",
    "ins_free",
    "ins_insurance",
    "ages_juveniles",
    "ages_adults",
    "ages_older_adults"
  )

# ========================================= #
# ----Treatment Providers Service Type ----
# ========================================= #
filter_selection_treatment_providers_service <-
  treatment_providers_data_all %>%
  dplyr::transmute(
    service_list = stringr::str_split(.data$services, pattern = ", ")
  ) %>%
  tidyr::unnest(service_list) %>%
  dplyr::count(service_list) %>%
  dplyr::filter(.data$service_list != "Equine)") %>%
  dplyr::mutate(
    service_list = ifelse(
      .data$service_list == "Alternative Therapy (Art",
      "Alternative Therapy (Art, Equine)",
      .data$service_list
    ),
    service_list = stringr::str_trim(.data$service_list, side = "both")
  ) %>%
  dplyr::arrange(-.data$n) %>%
  dplyr::pull(.data$service_list)

usethis::use_data(filter_selection_treatment_providers_service, overwrite = TRUE)
usethis::use_data(filter_selection_treatment_providers_spec, overwrite = TRUE)
