# ================================= #
# ----Treatment Providers Spec ----
# ================================= #
filter_selection_treatment_providers_spec <-
  c(
    "Any Gender" ="gender_all",
    "Male Gender" ="gender_male",
    "Female Gender" ="gender_female",
    "Pregnancy Services" ="pregnancy",
    "Veteran Specific Services" ="veteran",
    "Private Insurance" ="ins_private",
    "Medicaid/Medicare Insurance" ="ins_medicaid",
    "Self-Pay Insurance" ="ins_self",
    "Sliding Scale Insurance" = "ins_slide",
    "Finance Insurance" = "ins_finance",
    "Free/No Charge" = "ins_free",
    "Any Insurance" = "ins_insurance",
    "Juvenile" = "ages_juveniles",
    "Adults" = "ages_adults",
    "Older Adults" = "ages_older_adults"
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
