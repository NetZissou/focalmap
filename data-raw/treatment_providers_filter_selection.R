
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
usethis::use_data(filter_selection_treatment_providers_spec, overwrite = TRUE)
