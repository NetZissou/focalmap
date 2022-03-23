## code to prepare `naloxone_forms_data` dataset goes here
NALOXONE_DATA_DIRECTORY <- "/fs/ess/PDE0001/project_dawn"

OPIOID_OVERDOSE_DATA_DIRECTORY <- "/fs/ess/PDE0001/opioid overdose data"

DRUG_CRIME_DATA_DIRECTORY <- "/fs/ess/PDE0001/drug crime data"

TREATMENT_PROVIDERS_DATA_DIRECTORY <- "/fs/ess/PDE0001/treatment provider data"

OHIO_BUSINESS_DATA_DIRECTORY <- "/fs/ess/PDE0001/business places data/Business-2020-OH.csv"

usethis::use_data(NALOXONE_DATA_DIRECTORY, overwrite = TRUE)
usethis::use_data(OPIOID_OVERDOSE_DATA_DIRECTORY, overwrite = TRUE)
usethis::use_data(DRUG_CRIME_DATA_DIRECTORY, overwrite = TRUE)
usethis::use_data(TREATMENT_PROVIDERS_DATA_DIRECTORY, overwrite = TRUE)
usethis::use_data(OHIO_BUSINESS_DATA_DIRECTORY, overwrite = TRUE)
