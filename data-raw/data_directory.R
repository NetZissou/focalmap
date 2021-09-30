## code to prepare `naloxone_forms_data` dataset goes here
NALOXONE_DATA_DIRECTORY <- "/fs/ess/PDE0001/project_dawn"

OPIOID_OVERDOSE_DATA_DIRECTORY <- "/fs/ess/PDE0001/opioid overdose data"

usethis::use_data(NALOXONE_DATA_DIRECTORY, overwrite = TRUE)
usethis::use_data(OPIOID_OVERDOSE_DATA_DIRECTORY, overwrite = TRUE)
