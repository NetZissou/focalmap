## code to prepare `schema` dataset goes here

# TODO: Only run this from interactive sessions
# schema_primary <-
#   FOCALPipe::generate_col_types(FOCALPipe::SCHEMA_PRIMARY$schema)
#
# schema_naloxone <-
#   FOCALPipe::generate_col_types(FOCALPipe::SCHEMA_NALOXONE$schema)

usethis::use_data(schema_primary, overwrite = TRUE)
usethis::use_data(schema_naloxone, overwrite = TRUE)

