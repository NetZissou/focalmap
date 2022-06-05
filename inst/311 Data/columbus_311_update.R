library(sf)
library(tidyverse)

reticulate::source_python('~/focalmap/inst/311 Data/columbus_311_30days.py')
file_name <-
  paste0(
    "/fs/ess/PDE0001/311/Columbus/API_result/",
    "columbus311_30days_",
    format(Sys.time(), "%m_%d_%Y"),
    ".geojson"
  )

data_30days <-
  sf::st_read(file_name) %>%
  transmute(
    CASE_ID,
    REQUEST_TYPE,
    LOCATION_DESCRIPTION = STREET,
    date = lubridate::as_datetime(REPORTED_DATE/1000),
    geometry
  )

# data_historic <-
#   sf::st_read(
#     "/fs/ess/PDE0001/311/Columbus/columbus_311_data/columbus_311_data.shp"
#   )

# data_historic %>%
#   dplyr::bind_rows(data_30days) %>%
#   distinct(CASE_ID, REQUEST_TYPE, LOCATION_DESCRIPTION, date, geometry) %>%
#   nrow()

sf::st_write(
  data_30days,
  "/fs/ess/PDE0001/311/Columbus/columbus_311_data/columbus_311_data.shp",
  append=TRUE
)
