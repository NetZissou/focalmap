library(sf)
library(tidyverse)


data_08_17 <-
  sf::st_read("/fs/ess/PDE0001/311/Columbus/columbus_historical_data_raw/cbus_311_requests_2008_2017")

data_3years <-
  sf::st_read("/fs/ess/PDE0001/311/Columbus/API_result/columbus311_3years.geojson") %>%
  dplyr::mutate(
    STATUS_DATE = lubridate::as_datetime(STATUS_DATE/1000),
    REPORTED_DATE = lubridate::as_datetime(REPORTED_DATE/1000)
  )

data_combined <-
  data_08_17 %>%
  dplyr::transmute(
    CASE_ID = paste0("08_17_case_", dplyr::row_number()),
    REQUEST_TYPE = SWR_TYPE,
    LOCATION_DESCRIPTION = LOCATION_D,
    date = STATUS_DAT,
    geometry
  ) %>%
  dplyr::bind_rows(
    data_3years %>%
      dplyr::select(
        CASE_ID,
        REQUEST_TYPE,
        LOCATION_DESCRIPTION = DEPARTMENT_NAME,
        date = REPORTED_DATE,
        geometry
      )
  ) %>%
  dplyr::arrange(date)

sf::st_write(
  data_combined,
  "/fs/ess/PDE0001/311/Columbus/columbus_311_historical_08_22/columbus_311_historical_08_22.shp"
)
