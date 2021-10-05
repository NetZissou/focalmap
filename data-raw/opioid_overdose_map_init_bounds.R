## code to prepare `opioid_overdose_map_init_bounds` dataset goes here
library(opioidDashboard)
library(dplyr)

od_data <- opioid_overdose_data()
opioid_overdose_map_init_bounds <-
  list(
    min_lng = as.numeric(quantile(od_data$lng, 0.05, na.rm = TRUE)),
    min_lat = as.numeric(quantile(od_data$lat, 0.05, na.rm = TRUE)),
    max_lng = as.numeric(quantile(od_data$lng, 0.95, na.rm = TRUE)),
    max_lat = as.numeric(quantile(od_data$lat, 0.95, na.rm = TRUE))
  )
usethis::use_data(opioid_overdose_map_init_bounds, overwrite = TRUE)


