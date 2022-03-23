#' initial bounds for opioid ovedose map
#' a list object
#' list(
#'   min_lng = min(od_data$lng, na.rm = TRUE),
#'   min_lat = min(od_data$lat, na.rm = TRUE),
#'   max_lng = max(od_data$lng, na.rm = TRUE),
#'   max_lat = max(od_data$lat, na.rm = TRUE)
#' )
"opioid_overdose_map_init_bounds"
