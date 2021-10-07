SF_PATH <- "/fs/ess/PDE0001/school_district_sf/OhioSchoolDistricts_updated_all.geojson"
GLOBAL_CRS_CODE <- 4326

ohio_school_district_sf <-
  load_map_shapefile(
    file = SF_PATH,
    crs = GLOBAL_CRS_CODE
  ) %>%
  dplyr::mutate(
    old_district_id = as.character(.data$old_district_id)
  ) %>%
  # adding centroid
  dplyr::mutate(
    center_long = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[1]]),
    center_lat = purrr::map_dbl(geometry, ~ sf::st_centroid(.x)[[2]])
  )

franklin_county_school_districts <- c(
  "Bexley City School District",
  "Canal Winchester Local School District",
  "Columbus City School District",
  "Dublin City School District",
  "Gahanna-Jefferson City School District",
  "Grandview Heights City School District",
  "Groveport Madison Local School District",
  "Hamilton Local School District",
  "Hilliard City School District",
  "New Albany-Plain Local School District",
  "Pickerington Local School District",
  "Reynoldsburg City School District",
  "South-Western City School District",
  "Upper Arlington City School District",
  "Westerville City School District",
  "Whitehall City School District",
  "Worthington City School District"
)

franklin_county_school_district_sf <-
  ohio_school_district_sf %>%
  dplyr::filter(
    .data$NAME %in% franklin_county_school_districts
  )

# Store school district shapefiles type
OHIO_SCHOOL_DISTRICT_SF_PATH <- "/fs/ess/PDE0001/school_district_sf/ohio_school_district_sf.rda"
FRANKLIN_COUNTY_SCHOOL_DISTRICT_SF_PATH <- "/fs/ess/PDE0001/school_district_sf/franklin_county_school_district_sf.rda"

# Save shapefiles to OSC server
save(ohio_school_district_sf, file = OHIO_SCHOOL_DISTRICT_SF_PATH)
save(franklin_county_school_district_sf, file = FRANKLIN_COUNTY_SCHOOL_DISTRICT_SF_PATH)

usethis::use_data(OHIO_SCHOOL_DISTRICT_SF_PATH, overwrite = TRUE)
usethis::use_data(FRANKLIN_COUNTY_SCHOOL_DISTRICT_SF_PATH, overwrite = TRUE)
usethis::use_data(franklin_county_school_districts, overwrite = TRUE)







