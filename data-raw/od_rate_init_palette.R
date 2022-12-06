overdose_case_rate_palette <- "YlOrRd"

od_data_all <- opioidDashboard::data_opioid_overdose()
zip_region_sf <- get_region_od_rate(
  "zip",
  get_zipcode_sf(),
  "GEOID",
  "GEOID",
  "TOTAL_POP",
  od_data = od_data_all
)


census_tract_region_sf <- get_region_od_rate(
  "census_tract",
  get_census_tract_sf(),
  "GEOID",
  "GEOID",
  "TOTAL_POP",
  od_data = od_data_all
)

school_district_region_sf <- get_region_od_rate(
  "school_district",
  get_ohio_school_district_sf() %>%
    dplyr::filter(.data$NAME %in% franklin_county_school_districts),
  "NAME",
  "NAME",
  "district_pop",
  od_data = od_data_all
)

od_case_rate_init_pal_list <-
  list(
    zip = leaflet::colorQuantile(
      palette = overdose_case_rate_palette,
      domain = zip_region_sf$rate_pal,
      n = 5
    ),

    school_district = leaflet::colorQuantile(
      palette = overdose_case_rate_palette,
      domain = school_district_region_sf$rate_pal,
      n = 5
    ),

    census_tract = leaflet::colorQuantile(
      palette = overdose_case_rate_palette,
      domain = census_tract_region_sf$rate_pal,
      n = 5
    )
  )

usethis::use_data(od_case_rate_init_pal_list, overwrite = TRUE)
usethis::use_data(overdose_case_rate_palette, overwrite = TRUE)

