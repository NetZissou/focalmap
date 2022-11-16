## code to prepare `FOCAL_DATA_DICTIONARY` dataset goes here



# list(
#   type = "",
#   class = c("spatial", "time-series"),
#   provider = "",
#   provider_contact = "",
#   interface = "",
#   availability = ,
#   update_frequency = "",
#   use_case = "",
#   attributes = "",
#   comment = "",
#   file_location = ""
# )


get_attr_html <- function(col_names) {

  col_names <-
    stringr::str_to_title(
      stringr::str_replace(col_names, "_", " ")
    )
  lists <- purrr::map_chr(col_names, ~as.character(shiny::tags$li(.x)))
  paste(
    c("<ul>", lists, "</ul>"),
    collapse = ""
  )
}


FOCAL_DATA_DICTIONARY <-
  list(
    # > Drug-related Crime Events ----


    list(
      type = "Drug-related Crime Events",
      class = c("spatial", "time-series"),
      provider = "Ohio Department of Public Safety",
      provider_contact = "Alan Wedd <awwedd@dps.ohio.gov>",
      interface = "email",
      availability = c("2019-01", "2022-06"),
      update_frequency = "yearly",
      use_case = "",
      attributes = opioidDashboard::data_drug_crime() %>%
        dplyr::select(1:9) %>%
        head(0) %>%
        dplyr::mutate(
          school_district = character(),
          census_tract = character(),
          EMS = character()
        ) %>%
        names() %>%
        get_attr_html(),
      comment = "Perform addtional geocoding phase to calculate case rate for zip code, EMS district, and school district",
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other", "Drug Crime"
      )
    ),

    # > Food Pantries ----

    list(
      type = "Food Pantries",
      class = c("spatial", "time-series"),
      provider = "foodpantries.org",
      provider_contact = "https://www.foodpantries.org/",
      interface = "web-scrapping",
      availability = c("available"),
      update_frequency = "whenever",
      use_case = "",
      attributes = get_attr_html(
        c(
          "name",
          "social_media_links",
          "website",
          "street_address",
          "city",
          "state",
          "zip_code",
          "telephone",
          "description (containing hours and special instructions)"
        )
      ),
      comment = "",
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other", "Food Pantries"
      )
    ),

    # > Doctors and Clinicians ----


    list(
      type = "Columbus Doctors and Clinicians",
      class = c("spatial"),
      provider = "Centers for Medicare & Medicaid Services",
      provider_contact = "https://data.cms.gov/provider-data/topics/doctors-clinicians",
      interface = "API",
      availability = c("available"),
      update_frequency = "whenever",
      use_case = "",
      attributes = get_attr_html(
        c(
          "name",
          "Medical School",
          "Graduation Year",
          "Primary Specialty",
          "Secondary Specialty",
          "Organization Legal Name",
          "Number of Group Members",
          "Address",
          "City",
          "State",
          "Zip",
          "Hospital Affiliations",
          "Phone Number"
        )
      ),
      comment = "Explore other type of diease treatments.",
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other",
        "Doctors and Clinicians",
        "doctors_columbus.csv"
      )
    ),

    list(
      type = "Columbus Hep C Treatment",
      class = c("spatial"),
      provider = "Centers for Medicare & Medicaid Services",
      provider_contact = "https://data.cms.gov/provider-data/topics/doctors-clinicians",
      interface = "API",
      availability = c("available"),
      update_frequency = "whenever",
      use_case = "",
      attributes = "Same as Columbus Doctors and Clinicians dataset",
      comment = "One doctor could have multiple work locations. Treatment locations would stack together on map because they usually nested within one institution.",
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other",
        "Doctors and Clinicians",
        "doctors_columbus_hepc.csv"
      )
    ),


    # > HIV Treatment ----
    list(
      type = "HIV Treatment Providers",
      class = c("spatial"),
      provider = "https://www.hiv.gov/",
      provider_contact = "Cathy Thomas <cathy.thomas@icf.com>",
      interface = "API",
      availability = c("available"),
      update_frequency = "",
      use_case = "",
      comment = "",
      attributes = get_attr_html(c(
        "service_name",
        "service_id",
        "street_address",
        "city",
        "country",
        "state",
        "zipcode",
        "lat",
        "lng",
        "telephone",
        "website",
        "language"
      )),
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other",
        "HIV",
        "hiv_columbus_treatment_service.RData"
      )
    ),

    # > Naloxone ----
    list(
      type = "Naloxone Kit Distribution",
      class = c("time-series"),
      provider = "ODH",
      provider_contact = "Anne Trinh <trinh.89@osu.edu>",
      interface = "email",
      availability = c("2020-02", "2022-10"),
      update_frequency = "monthly",
      use_case = "",
      attributes = get_attr_html(c(
        opioidDashboard::project_dawn_app_data() %>% names()
      )),
      comment = "The latest 30 to 60 days naloxone distribution data is preliminary. ",
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other",
        "Naloxone",
        "naloxone.csv"
      )
    ),

    # > Ohio Business ----

    list(
      type = "Ohio Businesses Location",
      class = c("spatial"),
      provider = "unknown",
      provider_contact = "unknown",
      interface = "unknown",
      availability =  c("available"),
      update_frequency = "unknown",
      use_case = "hot-spot detection",
      attributes = "",
      comment = paste0(
        "Hasn't been updated in a while. Some business locations might need to be updated. ", "\n",
        stringr::str_c(stringr::str_to_title(opioidDashboard::ELIGIBLE_BUSINESS_TYPE), collapse = "; ")
      ),
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other",
        "Ohio Businesses",
        "ohio_businesses.csv"
      )
    ),


    # > Opioid Treatment Providers ----

    list(
      type = "Opioid Treatment Providers",
      class = c("spatial"),
      provider = "https://relink.org/",
      provider_contact = "Barbara Campbell <bcampbell@thedaltonfoundation.org>",
      interface = "API",
      availability = c("available"),
      update_frequency = "whenever",
      use_case = "",
      attributes = "",
      comment = "Treatment providers filter need to be improved; grand category; key words; new hierarchy",
      file_location = fs::path(
        FOCALPipe::ROOT_PATH,
        "other",
        "Relink"
      )
    ),


    # > 311 Data ----

    list(
      type = "311 Data",
      class = c("spatial", "time-series"),
      provider = "Dublin",
      provider_contact = "Rick Frantz <RFrantz@dublin.oh.us>",
      interface = "web-download",
      availability = c("2019-01", "2022-02"),
      update_frequency = "unknown",
      use_case = "",
      attributes = get_attr_html(c(
        "date",
        "request_id",
        "request_type",
        "lat", "lng",
        "school_district",
        "zip",
        "EMS"
      )),
      comment = "",
      file_location = ""
    ),

    list(
      type = "311 Data",
      class = c("spatial", "time-series"),
      provider = "Columbus",
      provider_contact = "http://maps2.columbus.gov/arcgis/rest/services/Applications/ServiceRequests/MapServer/",
      interface = "API",
      availability = c("2008-01", "2022-05"),
      update_frequency = "monthly",
      use_case = "",
      attributes = "",
      comment = "Complete the columbus 311 data ingesting pipeline; Construct unique identifier. ",
      file_location = ""
    )
  )  %>%
  purrr::map_df(
    .f = function(metadata) {

      result <- tibble::tibble(
        type = metadata$type,
        class = list(metadata$class),
        provider = metadata$provider,
        provider_contact = metadata$provider_contact,
        interface = metadata$interface,
        availability = list(metadata$availability),
        update_frequency = metadata$update_frequency,
        use_case = metadata$use_case,
        attributes = metadata$attributes,
        comment = metadata$comment,
        file_location = metadata$file_location
      )

      return(result)
    }
  )


usethis::use_data(FOCAL_DATA_DICTIONARY, overwrite = TRUE)
