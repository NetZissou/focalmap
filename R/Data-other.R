data_hiv_testing_locations <- function(arrow = FALSE) {

  file_csv <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "HIV",
      "hiv_columbus_testing_locations.csv"
    )

  file_parquet <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "HIV",
      "hiv_columbus_testing_locations.parquet"
    )

  if (arrow) {
    # return(arrow::read_parquet(
    #   file = file_parquet
    # ))
  } else {
    vroom::vroom(
      file = file_csv
    ) %>%
      dplyr::mutate(
        popup_label = paste(sep = "",
          "HIV Testing Site",
          "<br/>",
          glue::glue(
            "<b><a href = {website} target='_blank'>{name}</a></b>",
            website = .data$website,
            name = .data$name
          ),
          "<br/>",
          # > Phone
          ifelse(
            is.na(.data$telephone),
            "",
            glue::glue(
              "<b>Contact: {telephone}</b><br/>",
              telephone = .data$telephone
            )
          ),
          # > Address
          .data$full_address, "<br/>",
          # > Language
          ifelse(
            is.na(.data$language),
            "",
            glue::glue(
              "<b>Language: </b><br/>{language}",
              language = .data$language
            )
          )
        )
      ) %>%
      return()
  }
}

data_food_pantries <- function(arrow = FALSE) {
  file_csv <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "Food Pantries",
      "food_pantries_columbus.csv"
    )

  file_parquet <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "Food Pantries",
      "food_pantries_columbus.parquet"
    )

  if (arrow) {
    # return(arrow::read_parquet(
    #   file = file_parquet
    # ))
  } else {
    return(vroom::vroom(
      file = file_csv
    ) %>%
      dplyr::mutate(
        popup_label = paste(sep = "",
                            "<b>Food Pantries<b>",
                            "<br/>",
                            # > Name + website
                            glue::glue(
                              "<b><a href = {food_pantry_website} target='_blank'>{food_pantry_name}</a></b>",
                              food_pantry_website = ifelse(is.na(.data$website), .data$website_foodpantries, .data$website),
                              food_pantry_name = .data$name
                            ),
                            "<br/>",
                            # > Phone
                            ifelse(
                              is.na(.data$telephone),
                              "",
                              glue::glue(
                                "<b>Contact: {telephone}</b><br/>",
                                telephone = .data$telephone
                              )
                            ),
                            # > Social Media: Facebook
                            ifelse(
                              is.na(.data$facebook),
                              "",
                              glue::glue(
                                "<a href = {facebook_link} target='_blank'>Facebook</a><br/>",
                                facebook_link = .data$facebook
                              )
                            ),
                            # > Social Media: Instagram
                            ifelse(
                              is.na(.data$instagram),
                              "",
                              glue::glue(
                                "<a href = {instagram_link} target='_blank'>Instagram</a><br/>",
                                instagram_link = .data$instagram
                              )
                            ),
                            # > Social Media: Twitter
                            ifelse(
                              is.na(.data$twitter),
                              "",
                              glue::glue(
                                "<a href = {twitter_link} target='_blank'>Twitter</a><br/>",
                                twitter_link = .data$twitter
                              )
                            ),
                            # > Address
                            stringr::str_remove(
                              .data$full_address,
                              "United States, "
                            ),
                            "<br/>",
                            # Additional Description
                            ifelse(
                              is.na(.data$description),
                              "",
                              .data$description
                            )


        )
      )
    )
  }
}

data_hepc_treatment <- function(arrow = FALSE) {

  file_csv <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other",
      "Doctors and Clinicians",
      "doctors_columbus_hepc.csv"
    )

  file_parquet <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other",
      "Doctors and Clinicians",
      "doctors_columbus_hepc.parquet"
    )

  if (arrow) {
    # return(arrow::read_parquet(
    #   file = file_parquet
    # ))
  } else {
    return(vroom::vroom(
      file = file_csv
    ))
  }
}

data_columbus_311_heatmap <- function() {
  return(
    readr::read_csv(
      fs::path(
        opioidDashboard::ROOT_PATH,
        "other", "311", "Heatmap", "heatmap_data_columbus_2022.csv"
      )
    )
  )
}

data_public_places <- function() {
  return(
    sf::st_read(
      fs::path(
        opioidDashboard::ROOT_PATH,
        "other", "Public Places", "columbus_public_places_geocoded.geojson"
      )
    )
  )
}
