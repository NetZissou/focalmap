#' Load treament providers data from OSC
#'
#' @param dir directory on OSC that contains treament providers data
#' @param tier_2_only only show tier 2 treatment providers
#' @return treatment providers data
#' @export
treatment_providers_data <- function(
  dir = opioidDashboard::TREATMENT_PROVIDERS_DATA_DIRECTORY,
  tier_2_only = TRUE
) {

  path <- paste0(
    dir, "/", "relinkProviders_FOCAL.csv"
  )
  suppressMessages(
    data <- readr::read_csv(
      file = path
    )
  )

  # ================== #
  # ---- Renaming ----
  # ================== #
  data <-
    data %>%
    dplyr::select(
      dplyr::everything(),
      lat = .data$latitude,
      lng = .data$longitude
    ) %>%
    dplyr::mutate(
      gender_all = (.data$gender == "all"),
      gender_female = (.data$gender == "female"),
      gender_male = (.data$gender == "male")
    )

  # ================================== #
  # ---- Pre-generate pop content ----
  # ================================== #
  popup_content <- function(name, address, email, phone, website) {

    # Clean missing values
    if (is.na(address)) {
      address <- ""
    }

    if (is.na(email)) {
      email <- ""
    }

    if (is.na(phone)) {
      phone <- ""
    }

    if (is.na(website)) {
      website <- ""
    }

    name_link_html <-
      sprintf(
        "<b><a href='%s' target='_blank'>%s</a></b>",
        website,
        name
      )

    address_html <-
      sprintf(
        "Address: %s",
        address
      )

    phone_html <-
      sprintf(
        "Phone: %s",
        phone
      )

    email_html <-
      sprintf(
        "Email: %s",
        email
      )

    content <-
      paste(
        name_link_html,
        address_html,
        phone_html,
        email_html,
        sep = "<br/>"
      )

    return(content)

  }

  label_content <- function(name) {
    content <-
      sprintf(
        "%s",
        name
      )
    return(content)
  }
  data <- data %>%
    dplyr::mutate(
      popup = purrr::pmap_chr(
        .l = list(.data$name, .data$address, .data$email, .data$phone, .data$website),
        .f = popup_content
      ),
      label = purrr::pmap_chr(
        .l = list(.data$name),
        .f = label_content
      )
    )

  # =========================== #
  # ---- Service Type List ----
  # =========================== #
  service_type_tbl <- data %>%
    dplyr::transmute(
      id = .data$id,
      service_list = stringr::str_split(.data$services, pattern = ", ")
    ) %>%
    tidyr::unnest(.data$service_list) %>%
    dplyr::filter(.data$service_list != "Equine)") %>%
    dplyr::mutate(
      service_list = ifelse(
        .data$service_list == "Alternative Therapy (Art",
        "Alternative Therapy (Art, Equine)",
        .data$service_list
      ),
      service_list = stringr::str_trim(.data$service_list, side = "both")
    ) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(
      service_list = list(.data$service_list)
    )

  data <-
    data %>%
    dplyr::left_join(
      service_type_tbl,
      by = "id"
    )

  # =============================== #
  # ---- Only tier 2 providers ----
  # =============================== #

  if (tier_2_only) {
    data <- data %>%
      dplyr::filter(
        .data$include_in_focal_tier2 == 1
      )
  }
  return(data)
}
