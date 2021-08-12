demographicsUI <- function(id) {

  shinydashboard::box(
    title = "Kit Distribution by Demographics",
    width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,

    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinyWidgets::pickerInput(
          inputId = shiny::NS(id, "group_level"),
          label = "Group by",
          choices = c(
            "Age group" = "age",
            "Ethnicity group" = "ethnicity",
            "Assigned Sex" = "birth_sex"
          ),
          multiple = TRUE
        )
      )
    ),

    highcharter::highchartOutput(
      outputId = shiny::NS(id, "demographics")
    )
  )
}


demographicsServer <-
  function(id, kit_join_personal) {
    shiny::moduleServer(id, function(input, output, session) {


    })
  }






group_count <- function(data, group_var) {

  data %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_var)
      )
    ) %>%
    dplyr::summarise(
      n = dplyr::n()
    )
}

var_dtype <- function(df, variables) {

  atomic_dtype <- function(df, var) {
    return(
      class(df[[var]])
    )
  }

  map_chr(
    .x = variables,
    .f = ~atomic_dtype(df = df, var = .x)
  ) %>%
    purrr::set_names(
      variables
    )
}





