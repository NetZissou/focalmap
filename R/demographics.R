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

    # apexcharter::apexchartOutput(
    #   outputId = shiny::NS(id, "demo_plot_single")
    # )
    highcharter::highchartOutput(
      outputId = shiny::NS(id, "demo_plot_single"),
      height = "270px"
    ),
    shiny::htmlOutput(
      outputId = shiny::NS(id, "demo_plot_facet")
    )
  )
}


demographicsServer <-
  function(id, kit_join_personal) {
    shiny::moduleServer(id, function(input, output, session) {

      output$demo_plot_single <-
        #apexcharter::renderApexchart({
        highcharter::renderHighchart({
          shiny::req(input$group_level)
          #shiny::req(length(input$group_level) == 1)
          #print(input$group_level)
          #print(kit_join_personal)
          first_group_var <- c(input$group_level)[1]
          p <- plot_group(kit_join_personal, first_group_var)
          p

        })

      output$demo_plot_facet <-
        shiny::renderUI({
          shiny::req(input$group_level)
          shiny::req(length(input$group_level) > 1)
          p <- plot_group(kit_join_personal, input$group_level)
          p
        })

      # shiny::observeEvent(input$group_level, {
      #   shiny::req(input$group_level)
      #   check_flag <- (length(input$group_level) > 1)
      #   if (check_flag) {
      #     shiny::removeUI(selector = shiny::NS(id,"demo_plot_single"))
      #     cat("Single plot removed")
      #   } else {
      #     shiny::removeUI(selector = shiny::NS(id,"demo_plot_facet"))
      #     cat("Multiple plot removed")
      #   }
      # })

    })
  }




plot_group <- function(data, group_var) {

  n_group_var <- length(group_var)
  plot_data <-
    data %>%
    group_count(group_var = c(group_var))

    # apexcharter::apex(
    #   apexcharter::aes(x = .data[[group_var]], y = .data$Count), type = "column"
    # )


  if (length(group_var) > 1) {
    # Multiple grouping

    # When age is in the selection
    if ("age" %in% group_var) {

      if (length(group_var) == 3) {
        facet_var <- "ethnicity"
        age_key <-
          plot_data %>%
          dplyr::ungroup() %>%
          dplyr::distinct(.data[["age"]], .data[["birth_sex"]]) %>%
          dplyr::ungroup()
      } else {
        facet_var <- dplyr::setdiff(group_var, "age")
        age_key <-
          plot_data %>%
          dplyr::ungroup() %>%
          dplyr::distinct(.data[["age"]]) %>%
          dplyr::ungroup()
      }



      plot_object <-
        plot_data %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data[[facet_var]]) %>%
        dplyr::group_split() %>%
        purrr::map(.f =
                     function(df, age_key, facet_var) {

          facet_title <- df %>%
            dplyr::select(.data[[facet_var]]) %>%
            dplyr::distinct() %>%
            dplyr::pull()

          facet_plot_data <-
            age_key %>%
            dplyr::left_join(
              df
            ) %>%
            tidyr::replace_na(list(
              n = 0,
              ethnicity = facet_title
            )) %>%
            dplyr::rename(Count = .data$n)

          if (length(group_var) == 3) {
            facet_plot_object <-
              facet_plot_data %>%
              highcharter::hchart(
                type = "column",
                highcharter::hcaes(x = .data[["age"]], y = .data[["Count"]], group = .data[["birth_sex"]])
              )
          } else {
            facet_plot_object <-
              facet_plot_data %>%
              highcharter::hchart(
                type = "column",
                highcharter::hcaes(x = .data[["age"]], y = .data[["Count"]]),
                name = "Count of kits distributed"
              )
          }

          facet_plot_object %>%
            highcharter::hc_title(text = facet_title) %>%
            highcharter::hc_xAxis(title = list(text = "")) %>%
            highcharter::hc_yAxis(title = list(text = ""))

        },
        age_key = age_key, facet_var = facet_var) %>%
        highcharter::hw_grid(rowheight = 270, ncol = 3)
    } else {
      # When age is not in the selection
      # group vars could only be ethnicity and birth sex
      facet_var <- "ethnicity"
      plot_object <-
        plot_data %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data[[facet_var]]) %>%
        dplyr::group_split() %>%
        purrr::map(.f = function(df) {

          facet_title <- df %>%
            dplyr::select(.data[[facet_var]]) %>%
            dplyr::distinct() %>%
            dplyr::pull()

          df %>%
            tidyr::replace_na(list(
              n = 0
            )) %>%
            dplyr::rename(Count = .data$n) %>%
            highcharter::hchart(
              type = "column",
              highcharter::hcaes(x = .data[["birth_sex"]], y = .data[["Count"]]),
              name = "Count of kits distributed"
            ) %>%
            highcharter::hc_title(text = facet_title) %>%
            highcharter::hc_xAxis(title = list(text = "")) %>%
            highcharter::hc_yAxis(title = list(text = ""))
        }) %>%
        highcharter::hw_grid(rowheight = 225, ncol = 3)
    }

  } else {
    plot_object <-
      plot_data %>%
      dplyr::rename(Count = .data$n) %>%
      highcharter::hchart(
        type = "column",
        highcharter::hcaes(x = .data[[group_var]], y = .data[["Count"]]),
        name = "Count of kits distributed"
      ) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_yAxis(title = list(text = ""))
  }

  return(plot_object)
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

  purrr::map_chr(
    .x = variables,
    .f = ~atomic_dtype(df = df, var = .x)
  ) %>%
    purrr::set_names(
      variables
    )
}





