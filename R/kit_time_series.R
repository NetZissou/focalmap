kit_distribution_ts_ui <- function(id) {

  shinydashboard::box(
    title = "Time Series of Naloxone Kits Distribution",
    width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,

    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinyWidgets::pickerInput(
          inputId = shiny::NS(id, "group_level_1"),
          label = "Group by",
          choices = c(
            "None" = "none",
            "Age group" = "age",
            "Ethnicity group" = "ethnicity",
            "Assigned Sex" = "birth_sex"
          )
        )
      )
    ),

    highcharter::highchartOutput(
      outputId = shiny::NS(id, "kit_ts_plot")
    )
  )
}

#' @importFrom rlang ".data"
kit_distribution_ts_server <-
  function(id, kit_join_personal) {
    shiny::moduleServer(id, function(input, output, session) {
      output$kit_ts_plot <-
        highcharter::renderHighchart({
          shiny::req(input$group_level_1)

          groups <- c("date")

          if (input$group_level_1 != "none") {
            groups <-
              c(groups, input$group_level_1)
          }

          ts_data <-
            kit_join_personal %>%
            dplyr::mutate(
              date = tsibble::yearmonth(.data$date)
            ) %>%
            dplyr::group_by(
              dplyr::across(.cols = groups)
            ) %>%
            dplyr::summarise_at(
              dplyr::vars(.data$kit_number), .f = list(n = sum)) %>%
            dplyr::mutate(
              date = as.Date(.data$date)
            )



          # ts_data %>%
          #   apexcharter::apex(
          #     type = "area",
          #     mapping = apexcharter::aes(
          #       x = .data$date, y = .data$n, group = .data$age
          #     )
          #   ) %>%
          #   apexcharter::ax_yaxis(
          #     decimalsInFloat = 0,
          #     labels = list(
          #       formatter = apexcharter::format_num(".2s")
          #     )
          #   )

          if (input$group_level_1 == "none") {
            ts_plot <-
              highcharter::hchart(
                ts_data, type  = "area", name = "Number of Naloxone Kits Distributed",
                highcharter::hcaes(x = .data$date, y = .data$n)
              )

            ts_plot_title <- "<b>Number of Naloxone Kits Distributed</b>"
          } else {
            ts_plot <-
              highcharter::hchart(
                ts_data, type  = "area",
                highcharter::hcaes(x = .data$date, y = .data$n, group = .data[[input$group_level_1]])
              )
            ts_plot_title <- sprintf(
              "<b>Number of Naloxone Kits Distributed </b> <i>by %s Group</i>",
              tools::toTitleCase(
                stringr::str_replace_all(
                  input$group_level_1,
                  "_", " "
                )
              )
            )
          }

          ts_plot %>%
            highcharter::hc_title(
              text = ts_plot_title,
              align = "left",
              style = list(useHTML = TRUE)
            ) %>%
            highcharter::hc_xAxis(
              title = FALSE,
              type = "datetime"
            ) %>%
            highcharter::hc_yAxis(
              title = FALSE
            ) %>%
            highcharter::hc_navigator(
              enabled = TRUE
            ) %>%
            highcharter::hc_exporting(
              enabled = TRUE,
              filename = "kit_time_series"
            )
        })

    })



  }
