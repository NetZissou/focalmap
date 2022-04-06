projectDAWNFilterUI <- function(id) {
  shinydashboard::box(
    title = "Global Filters",
    width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shinyWidgets::sliderTextInput(
          inputId = shiny::NS(id, "date_range"),
          label = "Date Range",
          choices = purrr::map_chr(
            .x = seq(
              from = as.Date("2020-01-01"),
              to = Sys.Date(),
              by = "1 month"),
            .f = function(date) {
              year <- lubridate::year(date)
              month_num <- lubridate::month(date)
              return(paste0(year, "-", month_num))
            }
          ),
          selected = purrr::map_chr(
            .x = c(as.Date("2020-01-01"), Sys.Date()),
            .f = function(date) {
              year <- lubridate::year(date)
              month_num <- lubridate::month(date)
              return(paste0(year, "-", month_num))
            }
          )
        )
      )
    )
  )
}

projectDAWNFilterServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # TODO: wrap functions
    naloxone_data_all <- opioidDashboard::project_dawn_app_data()

    opioid_overdose_data_all <- opioidDashboard::opioid_overdose_data()


    project_DAWN_data <- shiny::reactiveValues(
      naloxone_data = naloxone_data_all,
      overdose_data = opioid_overdose_data_all
    )

    shiny::observe({

      # print(paste0(
      #   "Filter min: ",
      #   lubridate::ym(input$date_range[1])
      # ))
      # print(paste0(
      #   "Filter max: ",
      #   lubridate::ym(input$date_range[2])
      # ))

      project_DAWN_data$naloxone_data <-
        naloxone_data_all %>%
        dplyr::filter(
          .data$date > lubridate::ym(input$date_range[1]),
          .data$date < lubridate::ym(input$date_range[2])
        )

      naloxone_date_max <-
        max(project_DAWN_data$naloxone_data$date)
      naloxone_date_min <-
        min(project_DAWN_data$naloxone_data$date)

      # print(paste0(
      #   "Naloxone min: ",
      #   naloxone_date_min
      # ))
      # print(paste0(
      #   "Naloxone max: ",
      #   naloxone_date_max
      # ))

      project_DAWN_data$overdose_data <-
        opioid_overdose_data_all %>%
        dplyr::filter(
          .data$date >= naloxone_date_min,
          .data$date <= naloxone_date_max
        )
    })

    return(project_DAWN_data)
  })
}


projectDAWNTimeSeriesUI  <- function(id) {

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
            "Assigned Sex" = "birth_sex",
            "Program" = "program_cat",
            "Reason" = "reason"
          )
        )
      )
    ),
    highcharter::highchartOutput(
      outputId = shiny::NS(id, "od_ts_plot"),
      height = "300px"
    ),
    highcharter::highchartOutput(
      outputId = shiny::NS(id, "kit_ts_plot")
    )
  )
}

projectDAWNTimeSeriesServer <- function(id, project_DAWN_data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::req(shiny::is.reactivevalues(project_DAWN_data))


    chart_theme_function <- highcharter::hc_theme_flat


    # ============================== #
    # ---- Overdose Time Series ----
    # ============================== #

    output$od_ts_plot <-
      highcharter::renderHighchart({
        shiny::req(input$group_level_1)

        case_od_group <- function(x) {
          dplyr::case_when(
            x == "birth_sex" ~ "sex",
            x == "ethnicity" ~ "race",
            x == "age" ~ "age_cat",
            TRUE ~ "none"
          )
        }

        groups <- c("date")

        if (case_od_group(input$group_level_1) != "none") {
          groups <-
            c(groups, case_od_group(input$group_level_1))
        }


        ts_data <-
          project_DAWN_data$overdose_data %>%
          dplyr::mutate(
            date = tsibble::yearmonth(.data$date)
          ) %>%
          dplyr::group_by(
            dplyr::across(.cols = groups)
          ) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::mutate(
            date = as.Date(.data$date)
          ) %>%
          stats::na.omit()

        if (length(groups) == 1) {
          ts_plot <-
            highcharter::hchart(
              ts_data, type  = "area", name = "Number of Opioid Overdose Cases",
              highcharter::hcaes(x = .data$date, y = .data$n)
            )
          ts_plot_title <-
            "<b>Number of Opioid Overdose Cases</b>"
        } else {
          ts_plot <-
            highcharter::hchart(
              ts_data, type  = "area",
              highcharter::hcaes(x = .data$date, y = .data$n, group = .data[[case_od_group(input$group_level_1)]])
            )
          ts_plot_title <- sprintf(
            "<b>Number of Opioid Overdose Cases </b> <i>by %s Group</i>",
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
            filename = "opioid_overdose_ts"
          ) %>%
          highcharter::hc_add_theme(
            chart_theme_function()
          )
      })

    # ================================== #
    # ---- Naloxone Kit Time Series ----
    # ================================== #
    output$kit_ts_plot <-
      highcharter::renderHighchart({
        shiny::req(input$group_level_1)

        groups <- c("date")

        if (input$group_level_1 != "none") {
          groups <-
            c(groups, input$group_level_1)
        }

        ts_data <-
          project_DAWN_data$naloxone_data %>%
          dplyr::mutate(
            date = tsibble::yearmonth(.data$date)
          ) %>%
          dplyr::group_by(
            dplyr::across(.cols = groups)
          ) %>%
          dplyr::summarise_at(
            dplyr::vars(.data$kit_number), .f = list(n = ~sum(.x, na.rm = TRUE))) %>%
          dplyr::mutate(
            date = as.Date(.data$date)
          ) %>%
          stats::na.omit()

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
          ) %>%
          highcharter::hc_add_theme(
            chart_theme_function()
          )

      })

  })
}
























