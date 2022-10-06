devInfoUI <- function(id) {
  shiny::tagList(
    shiny::h1("Development Cycle"),
    highcharter::highchartOutput(shiny::NS(id, "dev_cycle")),

    shiny::h1("Tab Direction"),

    shiny::a("For ... information, please use the"),
    shiny::actionLink(shiny::NS(id, "go_to_tab_data_filters"), "Data Filters Tab."),

    shiny::br(),

    shiny::a("For ... information, please use the"),
    shiny::actionLink(shiny::NS(id, "go_to_tab_project_dawn"), "Project DAWN Tab."),

    shiny::br(),

    shiny::a("For ... information, please use the"),
    shiny::actionLink(shiny::NS(id, "go_to_tab_hot_spot_detection"), "Hot Spot Detection Tab."),

    shiny::br(),

    shiny::a("For ... information, please use the"),
    shiny::actionLink(shiny::NS(id, "go_to_tab_case_rate_map"), "Case Rate Map Tab.")


  )
}

devInfoServer <- function(id, parent_session) {

  shiny::moduleServer(id, function(input, output, session){

    # =========================== #
    # ---- Development Cycle ----
    # =========================== #
    output$dev_cycle <- highcharter::renderHighchart({

      tbl <- tibble::tibble(
        a = paste0("feature/feature_", c(1:4)),
        b = rep("develop", 4),
        c = c("prod", "prod", "master(DEV SITE)", "master(DEV SITE)"),
        d = c("tags(PRODUCTION SITE)", "tags(PRODUCTION SITE)", NA, NA)
      )

      highcharter::hchart(
        highcharter::data_to_sankey(tbl),
        "sankey",
        name = "Dev Cycle"
      )
    })

    # ====================== #
    # ----Tab Direction ----
    # ====================== #

    shiny::observeEvent(input$go_to_tab_data_filters, {

      shiny::updateNavbarPage(
        session = parent_session,
        inputId = "navbar",
        selected = "tab_data_filters"
      )
    })

    shiny::observeEvent(input$go_to_tab_project_dawn, {

      shiny::updateNavbarPage(
        session = parent_session,
        inputId = "navbar",
        selected = "tab_project_dawn"
      )
    })

    shiny::observeEvent(input$go_to_tab_hot_spot_detection, {

      shiny::updateNavbarPage(
        session = parent_session,
        inputId = "navbar",
        selected = "tab_hot_spot_detection"
      )
    })

    shiny::observeEvent(input$go_to_tab_case_rate_map, {

      shiny::updateNavbarPage(
        session = parent_session,
        inputId = "navbar",
        selected = "tab_case_rate_map"
      )
    })

  })
}



