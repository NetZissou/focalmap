devInfoUI <- function(id) {
  shiny::tagList(
    shiny::h1("Development Cycle"),
    highcharter::highchartOutput(shiny::NS(id, "dev_cycle")),


    shiny::h1("User Table"),
    reactable::reactableOutput(
      shiny::NS(id, "user_table")
    ),

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

    # ==================== #
    # ---- User Table ----
    # ==================== #
    output$user_table <- reactable::renderReactable({
      opioidDashboard::FOCAL_USER_TABLE %>%
        purrr::set_names(
          stringr::str_to_title(names(opioidDashboard::FOCAL_USER_TABLE))
        ) %>%
        reactable::reactable(
          # Table Format
          filterable = TRUE,
          outlined = TRUE,
          fullWidth = TRUE,
          #defaultColDef = reactable::colDef(minWidth = 50),
          #height = 400,
          #bordered = TRUE,
          # Selection
          #selection = "multiple", onClick = "select",
          highlight = TRUE,
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),

          columns = list(
            Duo = reactable::colDef(cell = function(value) {
              # Render as an X mark or check mark
              if (!value) "\u274c No" else "\u2714\ufe0f Yes"
            }),

            Onboard = reactable::colDef(cell = function(value) {
              # Render as an X mark or check mark
              if (!value) "\u274c No" else "\u2714\ufe0f Yes"
            })
          ),
          groupBy = "Agency"
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



