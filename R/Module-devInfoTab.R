devInfoUI <- function(id) {
  shiny::tagList(
    shiny::h1("Development Cycle"),
    highcharter::highchartOutput(shiny::NS(id, "dev_cycle"))
  )
}

devInfoServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session){

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
  })
}
