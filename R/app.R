#' Opioid app activation function
#'
#' @export
app <- function() {
  shiny::shinyApp(
    opioidDashboard::ui,
    opioidDashboard::server
  )
}
