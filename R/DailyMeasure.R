#' DailyMeasure
#' (C) David Fong, 2019
#'
#' launches the DailyMeasure app
#' @return shiny application object
#'
#' @import shiny tidyverse dbplyr
#' @importFrom shinydashboard menuItem tabItems tabItem
#'
#' @export
launchApp <- function () {

  ##### Run the application ###########################################
  shinyApp(ui = DailyMeasureUI, server = DailyMeasureServer)

}
