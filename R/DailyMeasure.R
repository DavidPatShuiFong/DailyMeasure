#' DailyMeasure
#' (C) David Fong, 2019
#'
#' launches the DailyMeasure app
#' @return None
#'
#' @import shiny tidyverse dbplyr
#' @importFrom shinydashboard menuItem tabItems tabItem
#'
#' @export
DailyMeasure <- function () {

  ##### Run the application ###########################################
  shinyApp(ui = DailyMeasureUI(), server = DailyMeasureServer)

}
