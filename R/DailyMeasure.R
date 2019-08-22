#' DailyMeasure
#' (C) David Fong, 2019
#'
#' launches the DailyMeasure app
#'
#' @param appmode set to "chrome" if wanting to launch chrome in app mode
#'  app-mode has not forward/back buttons, tabs, or address-bar
#' @param launch.browser=TRUE normally launches a web-browser. within
#'  Rstudio, launch.browser=FALSE will start a RStudio browser.
#'
#' @return None
#'
#' @import shiny dbplyr
#' @importFrom shinydashboard menuItem tabItems tabItem
#'
#' @export
DailyMeasure <- function (appmode = "chrome", launch.browser = TRUE) {

  if (appmode == "chrome") {
    launch.browser = function(shinyurl) {
      shell(paste0("start chrome --app=", shinyurl))
    }
  }

  ##### Run the application ###########################################
  shiny::shinyApp(ui = DailyMeasureUI(), server = DailyMeasureServer,
                  options = list(launch.browser = launch.browser))

}
