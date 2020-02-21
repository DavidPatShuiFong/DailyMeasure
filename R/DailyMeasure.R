#' DailyMeasure
#' (C) David Fong, 2019
#'
#' launches the DailyMeasure app
#'
#' @param appmode set to "chrome" if wanting to launch chrome in app mode
#'  app-mode has not forward/back buttons, tabs, or address-bar
#' @param launch.browser=TRUE normally launches a web-browser. within
#'  Rstudio, launch.browser=FALSE will start a RStudio browser.
#' @param host set host. "0.0.0.0" if wanting access outside the local machine
#' @param port set to port number if to open on specified port. default is NULL,
#'  which is a random port number
#' @param demonstration set to TRUE if to run in demonstration mode where
#'  some settings cannot be changed. default is FALSE
#'
#' @return None
#'
#' @import shiny dbplyr
#' @importFrom shinydashboard menuItem tabItems tabItem
#'
#' @export
GPstat <- function (appmode = "chrome", launch.browser = TRUE,
                    host = getOption("shiny.host", "127.0.0.1"), port = NULL,
                    demonstration = FALSE, ...) {

  if (appmode == "chrome") {
    launch.browser = function(shinyurl) {
      shell(paste0("start chrome --app=", shinyurl))
    }
  }

  ##### Run the application ###########################################
  shiny::shinyApp(ui = DailyMeasureUI(), server = DailyMeasureServer,
                  options = list(launch.browser = launch.browser,
                                 host = host, port = port),
                  onStart = function() {
                    .bcdyz.option <<- list(demonstration = demonstration) # set global
                    # accessible as .bcdyz.option in the server
                    # example of this is found in https://github.com/rstudio/shiny/issues/440
                    # https://stackoverflow.com/questions/31118236/how-to-set-global-variable-values-in-the-onstart-parameter-of-shiny-application
                  }, ...)
  # shiny::runApp(app, ...) - runApp does NOT pass through unicode e.g. Chinese/Greek un-modified!

}
