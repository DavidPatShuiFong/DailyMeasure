.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Daily Measure!")
  shiny::addResourcePath('icons',
                         system.file("www/assets/icons", package = "DailyMeasure"))
}
