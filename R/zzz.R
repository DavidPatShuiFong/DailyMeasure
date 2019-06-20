.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Daily Measure!")
  shiny::addResourcePath('icons',
                         system.file("www/assets/icons", package = "DailyMeasure"))
  shiny::addResourcePath('themes',
                         system.file("www/themes", package = "DailyMeasure"))
  # 'themes' referred to in fomantic/semantic CSS
}
