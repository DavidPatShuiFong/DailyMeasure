.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to GPStat!, part of the Daily Measure software suite.")
  shiny::addResourcePath(
    "icons",
    system.file("www/assets/icons", package = "DailyMeasure")
  )
  shiny::addResourcePath(
    "themes",
    system.file("www/themes", package = "DailyMeasure")
  )
  # 'themes' referred to in fomantic/semantic CSS
  # note that 'DailyMeasure' needs to be loads as a library()
  # e.g. library(DailyMeausre), for these ResourcePaths to be valid
}
