# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
