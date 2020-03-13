library(shiny)
library(DTedit)
library(DT)
library(airtabler)
library(dMeasure)
library(dbConnection)
library(dbplyr)
library(dplyr)
library(pipeR)
library(markdown)
library(odbc)
library(rintrojs)
library(shinyFiles)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinytoastr)
library(sodium)
library(jsonlite)
library(R6)
library(configr)
library(stringi)
library(stringr)
library(tibble)
library(tidyr)
library(tidyselect)
library(yaml)
library(pool)
library(knitr)
library(rmarkdown)
library(printr)

library(DailyMeasure)

ui <- DailyMeasureUI()
server <- DailyMeasureServer

demonstration = FALSE

shiny::shinyApp(ui = ui, server = server,
                onStart = function() {
                  .bcdyz.option <<- list(demonstration = demonstration) # set global
                  # accessible as .bcdyz.option in the server
                  # example of this is found in https://github.com/rstudio/shiny/issues/440
                  # https://stackoverflow.com/questions/31118236/how-to-set-global-variable-values-in-the-onstart-parameter-of-shiny-application
                })