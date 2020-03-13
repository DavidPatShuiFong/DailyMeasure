library(shiny)
library(DailyMeasure)

ui <- DailyMeasure::DailyMeasureUI()
server <- DailyMeasure::DailyMeasureServer

demonstration = FALSE

shiny::shinyApp(ui = ui, server = server,
                onStart = function() {
                  .bcdyz.option <<- list(demonstration = demonstration) # set global
                  # accessible as .bcdyz.option in the server
                  # example of this is found in https://github.com/rstudio/shiny/issues/440
                  # https://stackoverflow.com/questions/31118236/how-to-set-global-variable-values-in-the-onstart-parameter-of-shiny-application
                })