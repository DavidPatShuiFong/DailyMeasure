#' DailyMeasure
#' (C) David Fong, 2019
#'
#' launches the DailyMeasure app
#' @return shiny application object
#'
#' @import shiny
#' @importFrom DT dataTableOutput renderDataTable dataTableProxy replaceData DTOutput
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shinydashboardPlus dashboardPagePlus dashboardHeaderPlus userOutput
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinytoastr useToastr
#' @importFrom shinycssloaders withSpinner
#'
#' @export
launchApp <- function () {

  ##### Configuration file ######################################################

  if (is.yaml.file('./DailyMeasure_cfg.yaml')) {
    # if config file exists and is a YAML-type file
    local_config <- read.config("./DailyMeasure_cfg.yaml") #  config in local location
  } else {
    # local config file does not exist. possibly first-run
    local_config <- list()
    local_config$config_file <- c("./DailyMeasure_cfg.sqlite")
    # main configuration file, could be set to 'common location'
    # write the (minimalist) local config file
    write.config(local_config, file.path = "./DailyMeasure_cfg.yaml", write.type = "yaml")
  }
  print(paste0("Local config:", local_config))


  ##### Run the application ###########################################
  shinyApp(ui = DailyMeasureUI, server = DailyMeasureServer)

}
