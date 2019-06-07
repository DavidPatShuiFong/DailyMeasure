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

  ##### Run the application ###########################################
  shinyApp(ui = DailyMeasureUI, server = DailyMeasureServer)

}
