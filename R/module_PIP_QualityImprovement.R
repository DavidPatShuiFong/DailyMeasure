###### PIP QIM modules ###################################################

#' qim_UI - Practice Incentive Program Quality Improvement Measures module
#'
#' PIP QI module
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
qim_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::tabBox(
      id = "tab_admin",
      title = "Quality Improvement Measures",
      width = 12,
      height = "85vh",
      shiny::tabPanel(
        title = "Diabetes",
        width = 12,
        shiny::br(),
        qim_diabetes_UI(ns("qim_diabetes"))
      )
    )
  )
}

qim_diabetes_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
#      shiny::column(2, offset = 6, # note that total 'column' width = 12
#                    shinyWidgets::checkboxGroupButtons(
#                      inputId = ns("ignore_old"),
#                      checkIcon = list(yes = shiny::icon("calendar-times"),
#                                       no = shiny::icon("calendar-alt")),
#                      choices = c("Ignore old measurements"),
#                      status = "primary",
#                      width = "30em")),
#      shiny::column(2,
#                    shiny::uiOutput(ns("demographic_group"))),
#      shiny::column(2, # note that total 'column' width = 12
#                    shiny::uiOutput(ns("measure_group")))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("report_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )

}

#' Quality Improvement user interface
#'
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @return none
qim <- function(input, output, session, dM) {
  ns <- session$ns

  # result management
  callModule(qim_diabetes, "qim_diabetes", dM)
}

#' Quality Improvement - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_diabetes <- function(input, output, session, dM) {
  ns <- session$ns

#  output$action_choice <- renderUI({
#    shinyWidgets::dropdown(
#      input_id = "demographic_group",
#      shinyWidgets::checkboxGroupButtons(
#        inputId = ns("demographic_chosen"), label = "Demographic grouping",
#        choices = dM$qim_demographicGroupings,
#        selected = dM$qim_demographicGroupings,
#        # all choices initially selected
#        status = "primary",
#        checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
#      icon = icon("gear"),
#      label = "Demographic groupings"
#    )
#  })
#  observeEvent(input$demographic_chosen, {
#    # change the filter depending on the dropdown
#    dM$qim_demographic_group <- input$demographic_chosen
#  })

#  output$measure_group <- renderUI({
#    shinyWidgets::dropdown(
#      input_id = "measure_group_dropdown",
#      icon = icon("gear"),
#      label = "Diabetes Measures",
#      shinyWidgets::checkboxGroupButtons(
#        inputId = ns("measure_chosen"), label = "Measures Chosen",
#        choices = dM$qim_diabetes_measureTypes,
#        selected = dM$qim_diabetes_measureTypes,
        # initially all chosen
#        status = "primary",
#      )
#    )
#  })
#  observeEvent(input$measure_chosen, {
#    dM$qim_diabetes_measure <- input$measure_chosen
#  })

#  observeEvent(input$ignore_old, ignoreNULL = FALSE, {
#    # if selected, will filter out appointments older than current date
#    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
#  })

  results <-
    shiny::eventReactive(
      c(dM$qim_diabetes_reportR()), ignoreInit = TRUE, {
          # respond to changes in $qim_diabetes_reportR
          # when clinician or dates is changed
          report <- dM$qim_diabetes_report

          return(report)
        })

  output$report_table <- DT::renderDT({
    results()
  },
  server = TRUE)
}
