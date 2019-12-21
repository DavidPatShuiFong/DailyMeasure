###### conditions modules ###################################################

#' conditions_UI - admin module
#'
#' conditions module
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
conditions_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::tabBox(
      id = "tab_conditions",
      title = "Conditions",
      width = 12,
      height = "85vh",
      shiny::tabPanel(
        title = "Post-natal",
        width = 12,
        shiny::br(),
        conditions_postnatal_datatableUI(ns("post_natal"))
      )
    )
  )
}

conditions_postnatal_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, offset = 3,
                    shinyWidgets::switchInput(
                      inputId = ns("include_edc"),
                      label = paste(" Include EDC"),
                      labelWidth = "10em",
                      width = "16em")),
      shiny::column(3,
                    shiny::sliderInput(
                      ns("days_postnatal"),
                      label = "Days postnatal",
                      min = 0, max = 180,
                      value = c(0,180))),
      shiny::column(3, # note that total 'column' width = 12
                    shinyWidgets::pickerInput(
                      inputId = ns("pregnancy_outcomes"),
                      label = "Pregnancy outcomes",
                      choices = c("Not recorded", "Live birth",
                                  "Miscarriage", "Termination",
                                  "Ectopic", "Intra-uterine fetal death",
                                  "Stillbirth", "Hydatiform mole"),
                      selected = c("Not recorded", "Live birth"),
                      options = list(style = "btn-primary"),
                      multiple = TRUE))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("postnatal_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

#' administration user interface
#'
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @return none
conditions <- function(input, output, session, dM) {
  ns <- session$ns

  # data quality
  callModule(conditions_postnatal_datatable, "post_natal", dM)

}

#' data quality module - server
#'
#' with appointments
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @return none
conditions_postnatal_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  postnatal <-
    shiny::eventReactive(
      c(dM$appointments_listR()), ignoreInit = TRUE, {
          # respond to appointments_listR()
          # when clinician or dates is changed
        appointments <- NULL

        return(appointments)
      })

  ### create tag-styled datatable (or 'printable' datatable)
  postnatal_table <- shiny::reactive({
    if (!is.null(dM$appointments_list)) {
        datatable_styled(postnatal(),
                         extensions = c("Buttons", "Scroller"),
                         scrollX = TRUE) # don't collapse columns
    }
  })

  output$postnatal_table <- DT::renderDT({
    postnatal_table()
  },
  server = TRUE)
}
