##### Billings module ##########################################

#' Appointments module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
billings_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
                    shinyWidgets::switchInput(
                      inputId = ns("printcopy_view"),
                      label = paste("<i class=\"fas fa-print\"></i>",
                                    "<i class=\"far fa-copy\"></i>",
                                    "  Print and Copy View"),
                      labelWidth = "12em",
                      width = "20em")
      ),
      shiny::column(3, offset = 5, # note that total 'column' width = 12
                    shinyWidgets::switchInput(
                      inputId = ns("allbillings_view"),
                      label = paste("Show all day's billings"),
                      labelWidth = "12em",
                      width = "20em")
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("billings_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

##### server side ##########################################

#' appointment list module - server
#'
#' list of appointments and billings
#' within selected range of dates and providers
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @return none
#'
billings_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  billings_list <- shiny::eventReactive(
    c(dM$billings_listR(),
      input$printcopy_view), {
      shiny::validate(
        shiny::need(dM$billings_listR(),
                    "No appointments in chosen range"),
        shiny::need(nrow(dM$billings_listR()) > 0,
                    "No appointments in chosen range")
      )

      billingslist <- dM$list_billings(lazy = TRUE,
                                       screentag = !input$printcopy_view,
                                       screentag_print = input$printcopy_view)

      return(billingslist)
    })

  shiny::observeEvent(input$allbillings_view, ignoreNULL = TRUE, {
    dM$own_billings <- !input$allbillings_view
  })

  styled_billings_list <- shiny::reactive({
    shiny::validate(
      shiny::need(billings_list(), "No appointments in selected range")
    )

    if (input$printcopy_view == TRUE) {
      # printable/copyable view
      datatable_styled(billings_list() %>>%
                         dplyr::select(Patient, Date, AppointmentTime, Status, VisitType,
                                       Provider, billingtag_print),
                       colnames = c('Billings' = 'billingtag_print'))
    } else {
      # fomantic/semantic tag view
      datatable_styled(billings_list() %>%
                         dplyr::select(Patient, Date, AppointmentTime, Status, VisitType,
                                       Provider, billingtag),
                       escape = c(5),
                       buttons = list('colvis'), # no copy/print buttons
                       colnames = c('Billings' = 'billingtag'))
    }
  })

  output$billings_table <- DT::renderDT({
    styled_billings_list()
  })
}
