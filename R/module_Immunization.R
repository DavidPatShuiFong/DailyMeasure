##### Immunization modules ##########################################

#' immunization module - UI function
#'
#' Display immunization opportunities within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
vax_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(5,
                    shinyWidgets::switchInput(
                      inputId = ns("printcopy_view"),
                      label = paste("<i class=\"fas fa-print\"></i>",
                                    "<i class=\"far fa-copy\"></i>",
                                    " Print and Copy View"),
                      labelWidth = "12em",
                      width = "20em")
      ),
      shiny::column(2, offset = 5, # note that total 'column' width = 12
                    shiny::uiOutput(ns("vax_item_choice"))
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("vax_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

#' immunization module - UI function
#'
#' vaccinations done, pending or never done for appointment list
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 objection
#'
#' @return none
#'
vax_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  output$vax_item_choice <- shiny::renderUI({
    shinyWidgets::dropdown(
      inputid = "choice_dropdown",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("vax_chosen"), label = "Vaccination items shown",
        choices = dM$vaccine_choices,
        selected = dM$vaccine_choices,
        # all choices initially selected
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
      icon = icon("gear"),
      label = "Vaccination items shown"
    )
  })

  vax_list <- shiny::eventReactive(
    c(dM$appointments_listR(),
      input$vax_chosen,
      input$printcopy_view), ignoreInit = TRUE, {
        shiny::validate(
          shiny::need(dM$appointments_listR(),
                      "No appointments in chosen range"),
          shiny::need(nrow(dM$appointments_listR()) > 0,
                      "No appointments in chosen range")
        )

        vlist <- dM$list_vax(lazy = TRUE,
                             vaxtag = !input$printcopy_view,
                             vaxtag_print = input$printcopy_view,
                             chosen = input$vax_chosen) %>>%
          dplyr::collect()
        return(vlist)

      })

  styled_vax_list <- shiny::reactive({
    shiny::validate(
      shiny::need(dM$appointments_listR(),
                  "No appointments in selected range")
    )
    dummy <- vax_list()

    if (input$printcopy_view == TRUE) {
      # printable/copyable view
      datatable_styled(vax_list() %>>%
                         dplyr::select(Patient, AppointmentDate, AppointmentTime,
                                       Provider, DOB, Age, vaxtag_print),
                       colnames = c('Vaccination' = 'vaxtag_print'))
    } else {
      # fomantic/semantic tag view
      datatable_styled(vax_list() %>>%
                         dplyr::select(Patient, AppointmentDate, AppointmentTime,
                                       Provider, DOB, Age, vaxtag),
                       escape = c(7),
                       copyHtml5 = NULL, printButton = NULL,
                       downloadButton = NULL, # no copy/print buttons
                       colnames = c('Vaccination' = 'vaxtag'))
    }
  })

  output$vax_table <- DT::renderDT({
    styled_vax_list()
  })
}
