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
      shiny::column(4,
                    shinyWidgets::switchInput(
                      inputId = ns("printcopy_view"),
                      label = "<i class=\"fas fa-print\"></i> </i><i class=\"far fa-copy\"></i>  Print and Copy View",
                      labelWidth = "100%")
      ),
      shiny::column(2, offset = 6, # note that total 'column' width = 12
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

zostavax_list <- function(appointments_list, dM) {
  # return datatable of appointments where Zostavax is recommended (might already be given)
  #  Patient, InternalID, AppointmentDate, ApppointmentTime, Provider, DOB, Age
  #  vaxtag, vaxtag_print (these two are the 'semantic' tags and printable tags)
  # input - appointment_list - reactive of appointment list
  # input - dM - access to Best Practice EMR database

  return(dM$zostavax_list(vaxtag = TRUE, vaxtag_print = TRUE))

}

influenza_list <- function(appointments_list, dM) {
  # return datatable of appointments where influenza is recommended (might already be given)
  #  Patient, InternalID, AppointmentDate, ApppointmentTime, Provider, DOB, Age
  #  vaxtag, vaxtag_print (these two are the 'semantic' tags and printable tags)
  # input - dM - access to Best Practice EMR database

  return(dM$influenza_list(vaxtag = TRUE, vaxtag_print = TRUE))

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
#' @include fomantic_definitions.R calculation_definitions.R
#'
#' @return none
#'
vax_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  vax_names <- c("Zostavax", "Influenza")

  output$vax_item_choice <- shiny::renderUI({
    shinyWidgets::dropdown(
      inputid = "choice_dropdown",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("vax_chosen"), label = "Vaccination items shown",
        choices = vax_names, selected = vax_names,
        # all choices initially selected
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
      icon = icon("gear"),
      label = "Vaccination items shown"
    )
  })

  vax_list <- shiny::eventReactive(
    c(dM$appointments_listR(), input$vax_chosen), {
      shiny::validate(
        shiny::need(dM$appointments_listR(),
                    "No appointments in chosen range"),
        shiny::need(nrow(dM$appointments_listR())>0,
                    "No appointments in chosen range")
      )

      vlist <- NULL
      # Zostavax (herpes zoster 'shingles' vaccine)
      if ("Zostavax" %in% input$vax_chosen)
      {vlist <- rbind(vlist, zostavax_list(appointments_list, dM))}
      # influenza
      if ("Influenza" %in% input$vax_chosen)
      {vlist <- rbind(vlist, influenza_list(appointments_list, dM))}

      if (!is.null(vlist)) {
        vlist <- vlist %>>%
          dplyr::group_by(Patient, InternalID, AppointmentDate, AppointmentTime,
                          Provider, DOB, Age) %>>%
          # gathers vaccination notifications on the same appointment into a single row
          dplyr::summarise(vaxtag = paste(vaxtag, collapse = ""),
                           vaxtag_print = paste(vaxtag_print, collapse = ", ")) %>>%
          dplyr::ungroup()
      }
      vlist
    })

  styled_vax_list <- shiny::reactive({
    shiny::validate(
      shiny::need(dM$appointments_listR(),
                  "No appointments in selected range"),
      shiny::need(input$vax_chosen,
                  "Choose at least one vaccination to display")
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
                       dom = 'frltip', # no copy/print buttons
                       colnames = c('Vaccination' = 'vaxtag'))
    }
  })

  output$vax_table <- DT::renderDT({
    styled_vax_list()
  })
}
