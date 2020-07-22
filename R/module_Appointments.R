# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

##### Appointments module ##########################################

#' Appointments module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
appointments_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # print-view only (no semantic/fomantic buttons)
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("appointments_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL
    )
  )
}

##### server side #####################

#' appointment list module - server
#'
#' list of appointments within selected range of dates and providers
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM - dMeasure R6 object
#'
#' @return none
appointments_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  # fomantic/semantic UI definitions not required

  # appointment list
  # output$appointments_dt <- DT::renderDT({datatable_styled(
  #   dM$appointments_filtered_timeR() %>>%
  #     dplyr::select(Patient, AppointmentDate, AppointmentTime, Provider, Status))
  # },
  # server = FALSE)

  styled_appointments_list <- shiny::reactive({
    shiny::validate(
      shiny::need(
        dM$appointments_filtered_timeR(),
        "No appointments in selected range"
      )
    )
    shiny::req(dM$clinicians)

    datatable_styled(dM$appointments_filtered_timeR() %>>%
      dplyr::select(
        Patient, AppointmentDate, AppointmentTime,
        Provider, Status
      ))
  })

  output$appointments_table <- DT::renderDT({
    styled_appointments_list()
  })
}
