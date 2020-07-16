# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
      shiny::column(
        4,
        shinyWidgets::switchInput(
          inputId = ns("printcopy_view"),
          label = paste(
            "<i class=\"fas fa-print\"></i>",
            "<i class=\"far fa-copy\"></i>",
            " Print and Copy View"
          ),
          labelWidth = "12em",
          width = "20em"
        )
      ),
      shiny::column(2,
        offset = 2,
        shinyWidgets::pickerInput(
          inputId = ns("appointment_contact_view"),
          choices = c("Appointment view", "Contact view"),
          choicesOpt = list(icon = c("fa fa-calendar-alt", "fa fa-handshake"))
        )
      ),
      shiny::column(2,
        offset = 0,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("include_uptodate"),
          choiceNames = c("Include up-to-date"),
          choiceValues = c(1),
          selected = 1,
          status = "primary",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon")
          )
        )
      ),
      shiny::column(2,
        offset = 0, # note that total 'column' width = 12
        shiny::uiOutput(ns("vax_item_choice"))
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("vax_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL
    )
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

  vax_chosen <- shiny::reactiveVal(
    dM$vaccine_choices
  )
  output$vax_item_choice <- shiny::renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("choice_dropdown"),
        label = "Vaccination settings",
        icon = shiny::icon("gear")
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          label = "Vaccination items shown",
          inputId = ns("vax_chosen"),
          choices = dM$vaccine_choices,
          selected = dM$vaccine_choices,
          # all choices initially selected
          status = "primary",
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$choice_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$choice_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        vax_chosen(input$vax_chosen)
      }
    }
  )

  vax_list <- shiny::eventReactive(
    c(
      dM$appointments_listR(),
      vax_chosen(),
      input$include_uptodate,
      input$printcopy_view
    ),
    ignoreInit = FALSE, ignoreNULL = FALSE, {
      shiny::validate(
        shiny::need(
          dM$appointments_listR(),
          "No appointments in chosen range"
        ),
        shiny::need(
          nrow(dM$appointments_listR()) > 0,
          "No appointments in chosen range"
        )
      )

      vlist <- dM$list_vax(
        lazy = TRUE,
        include_uptodate = !is.null(input$include_uptodate),
        vaxtag = !input$printcopy_view,
        vaxtag_print = input$printcopy_view,
        chosen = vax_chosen()
      ) %>>%
        dplyr::collect()
      return(vlist)
    }
  )

  styled_vax_list <- shiny::reactive({
    dummy <- vax_list()

    if (input$printcopy_view == TRUE) {
      # printable/copyable view
      datatable_styled(vax_list() %>>%
          dplyr::select(
            Patient, AppointmentDate, AppointmentTime,
            Provider, DOB, Age, vaxtag_print
          ),
        colnames = c("Vaccination" = "vaxtag_print")
      )
    } else {
      # fomantic/semantic tag view
      datatable_styled(vax_list() %>>%
          dplyr::select(
            Patient, AppointmentDate, AppointmentTime,
            Provider, DOB, Age, vaxtag
          ),
        escape = c(7),
        copyHtml5 = NULL, printButton = NULL,
        downloadButton = NULL, # no copy/print buttons
        colnames = c("Vaccination" = "vaxtag")
      )
    }
  })

  #-------------------------------------------------
  # contact vaccination list

  vax_contact_list <- shiny::eventReactive(
    c(
      dM$contact_count_listR(),
      vax_chosen(),
      input$include_uptodate,
      input$printcopy_view
    ),
    ignoreInit = FALSE, ignoreNULL = FALSE, {
      shiny::validate(
        shiny::need(
          dM$contact_count_listR(),
          "No contacts in chosen range"
        ),
        shiny::need(
          nrow(dM$contact_count_listR()) > 0,
          "No contacts in chosen range"
        )
      )

      vlist <- dM$list_vax(
        intID = dM$contact_count_listR() %>>%
          dplyr::pull(InternalID),
        include_uptodate = !is.null(input$include_uptodate),
        lazy = TRUE,
        vaxtag = !input$printcopy_view,
        vaxtag_print = input$printcopy_view,
        chosen = vax_chosen()
      ) %>>%
        dplyr::collect()
      return(vlist)
    }
  )

  styled_vax_contact_list <- shiny::reactive({
    intID <- vax_contact_list() %>>% dplyr::pull(InternalID) %>>% c(-1)

    if (input$printcopy_view == TRUE) {
      # printable/copyable view
      datatable_styled(vax_contact_list() %>>%
          dplyr::left_join(dM$db$patients %>>%
              dplyr::filter(InternalID %in% intID) %>>%
              dplyr::select(
                InternalID, ExternalID, DOB,
                Firstname, Surname,
                HomePhone, MobilePhone
              ) %>>%
              dplyr::collect() %>>%
              dplyr::mutate(DOB = as.Date(DOB), Date = Sys.Date()) %>>%
              # initially Date is a dttm (POSIXt) object,
              # which makes the subsequent calc_age very slow,
              # and throws up warnings
              dplyr::mutate(
                Age = dMeasure::calc_age(DOB, Date),
                Patient = paste(Firstname, Surname)
              ),
            by = "InternalID"
          ) %>>%
          dplyr::select(
            Patient, ExternalID, DOB, Age,
            HomePhone, MobilePhone, vaxtag_print
          ),
        colnames = c("Vaccination" = "vaxtag_print")
      )
    } else {
      # fomantic/semantic tag view
      datatable_styled(vax_contact_list() %>>%
          dplyr::left_join(dM$db$patients %>>%
              dplyr::filter(InternalID %in% intID) %>>%
              dplyr::select(
                InternalID, ExternalID, DOB,
                Firstname, Surname,
                HomePhone, MobilePhone
              ) %>>%
              dplyr::collect() %>>%
              dplyr::mutate(DOB = as.Date(DOB), Date = Sys.Date()) %>>%
              # initially Date is a dttm (POSIXt) object,
              # which makes the subsequent calc_age very slow,
              # and throws up warnings
              dplyr::mutate(
                Age = dMeasure::calc_age(DOB, Date),
                Patient = paste(Firstname, Surname)
              ),
            by = "InternalID"
          ) %>>%
          dplyr::select(
            Patient, ExternalID, DOB, Age,
            HomePhone, MobilePhone, vaxtag
          ),
        escape = c(7),
        copyHtml5 = NULL, printButton = NULL,
        downloadButton = NULL, # no copy/print buttons
        colnames = c("Vaccination" = "vaxtag")
      )
    }
  })

  #----------------------------------------------------

  output$vax_table <- DT::renderDT({
    if (input$appointment_contact_view == "Appointment view") {
      styled_vax_list()
    } else {
      styled_vax_contact_list()
    }
  })
}
