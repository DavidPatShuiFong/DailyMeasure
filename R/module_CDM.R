##### CDM (chronic disease management) modules ##########################################

#' Chronic Disease Management (CDM) module - UI function
#'
#' Display CDM status and opportunities within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
cdm_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
                    shinyWidgets::switchInput(
                      inputId = ns("printcopy_view"),
                      label = paste("<i class=\"fas fa-print\"></i>",
                                    "<i class=\"far fa-copy\"></i>",
                                    " Print and Copy View"),
                      labelWidth = "12em",
                      width = "20em")
      ),
      shiny::column(2, offset = 6, # note that total 'column' width = 12
                    shiny::uiOutput(ns("cdm_item_choice"))
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("cdm_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

#' Chronic disease management list module - server
#'
#' chronic disease management items claimed, pending or unclaimed for appointment list
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMCDM dMeasureCDM R6 object
#'  access to appointments lists, condition lists, history and EMR database
#'
#' @return none
cdm_datatable <- function(input, output, session, dMCDM) {
  ns <- session$ns

  # MBS (medicare benefits schedule) item numbers for CDM
  cdm_item <- data.frame(
    code = c(721, 723, 732, 703, 705, 707, 2517, 2521, 2525, 2546, 2552, 2558, 2700, 2701, 2715, 2717),
    name = c('GPMP', 'TCA', 'GPMP R/V', 'HA', 'HA', 'HA', 'DiabetesSIP', 'DiabetesSIP', 'DiabetesSIP',
             'AsthmaSIP', 'AsthmaSIP', 'AsthmaSIP', 'MHCP', 'MHCP', 'MHCP', 'MHCP')
  )

  cdm_item_names <- as.character(unique(cdm_item$name)) # de-factored

  output$cdm_item_choice <- renderUI({
    shinyWidgets::dropdown(
      input_id = "choice_dropdown",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("cdm_chosen"), label = "CDM items shown",
        choices = cdm_item_names,
        selected = setdiff(cdm_item_names, c("DiabetesSIP", "AsthmaSIP")),
        # DiabetesSIP and AsthmaSIP no longer valid items :(
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
      icon = icon("gear"),
      label = "CDM items shown"
    )
  })

  # filter to CDM item billed prior to (or on) the day of displayed appointments
  # only show most recent billed item in each category

  appointments_billings_cdm <-
    shiny::eventReactive(
      c(dMCDM$dM$appointments_filteredR(),
        input$cdm_chosen,
        input$printcopy_view), {
          shiny::validate(
            shiny::need(dMCDM$dMBillings$appointments_billingsR(),
                        "No appointments defined")
          )
          # respond to appointments_filteredR, since that is what is changed
          # when clinician or dates is changed

          original_date_b <- dMCDM$dM$date_b
          # $date_b might be changed by dMCDM$appointment_billings_cdm
          # due to user chosen not having subscription, and not able
          # to view the date range chosen

          appointments <- dMCDM$appointments_billings_cdm(
            cdm_chosen = input$cdm_chosen,
            lazy = TRUE, # no need to re-calculate $appointments_billings
            screentag = !input$printcopy_view,
            screentag_print = input$printcopy_view)

          if (original_date_b != dMCDM$dM$date_b) {
            # warning generated if dates have been changed as
            # the result of subscription
            shinytoastr::toastr_warning(
              message = paste("A chosen user has no subscription for chosen date range.",
                              "Dates changed (minimum one week old)."),
              position = "bottom-left",
              closeButton = TRUE,
              timeOut = 0)} # keep open until closed

          return(appointments)
        })

  ### create tag-styled datatable (or 'printable' datatable)

  cdm_styled_datatable <- shiny::reactive({
    if (!is.null(appointments_billings_cdm()) &
        !is.null(dMCDM$dM$appointments_filtered_timeR())) {
      if (input$printcopy_view == TRUE) {
        # printable/copyable view
        datatable_styled(dMCDM$dM$appointments_filtered_timeR() %>>%
                           dplyr::inner_join(appointments_billings_cdm(),
                                             by = c('InternalID', 'AppointmentDate',
                                                    'AppointmentTime', 'Provider')) %>>%
                           dplyr::select(Patient, AppointmentDate, AppointmentTime,
                                         Provider, cdm_print),
                         colnames = c('Patient', 'Appointment Date', 'Appointment Time',
                                      'Provider', 'CDM items'))
      } else {
        # fomantic/semantic tag view
        datatable_styled(dMCDM$dM$appointments_filtered_timeR() %>>%
                           dplyr::inner_join(appointments_billings_cdm(),
                                             by = c('InternalID', 'AppointmentDate',
                                                    'AppointmentTime', 'Provider')) %>>%
                           dplyr::select(Patient, AppointmentDate,
                                         AppointmentTime, Provider, cdm),
                         colnames = c('Patient', 'Appointment Date', 'Appointment Time',
                                      'Provider', 'CDM items'),
                         printButton = NULL, copyHtml5 = NULL,
                         downloadButton = NULL,  # no copy/print buttons
                         escape = c(5)) # only interpret HTML for last column
      }
    }
  })

  output$cdm_table <- DT::renderDT({
    cdm_styled_datatable()
  },
  server = TRUE)
}
