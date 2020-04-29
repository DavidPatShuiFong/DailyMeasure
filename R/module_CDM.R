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
        offset = 2, # note that total 'column' width = 12
        shiny::uiOutput(ns("cdm_item_choice"))
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("cdm_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL
    )
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
    code = c(
      721, 92024, 92068, 723, 92025, 92069, 732, 92028, 92072,
      703, 705, 707,
      2517, 2521, 2525,
      2546, 2552, 2558,
      2700, 2701, 92112, 92124, 92113, 92125,
      2715, 2717, 92116, 92128, 92117, 92129
    ),
    name = c(
      "GPMP", "GPMP", "GPMP", "TCA", "TCA", "TCA", "GPMP R/V", "GPMP R/v", "GPMP R/V",
      "HA", "HA", "HA",
      "DiabetesSIP", "DiabetesSIP", "DiabetesSIP",
      "AsthmaSIP", "AsthmaSIP", "AsthmaSIP",
      "MHCP", "MHCP", "MHCP", "MHCP", "MHCP", "MHCP",
      "MHCP", "MHCP", "MHCP", "MHCP", "MHCP", "MHCP"
    )
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
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),
      icon = icon("gear"),
      label = "CDM items shown"
    )
  })

  # filter to CDM item billed prior to (or on) the day of displayed appointments
  # only show most recent billed item in each category

  billings_cdm_list <-
    shiny::eventReactive(
      c(
        dMCDM$dM$appointments_filteredR(),
        dMCDM$dM$contact_count_listR(),
        input$cdm_chosen,
        input$appointment_contact_view,
        input$printcopy_view
      ), {
        shiny::validate(
          shiny::need(
            dMCDM$dMBillings$appointments_billingsR(),
            "No appointments defined"
          )
        )
        # respond to appointments_filteredR, since that is what is changed
        # when clinician or dates is changed

        if (input$appointment_contact_view == "Appointment view") {
          intID <- NULL
        } else {
          intID <- dMCDM$dM$contact_count_listR() %>>%
            dplyr::pull(InternalID)
        }

        billings_cdm_list <- dMCDM$billings_cdm(
          # called with 'appointment' method if intID = NULL,
          # otherwise called with 'intID' defined
          intID = intID, intID_Date = Sys.Date(),
          cdm_chosen = input$cdm_chosen,
          lazy = TRUE, # no need to re-calculate $appointments_billings
          screentag = !input$printcopy_view,
          screentag_print = input$printcopy_view
        )

        return(billings_cdm_list)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)

  cdm_styled_datatable <- shiny::reactive({
    shiny::validate(
      shiny::need(
        dMCDM$dM$appointments_filtered_timeR(),
        "No appointments in selected range"
      )
    )
    if (!is.null(billings_cdm_list()) &
      !is.null(dMCDM$dM$appointments_filtered_timeR())) {
      if (input$appointment_contact_view == "Appointment view") {
        cdm_list <- dMCDM$dM$appointments_filtered_timeR() %>>%
          dplyr::inner_join(billings_cdm_list(),
            by = c(
              "InternalID", "AppointmentDate",
              "AppointmentTime", "Provider"
            )
          )
        if (input$printcopy_view == TRUE) {
          # printable/copyable view
          datatable_styled(cdm_list %>>%
            dplyr::select(
              Patient, AppointmentDate, AppointmentTime,
              Provider, cdm_print
            ),
          colnames = c(
            "Patient", "Appointment Date", "Appointment Time",
            "Provider", "CDM items"
          )
          )
        } else {
          # fomantic/semantic tag view
          datatable_styled(cdm_list %>>%
            dplyr::select(
              Patient, AppointmentDate, AppointmentTime,
              Provider, cdm
            ),
          colnames = c(
            "Patient", "Appointment Date", "Appointment Time",
            "Provider", "CDM items"
          ),
          printButton = NULL, copyHtml5 = NULL,
          downloadButton = NULL, # no copy/print buttons
          escape = c(5)
          ) # only interpret HTML for last column
        }
      } else { # 'Contact view'
        intID <- billings_cdm_list() %>>%
          dplyr::pull(InternalID)
        cdm_list <- billings_cdm_list() %>>%
          dplyr::left_join(dMCDM$dM$db$patients %>>% # attach names etc.
            dplyr::filter(InternalID %in% c(intID, -1)) %>>%
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
          )
        if (input$printcopy_view == TRUE) {
          # printable/copyable view
          datatable_styled(cdm_list %>>%
            dplyr::select(
              Patient, ExternalID, DOB, Age,
              HomePhone, MobilePhone, cdm_print
            ),
          colnames = c(
            "Patient", "ExternalID", "DOB", "Age",
            "Home Phone", "Mobile Phone", "CDM items"
          )
          )
        } else {
          # fomantic/semantic tag view
          datatable_styled(cdm_list %>>%
            dplyr::select(
              Patient, ExternalID, DOB, Age,
              HomePhone, MobilePhone, cdm
            ),
          colnames = c(
            "Patient", "ExternalID", "DOB", "Age",
            "Home Phone", "Mobile Phone", "CDM items"
          ),
          printButton = NULL, copyHtml5 = NULL,
          downloadButton = NULL, # no copy/print buttons
          escape = c(7)
          ) # only interpret HTML for last column
        }
      }
    }
  })

  output$cdm_table <- DT::renderDT({
    cdm_styled_datatable()
  },
  server = TRUE
  )
}
