# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

##### cancer screening modules ##########################################

#' Cancer screen User Interface module
#'
#' Datatable with list of patients and cancer screening opportunities.
#' Includes 'printable' toggle, and selectable cancer screening dropdown.
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
cancerscreen_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        5,
        shinyWidgets::switchInput(
          inputId = ns("printcopy_view"),
          label = paste(
            "<i class=\"fas fa-print\"></i>",
            "</i><i class=\"far fa-copy\"></i>",
            " Print and Copy View"
          ),
          labelWidth = "12em",
          width = "20em"
        )
      ),
      shiny::column(2,
        offset = 3,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("include_uptodate"),
          choiceNames = c("Include up-to-date"),
          choiceValues = c(1),
          selected = 1,
          status = "primary",
          checkIcon = list(
            yes = shiny::icon("ok", lib = "glyphicon"),
            no = shiny::icon("remove", lib = "glyphicon")
          )
        )
      ),
      shiny::column(2,
        offset = 0, # note that total 'column' width = 12
        shiny::uiOutput(ns("cancerscreen_choice"))
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("cancerscreen_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL
    )
  )
}

##### server side #######

#' Cancer screening module server
#'
#' Chronic disease management items claimed, pending or unclaimed for appointment list
#'
#' @param input (as required by modules)
#' @param output (as required by modules)
#' @param session (as required by modules)
#' @param dM dMeasure R6 object
#'
#' @return None
cancerscreen_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  # Cancer screening types
  cancerscreen_names <- c("Bowel", "Cervical", "Breast")

  cancerscreen_chosen <- shiny::reactiveVal(
    cancerscreen_names
    # initially all choices selected
  )
  output$cancerscreen_choice <- shiny::renderUI({
    shinyWidgets::dropMenu(
      # placing the drop-down in the render UI (as opposed to module UI)
      # allows cancerscreen_chosen() to be defined at time of rendering
      shiny::actionButton(
        ns("choice_dropdown"),
        label = "Cancer screen settings",
        icon = shiny::icon("gear")
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("cancerscreen_chosen"), label = "Cancer Screen items shown",
          choices = cancerscreen_names,
          selected = cancerscreen_chosen(),
          status = "primary",
          checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
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
        cancerscreen_chosen(input$cancerscreen_chosen)
      }
    }
  )

  cancerscreen_list <- shiny::reactive({
    shiny::validate(
      shiny::need(dM$appointments_listR(), "No appointments defined"),
      shiny::need(nrow(dM$appointments_listR()) > 0, "No appointments in chosen range")
    )
    screenlist <- NULL
    # Bowel cancer
    if ("Bowel" %in% cancerscreen_chosen()) {
      screenlist <- rbind(
        screenlist,
        dM$list_fobt(
          appointments_list = dM$appointments_listR(),
          include_uptodate = (!is.null(input$include_uptodate)),
          screentag = TRUE, screentag_print = TRUE
        )
      )
    }
    # both HTML and printable versions of tags requested
    # Cervical cancer
    if ("Cervical" %in% cancerscreen_chosen()) {
      screenlist <- rbind(
        screenlist,
        dM$list_cst(
          appointments_list = dM$appointments_listR(),
          include_uptodate = (!is.null(input$include_uptodate)),
          screentag = TRUE, screentag_print = TRUE
        )
      )
    }

    # Breast cancer
    if ("Breast" %in% cancerscreen_chosen()) {
      screenlist <- rbind(
        screenlist,
        dM$list_mammogram(
          appointments_list = dM$appointments_listR(),
          include_uptodate = (!is.null(input$include_uptodate)),
          screentag = TRUE, screentag_print = TRUE
        )
      )
    }


    if (is.null(screenlist)) {
      return(screenlist)
    }

    screenlist <- screenlist %>>%
      dplyr::group_by(
        Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
        DOB, Age
      ) %>>%
      # gathers vaccination notifications on the same appointment into a single row
      dplyr::summarise(
        screentag = paste(screentag, collapse = ""),
        screentag_print = paste(screentag_print, collapse = ", ")
      ) %>>%
      # both HTML and printable versions of tags processed (and requested earlier)
      dplyr::ungroup()

    return(screenlist)
  })

  styled_cancerscreen_list <- shiny::reactive({
    shiny::validate(
      shiny::need(
        dM$appointments_listR(),
        "No appointments in selected range"
      ),
      shiny::need(
        nrow(dM$appointments_listR()) > 0,
        "No appointments in chosen range"
      ),
      shiny::need(
        cancerscreen_list(),
        "Choose at least one screening to display"
      )
    )
    dummy <- cancerscreen_list()

    if (input$printcopy_view == TRUE) {
      # printable/copyable view
      datatable_styled(cancerscreen_list() %>>%
        dplyr::select(c(
          "Patient", "AppointmentDate", "AppointmentTime",
          "Provider", "DOB", "Age", "screentag_print"
        )),
      colnames = c("Screening" = "screentag_print")
      )
    } else {
      # fomantic/semantic tag view
      datatable_styled(cancerscreen_list() %>>%
        dplyr::select(c(
          "Patient", "AppointmentDate", "AppointmentTime",
          "Provider", "DOB", "Age", "screentag"
        )),
      escape = c(7),
      copyHtml5 = NULL, printButton = NULL,
      downloadButton = NULL, # no copy/print buttons
      colnames = c("Screening" = "screentag")
      )
    }
  })

  output$cancerscreen_table <- DT::renderDT({
    styled_cancerscreen_list()
  })
}
