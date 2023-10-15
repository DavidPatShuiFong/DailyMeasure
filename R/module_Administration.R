# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

###### administration modules ###################################################

#' administration_UI - admin module
#'
#' admin module
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
administration_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::tabBox(
      id = "tab_admin",
      title = "Administration",
      width = 12,
      height = "85vh",
      shiny::tabPanel(
        title = "Data Quality",
        width = 12,
        shiny::br(),
        admin_dataQuality_datatableUI(ns("data_quality"))
      ),
      shiny::tabPanel(
        title = "Result Management",
        width = 12,
        shiny::br(),
        admin_result_datatableUI(ns("result_management"))
      ),
      shiny::tabPanel(
        title = "Document Search",
        width = 12,
        shiny::br(),
        admin_document_datatableUI(ns("document_search"))
      ),
      shiny::tabPanel(
        title = "Visit (progress notes) search",
        width = 12,
        shiny::br(),
        admin_visit_datatableUI(ns("visit_search"))
      ),
      shiny::tabPanel(
        title = "Action Search",
        width = 12,
        shiny::br(),
        admin_action_datatableUI(ns("action_search"))
      ),
      shiny::tabPanel(
        title = "myHealth (PCEHR)",
        width = 12,
        shiny::br(),
        admin_pcehr_datatableUI(ns("pcehr_search"))
      )
    )
  )
}

admin_dataQuality_datatableUI <- function(id) {
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
      shiny::column(
        2, # note that total 'column' width = 12
        shiny::uiOutput(ns("dataQuality_choice"))
      )
    ),
    DT::DTOutput(ns("dataQuality_table"))
  )
}

admin_result_datatableUI <- function(id) {
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
                    offset = 2, # note that total 'column' width = 12
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("ignorePast_appt"),
                      checkIcon = list(
                        yes = shiny::icon("calendar-times"),
                        no = shiny::icon("calendar-alt")
                      ),
                      choices = c("Ignore Past Appointments"),
                      status = "primary",
                      width = "30em"
                    )
      ),
      shiny::column(
        2,
        offset = 2,
        shiny::uiOutput(ns("result_settings"))
      )
    ),
    DT::DTOutput(ns("result_table"))
  )
}

admin_document_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::textOutput(ns("document_search_text"))
      ),
      shiny::column(
        3, # note that total 'column' width = 12
        offset = 2,
        shiny::uiOutput(ns("document_search_choice"))
      )
    ),
    DT::DTOutput(ns("documentSearch_table"))
  )
}

admin_visit_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::textOutput(ns("visit_search_text"))
      ),
      shiny::column(
        3, # note that total 'column' width = 12
        offset = 2,
        shiny::uiOutput(ns("visit_search_choice"))
      )
    ),
    DT::DTOutput(ns("visitSearch_table"))
  )
}

admin_action_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::textOutput(ns("action_search_text"))
      ),
      shiny::column(
        3, # note that total 'column' width = 12
        offset = 2,
        shiny::uiOutput(ns("action_search_choice"))
      )
    ),
    DT::DTOutput(ns("actionSearch_table"))
  )
}

admin_pcehr_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3, # note that total 'column' width = 12
        offset = 6,
        shiny::uiOutput(ns("pcehr_choice"))
      )
    ),
    DT::DTOutput(ns("pcehr_table"))
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
administration <- function(input, output, session, dM) {
  ns <- session$ns

  # data quality
  shiny::callModule(admin_dataQuality_datatable, "data_quality", dM)

  # result management
  shiny::callModule(admin_result_datatable, "result_management", dM)

  shiny::callModule(admin_document_datatable, "document_search", dM)

  shiny::callModule(admin_visit_datatable, "visit_search", dM)

  shiny::callModule(admin_action_datatable, "action_search", dM)

  shiny::callModule(admin_pcehr_datatable, "pcehr_search", dM)
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
admin_dataQuality_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  dataQuality_chosen <- shiny::reactiveVal(
    dM$dataQuality_choices
  )
  output$dataQuality_choice <- renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        ns("dataQuality_choice_dropdown"),
        label = "Data Quality settings",
        icon = shiny::icon("gear")
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("dataQuality_chosen"), label = "Data Quality choices",
          choices = dM$dataQuality_choices,
          selected = dataQuality_chosen(),
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
    input$dataQuality_choice_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$dataQuality_choice_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        dataQuality_chosen(input$dataQuality_chosen)
      }
    }
  )

  dataQuality <-
    shiny::eventReactive(
      c(
        dM$appointments_listR(),
        dataQuality_chosen(),
        input$printcopy_view
      ),
      ignoreInit = TRUE, {
        # respond to appointments_listR()
        # when clinician or dates is changed
        appointments <- dM$list_dataQuality(
          lazy = TRUE,
          qualitytag = !input$printcopy_view,
          qualitytag_print = input$printcopy_view,
          chosen = dataQuality_chosen()
        ) %>>%
          dplyr::collect()
        # no need to re-calculate $appointments_list

        return(appointments)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  dataQuality_table <- shiny::reactive({
    if (!is.null(dM$appointments_list)) {
      if (input$printcopy_view == TRUE) {
        # printable/copyable view
        datatable_styled(
          dataQuality() %>>%
            dplyr::select(
              Patient, AppointmentDate, AppointmentTime,
              Provider, DOB, Age, qualitytag_print
            ),
          colnames = c("Data Quality" = "qualitytag_print"),
          extensions = c("Buttons", "Scroller"),
          scrollX = TRUE
        ) # don't collapse columns
      } else {
        # fomantic/semantic tag view
        datatable_styled(
          dataQuality() %>>%
            dplyr::select(
              Patient, AppointmentDate, AppointmentTime,
              Provider, DOB, Age, qualitytag
            ),
          colnames = c("Data Quality" = "qualitytag"),
          printButton = NULL, # no copy/print buttons
          copyHtml5 = NULL,
          downloadButton = NULL,
          scrollX = "100%", # allow horizontal scroll-bar
          extensions = c("Buttons", "Scroller"),
          # no 'Responsive' column collapsing
          escape = c(7)
        ) # only interpret HTML for last column
      }
    }
  })

  output$dataQuality_table <- DT::renderDT({
    dataQuality_table()
  },
  server = TRUE
  )
}

#' result management module - server
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
admin_result_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  action_names <- c(
    "No action", "Reception to advise",
    "Nurse to advise", "Doctor to advise",
    "Send routine reminder", "Non-urgent appointment",
    "Urgent appointment"
  )
  action_chosen <- shiny::reactiveVal(
    c("Non-urgent appointment", "Urgent appointment")
  )
  actioned_chosen <- shiny::reactiveVal(
    c("Any status")
  )
  actioned_date <- shiny::reactiveVal(
    Sys.Date()
  )
  output$result_settings <- renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("results_choice_dropdown"),
        label = "Result settings",
        icon = shiny::icon("gear")
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("action_chosen"),
          label = "Actions shown",
          choices = action_names,
          selected = action_chosen(),
          status = "primary",
          checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
        ),
        shinyWidgets::radioGroupButtons(
          inputId = ns("actioned_chosen"),
          label = "Actioned status",
          choices = c("Any status", "Not actioned", "Actioned", "Actioned before..."),
          selected = actioned_chosen(),
          status = "primary",
        ),
        shiny::dateInput(
          ns("actioned_date"),
          label = "Actioned before:",
          format = "D dd/M/yyyy",
          min = Sys.Date() - 6000, max = Sys.Date(),
          value = actioned_date()
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$results_choice_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$results_choice_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        action_chosen(input$action_chosen)
        actioned_chosen(input$actioned_chosen)
      }
    }
  )
  shiny::observeEvent(
    action_chosen(),
    ignoreNULL = FALSE, {
      # change the filter depending on the dropdown
      dM$filter_incoming_Action <- action_chosen()
    })

  shiny::observeEvent(
    actioned_chosen(), {
      if (actioned_chosen() == "Actioned before...") {
        shinyjs::enable("actioned_date")
        dM$filter_incoming_Actioned <- as.Date(actioned_date())
      } else {
        shinyjs::disable("actioned_date")
        switch(actioned_chosen(),
               "Any status" = {
                 dM$filter_incoming_Actioned <- NULL
               },
               "Not actioned" = {
                 dM$filter_incoming_Actioned <- FALSE
               },
               "Actioned" = {
                 dM$filter_incoming_Actioned <- TRUE
               }
        )
      }
    })
  shiny::observeEvent(actioned_date(), {
    if (actioned_chosen() == "Actioned before...") {
      dM$filter_incoming_Actioned <- as.Date(actioned_date())
    }
  })

  shiny::observeEvent(input$ignorePast_appt, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$filter_incoming_ignorePast <- ("Ignore Past Appointments" %in% input$ignorePast_appt)
  })

  results <-
    shiny::eventReactive(
      c(
        dM$correspondence_filtered_namedR(),
        dM$investigations_filtered_namedR(),
        input$printcopy_view
      ),
      ignoreInit = TRUE, {
        # respond to investigations_filtered_namedR or correspondence_filtered_namedR
        # when clinician or dates is changed
        incoming <- dM$view_incoming(
          lazy = TRUE,
          screentag = !input$printcopy_view,
          screentag_print = input$printcopy_view
        ) %>>%
          dplyr::collect()
        # no need to re-calculate $appointments_billings

        return(incoming)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)

  result_management_table <- shiny::reactive({
    shiny::req(
      !is.null(dM$investigations_filtered),
      !is.null(dM$correspondence_filtered)
    )

    if (input$printcopy_view == TRUE) {
      # printable/copyable view
      datatable_styled(
        results() %>>%
          dplyr::select(
            Patient, RecordNo, DOB, Age,
            TestName, Reported, Checked, CheckedBy,
            Notation, Action, Actioned, Comment, labeltag_print
          ),
        colnames = c(
          "Patient", "RecordNo", "DOB", "Age",
          "Report",
          "Reported", "Checked", "Checked By",
          "Notation", "Action", "Actioned", "Comment",
          "Appointments"
        ),
        extensions = c("Buttons", "Scroller"),
        scrollX = TRUE
      ) # don't collapse columns
    } else {
      # fomantic/semantic tag view
      datatable_styled(
        results() %>>%
          dplyr::select(
            patienttag, RecordNo,
            testtag, Checked, CheckedBy,
            Notation, Action, Actioned, Comment, labeltag
          ),
        colnames = c(
          "Patient", "RecordNo",
          "Report", "Checked", "Checked By",
          "Notation", "Action", "Actioned", "Comment",
          "Appointments"
        ),
        printButton = NULL, # no copy/print buttons
        copyHtml5 = NULL,
        downloadButton = NULL,
        scrollX = "100%", # allow horizontal scroll-bar
        extensions = c("Buttons", "Scroller"),
        # no 'Responsive' column collapsing
        escape = c(1, 3, 10)
      ) # only interpret HTML for some columns
    }
  })

  output$result_table <- DT::renderDT({
    result_management_table()
  },
  server = TRUE
  )
}

#' document search module - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to documents
#'
#' @return none
admin_document_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  search_text <- shiny::reactiveVal("discharge")
  # the default search string
  output$document_search_text <-
    shiny::renderText({paste("Search text: ", search_text())})
  output$document_search_choice <- renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("dataSearch_choice_dropdown"),
        icon = shiny::icon("gear"),
        label = "Document Search settings"
      ),
      shiny::tags$div(
        shiny::textInput(
          inputId = ns("documentSearch_chosen"),
          label = "Search text",
          value = search_text()
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$dataSearch_choice_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$dataSearch_choice_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        search_text(input$documentSearch_chosen)
      }
    }
  )

  documentSearch <-
    shiny::eventReactive(
      c(
        dM$cliniciansR(),
        dM$date_aR(),
        dM$date_bR(),
        search_text()
      ),
      ignoreInit = TRUE, ignoreNULL = FALSE, {
        # respond when clinician or dates is changed
        shiny::req(search_text()) # cannot be empty string

        ChosenUserID <- dM$UserFullConfig %>>%
          dplyr::filter(Fullname %in% dM$clinicians) %>>%
          dplyr::pull(UserID) %>>% c(-1)
        # add a dummy value if empty

        wildcard_search <- paste0("%", search_text(), "%")
        date_a <- dM$date_a
        date_b <- dM$date_b

        correspondence <- dM$db$correspondenceInRaw %>>%
          dplyr::filter(
            UserID %in% ChosenUserID |
              CheckedBy %in% ChosenUserID,
            dplyr::between(CorrespondenceDate, date_a, date_b) |
              dplyr::between(CheckDate, date_a, date_b),
            Category %like% wildcard_search |
              Subject %like% wildcard_search |
              Detail %like% wildcard_search |
              Comment %like% wildcard_search
          ) %>>%
          dplyr::select(InternalID, UserID, CheckedBy,
                        CorrespondenceDate, CheckDate, ActionDate,
                        Category, Subject, Detail, Comment) %>>%
          dplyr::collect()

        intID <- correspondence %>>% dplyr::pull(InternalID) %>>% c(-1)

        correspondence <- correspondence %>>%
          dplyr::left_join(
            dM$UserFullConfig %>>%
              dplyr::filter(UserID %in% ChosenUserID) %>>%
              dplyr::select(UserID, Fullname),
            by = c("UserID")
          ) %>>%
          dplyr::rename(Clinician = Fullname) %>>%
          dplyr::select(-c(UserID)) %>>%
          dplyr::left_join(
            dM$UserFullConfig %>>%
              dplyr::filter(UserID %in% ChosenUserID) %>>%
              dplyr::select(UserID, Fullname),
            by = c("CheckedBy" = "UserID")
          ) %>>%
          dplyr::rename(Checked = Fullname) %>>%
          dplyr::select(-c(CheckedBy)) %>>%
          dplyr::left_join(
            dM$db$patients %>>%
              dplyr::filter(InternalID %in% intID) %>>%
              dplyr::select(InternalID, ID = ExternalID,
                            Firstname, Surname, DOB) %>>%
              dplyr::collect(),
            by = c("InternalID")
          ) %>>%
          dplyr::mutate(
            Name = paste(Firstname, Surname),
            DOB = as.Date(DOB)
          ) %>>%
          dplyr::select(-c(InternalID, Firstname, Surname))

        return(correspondence)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  documentSearch_table <- shiny::reactive({

    datatable_styled(documentSearch() %>>%
                       dplyr::select(
                         Name, ID, DOB,
                         Clinician, Checked,
                         CorrespondenceDate, CheckDate, ActionDate,
                         Category, Subject, Detail, Comment
                       ),
                     extensions = c("Buttons", "Scroller"),
                     scrollX = TRUE
    )
  })

  output$documentSearch_table <- DT::renderDT({
    documentSearch_table()
  },
  server = TRUE
  )
}

#' visit search module - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to visits (visit progress notes, visit reasons)
#'
#' @return none
admin_visit_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  search_text <- shiny::reactiveVal("interpreter")
  # the default search string
  output$visit_search_text <-
    shiny::renderText({paste("Search text: ", search_text())})
  output$visit_search_choice <- renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("dataSearch_choice_dropdown"),
        icon = shiny::icon("gear"),
        label = "Visit Search settings"
      ),
      shiny::tags$div(
        shiny::textInput(
          inputId = ns("visitSearch_chosen"),
          label = "Search text",
          value = search_text()
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$dataSearch_choice_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$dataSearch_choice_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        search_text(input$visitSearch_chosen)
      }
    }
  )

  visitSearch <-
    shiny::eventReactive(
      c(
        dM$cliniciansR(),
        dM$date_aR(),
        dM$date_bR(),
        search_text()
      ),
      ignoreInit = TRUE, ignoreNULL = FALSE, {
        # respond when clinician or dates is changed
        shiny::req(search_text()) # cannot be empty string

        ChosenUserID <- dM$UserFullConfig %>>%
          dplyr::filter(Fullname %in% dM$clinicians) %>>%
          dplyr::pull(UserID) %>>% c(-1)
        # add a dummy value if empty

        wildcard_search <- paste0("%", search_text(), "%")
        date_a <- dM$date_a
        date_b <- dM$date_b

        visits <- dM$db$visits %>>%
          dplyr::filter(
            UserID %in% ChosenUserID,
            dplyr::between(VisitDate, date_a, date_b),
            VisitNotes %like% wildcard_search
          ) %>>%
          dplyr::select(InternalID, UserID, VisitDate, VisitNotes) %>>%
          dplyr::collect()

        intID <- visits %>>% dplyr::pull(InternalID) %>>% c(-1)

        visits <- visits %>>%
          dplyr::left_join(
            dM$UserFullConfig %>>%
              dplyr::filter(UserID %in% ChosenUserID) %>>%
              dplyr::select(UserID, Fullname),
            by = c("UserID")
          ) %>>%
          dplyr::rename(Clinician = Fullname) %>>%
          dplyr::select(-c(UserID)) %>>%
          dplyr::left_join(
            dM$db$patients %>>%
              dplyr::filter(InternalID %in% intID) %>>%
              dplyr::select(
                InternalID, ID = ExternalID,
                Firstname, Surname, DOB) %>>%
              dplyr::collect(),
            by = c("InternalID")
          ) %>>%
          dplyr::mutate(
            Name = paste(Firstname, Surname),
            DOB = as.Date(DOB)
          ) %>>%
          dplyr::select(-c(InternalID, Firstname, Surname))

        return(visits)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  visitSearch_table <- shiny::reactive({

    d <- visitSearch() %>>%
      dplyr::select(
        Name, ID, DOB,
        Clinician, VisitDate, VisitNotes
      )

    if (nrow(visitSearch()) > 0) {
      # strip_rtf doesn't handle empty dataframes and is not vectorised
      d$VisitNotes <- lapply(
        d$VisitNotes,
        function(x)
          strip_rtf(x)
      )
    }

    datatable_styled(
      d,
      extensions = c("Buttons", "Scroller"),
      scrollX = TRUE
    )
  })

  output$visitSearch_table <- DT::renderDT({
    visitSearch_table()
  },
  server = TRUE
  )
}

#' action search module - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to actions
#'
#' @return none
admin_action_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  search_text <- shiny::reactiveVal("covid")
  # the default search string
  search_dates <- shiny::reactiveVal(c("Added", "Due", "Performed"))
  # the default date ranges to search
  output$action_search_text <-
    shiny::renderText({paste("Search text: ", search_text())})
  output$action_search_choice <- renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("dataSearch_choice_dropdown"),
        icon = shiny::icon("gear"),
        label = "Action Search settings"
      ),
      shiny::tags$div(
        shiny::textInput(
          inputId = ns("actionSearch_chosen"),
          label = "Search text",
          value = search_text()
        ),
        shinyWidgets::pickerInput(
          inputId = ns("actionSearch_date"),
          label = "Date ranges to search",
          choices = c("Added", "Due", "Performed"),
          selected = search_dates(),
          multiple = TRUE
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$dataSearch_choice_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$dataSearch_choice_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        search_text(input$actionSearch_chosen)
        search_dates(input$actionSearch_date)
      }
    }
  )

  actionSearch <-
    shiny::eventReactive(
      c(
        dM$cliniciansR(),
        dM$date_aR(),
        dM$date_bR(),
        search_text(),
        search_dates()
      ),
      ignoreInit = TRUE, ignoreNULL = FALSE, {
        # respond when clinician or dates is changed
        shiny::req(search_text()) # cannot be empty string

        ChosenUserID <- dM$UserFullConfig %>>%
          dplyr::filter(Fullname %in% dM$clinicians) %>>%
          dplyr::pull(UserID) %>>% c(-1)
        # add a dummy value if empty

        wildcard_search <- paste0("%", search_text(), "%")
        date_a <- dM$date_a
        date_b <- dM$date_b
        search_added <- as.numeric("Added" %in% search_dates())
        # will be '1' if true. unfortunately, logicals don't work well in MSSQL
        search_due <- as.numeric("Due" %in% search_dates())
        search_performed <- as.numeric("Performed" %in% search_dates())

        actions <- dM$db$actions %>>%
          dplyr::filter(
            UserID %in% ChosenUserID,
            (search_added == 1 & dplyr::between(Added, date_a, date_b)) |
              (search_due == 1 & dplyr::between(DueDate, date_a, date_b)) |
              (search_performed == 1 & dplyr::between(Performed, date_a, date_b)),
            ActionText %like% wildcard_search |
            Comment %like% wildcard_search
          ) %>>%
          dplyr::select(
            InternalID, UserID,
            Added, DueDate, Performed,
            ActionText, Comment
          ) %>>%
          dplyr::collect()

        intID <- actions %>>% dplyr::pull(InternalID) %>>% c(-1)

        actions <- actions %>>%
          dplyr::left_join(
            dM$UserFullConfig %>>%
              dplyr::filter(UserID %in% ChosenUserID) %>>%
              dplyr::select(UserID, Fullname),
            by = c("UserID")
          ) %>>%
          dplyr::rename(Clinician = Fullname) %>>%
          dplyr::select(-c(UserID)) %>>%
          dplyr::left_join(
            dM$db$patients %>>%
              dplyr::filter(InternalID %in% intID) %>>%
              dplyr::select(InternalID, ID = ExternalID,
                            Firstname, Surname, DOB) %>>%
              dplyr::collect(),
            by = c("InternalID")
          ) %>>%
          dplyr::mutate(
            Name = paste(Firstname, Surname),
            DOB = as.Date(DOB)
          ) %>>%
          dplyr::select(-c(InternalID, Firstname, Surname))

        return(actions)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  actionSearch_table <- shiny::reactive({

    datatable_styled(
      actionSearch() %>>%
        dplyr::select(
          Name, ID, DOB,
          Clinician,
          Added, DueDate, Performed,
          ActionText, Comment
        ),
      extensions = c("Buttons", "Scroller"),
      scrollX = TRUE
    )
  })

  output$actionSearch_table <- DT::renderDT({
    actionSearch_table()
  },
  server = TRUE
  )
}

#' myHealth (pcehr) search module - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to documents
#'
#' @return none
admin_pcehr_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  pcehr_types <- c("Download", "Shared health summary", "Event")
  pcehr_chosen <- shiny::reactiveVal("Shared health summary")
  # the default search string
  output$pcehr_choice <- renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("pcehr_choice_dropdown"),
        icon = shiny::icon("gear"),
        label = "myHealth settings"
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("pcehr_chosen"),
          label = "myHealth document types",
          choices = pcehr_types,
          selected = pcehr_chosen(),
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
    input$pcehr_choice_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$pcehr_choice_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        pcehr_chosen(input$pcehr_chosen)
      }
    }
  )

  pcehr_documents <-
    shiny::eventReactive(
      c(
        dM$cliniciansR(),
        dM$date_aR(),
        dM$date_bR(),
        pcehr_chosen()
      ),
      ignoreInit = TRUE, ignoreNULL = FALSE, {
        # respond to appointments_listR()
        # when clinician or dates is changed
        shiny::req(pcehr_chosen()) # cannot be empty string

        ChosenUserID <- dM$UserFullConfig %>>%
          dplyr::filter(Fullname %in% dM$clinicians) %>>%
          dplyr::pull(UserID) %>>% c(-1)
        # add a dummy value if empty

        date_a <- dM$date_a
        date_b <- dM$date_b

        ChosenDocumentType <- c(-1)
        if ("Download" %in% pcehr_chosen()) {
          ChosenDocumentType <- c(ChosenDocumentType, 0)
        }
        if ("Shared health summary" %in% pcehr_chosen()) {
          ChosenDocumentType <- c(ChosenDocumentType, 1)
        }
        if ("Event" %in% pcehr_chosen()) {
          ChosenDocumentType <- c(ChosenDocumentType, 2)
        }

        documents <- dM$db$pcehrdocuments %>>%
          dplyr::filter(
            UserID %in% ChosenUserID |
              CreatedBy %in% ChosenUserID |
              UpdatedBy %in% ChosenUserID,
            dplyr::between(DocumentDate, date_a, date_b) |
              dplyr::between(Updated, date_a, date_b),
            DocumentType %in% ChosenDocumentType
          ) %>>%
          dplyr::select(InternalID, UserID, DocumentType, DocumentDate,
                        CreatedBy, UpdatedDate = Updated, UpdatedBy) %>>%
          dplyr::collect()

        intID <- documents %>>% dplyr::pull(InternalID) %>>% c(-1)

        documents <- documents %>>%
          dplyr::left_join(
            dM$UserFullConfig %>>%
              dplyr::filter(UserID %in% ChosenUserID) %>>%
              dplyr::select(UserID, Fullname),
            by = c("UserID")
          ) %>>%
          dplyr::rename(Clinician = Fullname) %>>%
          dplyr::select(-c(UserID)) %>>%
          dplyr::left_join(
            dM$UserFullConfig %>>%
              dplyr::filter(UserID %in% ChosenUserID) %>>%
              dplyr::select(UserID, Fullname),
            by = c("CreatedBy" = "UserID")
          ) %>>%
          dplyr::rename(Created = Fullname) %>>%
          dplyr::select(-c(CreatedBy)) %>>%
          dplyr::left_join(
            dM$UserFullConfig %>>%
              dplyr::filter(UserID %in% ChosenUserID) %>>%
              dplyr::select(UserID, Fullname),
            by = c("UpdatedBy" = "UserID")
          ) %>>%
          dplyr::rename(Updated = Fullname) %>>%
          dplyr::select(-c(UpdatedBy)) %>>%
          dplyr::left_join(
            dM$db$patients %>>%
              dplyr::filter(InternalID %in% intID) %>>%
              dplyr::select(InternalID, ID = ExternalID,
                            Firstname, Surname, DOB) %>>%
              dplyr::collect(),
            by = c("InternalID")
          ) %>>%
          dplyr::mutate(
            Name = paste(Firstname, Surname),
            DOB = as.Date(DOB)
          ) %>>%
          dplyr::select(-c(InternalID, Firstname, Surname)) %>>%
          dplyr::mutate(
            DocumentType = dplyr::case_when(
              DocumentType == 0 ~ "Downloaded",
              DocumentType == 1 ~ "Shared health summary",
              DocumentType == 2 ~ "Event"
            )
          )

        return(documents)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  pcehr_table <- shiny::reactive({

    datatable_styled(
      pcehr_documents() %>>%
        dplyr::select(
          Name, ID, DOB,
          Clinician, DocumentType, DocumentDate,
          Created, Updated, UpdatedDate
        ),
      extensions = c("Buttons", "Scroller"),
      scrollX = TRUE
    )
  })

  output$pcehr_table <- DT::renderDT({
    pcehr_table()
  },
  server = TRUE
  )
}

