# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
      id = ns("tab_conditions"),
      title = "Conditions",
      width = 12,
      height = "85vh",
      shiny::tabPanel(
        title = "Post-natal",
        width = 12,
        shiny::br(),
        shiny::div(
          id = "postnatal_datatable_wrapper",
          conditions_postnatal_datatableUI(ns("post_natal"))
        )
      ),
      shiny::tabPanel(
        title = "Asthma",
        width = 12,
        shiny::br(),
        shiny::div(
          id = "asthma_datatable_wrapper",
          conditions_asthma_datatableUI(ns("asthma"))
        )
      )
    )
  )
}

conditions_asthma_datatableUI <- function(id) {
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
        2,
        offset = 2,
        shinyWidgets::pickerInput(
          inputId = ns("appointment_contact_view"),
          choices = c("Appointment view", "Contact view"),
          choicesOpt = list(icon = c("fa fa-calendar-alt", "fa fa-handshake"))
        )
      ),
      shiny::column(
        2,
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
      )
    ),
    DT::DTOutput(ns("asthma_table"))
  )
}

pregnancy_outcome_levels <- c(
  "Not recorded", "Live birth",
  "Miscarriage", "Termination",
  "Ectopic", "Intra-uterine fetal death",
  "Stillbirth", "Hydatiform mole"
)
pregnancy_outcome_factor <- factor(pregnancy_outcome_levels)

conditions_postnatal_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        1,
        shinyWidgets::dropdown(
          shiny::tags$h3("Post-natal list"),
          shiny::tags$h4("Search for post-natal (or potentially post-natal) patients."),
          shiny::br(),
          "Filtered by number of",
          shiny::HTML("<strong>days post-natal</strong>"),
          ", whether a 'visit' recorded by the",
          "selected clinicians during the potential pregnancy period, and any",
          "recorded", shiny::HTML("<strong>pregnancy outcome</strong>"), ".",
          shiny::br(), shiny::br(),
          "The list only includes patients who have a recorded visit with a clinician during",
          "the past (280 + 30 + 'maximum days post-natal') days. The clinician list",
          "to include is chosen with the",
          shiny::HTML("<strong>Appointment Details</strong>"),
          "tab on the",
          "right side-bar. Applicable visit-types (by default, most 'in-person'",
          "visit types) are chosen in the",
          shiny::HTML("<strong>Contact details - Visit types shown</strong>"),
          "panel of the right side-bar.",
          shiny::br(), shiny::br(),
          "The list of post-natal patients is attached to appointments, within",
          "the selected",
          shiny::HTML("<strong>date range</strong>"),
          "(right side-bar, 'Selected date range') and",
          "with clinicians as chosen in the right side-bar.",
          shiny::br(), shiny::br(),
          "Pregnancies which have a defined end-date are 'post-natal'.",
          "By default, pregnancies which are after the due date,",
          "as defined by the EDC ('estimated date of confinement'), are also",
          "considered post-natal. If only pregnancies which have a defined",
          "end-date are to be included in this list, turn off the",
          shiny::HTML("<strong>Include EDC</strong>"), "switch.",
          status = "primary",
          size = "sm",
          width = "600px",
          icon = icon("question"),
          animate = shinyWidgets::animateOptions(
            enter = shinyWidgets::animations$fading_entrances$fadeIn,
            exit = shinyWidgets::animations$fading_exits$fadeOut
          ),
          tooltip = shinyWidgets::tooltipOptions(
            placement = "top",
            title = "Post-natal list help"
          )
        )
      ),
      shiny::column(2,
                    offset = 4,
                    shinyWidgets::dropdown(
                      inputId = ns("include_edc_dropdown"),
                      icon = icon("birthday-cake"),
                      label = "Include EDC",
                      shinyWidgets::switchInput(
                        inputId = ns("include_edc"),
                        value = TRUE,
                        label = paste(" Include EDC"),
                        labelWidth = "10em",
                        width = "16em"
                      ),
                      "Pregnancies which have a defined end-date are post-natal (by definition).",
                      shiny::br(), shiny::br(),
                      "If 'Include EDC' is", shiny::HTML("<strong>ON</strong>"),
                      "pregnancies which have no defined end-date ('Not recorded' outcome)",
                      "are also defined as post-natal after the defined",
                      "'estimated date of confinement (EDC)'"
                    )
      ),
      shiny::column(
        2,
        shiny::tags$style(text = "text/css", ".menu{dropdown-menu: dropdown-menu-left;}"),
        shinyWidgets::dropdown(
          inputId = ns("days_postnatal_dropdown"),
          right = TRUE,
          icon = icon("calendar"),
          label = "Days post-natal",
          shiny::sliderInput(
            ns("days_postnatal"),
            label = "Days postnatal",
            min = 0, max = 180,
            value = c(0, 180)
          ),
          "Number of days post-natal, minimum and maximum."
        )
      ),
      shiny::column(
        3, # note that total 'column' width = 12
        shinyWidgets::dropdown(
          inputId = ns("pregnancy_outcomes_dropdown"),
          icon = icon("baby"),
          right = TRUE,
          label = "Pregnancy outcomes",
          shinyWidgets::pickerInput(
            inputId = ns("pregnancy_outcomes"),
            label = "Pregnancy outcomes",
            choices = pregnancy_outcome_levels,
            selected = c("Not recorded", "Live birth"),
            options = list(style = "btn-primary"),
            multiple = TRUE
          ),
          "Pregnancy outcome is 'not recorded' until the pregnancy outcome is explicitly"
        )
      )
    ),
    DT::DTOutput(ns("postnatal_table"))
  )
}

#' conditions server
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
  callModule(conditions_asthma_datatable, "asthma", dM)
}

#' condition post-natal - server
#'
#' search for post-natal (or potentially post-natal) patients.
#'
#' Filtered by number of days post-natal, whether a 'visit' recorded by the
#' selected physicians during the potential pregnancy period, and any
#' recorded pregnancy outcome.
#'
#' Attached to appointments within the defined appointment search period.
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, visits, user interface elements
#'  and post-natal status database
#'
#' @return none
conditions_postnatal_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  postnatal <-
    shiny::eventReactive(
      c(
        input$days_postnatal,
        input$pregnancy_outcomes,
        input$include_edc,
        dM$cliniciansR(), dM$appointments_listR(),
        dM$date_aR(), dM$date_bR(),
        dM$visit_typeR()
      ),
      ignoreInit = FALSE, {
        # respond to appointments_listR()
        # when clinician or dates is changed
        shiny::req(dM$appointments_listR())
        shiny::req(dM$clinicians)

        today <- Sys.Date()
        search_back <- today - (280 + 30 + input$days_postnatal[2])
        # Naegele's rule (280) + a generous post-dates birth margin
        # + maximum number of days postnatal to check

        contactID <- dM$list_visits(
          date_from = search_back,
          date_to = today
        ) %>>%
          dplyr::pull(InternalID) %>>% unique()
        # accepts the 'default' clinicians choice
        # and visit types
        #
        # finds list of IDs who have been seen by clinicians within
        # the recent past (depending on current system date)
        # who may have been pregnant at time of visit

        d <- data.frame(InternalID = contactID) %>>%
          dplyr::mutate(Date = today)
        # we create a data frame with 'dummy' appointments on reference date
        # reference date for post-natal status is 'today'
        # if contactID is an empty vector, this should create a zero-row data.frame
        contactID <- c(contactID, -1)
        # need to add a 'dummy', empty vector causes errors later
        # if '-1' was added before 'd' was created, then an empty vector
        # creates a data.frame with one row (ID = -1), which could
        # happen when the clinical database isn't even open

        outcomes_string <- as.character(input$pregnancy_outcomes)
        # needs as.character conversion, for some reason,
        # outside the factor function
        outcomes <- as.integer(
          factor(outcomes_string,
                 levels = pregnancy_outcome_levels
          )
        ) - 1
        # converts the outcomes from a vector of strings to
        # a vector of integers
        # the 'outcome' code starts at '0', not '1',
        # so need to subtract one

        patientPregnancyDetails <-
          dM$postnatal_list(
            appointments = d,
            days_min = input$days_postnatal[1],
            days_max = input$days_postnatal[2],
            outcome = outcomes,
            include_edc = input$include_edc
          )
        # post-natal patients within required parameters
        # consists of -
        #  InternalID  EDCbyDate   EDCbyScan   EndDate     OutcomeCode
        postnatalID <- patientPregnancyDetails %>>%
          dplyr::pull(InternalID) %>>% c(-1) # need to add a dummy

        if (nrow(patientPregnancyDetails) > 0) {
          patientDetails <- dM$db$patients %>>%
            dplyr::filter(InternalID %in% postnatalID) %>>%
            dplyr::mutate(Name = paste(Firstname, Surname)) %>>%
            dplyr::select(InternalID, ExternalID, Name)
        } else {
          patientDetails <- data.frame(
            InternalID = numeric(0),
            ExternalID = character(0),
            Name = character(0)
          )
        }

        patientAppointments <- dM$appointments_listR() %>>%
          # accept defaults for $date_a, $date_b and $clinicians
          dplyr::filter(InternalID %in% postnatalID) %>>%
          dplyr::select(-Patient) # we don't need the name

        postnatalList <- patientDetails %>>%
          dplyr::left_join(patientPregnancyDetails, by = "InternalID", copy = TRUE) %>>%
          dplyr::left_join(patientAppointments, by = "InternalID", copy = TRUE) %>>%
          dplyr::select(-c(InternalID, Age)) %>>% # we don't need the internalID now
          dplyr::collect()

        return(postnatalList)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  postnatal_table <- shiny::reactive({
    shiny::req(!is.null(postnatal()))

    d <- postnatal() %>>%
      dplyr::mutate(Outcome = as.character(pregnancy_outcome_levels[OutcomeCode + 1])) %>>%
      # converts OutcomeCode to strings
      # need to add one because OutComesCode starts at zero, but the levels start at one!
      dplyr::rename(RecordNo = ExternalID) %>>%
      dplyr::select(Name, DOB, RecordNo, EDCbyDate, EDCbyScan, EndDate, Outcome, AppointmentDate, AppointmentTime, Status, Provider)

    datatable_styled(d,
                     extensions = c("Buttons", "Scroller"),
                     scrollX = TRUE
    ) # don't collapse columns
  })

  output$postnatal_table <- DT::renderDT({
    postnatal_table()
  },
  server = TRUE
  )
}

#' condition asthma - server
#'
#' search for asthma patients.
#'
#' Attached to appointments within the defined appointment search period.
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, visits, user interface elements
#'  and post-natal status database
#'
#' @return none
conditions_asthma_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  list_asthma <- function(
    contact = FALSE, # contact (if TRUE) or appointment system (if FALSE)
    date_from = NA,
    date_to = NA,
    clinicians = NA,
    min_contact = NA,
    min_date = NA,
    contact_type = NA,
    ignoreOld = FALSE,
    lazy = FALSE) {
    if (is.na(date_from)) {
      date_from <- dM$date_a
    }
    if (is.na(date_to)) {
      date_to <- dM$date_b
    }
    if (length(clinicians) == 1 && is.na(clinicians)) {
      # sometimes clinicians is a list, in which case it cannot be a single NA!
      # 'if' is not vectorized so will only read the first element of the list
      # but if clinicians is a single NA, then read $clinicians
      clinicians <- dM$clinicians
    }
    if (is.na(min_contact)) {
      min_contact <- dM$contact_min
    }
    if (is.na(min_date)) {
      min_date <- dM$contact_minDate
    }

    # no additional clinician filtering based on privileges or user restrictions

    if (all(is.na(clinicians)) || length(clinicians) == 0) {
      clinicians <- c("") # dplyr::filter does not work on zero-length list()
    }

    if (dM$emr_db$is_open()) {
      # only if EMR database is open
      if (dM$Log) {
        log_id <- self$dM$config_db$write_log_db(
          query = "asthma_condition",
          data = list(date_from, date_to, clinicians)
        )
      }

      if (contact) {
        if (!lazy) {
          dM$list_contact_asthma(
            date_from, date_to, clinicians,
            min_contact, min_date,
            contact_type,
            lazy
          )
        }
        asthma_list <- dM$contact_asthma_list %>>%
          dplyr::select(-c(Count, Latest)) # don't need these fields
        asthmaID <- asthma_list %>>% dplyr::pull(InternalID) %>>%
          c(-1) # make sure not empty vector, which is bad for SQL filter
      } else {
        if (!lazy) {
          dM$filter_appointments()
        }
        asthmaID <- c(dM$asthma_list(), -1)
        asthma_list <- dM$db$patients %>>%
          dplyr::filter(InternalID %in% asthmaID) %>>%
          dplyr::select(Firstname, Surname, InternalID) %>>%
          dplyr::collect() %>>%
          dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
          dplyr::select(Patient, InternalID)
        # derived from self$appointments_filtered
      }

      fluvaxList <- dM$influenzaVax_obs(copdID,
                                        date_from = ifelse(ignoreOld,
                                                           NA,
                                                           as.Date(-Inf, origin = "1970-01-01")
                                        ),
                                        # if ignoreOld, then influenza_vax will (given NA)
                                        # calculate date_from as fifteen months before date_to
                                        date_to = date_to
      )
      # returns InternalID, FluVaxName, FluvaxDate

      qim_asthma_list <- asthma_list %>>%
        dplyr::left_join(fluvaxList,
                         by = "InternalID",
                         copy = TRUE
        ) %>>%
        dplyr::left_join(dM$db$patients %>>%
                           dplyr::filter(InternalID %in% asthmaID) %>>%
                           dplyr::select(InternalID, DOB, Sex, RecordNo),
                         by = "InternalID",
                         copy = TRUE
        ) %>>%
        dplyr::select(
          Patient, InternalID, RecordNo, Sex,
          FluvaxDate, FluvaxName
        )

      if (dM$Log) {
        dM$config_db$duration_log_db(log_id)
      }

    }
    return(qim_asthma_list)
  }

  asthma <-
    shiny::eventReactive(
      c(
        dM$cliniciansR(), dM$appointments_listR(),
        dM$date_aR(), dM$date_bR(),
        dM$visit_typeR()
      ),
      ignoreInit = FALSE, {
        # respond to appointments_listR()
        # when clinician or dates is changed
        shiny::req(dM$appointments_listR())

        contactID <- dM$list_visits(
          date_from = search_back,
          date_to = today
        ) %>>%
          dplyr::pull(InternalID) %>>% unique()
        # accepts the 'default' clinicians choice
        # and visit types
        #
        # finds list of IDs who have been seen by clinicians within
        # the recent past (depending on current system date)
        # who may have been pregnant at time of visit


        if (nrow(patientPregnancyDetails) > 0) {
          patientDetails <- dM$db$patients %>>%
            dplyr::filter(InternalID %in% postnatalID) %>>%
            dplyr::mutate(Name = paste(Firstname, Surname)) %>>%
            dplyr::select(InternalID, ExternalID, Name)
        } else {
          patientDetails <- data.frame(
            InternalID = numeric(0),
            ExternalID = character(0),
            Name = character(0)
          )
        }

        patientAppointments <- dM$appointments_listR() %>>%
          # accept defaults for $date_a, $date_b and $clinicians
          dplyr::filter(InternalID %in% postnatalID) %>>%
          dplyr::select(-Patient) # we don't need the name

        postnatalList <- patientDetails %>>%
          dplyr::left_join(patientPregnancyDetails, by = "InternalID", copy = TRUE) %>>%
          dplyr::left_join(patientAppointments, by = "InternalID", copy = TRUE) %>>%
          dplyr::select(-c(InternalID, Age)) %>>% # we don't need the internalID now
          dplyr::collect()

        return(postnatalList)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  postnatal_table <- shiny::reactive({
    shiny::req(!is.null(postnatal()))

    d <- postnatal() %>>%
      dplyr::mutate(Outcome = as.character(pregnancy_outcome_levels[OutcomeCode + 1])) %>>%
      # converts OutcomeCode to strings
      # need to add one because OutComesCode starts at zero, but the levels start at one!
      dplyr::rename(RecordNo = ExternalID) %>>%
      dplyr::select(Name, DOB, RecordNo, EDCbyDate, EDCbyScan, EndDate, Outcome, AppointmentDate, AppointmentTime, Status, Provider)

    datatable_styled(d,
                     extensions = c("Buttons", "Scroller"),
                     scrollX = TRUE
    ) # don't collapse columns
  })

  output$postnatal_table <- DT::renderDT({
    postnatal_table()
  },
  server = TRUE
  )
}

#' condition asthma - server
#'
#' search for asthma patients.
#'
#' Attached to appointments within the defined appointment search period.
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, visits, user interface elements
#'  and post-natal status database
#'
#' @return none
conditions_asthma_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  asthma <-
    shiny::eventReactive(
      c(
        input$appointment_contact_view,
        input$include_uptodate,
        dM$cliniciansR(), dM$appointments_listR(),
        dM$date_aR(), dM$date_bR(),
        dM$contact_typeR(),
        dM$contact_minR(),
        dM$contact_minDateR()
      ),
      ignoreInit = FALSE, {
        # respond to appointments_listR()
        # when clinician or dates is changed
        shiny::req(dM$appointments_listR())

        asthma_list <- dM$list_asthma_details(
          contact = (input$appointment_contact_view == "Contact view"),
          include_uptodate = (!is.null(input$include_uptodate))
        )

        return(asthma_list)
      }
    )

  ### create tag-styled datatable (or 'printable' datatable)
  asthma_table <- shiny::reactive({
    shiny::req(!is.null(asthma()))
    shiny::req(dM$clinicians)

    df <- asthma()

    contact <- input$appointment_contact_view == "Contact view"
    if (contact) {
      df <- df %>>%
        dplyr::mutate(ComparisonDate = dM$date_b)
      # compare vax date to end of contact period
    } else {
      df <- df %>>%
        dplyr::mutate(ComparisonDate = AppointmentDate)
      # compare vax date to day of appointment
    }

    df <- df %>>%
      dplyr::left_join(dM$db$preventive_health %>>%
                         # those who have been removed from the reminder system for influenza
                         dplyr::filter(ITEMID == 1) %>>%
                         dplyr::rename(DeclinedFluvax = ITEMID),
                       by = "InternalID",
                       copy = TRUE
      ) %>>%
      dplyr::mutate(
        FluvaxStatus =
          dplyr::if_else(
            is.na(FluvaxDate) |
              (FluvaxDate == as.Date(-Inf, origin = "1970-01-01")),
            dplyr::if_else(
              is.na(DeclinedFluvax),
              "Never given", # no previous vax, not declined
              "Removed" # removed from immunization reminders
            ),
            paste0(
              dplyr::if_else(
                format(FluvaxDate, "%Y") == format(ComparisonDate, "%Y"),
                # this compares dates, is year same as end of contact period?
                # https://stackoverflow.com/questions/36568070/extract-year-from-date
                "Up-to-date",
                "Old"
              )
            )
          ),
        AsthmaPlanStatus =
          dplyr::if_else(
            is.na(PlanDate) |
              (PlanDate == as.Date(-Inf, origin = "1970-01-01")),
            "Never done", # no previous asthma plan\
            paste0(
              dplyr::if_else(
                dMeasure::interval(PlanDate, ComparisonDate)$year == 0,
                "Up-to-date", # less than twelve months
                "Old"
              )
            )
          )
      ) %>>%
      dplyr::select(-c(InternalID, DeclinedFluvax, ComparisonDate)) # no longer need these

    if (input$printcopy_view) {
      df <- df %>>%
        dplyr::mutate(
          vaxtag_print =
            paste0(
              "Influenza", # printable version of information
              dplyr::case_when(
                FluvaxStatus == "Never given" ~ " (Never given)",
                # no previous vax, not declined
                FluvaxStatus == "Removed" ~
                  " (Removed from influenza immunization reminders)",
                FluvaxStatus == "Up-to-date" ~
                  paste0(" (Up-to-date : ", FluvaxDate, ") "),
                FluvaxStatus == "Old" ~
                  paste0(" (DUE : ", FluvaxDate, ") ")
              )
            ),
          plantag_print =
            paste0(
              "Asthma plan", # printable version of information
              dplyr::case_when(
                AsthmaPlanStatus == "Never done" ~ " (Never done)",
                # no previous asthma plan
                AsthmaPlanStatus == "Up-to-date" ~
                  paste0(" (Up-to-date : ", PlanDate, ") "),
                AsthmaPlanStatus == "Old" ~
                  paste0(" (DUE : ", PlanDate, ") ")
              )
            )
        )
    } else {
      df <- df %>>%
        dplyr::mutate(
          vaxtag =
            dMeasure::semantic_tag(
              paste0(" Influenza "),
              colour =
                dplyr::case_when(
                  FluvaxStatus == "Never given" ~ c("red"),
                  # no previous vax, not declined
                  FluvaxStatus == "Removed" ~ c("purple"),
                  FluvaxStatus == "Up-to-date" ~ c("green"),
                  FluvaxStatus == "Old" ~ c("yellow")
                ),
              # red if not given, purple if removed from flu vax reminders
              # and green if has had the vax this year. yellow if 'old' vax
              popuphtml =
                paste0(
                  "<h4>",
                  dplyr::case_when(
                    FluvaxStatus == "Never given" ~ "Never given",
                    # no previous vax, not declined
                    FluvaxStatus == "Removed" ~
                      "Removed from influenza immunization reminders",
                    FluvaxStatus == "Up-to-date" ~
                      paste0(" Up-to-date : ", FluvaxDate),
                    FluvaxStatus == "Old" ~
                      paste0(" DUE : ", FluvaxDate) # old
                  ),
                  "</h4>"
                )
            ),
          plantag =
            dMeasure::semantic_tag(
              paste0(" Asthma plan "),
              colour =
                dplyr::case_when(
                  AsthmaPlanStatus == "Never done" ~ c("red"),
                  # no previous asthma plan
                  AsthmaPlanStatus == "Up-to-date" ~ c("green"),
                  AsthmaPlanStatus == "Old" ~ c("yellow")
                ),
              # red if not given
              # and green if has had the plan within year. yellow if 'old' plan
              popuphtml =
                paste0(
                  "<h4>",
                  dplyr::case_when(
                    AsthmaPlanStatus == "Never done" ~ "Never done",
                    # no previous asthma plan
                    AsthmaPlanStatus == "Up-to-date" ~
                      paste0(" Up-to-date : ", PlanDate),
                    AsthmaPlanStatus == "Old" ~
                      paste0(" DUE : ", PlanDate) # old
                  ),
                  "</h4>"
                )
            )
        )
    }

    df <- df %>>%
      dplyr::select(-c(FluvaxDate, FluvaxName, PlanDate,
                       FluvaxStatus, AsthmaPlanStatus))
    # flu vax and asthma plan information now incorporated
    #  into vaxtag/vaxtag_print and plantag, plantag_print

    if (input$printcopy_view == TRUE) {
      datatable_styled(df,
                       extensions = c("Buttons", "Scroller"),
                       scrollX = TRUE,
                       colnames = c("Vaccination" = "vaxtag_print",
                                    "Asthma Plan" = "plantag_print")
      ) # don't collapse columns
    } else {
      datatable_styled(df,
                       copyHtml5 = NULL, printButton = NULL,
                       downloadButton = NULL, # no copy/print buttons
                       escape = which(colnames(df) %in% c("vaxtag", "plantag")) - 1,
                       # escape the 'tag' columns so that HTML is interpreted
                       colnames = c("Vaccination" = "vaxtag",
                                    "Asthma Plan" = "plantag")
      )
    }
  })

  output$asthma_table <- DT::renderDT({
    asthma_table()
  },
  server = TRUE
  )
}
