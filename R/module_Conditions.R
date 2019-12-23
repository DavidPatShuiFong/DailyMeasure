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
      id = "tab_conditions",
      title = "Conditions",
      width = 12,
      height = "85vh",
      shiny::tabPanel(
        title = "Post-natal",
        width = 12,
        shiny::br(),
        conditions_postnatal_datatableUI(ns("post_natal"))
      )
    )
  )
}

pregnancy_outcome_levels <- c("Not recorded", "Live birth",
                              "Miscarriage", "Termination",
                              "Ectopic", "Intra-uterine fetal death",
                              "Stillbirth", "Hydatiform mole")
pregnancy_outcome_factor <- factor(pregnancy_outcome_levels)

conditions_postnatal_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(1,
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
                        exit = shinyWidgets::animations$fading_exits$fadeOut),
                      tooltip = shinyWidgets::tooltipOptions(placement = "top",
                                                             title = "Post-natal list help")
                    )),
      shiny::column(2, offset = 4,
                    shinyWidgets::dropdown(
                      inputId = ns("include_edc_dropdown"),
                      icon = icon("birthday-cake"),
                      label = "Include EDC",
                      shinyWidgets::switchInput(
                        inputId = ns("include_edc"),
                        value = TRUE,
                        label = paste(" Include EDC"),
                        labelWidth = "10em",
                        width = "16em"),
                      "Pregnancies which have a defined end-date are post-natal (by definition).",
                      shiny::br(), shiny::br(),
                      "If 'Include EDC' is", shiny::HTML("<strong>ON</strong>"),
                      "pregnancies which have no defined end-date ('Not recorded' outcome)",
                      "are also defined as post-natal after the defined",
                      "'estimated date of confinement (EDC)'"
                    )),
      shiny::column(2,
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
                        value = c(0,180)),
                      "Number of days post-natal, minimum and maximum."
                    )),
      shiny::column(3, # note that total 'column' width = 12
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
                        multiple = TRUE),
                      "Pregnancy outcome is 'not recorded' until the pregnancy outcome is explicitly"
                    )
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("postnatal_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
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
      c(input$days_postnatal,
        input$pregnancy_outcomes,
        input$include_edc,
        dM$cliniciansR(), dM$appointments_listR(),
        dM$date_aR(), dM$date_bR(),
        dM$visit_typeR()), ignoreInit = TRUE, {
          # respond to appointments_listR()
          # when clinician or dates is changed

          today <- Sys.Date()
          search_back <- today - (280 + 30 + input$days_postnatal[2])
          # Naegele's rule (280) + a generous post-dates birth margin
          # + maximum number of days postnatal to check

          contactID <- dM$list_visits(date_from = search_back,
                                      date_to = today) %>>%
            dplyr::pull(InternalID) %>>% unique() %>>%
            c(-1) # need to add a 'dummy', empty vector causes errors later
          # accepts the 'default' clinicians choice
          # and visit types
          #
          # finds list of IDs who have been seen by clinicians within
          # the recent past (depending on current system date)
          # who may have been pregnant at time of visit

          d <- data.frame(InternalID = contactID,
                          Date = today)
          # we create a data frame with 'dummy' appointments on reference date
          # reference date for post-natal status is 'today'

          outcomes_string <- as.character(input$pregnancy_outcomes)
          # needs as.character conversion, for some reason,
          # outside the factor function
          outcomes <- as.integer(
            factor(outcomes_string,
                   levels = pregnancy_outcome_levels)) - 1
          # converts the outcomes from a vector of strings to
          # a vector of integers
          # the 'outcome' code starts at '0', not '1',
          # so need to subtract one

          patientPregnancyDetails <-
            dM$postnatal_list(appointments = d,
                              days_min = input$days_postnatal[1],
                              days_max = input$days_postnatal[2],
                              outcome = outcomes,
                              include_edc = input$include_edc)
          # post-natal patients within required parameters
          postnatalID <- patientPregnancyDetails %>>%
            dplyr::pull(InternalID) %>>% c(-1) # need to add a dummy

          patientDetails <- dM$db$patients %>>%
            dplyr::filter(InternalID %in% postnatalID) %>>%
            dplyr::mutate(Name = paste(Firstname, Surname)) %>>%
            dplyr::select(InternalID, ExternalID, Name)

          patientAppointments <- dM$list_appointments() %>>%
            # accept defaults for $date_a, $date_b and $clinicians
            dplyr::filter(InternalID %in% postnatalID) %>>%
            dplyr::select(-Patient) # we don't need the name

          postnatalList <- patientDetails %>>%
            dplyr::left_join(patientPregnancyDetails, by = "InternalID", copy = TRUE) %>>%
            dplyr::left_join(patientAppointments, by = "InternalID", copy = TRUE) %>>%
            dplyr::select(-c(InternalID, Age)) %>>% # we don't need the internalID now
            dplyr::collect()

          return(postnatalList)
        })

  ### create tag-styled datatable (or 'printable' datatable)
  postnatal_table <- shiny::reactive({
    if (!is.null(dM$appointments_list)) {

      d <- postnatal() %>>%
        dplyr::mutate(Outcome = as.character(pregnancy_outcome_levels[OutcomeCode + 1])) %>>%
        # converts OutcomeCode to strings
        # need to add one because OutComesCode starts at zero, but the levels start at one!
        dplyr::rename(RecordNo = ExternalID) %>>%
        dplyr::select(Name, DOB, RecordNo, EDCbyDate, EDCbyScan, EndDate, Outcome, AppointmentDate, AppointmentTime, Status, Provider)

      datatable_styled(d,
                       extensions = c("Buttons", "Scroller"),
                       scrollX = TRUE) # don't collapse columns
    }
  })

  output$postnatal_table <- DT::renderDT({
    postnatal_table()
  },
  server = TRUE)
}
