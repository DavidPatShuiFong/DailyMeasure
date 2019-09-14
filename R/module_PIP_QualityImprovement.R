###### PIP QIM modules ###################################################

#' qim_UI - Practice Incentive Program Quality Improvement Measures module
#'
#' PIP QI module
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
qim_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::tabBox(
      id = "tab_admin",
      title = shiny::tagList(
        shiny::div(style = "display:inline-block",
                   shiny::uiOutput(ns("demographic_group"))),
        shiny::div(style = "display:inline-block",
                   "Quality Improvement Measures")),
      width = 12,
      height = "85vh",
      shiny::tabPanel(
        title = "Active",
        width = 12,
        shiny::br(),
        qim_active_UI(ns("qim_active"))
      ),
      shiny::tabPanel(
        title = "Diabetes",
        width = 12,
        shiny::br(),
        qim_diabetes_UI(ns("qim_diabetes"))
      ),
      shiny::tabPanel(
        title = "Cervical Screening",
        width = 12,
        shiny::br(),
        qim_cst_UI(ns("qim_cst"))
      ),
      shiny::tabPanel(
        title = "15+",
        width = 12,
        shiny::br(),
        qim_15plus_UI(ns("qim_15plus"))
      ),
      shiny::tabPanel(
        title = "65+",
        width = 12,
        shiny::br(),
        qim_65plus_UI(ns("qim_65plus"))
      ),
      shiny::tabPanel(
        title = "COPD (Lung Disease)",
        width = 12,
        shiny::br(),
        qim_copd_UI(ns("qim_copd"))
      ),
      shiny::tabPanel(
        title = "Cardiovascular risk",
        width = 12,
        shiny::br(),
        qim_cvdRisk_UI(ns("qim_cvdRisk"))
      )
    )
  )
}

qim_active_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
                    shinyWidgets::switchInput(
                      inputId = ns("list_view"),
                      label = paste("<i class=\"fas fa-clipboard-list\"></i>",
                                    " List View"),
                      labelWidth = "10em",
                      width = "15em"))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("active_qim_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

qim_diabetes_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
                    shinyWidgets::pickerInput(
                      inputId = ns("list_view"),
                      choices = c("Report", "List", "Appointments"),
                      choicesOpt = list(icon = c("fa fa-book-reader",
                                                 "fa fa-clipboard-list",
                                                 "fa fa-calendar-check")),
                      options = list(`icon-base` = ""),
                      width = "15em")),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("ignore_old"),
                      checkIcon = list(yes = shiny::icon("calendar-times"),
                                       no = shiny::icon("calendar-alt")),
                      choices = c("Ignore old measurements"),
                      selected = c("Ignore old measurements"),
                      status = "primary",
                      width = "30em")),
      shiny::column(2, offset = 4, # note that total 'column' width = 12
                    shiny::uiOutput(ns("measure_group")))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("diabetes_qim_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

qim_cst_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
                    shinyWidgets::switchInput(
                      inputId = ns("list_view"),
                      label = paste("<i class=\"fas fa-clipboard-list\"></i>",
                                    " List View"),
                      labelWidth = "10em",
                      width = "15em")),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("ignore_old"),
                      checkIcon = list(yes = shiny::icon("calendar-times"),
                                       no = shiny::icon("calendar-alt")),
                      choices = c("Ignore old measurements"),
                      selected = c("Ignore old measurements"),
                      status = "primary",
                      width = "30em"))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("cst_qim_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

qim_15plus_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
                    shinyWidgets::switchInput(
                      inputId = ns("list_view"),
                      label = paste("<i class=\"fas fa-clipboard-list\"></i>",
                                    " List View"),
                      labelWidth = "10em",
                      width = "15em")),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("ignore_old"),
                      checkIcon = list(yes = shiny::icon("calendar-times"),
                                       no = shiny::icon("calendar-alt")),
                      choices = c("Ignore old measurements"),
                      selected = c("Ignore old measurements"),
                      status = "primary",
                      width = "30em")),
      shiny::column(2, offset = 4, # note that total 'column' width = 12
                    shiny::uiOutput(ns("measure_group")))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("fifteenplus_qim_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

qim_65plus_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
                    shinyWidgets::switchInput(
                      inputId = ns("list_view"),
                      label = paste("<i class=\"fas fa-clipboard-list\"></i>",
                                    " List View"),
                      labelWidth = "10em",
                      width = "15em")),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("ignore_old"),
                      checkIcon = list(yes = shiny::icon("calendar-times"),
                                       no = shiny::icon("calendar-alt")),
                      choices = c("Ignore old measurements"),
                      selected = c("Ignore old measurements"),
                      status = "primary",
                      width = "30em"))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("sixtyfiveplus_qim_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

qim_copd_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
                    shinyWidgets::switchInput(
                      inputId = ns("list_view"),
                      label = paste("<i class=\"fas fa-clipboard-list\"></i>",
                                    " List View"),
                      labelWidth = "10em",
                      width = "15em")),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("ignore_old"),
                      checkIcon = list(yes = shiny::icon("calendar-times"),
                                       no = shiny::icon("calendar-alt")),
                      choices = c("Ignore old measurements"),
                      selected = c("Ignore old measurements"),
                      status = "primary",
                      width = "30em"))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("copd_qim_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

qim_cvdRisk_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
                    shinyWidgets::switchInput(
                      inputId = ns("list_view"),
                      label = paste("<i class=\"fas fa-clipboard-list\"></i>",
                                    " List View"),
                      labelWidth = "10em",
                      width = "15em")),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("ignore_old"),
                      checkIcon = list(yes = shiny::icon("calendar-times"),
                                       no = shiny::icon("calendar-alt")),
                      choices = c("Ignore old measurements"),
                      selected = c("Ignore old measurements"),
                      status = "primary",
                      width = "30em")),
      shiny::column(2, offset = 4, # note that total 'column' width = 12
                    shiny::uiOutput(ns("groups")))
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("cvdRisk_qim_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}


#' Quality Improvement user interface
#'
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @return none
qim <- function(input, output, session, dM) {
  ns <- session$ns

  # result management
  callModule(qim_active, "qim_active", dM)
  callModule(qim_diabetes, "qim_diabetes", dM)
  callModule(qim_cst, "qim_cst", dM)
  callModule(qim_15plus, "qim_15plus", dM)
  callModule(qim_65plus, "qim_65plus", dM)
  callModule(qim_copd, "qim_copd", dM)
  callModule(qim_cvdRisk, "qim_cvdRisk", dM)

  output$demographic_group <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "demographic_group_dropdown",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("demographic_chosen"), label = "Demographic grouping",
        choices = dM$qim_demographicGroupings,
        selected = dM$qim_demographicGroupings,
        # all choices initially selected
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
      icon = icon("gear"),
      label = "Demographic groups"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })
  shiny::observeEvent(dM$qim_demographicGroupR(), ignoreNULL = FALSE, {
    # change the dropdown depending on the group chosen (possibly in another tab)
    shinyWidgets::updateCheckboxGroupButtons(
      session, inputId = "demographic_chosen",
      selected = dM$qim_demographicGroup)
  })

}

#' Quality Improvement 'active' list - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_active <- function(input, output, session, dM) {
  ns <- session$ns

  qim_active_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_active_listR(),
      dM$qim_active_reportR(),
      dM$qim_demographicGroupR()), ignoreInit = TRUE, {
        if (input$list_view) {
          datatable_styled(
            dM$qim_active_list %>>%
              dplyr::select(Patient, RecordNo,
                            Age5, Sex, Ethnicity,
                            MaritalStatus, Sexuality, Count) %>>%
              # re-orders the fields
              {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                             dM$qim_demographicGroup)
              # finds the demographics that were NOT chosen
              dplyr::select(., -remove_demographic)},
            columnDefs = list(list(targets = 1:2, visible = FALSE))
            # Patient Name and RecordNo hidden by default
            # can be shown again with 'colVis' button
          )
        } else {
          df <- dM$qim_active_report
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        }

      }
  )

  output$active_qim_table <- DT::renderDT({
    qim_active_datatable()
  },
  server = TRUE)

}

#' Quality Improvement diabetes - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_diabetes <- function(input, output, session, dM) {
  ns <- session$ns

  output$measure_group <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "measure_group_dropdown",
      icon = icon("gear"),
      label = "Diabetes Measures",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("measure_chosen"), label = "Measures Chosen",
        choices = dM$qim_diabetes_measureTypes,
        selected = dM$qim_diabetes_measureTypes,
        # initially all chosen
        status = "primary"
      )
    )
  })
  shiny::observeEvent(input$measure_chosen, ignoreNULL = FALSE, {
    dM$qim_diabetes_measure <- input$measure_chosen
  })

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  shiny::observeEvent(input$list_view, {
    if (input$list_view == "Appointments") {
      # This option only makes sense if 'Appointments' is a valid contact
      # and all types of appointments are valid contacts
      # and only one contact required to make a valid contact
      dM$contact_type <- union(dM$contact_type, "Appointments")
      dM$appointment_status <- dM$appointment_status_types
      dM$contact_min <- 1
      shinytoastr::toastr_warning(
        paste("Changing contact requirements to include appointments, all appointment types,",
              "and minimum contact of only 1 (one) required to be an 'active' patient."),
        closeButton = TRUE, timeOut = 0,
        position = "bottom-left", title = "Changing contact conditions")
    }
  })

  qim_diabetes_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_diabetes_listR(),
      dM$qim_diabetes_list_appointmentsR(),
      dM$qim_diabetes_reportR(),
      dM$qim_demographicGroupR()), ignoreInit = TRUE, {
        if (input$list_view == "List") {
          df <- dM$qim_diabetes_list %>>%
            dplyr::select(Patient, RecordNo,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          HbA1CDate, HbA1CValue, HbA1CUnits,
                          FluvaxDate, FluvaxName,
                          BPDate, BP) %>>%
            # re-orders the fields
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           dM$qim_demographicGroup)
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)} %>>%
            {if ("HbA1C" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(HbA1CDate, HbA1CValue, HbA1CUnits))}} %>>%
            {if ("Influenza" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(FluvaxDate, FluvaxName))}} %>>%
            {if ("BP" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(BPDate, BP))}}
          datatable_styled(df,
                           extensions = c('Buttons', 'Scroller'),
                           columnDefs = list(list(targets =
                                                    which(names(df) %in%
                                                            c("Patient", "RecordNo")),
                                                  # needs name by index as columns might be removed
                                                  # by demographic filters above
                                                  visible = FALSE)),
                           # Patient Name and RecordNo hidden by default
                           scrollX = TRUE) # this is a wide table
        } else if (input$list_view == "Report") {
          df <- dM$qim_diabetes_report
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        } else if (input$list_view == "Appointments") {
          df <- dM$qim_diabetes_list_appointments %>>%
            dplyr::select(Patient, RecordNo,
                          AppointmentDate, AppointmentTime, Provider, Status,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          HbA1CDate, HbA1CValue, HbA1CUnits,
                          FluvaxDate, FluvaxName,
                          BPDate, BP) %>>%
            # re-orders the fields
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           dM$qim_demographicGroup)
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)} %>>%
            {if ("HbA1C" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(HbA1CDate, HbA1CValue, HbA1CUnits))}} %>>%
            {if ("Influenza" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(FluvaxDate, FluvaxName))}} %>>%
            {if ("BP" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(BPDate, BP))}}
          datatable_styled(df,
                           extensions = c('Buttons', 'Scroller'),
                           columnDefs = list(list(targets =
                                                    which(names(df) %in%
                                                            c("Patient", "RecordNo")),
                                                  # needs name by index as columns might be removed
                                                  # by demographic filters above
                                                  visible = FALSE)),
                           scrollX = TRUE) %>>% # this is a wide table
            {if ("HbA1C" %in% input$measure_chosen) {
              DT::formatStyle(., 'HbA1CDate',
                              backgroundcolor = DT::styleInterval(as.Date(Sys.Date()-365), c("ffeeee", "eeffee"))
              )
            } else {.}}
        }

      }
  )

  output$diabetes_qim_table <- DT::renderDT({
    qim_diabetes_datatable()
  },
  server = TRUE)

}


#' Quality Improvement cervical screening test - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_cst <- function(input, output, session, dM) {
  ns <- session$ns

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_cst_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_cst_listR(),
      dM$qim_cst_reportR(),
      dM$qim_demographicGroupR()), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_cst_list %>>%
            dplyr::select(Patient, RecordNo,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          CSTDate, CSTName) %>>%
            # re-orders the fields
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           dM$qim_demographicGroup)
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)}
          datatable_styled(
            df,
            columnDefs = list(list(targets =
                                     which(names(df) %in%
                                             c("Patient", "RecordNo")),
                                   # Patient Name and RecordNo hidden by default
                                   # needs name by index as columns might be removed
                                   # by demographic filters above
                                   visible = FALSE)),
          )
        } else {
          df <- dM$qim_cst_report
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        }

      }
  )

  output$cst_qim_table <- DT::renderDT({
    qim_cst_datatable()
  },
  server = TRUE)

}

##### Quality Improvement Measures 15+ ###############################################
#' Quality Improvement fifteen plus (15+ years) - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_15plus <- function(input, output, session, dM) {
  ns <- session$ns

  output$measure_group <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "measure_group_dropdown",
      icon = icon("gear"),
      label = "15+ Measures",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("measure_chosen"), label = "Measures Chosen",
        choices = dM$qim_15plus_measureTypes,
        selected = dM$qim_15plus_measureTypes,
        # initially all chosen
        status = "primary"
      )
    )
  })
  shiny::observeEvent(input$measure_chosen, ignoreNULL = FALSE, {
    dM$qim_15plus_measure <- input$measure_chosen
  })

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_15plus_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_15plus_listR(),
      dM$qim_15plus_reportR(),
      dM$qim_demographicGroupR()), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_15plus_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           dM$qim_demographicGroup)
            # finds the demographics that were NOT chosen
            dplyr::select(., -c(remove_demographic, InternalID))} %>>%
            {if ("Smoking" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(SmokingDate, SmokingStatus))}} %>>%
            {if ("Weight" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(HeightDate, HeightValue, WeightDate, WeightValue,
                                        BMIDate, BMIValue, BMIClass,
                                        WaistDate, WaistValue))}} %>>%
            {if ("Alcohol" %in% input$measure_chosen) {.}
              else {dplyr::select(., -c(AlcoholDate, NonDrinker, DaysPerWeek,
                                        DrinksPerDay, AlcoholDescription,
                                        PastAlcoholLevel, YearStarted, YearStopped,
                                        AlcoholComment))}}
          return(datatable_styled(
            df, extensions = c('Buttons', 'Scroller'),
            columnDefs = list(list(targets =
                                     which(names(df) %in%
                                             c("Patient", "RecordNo", "HeightDate", "HeightValue",
                                               "WeightDate", "WeightValue", "AlcoholDescription",
                                               "PastAlcoholLevel", "YearStarted", "YearStopped",
                                               "AlcoholComment")),
                                   # needs name by index as columns might be removed
                                   # by demographic filters above
                                   visible = FALSE)),
            # Patient Name and RecordNo hidden by default, as well as various alcohol details etc.
            scrollX = TRUE))
        } else {
          df <- dM$qim_15plus_report
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        }

      }
  )

  output$fifteenplus_qim_table <- DT::renderDT({
    qim_15plus_datatable()
  },
  server = TRUE)

}

##### Quality Improvement Measures 65+ ###############################################
#' Quality Improvement 65+ plus years - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_65plus <- function(input, output, session, dM) {
  ns <- session$ns

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_65plus_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_65plus_listR(),
      dM$qim_65plus_reportR(),
      dM$qim_demographicGroupR()), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_65plus_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           dM$qim_demographicGroup)
            # finds the demographics that were NOT chosen
            dplyr::select(., -c(remove_demographic, InternalID))}
          return(datatable_styled(
            df, extensions = c('Buttons', 'Scroller'),
            columnDefs = list(list(targets =
                                     which(names(df) %in%
                                             c("Patient", "RecordNo")),
                                   # needs name by index as columns might be removed
                                   # by demographic filters above
                                   visible = FALSE)),
            # Patient Name and RecordNo hidden by default
            scrollX = TRUE))
        } else {
          df <- dM$qim_65plus_report
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        }
      }
  )

  output$sixtyfiveplus_qim_table <- DT::renderDT({
    qim_65plus_datatable()
  },
  server = TRUE)

}

##### Quality Improvement Measures COPD ###############################################
#' Quality Improvement COPD - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_copd <- function(input, output, session, dM) {
  ns <- session$ns

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_copd_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_copd_listR(),
      dM$qim_copd_reportR(),
      dM$qim_demographicGroupR()), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_copd_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           dM$qim_demographicGroup)
            # finds the demographics that were NOT chosen
            dplyr::select(., -c(remove_demographic, InternalID))}
          return(datatable_styled(
            df, extensions = c('Buttons', 'Scroller'),
            columnDefs = list(list(targets =
                                     which(names(df) %in%
                                             c("Patient", "RecordNo")),
                                   # needs name by index as columns might be removed
                                   # by demographic filters above
                                   visible = FALSE)),
            # Patient Name and RecordNo hidden by default
            scrollX = TRUE))
        } else {
          df <- dM$qim_copd_report
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        }

      }
  )

  output$copd_qim_table <- DT::renderDT({
    qim_copd_datatable()
  },
  server = TRUE)
}


##### Quality Improvement Measures cardiovascular risk ###############################################
#' Quality Improvement cardiovascular risk - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @include fomantic_definitions.R
#'
#' @return none
qim_cvdRisk <- function(input, output, session, dM) {
  ns <- session$ns

  output$groups <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "measure_group_dropdown",
      icon = icon("gear"),
      label = "Inclusions/Exclusions",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("groups_chosen"), label = "Groups chosen",
        choices = dM$qim_cvdRisk_measureTypes,
        selected = dM$qim_cvdRisk_measureTypes,
        # initially all chosen, which includes choices to
        #  'include' ATSI 35-44 years,
        # and 'exclude'
        #  those with known cardiovascular disease and age 75
        status = "primary"
      )
    )
  })
  shiny::observeEvent(input$groups_chosen, ignoreNULL = FALSE, {
    dM$qim_cvdRisk_measure <- input$groups_chosen
  })

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_cvdRisk_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_cvdRisk_listR(),
      dM$qim_cvdRisk_reportR(),
      dM$qim_demographicGroupR()), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_cvdRisk_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           dM$qim_demographicGroup)
            # finds the demographics that were NOT chosen
            dplyr::select(., -c(remove_demographic, InternalID))}
          return(datatable_styled(
            df, extensions = c('Buttons', 'Scroller'),
            columnDefs = list(list(targets =
                                     which(names(df) %in%
                                             c("Patient", "RecordNo")),
                                   # needs name by index as columns might be removed
                                   # by demographic filters above
                                   visible = FALSE)),
            # Patient Name and RecordNo hidden by default
            scrollX = TRUE) %>>%
              DT::formatRound(which(names(df) %in% c("CholHDLRatio", "frisk")), digits = 3))
        } else {
          df <- dM$qim_cvdRisk_report
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        }
      }
  )

  output$cvdRisk_qim_table <- DT::renderDT({
    qim_cvdRisk_datatable()
  },
  server = TRUE)

}
