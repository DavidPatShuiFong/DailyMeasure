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
      id = ns("tab_qim"),
      title = shiny::tagList(
        shiny::div(style = "display:inline-block",
                   shiny::uiOutput(ns("settings_group"))),
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
                    shinyWidgets::pickerInput(
                      inputId = ns("list_view"),
                      choices = c("Report", "List"),
                      choicesOpt = list(icon = c("fa fa-book-reader",
                                                 "fa fa-clipboard-list")),
                      options = list(`icon-base` = ""),
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
                    shiny::uiOutput(ns("list_group"))),
      shiny::column(2, offset = 7, # note that total 'column' width = 12
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
                    shiny::uiOutput(ns("list_group")))),
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
                    shiny::uiOutput(ns("list_group"))),
      shiny::column(2, offset = 7, # note that total 'column' width = 12
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
                    shiny::uiOutput(ns("list_group")))),
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
                    shiny::uiOutput(ns("list_group")))),
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
                    shiny::uiOutput(ns("list_group"))),
      shiny::column(2, offset = 7, # note that total 'column' width = 12
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
#' @param dMQIM dMeasure QIM R6 object
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  # result management
  callModule(qim_active, "qim_active", dMQIM, contact)
  callModule(qim_diabetes, "qim_diabetes", dMQIM, contact)
  callModule(qim_cst, "qim_cst", dMQIM, contact)
  callModule(qim_15plus, "qim_15plus", dMQIM, contact)
  callModule(qim_65plus, "qim_65plus", dMQIM, contact)
  callModule(qim_copd, "qim_copd", dMQIM, contact)
  callModule(qim_cvdRisk, "qim_cvdRisk", dMQIM, contact)

  if (!contact) {
    # only show the 'Active' panel if contact list
    #
    #  'Active' is whether the patient qualifies as 'active' depending
    #  on criteria such as appointments, visits (recordings in the file)
    #  or billings over a defined time period, and a certain number of
    #  times
    #
    # e.g. one definition of 'active' is 3 'visits' over 2 years
    # though an alternative definition could be three 'billings' over
    # three years

    # if an 'appointment' list is being used
    # then don't show the 'Active' panel
    shiny::removeTab(inputId = "tab_qim", target = "Active", session = session)
    # for some reason the above line doesn't remove the tab...
    shiny::updateTabsetPanel(session = session, inputId = "tab_qim", selected = "Diabetes")
  }

  output$settings_group <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "qim_dropdown",
      icon = icon("gear"),
      label = "Settings", width = "20em",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ignore_old"), label = "Measurements",
        checkIcon = list(yes = shiny::icon("calendar-times"),
                         no = shiny::icon("calendar-alt")),
        choices = c("Ignore old measurements"),
        selected = c("Ignore old measurements"),
        status = "primary",
        width = "30em"),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("demographic_chosen"), label = "Demographic grouping",
        choices = dMQIM$qim_demographicGroupings,
        selected = dMQIM$qim_demographicGroupings,
        # all choices initially selected
        status = "primary", width = "15em",
        checkIcon = list(yes = icon("ok", lib = "glyphicon")))
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dMQIM$qim_demographicGroup <- input$demographic_chosen
  })
  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dMQIM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })


}

#' Quality Improvement 'active' list - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim_active <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  qim_active_datatable <- shiny::eventReactive(
    c(input$list_view,
      dMQIM$qim_active_listR(),
      dMQIM$qim_active_reportR(),
      dMQIM$qim_demographicGroupR()), {
        if (input$list_view == "List") {
          datatable_styled(
            dMQIM$qim_active_listR() %>>%
              dplyr::select(Patient, RecordNo,
                            Age5, Sex, Ethnicity,
                            MaritalStatus, Sexuality, Count) %>>%
              # re-orders the fields
                            {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                           dMQIM$qim_demographicGroup)
                            # finds the demographics that were NOT chosen
                            dplyr::select(., -remove_demographic)},
            columnDefs = list(list(targets = 1:2, visible = FALSE))
            # Patient Name and RecordNo hidden by default
            # can be shown again with 'colVis' button
          )
        } else if (input$list_view == "Report") {
          df <- dMQIM$qim_active_reportR()
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
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim_diabetes <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c("fa fa-book-reader",
                                  "fa fa-clipboard-list"))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em")
  })
  output$measure_group <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "measure_group_dropdown",
      icon = icon("gear"),
      label = "Diabetes Measures",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("measure_chosen"), label = "Measures Chosen",
        choices = dMQIM$qim_diabetes_measureTypes,
        selected = dMQIM$qim_diabetes_measureTypes,
        # initially all chosen
        status = "primary"
      )
    )
  })
  shiny::observeEvent(input$measure_chosen, ignoreNULL = FALSE, {
    dMQIM$qim_diabetes_measure <- input$measure_chosen
  })

  qim_diabetes_datatable <- shiny::eventReactive(
    c(input$list_view,
      dMQIM$qim_diabetes_listR(),
      dMQIM$qim_diabetes_list_appointmentsR(),
      dMQIM$qim_diabetes_reportR(),
      dMQIM$qim_demographicGroupR()), {
        shiny::req(input$list_view) # this might not be defined on first run!
        if (input$list_view == "List") {
          df <- dMQIM$qim_diabetes_listR() %>>%
            dplyr::select(Patient, RecordNo,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          HbA1CDate, HbA1CValue, HbA1CUnits,
                          FluvaxDate, FluvaxName,
                          BPDate, BP) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
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
          df <- dMQIM$qim_diabetes_reportR()
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        } else if (input$list_view == "Appointments") {
          df <- dMQIM$qim_diabetes_list_appointmentsR() %>>%
            dplyr::select(Patient, RecordNo,
                          AppointmentDate, AppointmentTime, Provider, Status,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          HbA1CDate, HbA1CValue, HbA1CUnits,
                          FluvaxDate, FluvaxName,
                          BPDate, BP) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
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
                           colvis = NULL) %>>%
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
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim_cst <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c("fa fa-book-reader",
                                  "fa fa-clipboard-list"))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em")
  })

  qim_cst_datatable <- shiny::eventReactive(
    c(input$list_view,
      dMQIM$qim_cst_listR(),
      dMQIM$qim_cst_list_appointmentsR(),
      dMQIM$qim_cst_reportR(),
      dMQIM$qim_demographicGroupR()), {
        shiny::req(input$list_view)
        if (input$list_view == "List") {
          df <- dMQIM$qim_cst_listR() %>>%
            dplyr::select(Patient, RecordNo,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          CSTDate, CSTName) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
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
                                   visible = FALSE))
          )
        } else if (input$list_view == "Report") {
          df <- dMQIM$qim_cst_reportR()
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        } else if (input$list_view == "Appointments") {
          df <- dMQIM$qim_cst_list_appointmentsR() %>>%
            dplyr::select(Patient, RecordNo,
                          AppointmentDate, AppointmentTime, Provider, Status,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          CSTDate, CSTName) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
                          # finds the demographics that were NOT chosen
                          dplyr::select(., -remove_demographic)}
          datatable_styled(df,
                           extensions = c('Buttons', 'Scroller'), colvis = NULL,
                           scrollX = TRUE) # this is a wide table
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
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim_15plus <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c("fa fa-book-reader",
                                  "fa fa-clipboard-list"))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em")
  })
  output$measure_group <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "measure_group_dropdown",
      icon = icon("gear"),
      label = "15+ Measures",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("measure_chosen"), label = "Measures Chosen",
        choices = dMQIM$qim_15plus_measureTypes,
        selected = dMQIM$qim_15plus_measureTypes,
        # initially all chosen
        status = "primary"
      )
    )
  })
  shiny::observeEvent(input$measure_chosen, ignoreNULL = FALSE, {
    dMQIM$qim_15plus_measure <- input$measure_chosen
  })

  qim_15plus_datatable <- shiny::eventReactive(
    c(input$list_view,
      dMQIM$qim_15plus_listR(),
      dMQIM$qim_15plus_list_appointmentsR(),
      dMQIM$qim_15plus_reportR(),
      dMQIM$qim_demographicGroupR()), {
        shiny::req(input$list_view)
        if (input$list_view == "List") {
          df <- dMQIM$qim_15plus_listR() %>>%
          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                         dMQIM$qim_demographicGroup)
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
        } else if (input$list_view == "Report") {
          df <- dMQIM$qim_15plus_reportR()
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        } else if (input$list_view == "Appointments") {
          df <- dMQIM$qim_15plus_list_appointmentsR() %>>%
            dplyr::select(Patient, RecordNo,
                          AppointmentDate, AppointmentTime, Provider, Status,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          SmokingDate, SmokingStatus,
                          HeightDate, HeightValue, WeightDate, WeightValue,
                          BMIDate, BMIValue, BMIClass,
                          WaistDate, WaistValue,
                          AlcoholDate, NonDrinker, DaysPerWeek,
                          DrinksPerDay, AlcoholDescription,
                          PastAlcoholLevel, YearStarted, YearStopped,
                          AlcoholComment) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
                          # finds the demographics that were NOT chosen
                          dplyr::select(., -remove_demographic)} %>>%
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
          return(datatable_styled(df,
                                  extensions = c('Buttons', 'Scroller'), colvis = NULL,
                                  scrollX = TRUE)) # this is a wide table
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
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim_65plus <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c("fa fa-book-reader",
                                  "fa fa-clipboard-list"))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em")
  })

  qim_65plus_datatable <- shiny::eventReactive(
    c(input$list_view,
      dMQIM$qim_65plus_listR(),
      dMQIM$qim_65plus_list_appointmentsR(),
      dMQIM$qim_65plus_reportR(),
      dMQIM$qim_demographicGroupR()), {
        shiny::req(input$list_view)
        if (input$list_view == "List") {
          df <- dMQIM$qim_65plus_listR() %>>%
          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                         dMQIM$qim_demographicGroup)
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
        } else if (input$list_view == "Report") {
          df <- dMQIM$qim_65plus_reportR()
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        } else if (input$list_view == "Appointments") {
          df <- dMQIM$qim_65plus_list_appointmentsR() %>>%
            dplyr::select(Patient, RecordNo,
                          AppointmentDate, AppointmentTime, Provider, Status,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          FluvaxDate, FluvaxName) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
                          # finds the demographics that were NOT chosen
                          dplyr::select(., -remove_demographic)}
          datatable_styled(df,
                           extensions = c('Buttons', 'Scroller'), colvis = NULL,
                           scrollX = TRUE) # this is a wide table
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
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim_copd <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c("fa fa-book-reader",
                                  "fa fa-clipboard-list"))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em")
  })

  qim_copd_datatable <- shiny::eventReactive(
    c(input$list_view,
      dMQIM$qim_copd_listR(),
      dMQIM$qim_copd_list_appointmentsR(),
      dMQIM$qim_copd_reportR(),
      dMQIM$qim_demographicGroupR()), {
        shiny::req(input$list_view)
        if (input$list_view == "List") {
          df <- dMQIM$qim_copd_listR() %>>%
          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                         dMQIM$qim_demographicGroup)
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
        } else if (input$list_view == "Report") {
          df <- dMQIM$qim_copd_reportR()
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        } else if (input$list_view == "Appointments") {
          df <- dMQIM$qim_copd_list_appointmentsR() %>>%
            dplyr::select(Patient, RecordNo,
                          AppointmentDate, AppointmentTime, Provider, Status,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          FluvaxDate, FluvaxName) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
                          # finds the demographics that were NOT chosen
                          dplyr::select(., -remove_demographic)}
          datatable_styled(df,
                           extensions = c('Buttons', 'Scroller'), colvis = NULL,
                           scrollX = TRUE) # this is a wide table
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
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
qim_cvdRisk <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c("fa fa-book-reader",
                                  "fa fa-clipboard-list"))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em")
  })
  output$groups <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = "measure_group_dropdown",
      icon = icon("gear"),
      label = "Inclusions/Exclusions",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("groups_chosen"), label = "Groups chosen",
        choices = dMQIM$qim_cvdRisk_measureTypes,
        selected = dMQIM$qim_cvdRisk_measureTypes,
        # initially all chosen, which includes choices to
        #  'include' ATSI 35-44 years,
        # and 'exclude'
        #  those with known cardiovascular disease and age 75
        status = "primary"
      )
    )
  })
  shiny::observeEvent(input$groups_chosen, ignoreNULL = FALSE, {
    dMQIM$qim_cvdRisk_measure <- input$groups_chosen
  })

  qim_cvdRisk_datatable <- shiny::eventReactive(
    c(input$list_view,
      dMQIM$qim_cvdRisk_listR(),
      dMQIM$qim_cvdRisk_list_appointmentsR(),
      dMQIM$qim_cvdRisk_reportR(),
      dMQIM$qim_demographicGroupR()), {
        shiny::req(input$list_view)
        if (input$list_view == "List") {
          df <- dMQIM$qim_cvdRisk_listR() %>>%
          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                         dMQIM$qim_demographicGroup)
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
        } else if (input$list_view == "Report") {
          df <- dMQIM$qim_cvdRisk_reportR()
          dt <- datatable_styled(df)
          if (dim(df)[[2]] > 0) {
            # not an empty dataframe
            dt <- dt %>>%
              DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3)
          }
          return(dt)
        } else if (input$list_view == "Appointments") {
          df <- dMQIM$qim_cvdRisk_list_appointmentsR() %>>%
            dplyr::select(Patient, RecordNo,
                          AppointmentDate, AppointmentTime, Provider, Status,
                          Age5, Sex, Ethnicity,
                          MaritalStatus, Sexuality,
                          CardiovascularDisease, Diabetes,
                          SmokingDate, SmokingStatus,
                          UrineAlbuminDate, UrineAlbuminValue, UrineAlbuminUnits,
                          PersistentProteinuria,
                          eGFRDate, eGFRValue, eGFRUnits,
                          FamilialHypercholesterolaemia, LVH,
                          CholesterolDate, Cholesterol, HDL, LDL,
                          Triglycerides, CholHDLRatio,
                          BPDate, BP,
                          frisk, friskHI) %>>%
            # re-orders the fields
                          {remove_demographic <- setdiff(dMQIM$qim_demographicGroupings,
                                                         dMQIM$qim_demographicGroup)
                          # finds the demographics that were NOT chosen
                          dplyr::select(., -remove_demographic)}
          datatable_styled(df,
                           extensions = c('Buttons', 'Scroller'), colvis = NULL,
                           scrollX = TRUE) %>>% # this is a wide table
          DT::formatRound(which(names(df) %in% c("frisk")), digits = 3)
        }
      }
  )

  output$cvdRisk_qim_table <- DT::renderDT({
    qim_cvdRisk_datatable()
  },
  server = TRUE)

}
