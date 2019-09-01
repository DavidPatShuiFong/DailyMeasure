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
      title = "Quality Improvement Measures",
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
                      width = "15em")),
      shiny::column(2, offset = 4,
                    shiny::uiOutput(ns("demographic_group")))
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
      shiny::column(2, offset = 1,
                    shiny::uiOutput(ns("demographic_group"))),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
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
                      width = "30em")),
      shiny::column(2, offset = 1,
                    shiny::uiOutput(ns("demographic_group")))
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
      shiny::column(2, offset = 1,
                    shiny::uiOutput(ns("demographic_group"))),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
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
                      width = "30em")),
      shiny::column(2, offset = 1,
                    shiny::uiOutput(ns("demographic_group")))
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
                      width = "30em")),
      shiny::column(2, offset = 1,
                    shiny::uiOutput(ns("demographic_group")))
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
      shiny::column(2, offset = 1,
                    shiny::uiOutput(ns("demographic_group"))),
      shiny::column(2, offset = 1, # note that total 'column' width = 12
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
      label = "Demographic groupings"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })

  qim_active_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_active_listR(),
      dM$qim_active_reportR(),
      input$demographic_chosen), ignoreInit = TRUE, {
        if (input$list_view) {
          datatable_styled(
            dM$qim_active_list %>>%
              dplyr::select(Patient, RecordNo,
                            Age5, Sex, Ethnicity,
                            MaritalStatus, Sexuality) %>>%
              # re-orders the fields
              {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                             input$demographic_chosen)
              # finds the demographics that were NOT chosen
              dplyr::select(., -remove_demographic)},
            columnDefs = list(list(targets = 1:2, visible = FALSE))
            # Patient Name and RecordNo hidden by default
            # can be shown again with 'colVis' button
          )
        } else {
          datatable_styled(dM$qim_active_report)
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
      label = "Demographic groupings"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })

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

  qim_diabetes_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_diabetes_listR(),
      dM$qim_diabetes_reportR(),
      input$demographic_chosen), ignoreInit = TRUE, {
        if (input$list_view) {
          datatable_styled(
            dM$qim_diabetes_list %>>%
              dplyr::select(Patient, RecordNo,
                            Age5, Sex, Ethnicity,
                            MaritalStatus, Sexuality,
                            HbA1CDate, HbA1CValue, HbA1CUnits,
                            FluvaxDate, FluvaxName,
                            BPDate, BP) %>>%
              # re-orders the fields
              {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                             input$demographic_chosen)
              # finds the demographics that were NOT chosen
              dplyr::select(., -remove_demographic)} %>>%
              {if ("HbA1C" %in% input$measure_chosen) {.}
                else {dplyr::select(., -c(HbA1CDate, HbA1CValue, HbA1CUnits))}} %>>%
              {if ("Influenza" %in% input$measure_chosen) {.}
                else {dplyr::select(., -c(FluvaxDate, FluvaxName))}} %>>%
              {if ("BP" %in% input$measure_chosen) {.}
                else {dplyr::select(., -c(BPDate, BP))}},
            columnDefs = list(list(targets = 1:2, visible = FALSE))
            # Patient Name and RecordNo hidden by default
          )
        } else {
          datatable_styled(dM$qim_diabetes_report)
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
      label = "Demographic groupings"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_cst_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_cst_listR(),
      dM$qim_cst_reportR(),
      input$demographic_chosen), ignoreInit = TRUE, {
        if (input$list_view) {
          datatable_styled(
            dM$qim_cst_list %>>%
              dplyr::select(Patient, RecordNo,
                            Age5, Sex, Ethnicity,
                            MaritalStatus, Sexuality,
                            CSTDate, CSTName) %>>%
              # re-orders the fields
              {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                             input$demographic_chosen)
              # finds the demographics that were NOT chosen
              dplyr::select(., -remove_demographic)},
            columnDefs = list(list(targets = 1:2, visible = FALSE))
            # Patient Name and RecordNo hidden by default
          )
        } else {
          datatable_styled(dM$qim_cst_report)
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
      label = "Demographic groupings"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })

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
      input$demographic_chosen), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_15plus_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           input$demographic_chosen)
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
          return(datatable_styled(dM$qim_15plus_report))
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
      label = "Demographic groupings"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_65plus_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_65plus_listR(),
      dM$qim_65plus_reportR(),
      input$demographic_chosen), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_65plus_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           input$demographic_chosen)
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)}
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
          return(datatable_styled(dM$qim_65plus_report))
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
      label = "Demographic groupings"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })

  shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
    # if selected, will filter out appointments older than current date
    dM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
  })

  qim_copd_datatable <- shiny::eventReactive(
    c(input$list_view,
      dM$qim_copd_listR(),
      dM$qim_copd_reportR(),
      input$demographic_chosen), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_copd_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           input$demographic_chosen)
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)}
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
          return(datatable_styled(dM$qim_copd_report))
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
      label = "Demographic groupings"
    )
  })
  shiny::observeEvent(input$demographic_chosen, ignoreNULL = FALSE, {
    # change the filter depending on the dropdown
    dM$qim_demographicGroup <- input$demographic_chosen
  })

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
      input$demographic_chosen), ignoreInit = TRUE, {
        if (input$list_view) {
          df <- dM$qim_cvdRisk_list %>>%
            {remove_demographic <- setdiff(dM$qim_demographicGroupings,
                                           input$demographic_chosen)
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)}
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
          return(datatable_styled(dM$qim_cvdRisk_report))
        }
      }
  )

  output$cvdRisk_qim_table <- DT::renderDT({
    qim_cvdRisk_datatable()
  },
  server = TRUE)

}
