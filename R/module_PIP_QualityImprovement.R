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
