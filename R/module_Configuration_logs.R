###### logging configuration modules ###################################################


###### Logging configuration ##################################################
#' loggingUI
#'
#' enable logging, choose logging SQLite database file
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return shiny user interface element
logging_datatableUI <- function(id) {
  ns <- shiny::NS(id)


  shiny::tagList(
    shiny::wellPanel(
      "Enable logging",
      shiny::br(), shiny::br(),
      shiny::fluidRow(
        shiny::column(width = 3,
                      shinyWidgets::materialSwitch(
                        inputId = ns("enable_logging"),
                        label = "Enable logging",
                        right = TRUE,
                        value = FALSE,
                        status = "primary")),
        shiny::column(width = 8, "")
      )
    ),
    shinydashboardPlus::boxPlus(
      title = "Access log database location",
      width = 12,
      closable = FALSE,
      status = "primary",
      solidHeader = FALSE,
      collapsible = TRUE,
      shiny::column(
        width = 12,
        shiny::wellPanel(
          textOutput(ns('logDB_file_details'))
          # location of sqlite configuration file
        ),
        shiny::wellPanel(
          shinyFiles::shinyFilesButton(
            id = ns("choose_logDB_file"),
            label = "Choose log database file",
            title = "Choose log database file (must end in '.sqlite')",
            multiple = FALSE),
          shinyFiles::shinySaveButton(
            id = ns("create_logDB_file"),
            label = "Create log database file",
            title = "Create log database file (must end in '.sqlite')",
            filetype = list(sqlite = c('sqlite'))),
          shiny::helpText(
            paste("Choose location of an existing configuration file",
                  "or create a new configuration file"))
        )
      )
    ),
    shinydashboardPlus::boxPlus(
      title = "Access logs",
      width = 12,
      closable = FALSE,
      status = "primary",
      solidHeader = FALSE,
      collapsible = TRUE,
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::actionButton(ns("update_logs"), "Update",
                              shiny::icon("refresh"), class = "btn btn-primary")
        )
      ),
      shiny::column(
        width = 12,
        DT::DTOutput(ns("logs_table"))
      )
    )
  )
}

#' userconfig_resetpassword - server component

#' reset password of selected user
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object, access to databases, user config
#'
#' @return nothing
logging_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  shiny::observeEvent(dM$LogR(), {
    # update the switch if the $logR() changes
    # (it will change when the configuration database is read
    shinyWidgets::updateMaterialSwitch(
      session, "enable_logging",
      dM$LogR())
  })

  shiny::observeEvent(input$enable_logging, ignoreInit = TRUE, {
    dM$Log <- input$enable_logging
    # try to change logging status
    if (dM$Log != input$enable_logging) {
      # failure to change logging status
      shinyWidgets::updateMaterialSwitch(
        session, "enable_logging",
        dM$Log)
    }
  })

  output$logDB_file_details <- renderText({
    paste('Log file location: "', dM$LogFileR(), '"')
  })

  volumes <- c(shinyFiles::getVolumes()(), base = '.', home = Sys.getenv("USERPROFILE"))

  shinyFiles::shinyFileChoose(
    input, id = "choose_logDB_file",
    session = session,
    roots = volumes,
    filetypes = c("sqlite"), # only files ending in '.sqlite'
    hidden = TRUE # the default is that configuration files have '.' hidden prefix
  )

  observeEvent(input$choose_logDB_file, ignoreNULL = TRUE, {
    if (!is.integer(input$choose_logDB_file)) {
      # if input$choose_configuration_file is an integer,
      # it is just the 'click' event on the filechoose button
      # only can change configuration file if current not logging!
      inFile <- shinyFiles::parseFilePaths(volumes, input$choose_logDB_file)
      file_name <- paste(inFile$datapath)
      if (dM$Log == FALSE) {
        tryCatch(dM$LogFile <- file_name,
                 warning = function(w) {
                   shinytoastr::toastr_warning(message = w,
                                               position = "bottom-left")
                 })
      }
    }
  })

  shinyFiles::shinyFileSave(
    input, id = 'create_logDB_file',
    session = session,
    roots = volumes,
    hidden = TRUE
  )

  shiny::observeEvent(input$create_logDB_file, ignoreNULL = TRUE, {
    if (!is.integer(input$create_logDB_file)) {
      # if input$choose_configuration_file is an integer,
      # it is just the 'click' event on the filechoose button
      inFile <- shinyFiles::parseSavePath(volumes, input$create_logDB_file)
      file_name <- paste(inFile$datapath)
      if (dM$Log == FALSE) {
        # only if logging turned off
        # because creation of log file requires temporarily turning logging on!
        tryCatch(
          {dM$LogFile <- file_name
          dM$Log <- TRUE # turn logging on, then off, to create logging file
          dM$Log <- FALSE},
          warning = function(w) {
          shinytoastr::toastr_warning(message = w,
                                      position = "bottom-left")
        })
      } else {
        shinytoastr::toastr_warning(
          message = paste("Unable to create LogFile when logging is turned on.",
                          "Turn logging off first."),
          position = "bottom-left")
      }
    }
  })

  log_dt <- shiny::eventReactive(input$update_logs, ignoreInit = TRUE, {
    dM$ReadLog
  })

  output$logs_table <- DT::renderDT({
    DT::datatable(log_dt(),
                  fillContainer = TRUE,
                  extensions = c('Buttons', 'Scroller', 'Responsive'),
                  options = list(dom = 'frltiBp',
                                 buttons = list('copyHtml5', 'print', list(
                                   extend = 'collection',
                                   buttons = list(
                                     list(extend = 'csvHtml5', filename = 'DailyMeasureLog'),
                                     list(extend = 'excel', filename = 'DailyMeasureLog'),
                                     list(extend = 'pdf', filename = 'DailyMeasureLog')),
                                   text = 'Download'
                                 )),
                                 paging = FALSE,
                                 scrollY = "60vh")
                  ) %>>%
      DT::formatRound(columns = c("Duration"), digits = 3)
  })
}
