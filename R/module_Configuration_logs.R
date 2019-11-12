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
    shiny::fluidPage(
      shiny::fluidRow(
        shinydashboard::tabBox(
          title = "Configuration",
          id = "log_config",
          width = 9,
          shiny::tabPanel(
            "Logs",
            shiny::wellPanel(
              shiny::br(),
              shiny::fluidRow(
                shiny::column(1),
                shiny::column(11,
                              shiny::fluidRow(
                                shinyWidgets::materialSwitch(
                                  inputId = ns("enable_logging"),
                                  label = "Enable logging",
                                  right = TRUE,
                                  value = FALSE,
                                  status = "primary")
                              )
                )
              )
            )
          ),
          shiny::tabPanel(
            "Log file configuration",
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::wellPanel(
                  shiny::br(),
                  "Log database file:",
                  textOutput(ns('logDB_file_details')),
                  # location of sqlite configuration file
                  shiny::br()
                )),
              shiny::column(
                width = 8,
                shiny::wellPanel(
                  {if (.bcdyz.option$demonstration)
                  {shiny::span(shiny::p(), shiny::strong("Demonstration mode : Log file changes disabled"),
                               style = "color:red", shiny::p())}
                    else {}},
                  {x <- shinyFiles::shinyFilesButton(
                    id = ns("choose_logDB_file"),
                    label = "Choose log database file",
                    title = "Choose log database file (must end in '.sqlite')",
                    multiple = FALSE)
                  # disabled if demonstration mode
                  if (.bcdyz.option$demonstration) {shinyjs::disabled(x)} else {x}},
                  {x <- shinyFiles::shinySaveButton(
                    id = ns("create_logDB_file"),
                    label = "Create log database file",
                    title = "Create log database file (must end in '.sqlite')",
                    filetype = list(sqlite = c('sqlite')))
                  # disabled if demonstration mode
                  if (.bcdyz.option$demonstration) {shinyjs::disabled(x)} else {x}}
                  ,
                  shiny::helpText(
                    paste("Choose location of an existing configuration file",
                          "or create a new configuration file. Can only be",
                          "chosen/created if logging is turned off.")
                  )
                )
              )
            )
          )
        )
      ),
      shiny::fluidRow(
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
              shiny::actionButton(ns("update_logs"), "Update and Show",
                                  shiny::icon("refresh"), class = "btn btn-primary")
            )
          )
        )
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
    tryCatch(dM$Log <- input$enable_logging,
             warning = function(w) {
               shinytoastr::toastr_warning(message = w$message,
                                           position = "bottom-left")
             })
    # try to change logging status
    if (dM$Log != input$enable_logging) {
      # failure to change logging status
      shinyWidgets::updateMaterialSwitch(
        session, "enable_logging",
        dM$Log)
    }
  })

  shiny::observeEvent(dM$LogFileR(), {
    if (is.null(dM$LogFileR()) || dM$LogFileR() == "") {
      # if an empty string, then cannot enable logging
      # or view log file
      # shinyjs::disable("enable_logging") - has it's own warning tryCatch
      shinyjs::disable("update_logs")
    } else {
      # shinyjs::enable("enable_logging")
      shinyjs::enable("update_logs")
    }
  })

  shiny::observeEvent(dM$LogR(), {
    if (is.null(dM$LogR()) || dM$LogR() == FALSE) {
      # can only choose/create log file if logging currently disabled
      shinyjs::enable("choose_logDB_file")
      shinyjs::enable("create_logDB_file")
    } else {
      shinyjs::disable("choose_logDB_file")
      shinyjs::disable("create_logDB_file")
    }
  })

  output$logDB_file_details <- shiny::renderText({
    paste('"', dM$LogFileR(), '"')
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
            shinytoastr::toastr_warning(message = w$message,
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

  shiny::observeEvent(input$update_logs, ignoreInit = TRUE, {
    logs <- tryCatch(dM$ReadLog,
                     warning = function(w) {
                       shinytoastr::toastr_warning(message = w$message,
                                                   position = "bottom-left")
                     })
    if (!is.null(logs)) {
      shiny::showModal(shiny::modalDialog(
        title = "Logs",
        size = c("l"),
        DT::renderDT(DT::datatable(logs,
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
          DT::formatRound(columns = c("Duration"), digits = 3))
      ))
    }
  })
}
