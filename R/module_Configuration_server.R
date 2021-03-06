# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

##### configuration modules #####################################################################
##### servers - editable datatable module ######################################################

#' server configuration module - user interface function
#'
#' Server selector (dropdown)
#' Editable datatable with list of servers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
servers_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      shiny::uiOutput(ns("selection")), {
        if (.bcdyz.option$demonstration) {
          shiny::span(shiny::p(), shiny::strong("Demonstration mode : Server changes/additions disabled"),
            style = "color:red", shiny::p()
          )
        }
        else {}
      }
    ),
    shiny::fluidRow(
      shiny::div(
        style = "display: inline-block; vertical-align:top",
        shiny::h3("Server descriptions")
      ),
      shiny::div(
        style = "display: inline-block; vertical-align:-100%",
        # '-50%' still results in a '+50%' compared to the h3 title!
        # '-100%' results in a dropdown widget roughly in line with the title
        shinyWidgets::dropdown(
          shiny::tags$h3("Server descriptions"),
          shiny::br(),
          shiny::tags$h4("Address"),
          "This can be found in Best Practice through Setup-Configuration-Database menu.",
          "Click 'Find Servers', which will show the available Best Practice servers (usually just one!).",
          "Add '\\BPSINSTANCE' to the server name you are using.",
          shiny::br(),
          "For example, if your server name is 'CLINICSERVERPC', then the address is 'CLINICSERVERPC\\BPSINSTANCE'.",
          "This is the same as would be found as 'Server Name' in Microsoft's SQL Server Management Studio.",
          shiny::br(),
          shiny::tags$h4("Database"),
          "This will usually be 'BPSPatients'.",
          "The samples database is 'BPSSamples'",
          shiny::br(),
          shiny::tags$h4("Driver"),
          "Microsoft SQL database driver.",
          "'SQL Server' is usually available.",
          shiny::br(),
          shiny::tags$h4("UserID"),
          "This should always be 'bpsrawdata'.",
          shiny::br(),
          shiny::tags$h4("Password"),
          "This is set in Best Practice through the Setup-Configuration-Database menu.",
          shiny::br(),
          status = "primary",
          size = "xs",
          width = "600px",
          icon = icon("question-circle"),
          animate = shinyWidgets::animateOptions(
            enter = shinyWidgets::animations$fading_entrances$fadeIn,
            exit = shinyWidgets::animations$fading_exits$fadeOut
          ),
          tooltip = shinyWidgets::tooltipOptions(
            placement = "top",
            title = "Server description details"
          )
        )
      )
    ),
    DTedit::dteditmodUI(ns("servers"))
  )
}

#' server configuration module - server
#'
#' Server selector (dropdown)
#' Editable datatable with list of servers
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'  list of database servers, name of chosen server
#'  access to EMR database, access to configuration database
#'
#' @return count increments with each edit of server database
#'
#' @include calculation_definitions.R
#' required for simple encoding/decoding
servers_datatable <- function(input, output, session, dM) {
  # Practice locations/groups server part of module
  # returns server_list_change$count - increments with each GUI edit of server list
  # change in server_list_change to prompt change in selectable filter list of locations
  ns <- session$ns

  servers_list_change <- shiny::reactiveVal(0)

  servername_list <- shiny::reactiveVal(append(
    "None",
    dM$BPdatabaseNames
  ))
  # add 'None' to the list of databases
  shiny::observeEvent(dM$config_db_trigR(), {
    # observe any configuration database change
    shiny::validate(
      shiny::need(dM$BPdatabaseNames, "BPdatabase not defined")
    )
    servername_list(append("None", dM$BPdatabaseNames))
  })

  output$selection <- shiny::renderUI({
    # drop-down list of servers (including 'None')
    x <- shiny::selectInput(
      inputId = ns("server_chosen"),
      label = "Chosen Best Practice server",
      choices = servername_list(),
      selected = dM$BPdatabaseChoiceR()
    )
    if (.bcdyz.option$demonstration) {
      x <- shinyjs::disabled(x) # if demonstration mode, disable server selection
    }
    x
  })


  shiny::observeEvent(dM$BPdatabaseChoiceR(), {
    # if the choice has been changed, change the entry in the drop-down list
    # this might happen if a choice has been changed to 'None' because
    # the previously chosen database entry was not successfully opened
    shiny::updateSelectInput(session,
      inputId = "server_chosen",
      selected = dM$BPdatabaseChoiceR()
    )
  })

  shiny::observeEvent(input$server_chosen, {
    # when a different server is chosen from the input drop-down list
    # this will be the server 'Name', a character string

    newchoice <- input$server_chosen

    if (newchoice != dM$BPdatabaseChoice) {
      if (newchoice == "None") {
        shinytoastr::toastr_info(
          "Closing link to Best Practice",
          closeButton = TRUE,
          position = "bottom-left", title = "Best Practice database"
        )
      } else {
        shinytoastr::toastr_info(
          "Opening link to Best Practice",
          closeButton = TRUE,
          position = "bottom-left", title = "Best Practice database"
        )
      }
      dM$BPdatabaseChoice <- newchoice
      # selects the chosen database
      # the active $BPdatabaseChoice will also write to the configuration file
      # will reject the choice if not possible (e.g. bad database definition)
      if (dM$BPdatabaseChoice == "None" & newchoice != "None") {
        # if opening input$server_chosen failed, then
        # $BPdatabaseChoice 'reverts' to "None"
        shinytoastr::toastr_error(
          "Error opening Best Practice database",
          closeButton = TRUE, position = "bottom-left",
          timeOut = 10000
        ) # stays open ten seconds
      } else if (newchoice != "None") {
        shinytoastr::toastr_success(
          "Linking to Best Practice database successful!",
          closeButton = TRUE,
          position = "bottom-left",
          title = "Best Practice database"
        )
      }
    }
  })

  ### callback definitions for DTedit
  servers.insert.callback <- function(data, row) {
    # adding a new server description

    tryCatch(dM$server.insert(data[row, ]),
      error = function(e) stop(e)
    )
    # possible errors include $Name already being used
    # or not all entries described
    # $server.insert will write to the SQLite configuration

    data[row, ]$id <- max(c(data$id, 0), na.rm = TRUE) + 1
    # give a new ID which is the max of the current $id
    # (including zero, if there are no data$id)
    # this is a kludge, it is the same logic as used in dM$server.insert
    # another possibility could be to copy back from dM
    # e.g. data <- dM$BPdatabase
    data[row, ]$dbPassword <- dMeasure::simple_encode(data[row, ]$dbPassword)

    servers_list_change(servers_list_change() + 1)
    # this value returned by module

    return(data)
  }
  servers.update.callback <- function(data, olddata, row) {
    # change (update) a server description

    if (!is.na(data[row, ]$dbPassword) && # is.na shouldn't happen...
      data[row, ]$dbPassword == olddata[row, ]$dbPassword) {
      data[row, ]$dbPassword <- dMeasure::simple_decode(data[row, ]$dbPassword)
      # the password was not changed, but the 'old password' was encrypted!
      # need to decrypt before re-encrypting
    }

    tryCatch(dM$server.update(data[row, ]),
      error = function(e) stop(e)
    )
    # possible errors include the server is currently being used
    # or proposed name is same as another definition
    # $server.update will write to the SQLite configuration

    data[row, ]$dbPassword <- dMeasure::simple_encode(data[row, ]$dbPassword)

    servers_list_change(servers_list_change() + 1) # this value returned by module

    return(data)
  }
  servers.delete.callback <- function(data, row) {
    # delete a server description

    tryCatch(dM$server.delete(data[row, ]),
      error = function(e) stop(e)
    )
    # possible errors include the server is currently being used
    # $server.delete will write to the SQLite configuration

    servers_list_change(servers_list_change() + 1) # this value returned by module

    return(data[-c(row), ])
  }

  servers_dt_viewcols <- c("id", "Name", "Address", "Database", "Driver", "UserID")
  # columns viewed in DTedit when adding/editing/removing servers
  # 'id' is likely not necessary for end-users
  servers_dt_editcols <- c("Name", "Address", "Database", "Driver", "UserID", "dbPassword")

  server_driver_choices <- c(unique(unlist(odbc::odbcListDrivers()$name)))
  # it is not possible to add an "" empty-string option, which means currently
  # a 'default' choice cannot be chosen (although the 'default' of 'SQL Server' if
  # the string is empty is recognized by dMeasure)

  # depends on modularized version of DTedit
  shiny::observeEvent(dM$BPdatabaseR(), ignoreNULL = TRUE, once = TRUE, {
    servers_edited <- callModule(
      DTedit::dteditmod, "servers",
      thedata = dM$BPdatabaseR, # pass a ReactiveVal
      view.cols = servers_dt_viewcols, # no need to show 'id' in future
      edit.cols = servers_dt_editcols,
      input.types = c(
        Name = "textInput", Address = "textInput",
        Database = "textInput", Driver = "selectInput",
        UserID = "textInput", dbPassword = "passwordInput"
      ),
      input.choices = list(Driver = server_driver_choices),
      # a valid choice for Driver is "", which is the 'default'
      # e.g. 'SQL Server'
      callback.update = servers.update.callback,
      callback.insert = servers.insert.callback,
      callback.delete = servers.delete.callback,
      inputEvent = list(
        Name = function(x, value) {
          # need to add DTedit (modular) namespace to returned inputId
          # strangely, this doesn't need to be done for shinyFeedback (!)
          if (!is.null(value) && value != "") {
            result_color = "#5cb85c" # success
            result_icon = shiny::icon("ok", lib = "glyphicon")
          } else {
            result_color = "#F89406" # warning
            result_icon = shiny::icon("warning-sign", lib = "glyphicon")
          }
          shinyFeedback::hideFeedback(x) # hide previous feedback
          shinyFeedback::showFeedback(
            inputId = x,
            text = "'Name' needs to be defined",
            color = result_color,
            icon = result_icon
          )
        },
        Address = function(x, value) {
          # need to add DTedit (modular) namespace to returned inputId
          # strangely, this doesn't need to be done for shinyFeedback (!)
          if (!is.null(value) &&
              substring(value, nchar(value) - nchar("\\bpsinstance") + 1) == "\\bpsinstance") {
            result_color = "#5cb85c" # success
            result_icon = shiny::icon("ok", lib = "glyphicon")
          } else {
            result_color = "#F89406" # warning
            result_icon = shiny::icon("warning-sign", lib = "glyphicon")
          }
          shinyFeedback::hideFeedback(x) # hide previous feedback
          shinyFeedback::showFeedback(
            inputId = x,
            text = "Address is of the form 'SERVER\\bpsinstance'",
            color = result_color,
            icon = result_icon
          )
        },
        Database = function(x, value) {
          # need to add DTedit (modular) namespace to returned inputId
          # strangely, this doesn't need to be done for shinyFeedback (!)
          if (!is.null(value) &&
              (value == "bpspatients" || value == "bpssamples")) {
            result_color = "#5cb85c" # success
            result_icon = shiny::icon("ok", lib = "glyphicon")
          } else {
            result_color = "#F89406" # warning
            result_icon = shiny::icon("warning-sign", lib = "glyphicon")
          }
          shinyFeedback::hideFeedback(x) # hide previous feedback
          shinyFeedback::showFeedback(
            inputId = x,
            text = "Usually 'bpspatients', or 'bpssamples' (sample database)",
            color = result_color,
            icon = result_icon
          )
        },
        UserID = function(x, value) {
          # need to add DTedit (modular) namespace to returned inputId
          # strangely, this doesn't need to be done for shinyFeedback (!)
          if (!is.null(value) && value == "bpsrawdata") {
            result_color = "#5cb85c" # success
            result_icon = shiny::icon("ok", lib = "glyphicon")
          } else {
            result_color = "#F89406" # warning
            result_icon = shiny::icon("warning-sign", lib = "glyphicon")
          }
          shinyFeedback::hideFeedback(x) # hide previous feedback
          shinyFeedback::showFeedback(
            inputId = x,
            text = "Usually 'bpsrawdata'",
            color = result_color,
            icon = result_icon
          )
        }
      ),
      # only show new/copy/delete/update if not demonstration mode
      show.delete = .bcdyz.option$demonstration == FALSE,
      show.update = .bcdyz.option$demonstration == FALSE,
      show.insert = .bcdyz.option$demonstration == FALSE,
      show.copy = .bcdyz.option$demonstration == FALSE
    )
  })

  return(list(
    count = reactive({
      servers_list_change()
    })
  ))
  # increments each time a callback changes BPdatabase()
}
