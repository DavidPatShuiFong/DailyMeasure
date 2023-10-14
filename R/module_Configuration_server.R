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
          shiny::tags$h4("Password Extra Encryption"),
          "The Best Practice database password is *always* stored with encryption.",
          "You can add an *additional* encryption key.",
          "If you wish to encrypt the database password with an extra encryption key,",
          "the extra encryption key must be re-entered each time you enter a new database password,",
          "and you must re-enter a database password each time you enter an encryption key!",
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

  # much of the section below is (unfortunately) also in DailyMeasureServer.R
  # when the user manually sets a new server choice

  extraEncryptionModal <- function(failed = FALSE) {
    # this modal is shown if a server is chosen which has been encoded with an extra encryption key
    # in which case, the user must provide the key to use the modal
    #
    # ask for key/password used to provide extra encryption for database password
    # if 'failed' = TRUE, then display a message that the previous value was invalid
    shiny::modalDialog(
      shiny::passwordInput(
        inputId = ns("dbPasswordKey"),
        "Database Password Extra Encryption Key",
        placeholder = "Enter extra encryption key here, then click 'OK'"
      ),
      shiny::span(
        "The database password for this choice has been encrypted with an extra key/password"
      ),
      if (failed) {
        shiny::div(shiny::tags$b("Wrong key/password", style = "color: red;"))
      },
      footer = shiny::tagList(
        shiny::actionButton(inputId = ns("dbPasswordKey_cancel"), "Cancel"),
        shiny::actionButton(inputId = ns("dbPasswordKey_ok"), "OK")
      )
    )
  }

  new_server_choice <- shiny::reactiveVal(value = "") # name of new server choice
  new_server_choice_ready <- shiny::reactiveVal(value = 0)
  # new_server_choice_ready will be triggered when the new server choice is ready,
  # e.g. after the extra encryption key is verified

  shiny::observeEvent(input$server_chosen, {
    # when a different server is chosen from the input drop-down list

    # this will be the server 'Name', a character string
    newchoice <- input$server_chosen

    if (newchoice != dM$BPdatabaseChoice) {
      new_server_choice(newchoice) # set reactive 'global' (to the module) variable
      if (newchoice == "None") {
        new_server_choice_ready(shiny::isolate(new_server_choice_ready()) + 1)
        # trigger server change if server choice is 'none'
      } else {
        # server choice is something other than 'none'
        # first check if the new choice has extra encryption
        # applied to the database password
        newchoice_id <- dM$BPdatabase %>>%
          dplyr::filter(Name == newchoice) %>>%
          dplyr::pull(id)
        if (dM$dbPasswordExtraVerify(description = list(id = newchoice_id, key = ""))) {
          # if returns TRUE, then there is no need to ask for encryption key, since the
          # 'extra encryption' key is not set
          new_server_choice_ready(shiny::isolate(new_server_choice_ready()) + 1)
          # immediately trigger server change
        } else {
          # extra encryption for the database password has been set
          shiny::showModal(extraEncryptionModal())
          # show the modal, do not immediately trigger server change
          # (server change will only occur upon modal closing, either success or failure)
        }
      }
    }
  })

  shiny::observeEvent(input$dbPasswordKey_ok, ignoreNULL = TRUE, {
    # the Okay button has been clicked on the database extra key/password modal

    # test the validity of the password
    newchoice_id <- dM$BPdatabase %>>%
      dplyr::filter(Name == shiny::isolate(new_server_choice())) %>>%
      dplyr::pull(id)
    key_valid <- dM$dbPasswordExtraVerify(description = list(id = newchoice_id, key = input$dbPasswordKey))
    if (key_valid) {
      # success!
      # remove modal, then set the database password extra encryption key
      shiny::removeModal()
      dM$dbPasswordExtraEncryption <- input$dbPasswordKey
      new_server_choice_ready(shiny::isolate(new_server_choice_ready()) + 1)
      # trigger server change
    } else {
      # show the modal again, but indicate wrong key/password
      shiny::showModal(extraEncryptionModal(failed = TRUE))
    }
  })

  shiny::observeEvent(input$dbPasswordKey_cancel, ignoreNULL = TRUE, {
    # the cancel button has been clicked on the database extra key/password modal
    shiny::removeModal()
    # set server choice to 'None'
    new_server_choice("None") # set global variable
    # trigger server change (to 'None')
    new_server_choice_ready(shiny::isolate(new_server_choice_ready()) + 1)
  })

  shiny::observeEvent(new_server_choice_ready(), ignoreInit = TRUE, {
    # when a different server is chosen and is 'ready' for opening
    # in some cases, the server database password has had extra encryption applied
    # so we had to wait for the user to enter the (correct) encryption key
    # this section does *not* check for encryption key validiy, that is presumed
    # to have already been done

    # new_server_choice will have the name of the new server

    server_choice <- shiny::isolate(new_server_choice())
    if (server_choice == "None") {
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
    dM$BPdatabaseChoice <- server_choice
    # selects the chosen database
    # the active $BPdatabaseChoice will also write to the configuration file
    # will reject the choice if not possible (e.g. bad database definition)
    if (dM$BPdatabaseChoice == "None" & server_choice != "None") {
      # if opening input$server_chosen failed, then
      # $BPdatabaseChoice 'reverts' to "None"
      shinytoastr::toastr_error(
        "Error opening Best Practice database",
        closeButton = TRUE, position = "bottom-left",
        timeOut = 10000
      ) # stays open ten seconds
    } else if (server_choice != "None") {
      shinytoastr::toastr_success(
        "Linking to Best Practice database successful!",
        closeButton = TRUE,
        position = "bottom-left",
        title = "Best Practice database"
      )
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
    if (data[row, ]$dbPasswordExtraEncryption != "") {
      # if an extra encruption key is available, use the extra key
      data[row, ]$dbPassword <- dMeasure::simple_encode(
        data[row, ]$dbPassword, key = data[row, ]$dbPasswordExtraEncryption
      )
      data[row, ]$dbPasswordExtraEncryption <- sodium::password_store(
        data[row, ]$dbPasswordExtraEncryption
      ) # immediately hash the extra key
    }

    servers_list_change(servers_list_change() + 1)
    # this value returned by module

    return(data)
  }
  servers.update.callback <- function(data, olddata, row) {
    # change (update) a server description

    if (!is.na(data[row, ]$dbPassword) &&
        !is.na(olddata[row,]$dbPassword) &&
        # is.na shouldn't happen...but perhaps corrupted table
        data[row, ]$dbPassword == olddata[row, ]$dbPassword) {
      data[row, ]$dbPassword <- dMeasure::simple_decode(data[row, ]$dbPassword)
      # the password was not changed, but the 'old password' was encrypted!
      # need to decrypt before re-encrypting
      # note, this does *not* work if the password had previously been
      # given extra encryption with the user-defined extra key
    }

    tryCatch(dM$server.update(data[row, ]),
      error = function(e) stop(e)
    )
    # possible errors include the server is currently being used
    # or proposed name is same as another definition
    # $server.update will write to the SQLite configuration

    data[row, ]$dbPassword <- dMeasure::simple_encode(data[row, ]$dbPassword)
    if (data[row, ]$dbPasswordExtraEncryption != "") {
      # if an extra encruption key is available, use the extra key
      data[row, ]$dbPassword <- dMeasure::simple_encode(
        data[row, ]$dbPassword, key = data[row, ]$dbPasswordExtraEncryption
      )
      data[row, ]$dbPasswordExtraEncryption <- sodium::password_store(
        data[row, ]$dbPasswordExtraEncryption
      ) # immediately hash the extra key
    }

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
  servers_dt_editcols <- c(
    "Name", "Address", "Database", "Driver", "UserID", "dbPassword", "dbPasswordExtraEncryption"
  )
  servers_dt_editcols.labels <- c(
    "Name", "Address", "Database", "Driver", "UserID",
    "Database Password", "Database Password Extra Encryption"
  )

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
      edit.label.cols = servers_dt_editcols.labels,
      input.types = c(
        Name = "textInput", Address = "textInput",
        Database = "textInput", Driver = "selectInput",
        UserID = "textInput", dbPassword = "passwordInput",
        dbPasswordExtraEncryption = "passwordInput"
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
        },
        dbPasswordExtraEncryption = function(x, value) {
          # need to add DTedit (modular) namespace to returned inputId
          # strangely, this doesn't need to be done for shinyFeedback (!)
          if (!is.null(value) && value != "") {
            # place a warning if the extra password encryption is being defined
            # or already defined
            result_color = "#F89406" # warning
            result_icon = shiny::icon("warning-sign", lib = "glyphicon")
            text = paste(
              "If the extra encryption key is being defined, or has been defined,",
              "both the dbPassword and dbPasswordExtraEncryption need to be re-entered.",
              "If you no longer need an extra encryption key, 'delete' all the text in this field."
            )
          } else {
            # general information about extra password encryption if there is
            # no extra password/key defined
            result_color = "#5cb85c" # success
            result_icon = shiny::icon("ok", lib = "glyphicon")
            text = paste(
              "The Best Practice database password is *always* stored with encryption.",
              "You can add an *additional* encryption key."
            )
          }
          shinyFeedback::hideFeedback(x) # hide previous feedback
          shinyFeedback::showFeedback(
            inputId = x,
            text = text,
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
