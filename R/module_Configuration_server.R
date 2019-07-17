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
      shiny::uiOutput(ns("selection"))
    ),
    DTedit::dteditUI(ns("servers"))
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
  # as of 22nd June 2019, this function doesn't actually use emr_db
  #
  # Practice locations/groups server part of module
  # returns server_list_change$count - increments with each GUI edit of server list
  # change in server_list_change to prompt change in selectable filter list of locations
  ns <- session$ns

  servers_list_change <- shiny::reactiveVal(0)

  servername_list <- shiny::reactiveVal(append("None",
                                               dM$BPdatabaseNames))
  # add 'None' to the list of databases
  shiny::observeEvent(dM$config_db_trigR(), {
    # observe any configuration database change
    shiny::validate(
      shiny::need(!is.null(dM$BPdatabaseNames), "BPdatabase not defined")
    )
    servername_list(append("None", dM$BPdatabaseNames))
  })

  output$selection <- shiny::renderUI({
    # drop-down list of servers (including 'None')
    shiny::selectInput(inputId = ns("server_chosen"),
                       label = "Chosen Best Practice server",
                       choices = servername_list(),
                       selected = dM$BPdatabaseChoice)
  })

  shiny::observeEvent(dM$BPdatabaseChoiceR(), {
    # if the choice has been changed, change the entry in the drop-down list
    # this might happen if a choice has been changed to 'None' because
    # the previously chosen database entry was not successfully opened
    shiny::updateSelectInput(session,
                             inputId = "server_chosen",
                             selected = dM$BPdatabaseChoiceR())
  })

  shiny::observeEvent(input$server_chosen, {
    # when a different server is chosen from the input drop-down list
    # this will be the server 'Name', a character string
    dM$BPdatabaseChoice <- input$server_chosen
    # then need to update configuration file
    # the active $BPdatabaseChoice will also write to the configuration file
    # will reject the choice if not possible (e.g. bad database definition)
  })

  ### callback definitions for DTedit
  servers.insert.callback <- function(data, row) {
    # adding a new server description

    tryCatch(dM$server.insert(data[row,]),
             error = function(e) stop(e))
    # possible errors include $Name already being used
    # or not all entries described
    # $server.insert will write to the SQLite configuration

    servers_list_change(servers_list_change() + 1)
    # this value returned by module

    return(data)
  }
  servers.update.callback <- function(data, olddata, row) {
    # change (update) a server description

    tryCatch(dM$server.update(data[row,]),
             error = function(e) stop(e))
    # possible errors include the server is currently being used
    # or proposed name is same as another definition
    # $server.update will write to the SQLite configuration

    servers_list_change(servers_list_change() + 1) # this value returned by module

    return(data)
  }
  servers.delete.callback <- function(data, row) {
    # delete a server description

    tryCatch(dM$server.delete(data[row,]),
             error = function(e) stop(e))
    # possible errors include the server is currently being used
    # $server.delete will write to the SQLite configuration

    servers_list_change(servers_list_change() + 1) # this value returned by module

    return(data[-c(row),])
  }

  servers_dt_viewcols <- c("id", "Name", "Address", "Database", "UserID")
  # columns viewed in DTedit when adding/editing/removing servers
  # 'id' is likely not necessary for end-users
  servers_dt_editcols <- c("Name", "Address", "Database", "UserID", "dbPassword")

  # depends on modularized version of DTedit
  servers_edited <- callModule(DTedit::dtedit, "servers",
                               thedataframe = dM$BPdatabaseR, # pass a ReactiveVal
                               view.cols = servers_dt_viewcols, # no need to show 'id' in future
                               edit.cols = servers_dt_editcols,
                               input.types = c(Name = 'textInput', Address = 'textInput',
                                               Database = 'textInput', UserID = 'textInput',
                                               dbPassword = 'passwordInput'),
                               callback.update = servers.update.callback,
                               callback.insert = servers.insert.callback,
                               callback.delete = servers.delete.callback
  )

  return(list(
    count = reactive({servers_list_change()})
  ))
  # increments each time a callback changes BPdatabase()
}
