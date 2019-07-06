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
  ns <- NS(id)

  tagList(
    wellPanel(
      uiOutput(ns("selection"))
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
#' @param	BPdatabase reactiveval, list of servers
#' @param BPdatabaseChoice reactiveval, name of chosen server
#' @param emr_db R6 object, current Best Practice (electronic medical record 'EMR') database in use
#' @param config_db R6 object, access to configuration database
#'
#' @return count increments with each edit of server database
#'
#' @include calculation_definitions.R
#' required for simple encoding/decoding
servers_datatable <- function(input, output, session, BPdatabase, BPdatabaseChoice, emr_db, config_db) {
  # as of 22nd June 2019, this function doesn't actually use emr_db
  #
  # Practice locations/groups server part of module
  # returns server_list_change$count - increments with each GUI edit of server list
  # change in server_list_change to prompt change in selectable filter list of locations
  ns <- session$ns

  servers_dt_viewcols <- c("id", "Name", "Address", "Database", "UserID")
  # columns viewed in DTedit when adding/editing/removing servers
  # 'id' is likely not necessary for end-users
  servers_dt_editcols <- c("Name", "Address", "Database", "UserID", "dbPassword")

  chosen_database <- reactiveVal(NULL) # ID of chosen database
  servers_list_change <- reactiveVal(0)

  servername_list <- reactiveVal(append("None", isolate(BPdatabase()$Name)))
  # add 'None' to the list of databases
  observeEvent(c(BPdatabase(), reactive(config_db$conn())), {
    # when the database list is updated, either change in list
    # or configuration file has been opened
    validate(
      need(!is.null(BPdatabase()), "BPdatabase not defined")
    )
    servername_list(append("None", BPdatabase()$Name))
  })

  output$selection <- renderUI({
    # drop-down list of servers (including 'None')
    selectInput(inputId = ns("server_chosen"), label = "Chosen Best Practice server",
                choices = servername_list(), selected = isolate(BPdatabaseChoice()))
  })

  observeEvent(BPdatabaseChoice(), {
    # if the choice has been changed, changed the entry in the drop-down list
    # this might happen if a choice has been changed to 'None' because
    # the previously chosen database entry was not successfully opened
    updateSelectInput(session, inputId = "server_chosen", selected = BPdatabaseChoice())
  })

  observeEvent(input$server_chosen, {
    # when a different server is chosen from the input drop-down list
    chosen_database(input$server_chosen)
    # this will be the server 'Name', a character string
    BPdatabaseChoice(input$server_chosen)
    ## then need to update configuration file
    if (nrow(config_db$conn() %>% tbl("ServerChoice") %>% filter(id ==1) %>% collect())) {
      # already an entry in the ServerChoice table
      query <- "UPDATE ServerChoice SET Name = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(input$server_chosen, 1))
    } else {
      # create a new entry
      query <- "INSERT INTO ServerChoice (id, Name) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(1, input$server_chosen))
    }

    config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method
  })

  ### callback definitions for DTedit
  servers.insert.callback <- function(data, row) {
    # adding a new server description
    if (toupper(data[row,]$Name) %in% toupper(append(data[-row,]$Name, "None"))) {
      # if the proposed server is the same as one that already exists
      # (ignoring case)
      stop("New server name cannot be the same as existing names, or 'None'")
    } else if (stringi::stri_length(data[row,]$Name) == 0 |
               stringi::stri_length(data[row,]$Address) == 0 |
               stringi::stri_length(data[row,]$Database) == 0 |
               stringi::stri_length(data[row,]$UserID) == 0 |
               stringi::stri_length(data[row,]$dbPassword) == 0) {
      stop("All entries must be described")
    } else {

      newid <- max(c(as.data.frame(BPdatabase())$id, 0)) + 1
      # initially, BPdatabase()$id might be an empty set, so need to append a '0'
      data[row, ]$id <- newid
      data[row, ]$dbPassword <- simple_encode(data[row, ]$dbPassword)
      # immediately encode password.
      # stored encrypted both in memory and in configuration file

      query <- "INSERT INTO Server (id, Name, Address, Database, UserID, dbPassword) VALUES (?, ?, ?, ?, ?, ?)"
      data_for_sql <- as.list.data.frame(c(newid, data[row,]$Name, data[row,]$Address,
                                           data[row,]$Database, data[row,]$UserID,
                                           data[row,]$dbPassword))

      config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method


      BPdatabase(data) # update the dataframe in memory
      servers_list_change(servers_list_change() + 1)
      # this value returned by module

      return(BPdatabase())
    }
  }
  servers.update.callback <- function(data, olddata, row) {
    # change (update) a server description

    if (toupper(data[row,]$Name) %in% toupper(append(data[-row,]$Name, "None"))) {
      # if the proposed server is the same as one that already exists
      # (ignoring case)
      stop("New server name cannot be the same as existing names, or 'None'")
    } else if (toupper(data[row,]$Name) == toupper(BPdatabaseChoice())) {
      stop(paste0("Cannot edit '", data[row,]$Name, "', currently in use!"))
    } else if (stringi::stri_length(data[row,]$Name) == 0 |
               stringi::stri_length(data[row,]$Address) == 0 |
               stringi::stri_length(data[row,]$Database) == 0 |
               stringi::stri_length(data[row,]$UserID) == 0 |
               stringi::stri_length(data[row,]$dbPassword) == 0) {
      stop("All entries must be described")
    } else {
      data[row, ]$dbPassword <- simple_encode(data[row, ]$dbPassword)
      # immediately encode password.
      # stored encrypted both in memory and in configuration file

      query <- "UPDATE Server SET Name = ?, Address = ?, Database = ?, UserID = ?, dbPassword = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(data[row,]$Name, data[row,]$Address,
                                           data[row,]$Database, data[row,]$UserID,
                                           data[row,]$dbPassword, data[row,]$id))

      config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method

      BPdatabase(data) # store new values in copy of settings in memory
      servers_list_change(servers_list_change() + 1) # this value returned by module

      return(BPdatabase())
    }
  }
  servers.delete.callback <- function(data, row) {
    # delete a server description
    if (toupper(data[row,]$Name) == toupper(BPdatabaseChoice())) {
      stop(paste0("Cannot remove '", data[row,]$Name, "', currently in use!"))
    } else {
      query <- "DELETE FROM Server WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(data[row,]$id))

      config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method

      BPdatabase(data[-c(row),])
      servers_list_change(servers_list_change() + 1) # this value returned by module
    }
    return(BPdatabase())
  }

  # depends on modularized version of DTedit
  servers_edited <- callModule(DTedit::dtedit, "servers",
                               thedataframe = BPdatabase, # pass a ReactiveVal
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
