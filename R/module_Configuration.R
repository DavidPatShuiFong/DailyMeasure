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
#' @param emrpool reactiveval, current Best Practice (electronic medical record 'EMR') database pool in use
#' @param config_pool reactiveval, access to configuration database
#'
#' @return count increments with each edit of server database
#'
#' @include calculation_definitions.R
#' required for simple encoding/decoding
servers_datatable <- function(input, output, session, BPdatabase, BPdatabaseChoice, emrpool, config_pool) {
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
  observeEvent(c(BPdatabase(), config_pool()), {
    # when the database list is updated, either change in list
    # or configuration file has been opened
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
    ## then need to update configuraiton file
    if (nrow(config_pool() %>% tbl("ServerChoice") %>% filter(id ==1) %>% collect())) {
      # already an entry in the ServerChoice table
      query <- "UPDATE ServerChoice SET Name = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(input$server_chosen, 1))
    } else {
      # create a new entry
      query <- "INSERT INTO ServerChoice (id, Name) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(1, input$server_chosen))
    }

    connection <- pool::poolCheckout(config_pool()) # can't write with the pool
    rs <- DBI::dbSendQuery(connection, query)
    # parameterized query can handle apostrophes etc.
    DBI::dbBind(rs, data_for_sql)
    # for statements, rather than queries, we don't need to dbFetch(rs)
    # update database
    DBI::dbClearResult(rs)
    pool::poolReturn(connection)

  })

  ### callback definitions for DTedit
  servers.insert.callback <- function(data, row) {
    # adding a new server description
    if (toupper(data[row,]$Name) %in% toupper(append(data[-row,]$Name, "None"))) {
      # if the proposed server is the same as one that already exists
      # (ignoring case)
      stop("New server name cannot be the same as existing names, or 'None'")
    } else if (stringr::str_length(data[row,]$Name) == 0 |
               stringr::str_length(data[row,]$Address) == 0 |
               stringr::str_length(data[row,]$Database) == 0 |
               stringr::str_length(data[row,]$UserID) == 0 |
               stringr::str_length(data[row,]$dbPassword) == 0) {
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

      connection <- pool::poolCheckout(config_pool()) # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query)
      # parameterized query can handle apostrophes etc.
      DBI::dbBind(rs, data_for_sql)
      # for statements, rather than queries, we don't need to dbFetch(rs)
      # update database
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)

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
    } else if (toupper(data[row,]$Name == "NONE")) {
      stop("New server name cannot be 'None'!")
    } else if (stringr::str_length(data[row,]$Name) == 0 |
               stringr::str_length(data[row,]$Address) == 0 |
               stringr::str_length(data[row,]$Database) == 0 |
               stringr::str_length(data[row,]$UserID) == 0 |
               stringr::str_length(data[row,]$dbPassword) == 0) {
      stop("All entries must be described")
    } else {
      data[row, ]$dbPassword <- simple_encode(data[row, ]$dbPassword)
      # immediately encode password.
      # stored encrypted both in memory and in configuration file

      query <- "UPDATE Server SET Name = ?, Address = ?, Database = ?, UserID = ?, dbPassword = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(data[row,]$Name, data[row,]$Address,
                                           data[row,]$Database, data[row,]$UserID,
                                           data[row,]$dbPassword, data[row,]$id))

      connection <- pool::poolCheckout(config_pool()) # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query) # update database
      DBI::dbBind(rs, data_for_sql)
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)

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

      connection <- pool::poolCheckout(config_pool()) # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query) # update database
      DBI::dbBind(rs, data_for_sql)
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)

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

##### locations - editable datatable module #######################################################

#' location configuration module - UI function
#'
#' Editable datatable with list of locations
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
locations_datatableUI <- function(id) {
  ns <- NS(id)

  tagList(
    DTedit::dteditUI(ns("locations"))
  )
}

#' location configuration module - server
#'
#' Editable datatable with list of locations
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param	PracticeLocations reactiveval, list of locations
#' @param UserConfig reactiveval, list of users who have been configured
#' @param config_pool reactiveval, access to configuration database
#'
#' @return count - increments with each edit of server database
locations_datatable <- function(input, output, session,
                                PracticeLocations, UserConfig, config_pool) {
  # Practice locations/groups server part of module
  # input : PracticeLocations() reactiveval, list of PracticeLocations
  # input : Users - access to Users database. avoid deleting location currently in 'use' by user
  # input : config_pool - reactiveval, access to configuration database
  # returns location_list_change - increments with each GUI edit of location list
  # change in location_list_change to prompt change in selectable filter list of locations

  # callback functions for DTEdit
  ## locations

  locations_dt_viewcols <- c("id", "Name", "Description")
  # columns viewed in DTedit when adding/editing/removing locations
  # 'id' is likely not necessary for end-users

  userlocations <- reactiveVal()
  # list of user names
  observeEvent(UserConfig(), {
    userlocations(UserConfig()$Location %>% unlist(use.names = FALSE))
    # extract from EMR database. note that this is NOT reactive to underlying change in EMR database
    # can't exclude names already configured, because this is also used when
    # editing a current user configuration
  })

  location_list_change <- reactiveVal(0)

  ### callback definitions for DTedit location
  locations.insert.callback <- function(data, row) {
    # adding a new practice location
    if (length(grep(toupper(data[row, ]$Name),
                    toupper(as.data.frame(isolate(PracticeLocations()))$Name)))){
      # if the proposed new name is the same as one that already exists
      # (ignoring case). grep returns empty integer list if no match
      stop("New practice location name cannot be the same as existing names")
    } else if (is.null(data[row,]$Name)){
      stop("New practice location name cannot be 'empty'!")
    } else {

      newid <- max(c(as.data.frame(PracticeLocations())$id, 0)) + 1
      # initially, PracticeLocations$id might be an empty set
      # so need to append a '0'
      data[row, ]$id <- newid

      query <- "INSERT INTO Location (id, Name, Description) VALUES (?, ?, ?)"
      data_for_sql <- as.list.data.frame(c(newid, data[row,]$Name, data[row,]$Description))

      connection <- pool::poolCheckout(config_pool()) # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query)
      # parameterized query can handle apostrophes etc.
      DBI::dbBind(rs, data_for_sql)
      # for statements, rather than queries, we don't need to dbFetch(rs)
      # update database
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)

      PracticeLocations(data) # update the dataframe in memory
      location_list_change(location_list_change() + 1)
      # this value returned by module

      return(PracticeLocations())
    }
  }

  locations.update.callback <- function(data, olddata, row) {
    # change (update) a practice location

    if (length(grep(toupper(data[row, ]$Name),
                    toupper(data[-row,]$Name)))){
      # if the proposed new name is the same as one that already exists
      # (ignoring case). grep returns empty integer list if no match
      stop("New practice location name cannot be the same as existing names")
    } else if (is.null(data[row,]$Name)){
      stop("New practice location name cannot be 'empty'!")
    } else if ((olddata[row,]$Name %in% userlocations()) &
               (olddata[row,]$Name != data[row,]$Name)) {
      stop(paste0("Cannot change the name of '", olddata[row,]$Name,
                  "', this location is assigned to a user."))
    } else {
      query <- "UPDATE Location SET Name = ?, Description = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(data[row,]$Name, data[row,]$Description, data[row,]$id))

      connection <- pool::poolCheckout(config_pool()) # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query) # update database
      DBI::dbBind(rs, data_for_sql)
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)

      PracticeLocations(data)
      location_list_change(location_list_change() + 1)
      # this value returned by module

      return(PracticeLocations())
    }
  }
  locations.delete.callback <- function(data, row) {
    # delete a practice location
    if (data[row,]$Name %in% userlocations()) {
      stop(paste0("Cannot remove '", data[row,]$Name,
                  "', this location is assigned to a user."))
    } else {
      query <- "DELETE FROM Location WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(data[row,]$id))

      connection <- pool::poolCheckout(config_pool())
      # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query) # update database
      DBI::dbBind(rs, data_for_sql)
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)

      PracticeLocations(data[-c(row),])
      location_list_change(location_list_change() + 1) # this value returned by module
    }
    return(PracticeLocations())
  }

  # depends on modularized version of DTedit
  locations_edited <- callModule(DTedit::dtedit, "locations",
                                 thedataframe = PracticeLocations, # pass a ReactiveVal
                                 view.cols = locations_dt_viewcols, # no need to show 'id' in future
                                 edit.cols = c("Name", "Description"),
                                 edit.label.cols = c('Practice Locations', 'Description'),
                                 show.copy = FALSE,
                                 input.types = c(Name = 'textInput', Description = 'textInput'),
                                 callback.update = locations.update.callback,
                                 callback.insert = locations.insert.callback,
                                 callback.delete = locations.delete.callback
  )

  return(reactive({location_list_change()}))
  # increments each time a callback changes PracticeLocations()
}

# restriction types
#  ServerAdmin - only users with ServerAdmin attribute can view/change server settings
#  UserAdmin - only users with UserAdmin attribute can view/change user settings
#  GlobalActionView - only users with GlobalActionView attribute can see
#                     potential actions in "other" people's appointment books
#  GlobalBillView   - only users with GlobalBillView attribute can see
#                     billings in "other" people's appointment books
#  GlobalCDMView    - only users with GlobalCDMView attribute can see
#                     potential CDM actions in "other" people's appointment books
restrictionTypes <- list(
  list(
    id = "ServerAdmin", label = "Server Administrator",
    Description = "Only ServerAdmin users can change database server settings",
    callback = function (state, AttributeList, anyPassword) {
      # these callbacks have no immediate access to the parent environment
      # only called if state is changed from the old state
      # @param state          : attempted new state
      # @param AttributeList  : current list of user attributes in use
      # @param anyPassword    : any passwords TRUE or FALSE
      #
      # @return newstate      : returns 'state', if permissible
      #
      # note that this function does not change UserRestrictions()
      # changing UserRestrictions() is the responsibility of the calling function
      newstate = state
      if (state == TRUE) {
        # if trying to turn on ServerAdmin restriction
        if (!("ServerAdmin" %in% AttributeList)) {
          # no user is listed as having ServerAdmin attribute!
          # if this restriction is established, no one can edit the server
          # (though this could be worked around by changing user settings)
          shinytoastr::toastr_error(
            "At least one user must have 'ServerAdmin' attribute to enable 'ServerAdmin' restriction.",
            title = "Can't enable 'ServerAdmin' restriction",
            closeButton = TRUE, position = "top-center",
            timeOut = 5000) # stays open five seconds
          newstate = FALSE
        } else {
          newstate = TRUE
          # allow ServerAdmin to be restricted
        }
      } else {
        # turning off ServerAdmin restriction
        shinytoastr::toastr_warning(
          "Without this restriction, anyone can edit and change Best Practice database settings!",
          title = "Disabling 'ServerAdmin' restriction",
          closeButton = TRUE, position = "top-center",
          timeOut = 5000) # stays open five seconds
      }
      return(newstate)
    }
  ),
  list(
    id = "UserAdmin", label = "User Administrator",
    Description = "Only UserAdmin users can change user permissions",
    callback = function (state, AttributeList, anyPassword) {
      # these callbacks have no immediate access to the parent environment
      # only called if state is changed from the old state
      # @param state          : attempted new state
      # @param AttributeList  : current list of user attributes in use
      # @param anyPassword    : any passwords TRUE or FALSE
      #
      # @return newstate      : returns 'state', if permissible
      #
      # note that this function does not change UserRestrictions()
      # changing UserRestrictions() is the responsibility of the calling function
      newstate = state
      if (state == TRUE) {
        # if trying to turn on ServerAdmin restriction
        if (!("UserAdmin" %in% AttributeList)) {
          # no user is listed as having UserAdmin attribute!
          # if this restriction is established, no one can edit the user settings
          shinytoastr::toastr_error(
            "A least one user must have 'UserAdmin' attribute to enable the 'UserAdmin' restriction.",
            title = "Can't enable 'UserAdmin' restriction",
            closeButton = TRUE, position = "top-center",
            timeOut = 5000) # stays open five seconds
          newstate = FALSE
        } else {
          newstate = TRUE
          # allow UserAdmin to be restricted
        }
      } else {
          shinytoastr::toastr_warning(
            "Without this restriction, anyone can edit and change user permission settings!",
            title = "Disabling 'UserAdmin' restriction",
            closeButton = TRUE, position = "top-center",
            timeOut = 5000) # stays open five seconds
      }
      return(newstate)
    }
  ),
  list(
    id = "GlobalActionView", label = "Global Action View",
    Description = "GlobalActionView users can view actions in 'other' appointment lists",
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)}
  ),
  list(
    id = "GlobalBillView", label = "Global Bill View",
    Description = "GlobalBillView users can view billing status in 'other' appointment listss",
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)
    }
  ),
  list(
    id = "GlobalCDMView", label = "Global CDM View",
    Description = "GlobalCDMView users can view CDM status in 'other' appointment lists",
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)
    }
  ),
  list(
    id = "RequirePasswords", label = "Require Passwords",
    Description = "Password required from all users",
    callback = function (state, AttributeList, anyPassword) {
      # these callbacks have no immediate access to the parent environment
      # only called if state is changed from the old state
      # @param state          : attempted new state
      # @param AttributeList  : current list of user attributes in use
      # @param anyPassword    : any passwords TRUE or FALSE
      #
      # @return newstate      : returns 'state', if permissible
      #
      # note that this function does not change UserRestrictions()
      # changing UserRestrictions() is the responsibility of the calling function
      newstate = state
      if (state == TRUE) {
        # if trying to turn on RequirePasswords restriction
        if (anyPassword == FALSE) {
          # no user is listed as having a password!
          # if this restriction is established, no one will be able to log in
          shinytoastr::toastr_error(
            "A least one user must have a password to enable the 'Require Passwords' restriction.",
            title = "Can't enable 'Require Passwords' restriction",
            closeButton = TRUE, position = "top-center",
            timeOut = 5000) # stays open five seconds
          newstate = FALSE
        } else {
          newstate = TRUE
          # allow RequirePassword
        }
      } else {
        shinytoastr::toastr_warning(
          "Without this restriction, users do not require passwords",
          title = "Disabling 'Require Passwords' restriction",
          closeButton = TRUE, position = "top-center",
          timeOut = 5000) # stays open five seconds
      }
      return(newstate)
    }
  )
)

##### users config - editable datatable module ######################################################
#'
#' user configuration module - user interface function
#'
#' Editable datatable with list of servers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
userconfig_datatableUI <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel(
        title = "User settings",
        width = 12,
        DTedit::dteditUI(ns("userconfigs"))
      ),
      tabPanel(
        title = "Reset password",
        width = 12,
        wellPanel(
          "Only configured users can have passwords reset (or set)",
          br(), br(),
          uiOutput(ns("ConfiguredUserList")),
          br(),
          actionButton(ns('reset_password'), 'Reset Password', icon('broom'), class = 'btn btn-primary'))
      ),
      tabPanel(
        title = "Enabled Restrictions",
        width = 12,
        lapply(restrictionTypes,
               function(type) {
                 tagList(
                   br(),
                   fluidRow(
                     column(width = 3,
                            shinyWidgets::materialSwitch(
                              inputId = ns(type$id),
                              label = type$label,
                              right = TRUE,
                              value = FALSE,
                              status = "primary")),
                     column(width = 8, type$Description)
                   )
                 )
               }
        )
      )
    )
  )
}

#' user configuration module - server function
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param	UserConfig reactiveval, list of user config
#' @param UserRestrictions reactiveval, user restriction list
#' @param LocationNames list of location names (not including ID or Description)
#' @param db reactivevalues link to EMR database. includes $users and $dbversion
#' @param config_pool reactiveval, access to configuration database
#'
#' @return count - increments with each GUI edit of user configuration database
userconfig_datatable <- function(input, output, session,
                                 UserConfig, UserFullConfig, UserRestrictions,
                                 LocationNames, db, config_pool) {
  ns <- session$ns

  userconfig_dt_viewcols <- c("id", "Fullname", "AuthIdentity", "Location",
                              "Attributes")
  userconfig_dt_editcols <- userconfig_dt_viewcols[!userconfig_dt_viewcols %in% c("id")]
  # columns viewed in DTedit when adding/editing/removing user config


  ###### Password Removal ##################################################
  output$ConfiguredUserList <- renderUI({
    # create a list of configured users
    # (only configured users can have passwords)
    selectInput(inputId = ns('User_toReset_Password'), label = 'Selected User',
                choices = UserConfig()$Fullname)
  })
  observeEvent(UserConfig(), {
    # update the list if the UserConfig() changes
    # (it will change when the configuration database is read
    updateSelectInput(session, ns('User_tochange_Password'), label = 'Selected User',
                      choices = UserConfig()$Fullname)
  })
  observeEvent(input$reset_password, {
    # reset password button has been pressed
    validate(
      need(input$User_toReset_Password, "No user selected")
    )
    showModal(modalDialog(
      title= "Remove Password",
      paste0("Remove password of '", input$User_toReset_Password, "'."),
      br(), br(),
      paste0("The user will be asked for a new password when they next login."),
      br(),
      # ask for confirmation
      footer = tagList(actionButton(ns("confirmRemovePassword"),
                                    "Remove Password"),
                       modalButton("Cancel")
      )
    ))
  })
  observeEvent(input$confirmRemovePassword, {
    # reset password has been confirmed
    newUserConfig <-
      UserConfig() %>%
      mutate(Password =
               replace(Password, input$User_toReset_Password == Fullname, ""))
    UserConfig(newUserConfig) # replace password with empty string

    UserRow <- UserConfig() %>% filter(input$User_toReset_Password == Fullname)
    # just select the user for whom we are removing the password

    query <- "UPDATE Users SET Password = ? WHERE id = ?"
    # write to configuration database
    data_for_sql <- list(c(""), UserRow$id[[1]])

    connection <- pool::poolCheckout(config_pool())
    # can't write with the pool
    rs <- DBI::dbSendQuery(connection, query) # update database
    DBI::dbBind(rs, data_for_sql)
    DBI::dbClearResult(rs)
    pool::poolReturn(connection)

    removeModal()
  })

  ####### Restriction of permissions #############################################
  restrictionTypes_df <- data.frame(Reduce(rbind, restrictionTypes))
  # converts the list to a dataframe
  user_attribute_types <- unlist(restrictionTypes_df$id, use.names = FALSE)
  # user attribute types is defined in restrictionTypes

  usernames <- reactiveVal()
  # list of user names
  observeEvent(db$dbversion, {
    validate(
      need(UserFullConfig(), "No user list"),
      need(UserRestrictions(), "No restriction list")
    )
    # if the database has been connected
    if (!is.null(UserFullConfig())) {
      usernames(UserFullConfig() %>% select(Fullname) %>% collect() %>%
                  unlist(use.names = FALSE))
      # extract from EMR database. note that this is NOT reactive to
      # underlying change in EMR database
      # does not exclude names already configured, because this is also used when
      # editing a current user configuration
    }
  })

  update_UserRestrictions_database <- function() {
    # update UserRestrictions database
    # this is manually called when (one) restriction is added or removed
    # so only has to find 'one' row of difference between the 'new' list and the 'old' list
    originalRestrictions <- config_pool() %>% tbl("UserRestrictions") %>% collect()
    newRestrictions <- isolate(UserRestrictions())

    new_row <- anti_join(newRestrictions, originalRestrictions, by = "uid")
    if (nrow(new_row) > 0) {
      # if there is a new row, then add to configuration database
      query <- "INSERT INTO UserRestrictions (uid, Restriction) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(new_row$uid, new_row$Restriction))

      connection <- pool::poolCheckout(isolate(config_pool())) # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query)
      # parameterized query can handle apostrophes etc.
      DBI::dbBind(rs, data_for_sql)
      # for statements, rather than queries, we don't need to dbFetch(rs)
      # update database
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)
    } else {
      deleted_row <- anti_join(originalRestrictions, newRestrictions, by = "uid")
      if (nrow(deleted_row) > 0) {
        # if a row was deleted, then remove from configuration database
        query <- "DELETE FROM UserRestrictions WHERE uid = ?"
        data_for_sql <- as.list.data.frame(c(deleted_row$uid))

        connection <- pool::poolCheckout(isolate(config_pool()))
        # can't write with the pool
        rs <- DBI::dbSendQuery(connection, query) # update database
        DBI::dbBind(rs, data_for_sql)
        DBI::dbClearResult(rs)
        pool::poolReturn(connection)
      }
    }
  }

  observeEvent(config_pool(), {
    # if configuration pool has been initialized
    validate(
      need(UserRestrictions(), "No restriction list")
    )
    for (restriction in unlist(restrictionTypes_df$id, use.names = FALSE)) {
      # set the switches according the what is stored in the configuration database
      shinyWidgets::updateMaterialSwitch(
        session, restriction,
        restriction %in% (UserRestrictions()$Restriction))
    }
  })
  for (restriction in restrictionTypes) {
    # add each of the restrictionTypes to the user interface
    local({
      # evaluate expression in a new environment
      # (otherwise only the final expression in the loop is registered as an observer!)
      restrictionLocal <- restriction
      observeEvent(input[[restrictionLocal$id]], ignoreInit = TRUE, {
        if (input[[restrictionLocal$id]] !=
            (restrictionLocal$id) %in% UserRestrictions()$Restriction) {
          # change in state
          state <- restrictionLocal$callback(
            input[[restrictionLocal$id]],
            UserConfig()$Attributes %>% unlist(use.names = FALSE),
            # list of attributes in use
            (nchar(apply(cbind(unlist(UserConfig()$Password, use.names = FALSE)),
                        1,
                        function(x) paste(x[!is.na(x)], collapse = ""))) > 0)
            # any passwords are set?
            # this code to concatenate strings, NA or not, was found on StackOverflow
            # by 'Joe' https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
          )
          # returns state. same as the 'changed to' state, if it is permissible
          # e.g. it isn't permissible to set ServerAdmin/UserAdmin to 'TRUE' if
          # there is no user who has UserAdmin attribute
          if (state != input[[restrictionLocal$id]]) {
            # state returned is not the same as the state which was attempted
            # so change the state 'back' to the what is, in fact, the old state
            shinyWidgets::updateMaterialSwitch(
              session, restrictionLocal$id,
              state)
          } else {
            # state returned is the same as the attempted change
            if (state == TRUE) {
              UserRestrictions(bind_rows(isolate(UserRestrictions()),
                                         data.frame(uid = max(isolate(UserRestrictions()$uid), 0) + 1,
                                                    Restriction = restrictionLocal$id,
                                                    stringsAsFactors = FALSE)))
              # add entry to datatable of UserRestrictions
              # add one to maximum UID (at least zero), and add restriction to new row
              # note that this table in the database uses 'uid' rather than 'id'
              #  to reduce confusion with the use of 'id' for input names
              print(UserRestrictions())
              update_UserRestrictions_database()
            } else {
              # remove entry in datatable of UserRestrictions
              UserRestrictions(filter(isolate(UserRestrictions()),
                                      !(isolate(UserRestrictions()$Restriction) == restrictionLocal$id)))
              print(UserRestrictions())
              update_UserRestrictions_database()
            }
          }
        }
      })
    })
  }

  userconfig_list_change <- reactiveVal(0)
  # counts number of GUI edits of the user configuration table

  ### callback definitions for DTedit userconfig
  userconfig.insert.callback <- function(data, row) {
    # adding a new user configuration

    if (data[row,]$Fullname %in% data[-row,]$Fullname) {
      # if the proposed new name is the same as one that is configured elsewhere
      stop("This user is already configured")
    } else {
      newid <- max(c(as.data.frame(UserConfig())$id, 0)) + 1
      # initially, UserConfig()$id might be an empty set, so need to append a '0'
      data[row, ]$id <- newid

      query <- "INSERT INTO Users (id, Fullname, AuthIdentity, Location, Attributes) VALUES ($id, $fn, $au, $lo, $at)"
      data_for_sql <- list(id = newid, fn = data[row,]$Fullname, au = paste0(data[row,]$AuthIdentity, ""),
                           # $Location and $Attribute could both have multiple (or no) entries
                           lo = paste0(data[row,]$Location[[1]], "", collapse = ";"),
                           at = paste0(data[row,]$Attributes[[1]], "", collapse = ";"))
      connection <- pool::poolCheckout(config_pool()) # can't write with the pool
      rs <- DBI::dbSendQuery(connection, query)
      # parameterized query can handle apostrophes etc.
      DBI::dbBind(rs, data_for_sql)
      # for statements, rather than queries, we don't need to dbFetch(rs)
      # update database
      DBI::dbClearResult(rs)
      pool::poolReturn(connection)

      UserConfig(data) # update the dataframe in memory
      userconfig_list_change(userconfig_list_change() + 1)
      # this value returned by module

      return(UserConfig())
    }
  }
  userconfig.update.callback <- function(data, olddata, row) {
    # change (update) a user configuration

    # is restrictions have been placed on who can modify the server or user configuration
    # then at least one user must have the restricted attribute
    if ("ServerAdmin" %in% UserRestrictions()$Restriction) {
      if (!("ServerAdmin" %in% unlist(data$Attributes))) {
        # modified data would no longer have anyone with ServerAdmin attribute
        stop("Only 'ServerAdmin' users can change server settings.
              At least one user must have the 'ServerAdmin' attribute!")
      }
    }
    if ("UserAdmin" %in% UserRestrictions()$Restriction) {
      if (!("UserAdmin" %in% unlist(data$Attributes))) {
        # modified data would no longer have anyone with UserAdmin attribute
        stop("Only 'UserAdmin' users can change user permissions.
              At least one user must have the 'UserAdmin' attribute!")
      }
    }

    query <- "UPDATE Users SET Fullname = ?, AuthIdentity = ?, Location = ?, Attributes = ? WHERE id = ?"
    data_for_sql <- as.list(c(data[row,]$Fullname, paste0(data[row,]$AuthIdentity, ""),
                              paste0(data[row,]$Location[[1]], "", collapse = ";"),
                              paste0(data[row,]$Attributes[[1]], "", collapse = ";"),
                              data[row,]$id))
    # note extra "" within paste0 is required in the case of empty data
    connection <- pool::poolCheckout(config_pool())
    # can't write with the pool
    rs <- DBI::dbSendQuery(connection, query) # update database
    DBI::dbBind(rs, data_for_sql)
    DBI::dbClearResult(rs)
    pool::poolReturn(connection)

    UserConfig(data)
    userconfig_list_change(userconfig_list_change() + 1)
    # this value returned by module

    return(UserConfig())

  }
  userconfig.delete.callback <- function(data, row) {
    # delete a user configuration

    # is restrictions have been placed on who can modify the server or user configuration
    # then at least one user must have the restricted attribute
    if ("ServerAdmin" %in% UserRestrictions()$Restriction) {
      if (!("ServerAdmin" %in% unlist(data[-c(row),]$Attributes))) {
        # modified data would no longer have anyone with ServerAdmin attribute
        stop("Only 'ServerAdmin' users can change server settings.
             At least one user must have the 'ServerAdmin' attribute!")
      }
    }
    if ("UserAdmin" %in% UserRestrictions()$Restriction) {
      if (!("UserAdmin" %in% unlist(data[-c(row),]$Attributes))) {
        # modified data would no longer have anyone with UserAdmin attribute
        stop("Only 'UserAdmin' users can change user permissions.
             At least one user must have the 'UserAdmin' attribute!")
      }
    }

    query <- "DELETE FROM Users WHERE id = ?"
    data_for_sql <- as.list.data.frame(c(data[row,]$id))

    connection <- pool::poolCheckout(config_pool()) # can't write with the pool
    rs <- DBI::dbSendQuery(connection, query) # update database
    DBI::dbBind(rs, data_for_sql)
    DBI::dbClearResult(rs)
    pool::poolReturn(connection)

    UserConfig(data[-c(row),])
    userconfig_list_change(userconfig_list_change() + 1)
    # this value returned by module

    return(UserConfig())
  }

  # depends on modularized version of DTedit
  userconfig_edited <- callModule(DTedit::dtedit, "userconfigs",
                                  thedataframe = UserConfig, # pass a ReactiveVal
                                  view.cols = userconfig_dt_viewcols, # no need to show 'id' in future
                                  edit.cols = userconfig_dt_editcols,
                                  # edit.label.cols = ,
                                  show.copy = FALSE,
                                  input.types = c(Fullname = 'selectInputReactive',
                                                  AuthIdentity = 'textInput',
                                                  Location = 'selectInputMultipleReactive',
                                                  Attributes = 'selectInputMultiple'),
                                  input.choices = c(Location = 'LocationNames',
                                                    Fullname = 'Fullname',
                                                    Attributes = list(user_attribute_types)),
                                  input.choices.reactive = list(Fullname = usernames,
                                                                LocationNames = LocationNames),
                                  callback.update = userconfig.update.callback,
                                  callback.insert = userconfig.insert.callback,
                                  callback.delete = userconfig.delete.callback
  )

  return(reactive({userconfig_list_change()}))
  # increments each time a callback changes UserConfig
}

#' password configuration module - UI
#'
#' Allow user to set or change their user password.
#' Only shown if password can be set, i.e. user has been identified.
#'
#' @param id as required by shiny modules
#'
passwordConfig_UI <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      "Change User Password",
      br(), br(),
      actionButton(ns("ChangePassword"), "Change Password", icon("unlock-alt"))
    )
  )

}

#' password configuration module - server function
#'
#' Allow user to set or change their user password
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param	UserConfig reactiveval, list of user config
#' @param LoggedInUser reactiveval, currently logged in user
#' @param config_pool reactiveval, access to configuration database
#'
#' @return count - increments with each GUI edit of user configuration database
passwordConfig_server <- function(input, output, session,
                              UserConfig, LoggedInUser,
                              config_pool) {
  ns <- session$ns

  observeEvent(input$ChangePassword, {
    if (is.na(LoggedInUser()$Password) || (nchar(LoggedInUser()$Password) == 0)) {
      # empty or NA password, then asking for new password
      showModal(modalDialog(
        title="New password",
        tagList(
          passwordInput(ns("password1"), label = "Enter Password", value = ""),
          br(),
          passwordInput(ns("password2"), label = "Confirm Password", value = "")
        ),
        footer = tagList(actionButton(ns("confirmNewPassword"), "Confirm"),
                         modalButton("Cancel")
        )
      ))
    } else {
      showModal(modalDialog(
        title="Change password",
        tagList(
          passwordInput(ns("passwordOld"), label = "Old Password", value = ""),
          br(),
          passwordInput(ns("password1"), label = "Enter Password", value = ""),
          br(),
          passwordInput(ns("password2"), label = "Confirm Password", value = "")
        ),
        footer = tagList(actionButton(ns("confirmChangePassword"), "Confirm"),
                         modalButton("Cancel")
        )
      ))
    }
  })

  setPassword <- function(newpassword) {
    # set the password for the user

    newpassword <- simple_tag(newpassword)
    # tagging (hash) defined in calculation_definitions

    newUserConfig <-
      UserConfig() %>%
      mutate(Password =
               replace(Password,
                       LoggedInUser()$Fullname == Fullname,
                       newpassword))
    UserConfig(newUserConfig) # replace password with empty string

    query <- "UPDATE Users SET Password = ? WHERE id = ?"
    # write to configuration database
    data_for_sql <- list(newpassword, LoggedInUser()$id[[1]])

    connection <- pool::poolCheckout(config_pool())
    # can't write with the pool
    rs <- DBI::dbSendQuery(connection, query) # update database
    DBI::dbBind(rs, data_for_sql)
    DBI::dbClearResult(rs)
    pool::poolReturn(connection)
  }

  observeEvent(input$confirmNewPassword, {
    if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                closeButton = TRUE)
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long")
    } else {
      setPassword(input$password1)
      removeModal()
    }
  })

  observeEvent(input$confirmChangePassword, {
    if (!simple_tag_compare(input$passwordOld, LoggedInUser()$Password)) {
      shinytoastr::toastr_error("Old Password incorrect",
                                closeButton = TRUE)
    } else if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                closeButton = TRUE)
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long")
    } else {
      setPassword(input$password1)
      removeModal()
    }
  })
}
