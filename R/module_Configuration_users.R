###### user configuration modules ###################################################

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
    userAttribute = TRUE,
    # is this actually a userattribute
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
    userAttribute = TRUE,
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
    userAttribute = TRUE,
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)}
  ),
  list(
    id = "GlobalBillView", label = "Global Bill View",
    Description = "GlobalBillView users can view billing status in 'other' appointment lists",
    userAttribute = TRUE,
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)
    }
  ),
  list(
    id = "GlobalCDMView", label = "Global CDM View",
    Description = "GlobalCDMView users can view CDM status in 'other' appointment lists",
    userAttribute = TRUE,
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)
    }
  ),
  list(
    id = "RequirePasswords", label = "Require Passwords",
    Description = "Password required from all users",
    userAttribute = FALSE,
    # 'RequirePasswords' is not actually a user attribute
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

restrictionTypes_df <- data.frame(Reduce(rbind, restrictionTypes))
# converts the list to a dataframe
user_attribute_types <- unlist(filter(restrictionTypes_df, userAttribute == TRUE)$id,
                               use.names = FALSE)
# user attribute types is defined in restrictionTypes. only those with userAttribute TRUE


###### Password Removal ##################################################
#' userconfig_resetpasswordUI
#'
#' reset password of selected user
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return shiny user interface element
userconfig_resetpasswordUI <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      "Only configured users can have passwords reset (or set)",
      br(), br(),
      uiOutput(ns("ConfiguredUserList")),
      br(),
      actionButton(ns('reset_password'), 'Reset Password', icon('broom'), class = 'btn btn-primary'))
  )
}

#' userconfig_resetpassword - server component
#'
#' reset password of selected user
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param	UserConfig reactiveval, list of user config
#' @param config_pool reactiveval, access to configuration database
#'
#' @return nothing
userconfig_resetpassword <- function(input, output, session,
                                     UserConfig, config_pool) {
  ns <- session$ns

  output$ConfiguredUserList <- renderUI({
    # create a list of configured users
    # (only configured users can have passwords)
    selectInput(inputId = ns('User_toReset_Password'), label = 'Selected User',
                choices = UserConfig()$Fullname)
  })

  observeEvent(UserConfig(), {
    # update the list if the UserConfig() changes
    # (it will change when the configuration database is read
    updateSelectInput(session, ns('User_toReset_Password'), label = 'Selected User',
                      choices = UserConfig()$Fullname)
  })

  observeEvent(input$reset_password, {
    # reset password button has been pressed
    validate(
      need(input$User_toReset_Password, "No user selected")
    )
    showModal(modalDialog(
      # popup an information dialog ('modal') and 'Confirmation' request
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

}

####### Restriction of permissions #############################################
#' userconfig_enableRestrictionUI
#'
#' set or unset user restrictions
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return shiny user interface element
userconfig_enableRestrictionsUI <- function(id) {
  ns <- NS(id)

  tagList(
    lapply(restrictionTypes,
           # goes through restrictionTypes
           # extracts the names labelled in $label
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

}

#' userconfig_enableRestrictions - server component
#'
#' set or unset restrictions to user abilities
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param	UserConfig reactiveval, list of user config
#' @param UserRestrictions reactiveval, list of user restrictions
#' @param config_pool reactiveval, access to configuration database
#'
#' @return nothing
userconfig_enableRestrictions <- function (input, out, session,
                                           UserConfig, UserRestrictions,
                                           config_pool) {

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

}

#' userconfig_datatableUI - editable datatable module
#'
#' user configuration module - user interface function
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
        userconfig_resetpasswordUI(ns("reset_password"))
      ),
      tabPanel(
        title = "Enabled Restrictions",
        width = 12,
        userconfig_enableRestrictionsUI(ns("enable_restrictions"))
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

  # password reset module
  callModule(userconfig_resetpassword, "reset_password",
             UserConfig, config_pool)

  # enable/disable restrictions module
  callModule(userconfig_enableRestrictions, "enable_restrictions",
             UserConfig, UserRestrictions, config_pool)

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
