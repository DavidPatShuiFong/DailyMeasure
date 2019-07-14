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
#  RequirePasswords - identified users need to use password to be 'authenticated'


###### Password Removal ##################################################
#' userconfig_resetpasswordUI
#'
#' reset password of selected user
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return shiny user interface element
userconfig_resetpasswordUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::wellPanel(
      "Only configured users can have passwords reset (or set)",
      shiny::br(), shiny::br(),
      shiny::uiOutput(ns("ConfiguredUserList")),
      shiny::br(),
      shiny::actionButton(ns('reset_password'), 'Reset Password',
                          icon('broom'), class = 'btn btn-primary'))
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
userconfig_resetpassword <- function(input, output, session, dM) {
  ns <- session$ns
  
  output$ConfiguredUserList <- shiny::renderUI({
    # create a list of configured users
    # (only configured users can have passwords)
    shiny::selectInput(inputId = ns('User_toReset_Password'),
                       label = 'Selected User',
                       choices = dM$UserFullConfigR()$Fullname)
  })
  
  shiny::observeEvent(dM$UserFullConfigR(), {
    # update the list if the $UserFullConfigR() changes
    # (it will change when the configuration database is read
    shiny::updateSelectInput(session, ns('User_toReset_Password'),
                             label = 'Selected User',
                             choices = dM$UserFullConfigR()$Fullname)
  })
  
  shiny::observeEvent(input$reset_password, {
    # reset password button has been pressed
    shiny::validate(
      need(input$User_toReset_Password, "No user selected")
    )
    shiny::showModal(shiny::modalDialog(
      # popup an information dialog ('modal') and 'Confirmation' request
      title= "Remove Password",
      paste0("Remove password of '", input$User_toReset_Password, "'."),
      shiny::br(), shiny::br(),
      paste0("The user will be asked for a new password when they next login."),
      shiny::br(),
      # ask for confirmation
      footer = shiny::tagList(shiny::actionButton(ns("confirmRemovePassword"),
                                                  "Remove Password"),
                              shiny::modalButton("Cancel")
      )
    ))
  })
  
  shiny::observeEvent(input$confirmRemovePassword, {
    # reset password has been confirmed
    dm$password.reset(input$User_toReset_Password)
    # $password.reset sets $UserConfig and writes to SQLite configuration
    
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
  ns <- shiny::NS(id)
  
  shiny::tagList(
    lapply(restrictionTypes,
           # goes through restrictionTypes
           # extracts the names labelled in $label
           function(type) {
             shiny::tagList(
               shiny::br(),
               shiny::fluidRow(
                 shiny::column(width = 3,
                               shinyWidgets::materialSwitch(
                                 inputId = ns(type$id),
                                 label = type$label,
                                 right = TRUE,
                                 value = FALSE,
                                 status = "primary")),
                 shiny::column(width = 8, type$Description)
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
#' @param dM dMeasure R6 object. configuration database access,
#'  user restrictions and $UserConfig
#'
#' @return nothing
userconfig_enableRestrictions <- function (input, out, session, dM) {
  
  shiny::observeEvent(dM$config_db_trigR(), {
    # if configuration pool has been initialized
    validate(
      need(dM$UserRestrictionsR(), "No restriction list"),
      need(config_db$conn(), "Configuration database not defined")
    )
    for (restriction in unlist(restrictionTypes_df$id, use.names = FALSE)) {
      # set the switches according the what is stored in the configuration database
      shinyWidgets::updateMaterialSwitch(
        session, restriction,
        restriction %in% (UserRestrictions()$Restriction))
    }
  })
  
  for (restriction in dM$restrictionTypes) {
    # add each of the restrictionTypes to the user interface
    local({
      # evaluate expression in a new environment
      # (otherwise only the final expression in the loop is registered as an observer!)
      restrictionLocal <- restriction
      shiny::observeEvent(input[[restrictionLocal$id]], ignoreInit = TRUE, {
        if (input[[restrictionLocal$id]] !=
            (restrictionLocal$id) %in% isolate(self$UserRestrictions()$Restriction)) {
          # change in state
          state <- dM$userrestriction.change(restrictionLocal$id,
                                             input[[restrictionLocal$id]])
          # is the new 'state' permissible?
          # if permissible, $userrestriction.change will return the
          # same state as input[[restrictionLocal$id]]. otherwise returns
          # the 'permissible' state.
          # 
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
            # $userrestriction.change has already updated the SQLite configuration
            # and $UserRestrictions
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
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "User settings",
        width = 12,
        DTedit::dteditUI(ns("userconfigs"))
      ),
      shiny::tabPanel(
        title = "Reset password",
        width = 12,
        userconfig_resetpasswordUI(ns("reset_password"))
      ),
      shiny::tabPanel(
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
#' @param dM dMeasure R6 object. includes $UserFullConfigR, 
#'  $UserRestrictions, $location.list, links to EMR and configuration db
#' @param	UserConfig reactiveval, list of user config
#' @param UserRestrictions reactiveval, user restriction list
#' @param LocationNames list of location names (not including ID or Description)
#' @param db reactivevalues link to EMR database. includes $users and $dbversion
#' @param config_db reactiveval, access to configuration database
#'
#' @return count - increments with each GUI edit of user configuration database
userconfig_datatable <- function(input, output, session, dM, 
                                 UserConfig, UserFullConfig, UserRestrictions,
                                 LocationNames, db, config_db) {
  ns <- session$ns
  
  userconfig_dt_viewcols <- c("id", "Fullname", "AuthIdentity", "Location",
                              "Attributes")
  userconfig_dt_editcols <- 
    userconfig_dt_viewcols[!userconfig_dt_viewcols %in% c("id")]
  # columns viewed in DTedit when adding/editing/removing user config
  
  # password reset module
  callModule(userconfig_resetpassword, "reset_password", dM)
  
  # enable/disable restrictions module
  callModule(userconfig_enableRestrictions, "enable_restrictions", dM)
  
  usernames <- shiny::reactiveVal()
  # list of user names
  shiny::observeEvent(dM$dbversion(), {
    shiny::validate(
      shiny::need(dM$UserFullConfigR(), "No user list"),
      shiny::need(dM$UserRestrictions(), "No restriction list")
    )
    # if the database has been connected
    if (!is.null(dM$UserFullConfigR())) {
      usernames(dM$UserFullConfigR() %>>%
                  dplyr::select(Fullname) %>>%
                  dplyr::collect() %>>%
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
      
      config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method
      
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
    
    config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method
    
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
    
    config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method
    
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
