# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
      shiny::br(), shiny::br(), {
        if (.bcdyz.option$demonstration) {
          shiny::span(shiny::p(), shiny::strong("Demonstration mode : Password reset disabled"),
            style = "color:red", shiny::p()
          )
        }
        else {}
      },
      shiny::uiOutput(ns("ConfiguredUserList")),
      shiny::br(), {
        x <- shiny::actionButton(ns("reset_password"), "Reset Password",
          shiny::icon("broom"),
          class = "btn btn-primary"
        )
        # disabled if demonstration mode
        if (.bcdyz.option$demonstration) {
          shinyjs::disabled(x)
        } else {
          x
        }
      }
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
userconfig_resetpassword <- function(input, output, session, dM) {
  ns <- session$ns

  output$ConfiguredUserList <- shiny::renderUI({
    # create a list of configured users
    # (only configured users can have passwords)
    shiny::selectInput(
      inputId = ns("User_toReset_Password"),
      label = "Selected User",
      choices = dM$UserConfigR()$Fullname
    )
  })

  shiny::observeEvent(dM$UserConfigR(), {
    # update the list if the $UserConfigR() changes
    # (it will change when the configuration database is read
    shiny::updateSelectInput(session, ns("User_toReset_Password"),
      label = "Selected User",
      choices = dM$UserConfigR()$Fullname
    )
  })

  shiny::observeEvent(input$reset_password, {
    # reset password button has been pressed
    shiny::validate(
      need(input$User_toReset_Password, "No user selected")
    )
    shiny::showModal(shiny::modalDialog(
      # popup an information dialog ('modal') and 'Confirmation' request
      title = "Remove Password",
      paste0("Remove password of '", input$User_toReset_Password, "'."),
      shiny::br(), shiny::br(),
      paste0("The user will be asked for a new password when they next login."),
      shiny::br(),
      # ask for confirmation
      footer = shiny::tagList(
        shiny::actionButton(
          ns("confirmRemovePassword"),
          "Remove Password"
        ),
        shiny::modalButton("Cancel")
      )
    ))
  })

  shiny::observeEvent(input$confirmRemovePassword, {
    # reset password has been confirmed
    tryCatch({
      dM$password.reset(input$User_toReset_Password)
      shiny::removeModal()
    },
    # $password.reset sets $UserConfig and writes to SQLite configuration
    error = function(e) {
      shinytoastr::toastr_error(
        paste(e$message),
        title = "Reset password error",
        closeButton = TRUE, position = "bottom-left",
        timeOut = 10000
      ) # stays open ten seconds}
    }
    )
  })
}

####### Restriction of permissions #############################################
#' userconfig_enableRestrictionsUI
#'
#' set or unset user restrictions
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return shiny user interface element
userconfig_enableRestrictionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    {
      if (.bcdyz.option$demonstration) {
        shiny::span(shiny::p(), shiny::strong("Demonstration mode : Restriction changes disabled"),
          style = "color:red", shiny::p()
        )
      }
      else {}
    },
    lapply(
      dMeasure::restrictionTypes_list(),
      # goes through restrictionTypes
      # extracts the names labelled in $label
      function(type) {
        shiny::tagList(
          shiny::br(),
          shiny::fluidRow(
            shiny::column(width = 3, {
              x <- shinyWidgets::materialSwitch(
                inputId = ns(type$id),
                label = type$label,
                right = TRUE,
                value = FALSE,
                status = "primary"
              )
              # disabled if demonstration mode
              if (.bcdyz.option$demonstration) {
                shinyjs::disabled(x)
              } else {
                x
              }
            }),
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
userconfig_enableRestrictions <- function(input, out, session, dM) {
  shiny::observeEvent(dM$config_db_trigR(), {
    # if configuration pool has been initialized
    shiny::validate(
      need(dM$UserRestrictions(), "No restriction list")
    )
    for (restriction in unlist(dM$restrictionTypes_df$id, use.names = FALSE)) {
      # set the switches according the what is stored in the configuration database
      shinyWidgets::updateMaterialSwitch(
        session, restriction,
        restriction %in% (dM$UserRestrictions() %>>% dplyr::pull(Restriction))
      )
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
          (restrictionLocal$id) %in% (isolate(dM$UserRestrictions() %>>%
            dplyr::pull(Restriction)))) {
          # change in state
          state <- dM$userrestriction.change(
            restrictionLocal$id,
            input[[restrictionLocal$id]]
          )
          # is the new 'state' permissible?
          # if permissible, $userrestriction.change will return the
          # same state as input[[restrictionLocal$id]]. otherwise returns
          # the 'permissible' state.
          #
          # e.g. it isn't permissible to set ServerAdmin/UserAdmin to 'TRUE' if
          # there is no user who has UserAdmin attribute
          if (state$state != input[[restrictionLocal$id]]) {
            # state returned is not the same as the state which was attempted
            # so change the state 'back' to the what is, in fact, the old state
            shinyWidgets::updateMaterialSwitch(
              session, restrictionLocal$id,
              state$state
            )
            if (length(state$error) > 0) {
              shinytoastr::toastr_error(
                title = state$error$title,
                message = state$error$message,
                closeButton = TRUE, position = "bottom-left"
              )
            }
          } else {
            # state returned is the same as the attempted change
            # $userrestriction.change has already updated the SQLite configuration
            # and $UserRestrictions
            if (length(state$warn) > 0) {
              shinytoastr::toastr_warning(
                title = state$warn$title,
                message = state$warn$message,
                closeButton = TRUE, position = "bottom-left"
              )
            }
          }
        }
        if (suppressWarnings(dM$useradmin.permission())) {
          # warning will be issued if no UserAdmin permission, so suppress
          # UserAdmin restriction is either disabled or
          # this user has the permission
          for (restriction in unlist(dM$restrictionTypes_df$id, use.names = FALSE)) {
            # show the switches
            shinyjs::show(restriction)
          }
        } else {
          for (restriction in unlist(dM$restrictionTypes_df$id, use.names = FALSE)) {
            # hide the switches
            shinyjs::hide(restriction)
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
        shiny::tagList(
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::wellPanel(
                {
                  if (.bcdyz.option$demonstration) {
                    shiny::span(
                      shiny::p(),
                      shiny::strong("Demonstration mode : License read disabled"),
                      style = "color:red",
                      shiny::p()
                    )
                  }
                  else {}
                }, {
                  x <- shiny::tagList(
                    shiny::actionButton(ns("reread_subscription"),
                    "Re-read Subscriptions",
                    shiny::icon("book-reader"),
                    class = "btn btn-primary"
                  ))
                  # disabled if demonstration mode
                  if (.bcdyz.option$demonstration) {
                    shinyjs::disabled(x)
                  } else {
                    x
                  }
                },
                shiny::HTML("&nbsp;"),
                shiny::HTML("&nbsp;"),
                shiny::HTML("&nbsp;"),
                "Read subscription/license dates from GPstat!/DailyMeasure databases"
              )
            ),
            shiny::column(
              width = 6,
              shiny::wellPanel(
                shiny::downloadButton(ns("downloadUserList"), "Download"),
                shiny::HTML("&nbsp;"),
                shiny::HTML("&nbsp;"),
                shiny::HTML("&nbsp;"),
                "Download configured user list, including Identifiers."
              )
            )
          ),
          DTedit::dteditmodUI(ns("userconfigs"))
        )
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
#' @param dM dMeasure R6 object. includes $UserConfigR,
#'  $UserRestrictions, $location.list, $location_listR
#'  and links to EMR and configuration db
#'
#' @return count - increments with each GUI edit of user configuration database
userconfig_datatable <- function(input, output, session, dM) {
  ns <- session$ns

  # password reset module
  callModule(userconfig_resetpassword, "reset_password", dM)

  # enable/disable restrictions module
  callModule(userconfig_enableRestrictions, "enable_restrictions", dM)

  shiny::observeEvent(input$reread_subscription, {
    shiny::validate(
      need(dM$emr_db$is_open(), "Best Practice database not open"),
      need(dM$config_db$is_open(), "Configuration database not open")
    )
    tryCatch({
      dM$read_subscription_db(forcecheck = TRUE)
      # next line not executed if warning raised
      shinytoastr::toastr_success(
        "Subscription database read!",
        closeButton = TRUE,
        position = "bottom-left",
        title = "Best Practice database"
      )
    },
    warning = function(w) {
      shinytoastr::toastr_warning(
        w$message,
        closeButton = TRUE,
        position = "bottom-left",
        title = "Best Practice database"
      )
    }
    )
  })

  output$downloadUserList <- shiny::downloadHandler(
    filename = "GPstat UserList.csv",
    content = function(file) {
      write.csv(dM$UserConfigLicense %>>%
          dplyr::select(Fullname, Identifier, LicenseDate),
        file,
        row.names = FALSE)
    }
  )

  usernames <- shiny::reactiveVal()
  # list of user names
  shiny::observeEvent(dM$dbversion(), {
    # if the database has been connected
    if (!is.null(dM$UserFullConfig)) {
      usernames(dM$UserFullConfig %>>%
        dplyr::select(Fullname) %>>%
        dplyr::collect() %>>%
        unlist(use.names = FALSE))
      # extract from EMR database. note that this is NOT reactive to
      # underlying change in EMR database
      # does not exclude names already configured, because this is also used when
      # editing a current user configuration
    }
  })

  userconfig_list_change <- shiny::reactiveVal(0)
  # counts number of GUI edits of the user configuration table

  ### callback definitions for DTedit userconfig
  userconfig.insert.callback <- function(data, row) {
    # adding a new user configuration

    description <- data[row, ] %>>%
      dplyr::select(
        id, Fullname, AuthIdentity, Location,
        Attributes
      )

    Identifier <- dM$UserFullConfig %>>%
      dplyr::filter(Fullname == data[row, ]$Fullname) %>>%
      dplyr::pull(Identifier) # does not yet have identifier in 'data'

    License <- trimws(data[row, ]$License) # strip whitespace
    if (License == "") {
      License <- NA
    } # if empty, change to NA
    LicenseDate <- dMeasure::verify_license(
      License,
      Identifier
    )
    # returns the LicenseDate, or NA if not a valid License (including "")
    if (!is.na(License) && is.na(LicenseDate)) {
      # License string 'provided', but not valid
      stop("Invalid License. If no license available, License should be 'empty'.")
    }

    tryCatch(newdata <- dM$userconfig.insert(description),
      error = function(e) stop(e)
    )
    # possible errors include "This user is already configured"
    # or invalid description (although the UI should prevent invalid descriptions)

    # dM$userconfig.insert will change the SQLite configuration file if appropriate
    # and $UserConfig

    dM$update_subscription(
      Fullname = description$Fullname,
      License = License,
      verify = FALSE
    )
    # write license, already verified

    userconfig_list_change(userconfig_list_change() + 1)
    # this value returned by module

    return(data)
  }
  userconfig.update.callback <- function(data, olddata, row) {
    # change (update) a user configuration

    description <- data[row, ] %>>%
      dplyr::select(
        id, Fullname, AuthIdentity, Location,
        Attributes
      )

    License <- trimws(data[row, ]$License) # strip whitespace
    if (License == "") {
      License <- NA
    } # if empty, change to NA
    LicenseDate <- dMeasure::verify_license(
      License,
      data[row, ]$Identifier
    )
    # returns the LicenseDate, or NA if not a valid License (including "")
    if (!is.na(License) && is.na(LicenseDate)) {
      # License string 'provided', but not valid
      stop("Invalid License. If no license available, License should be 'empty'.")
    }

    tryCatch(newdata <- dM$userconfig.update(description),
      error = function(e) stop(e)
    )
    # possible errors include
    # if restrictions have been placed on who can modify the server or user configuration
    # then at least one user must have the restricted attribute

    # 'newdata' has more columns than 'data' (has 'Identifier' and 'LicenseDate')

    # dM$userconfig.update will change the SQLite configuration file if appropriate
    # and $UserConfig

    dM$update_subscription(
      Fullname = description$Fullname,
      License = License,
      verify = FALSE
    )
    # write license, already verified

    userconfig_list_change(userconfig_list_change() + 1)
    # this value returned by module

    return(data)
  }
  userconfig.delete.callback <- function(data, row) {
    # delete a user configuration

    description <- data[row, ]

    tryCatch(newdata <- dM$userconfig.delete(description),
      error = function(e) stop(e)
    )
    # possible errors include
    # if restrictions have been placed on who can modify the server or user configuration
    # then at least one user must have the restricted attribute

    # dM$userconfig.delete will change the SQLite configuration file if appropriate
    # and $UserConfig

    userconfig_list_change(userconfig_list_change() + 1)
    # this value returned by module

    newdata <- data[-c(row), ] # this 'creates' a no-row table
    # if there are no rows left
    # strangely, the return from $userconfig.delete is a 0x5 tibble
    # which results in the error 'Replacement has 1 row, data has 0'

    return(newdata)
  }

  userconfig_dt_viewcols <- c(
    "id", "Fullname", "AuthIdentity", "Location",
    "Attributes", "Identifier", "LicenseDate"
  )
  userconfig_dt_editcols <- c("Fullname", "AuthIdentity", "Location", "Attributes", "License")
  # columns viewed in DTedit when adding/editing/removing user config

  # depends on modularized version of DTedit
  shiny::observeEvent(dM$UserConfigLicenseR(), ignoreNULL = TRUE, once = TRUE, {
    userconfig_edited <-
      callModule(
        DTedit::dteditmod, "userconfigs",
        thedata = dM$UserConfigLicenseR, # pass a ReactiveVal
        view.cols = userconfig_dt_viewcols, # no need to show 'id' in future
        edit.cols = userconfig_dt_editcols,
        # edit.label.cols = ,
        show.copy = FALSE,
        input.types = c(
          Fullname = "selectInputReactive",
          Attributes = "selectInputMultiple",
          AuthIdentity = "textInput",
          Location = "selectInputMultipleReactive",
          License = "textInput"
        ),
        input.choices = c(
          Location = "LocationNames",
          Fullname = "Fullname",
          Attributes = list(dM$user_attribute_types)
        ),
        input.choices.reactive = list(
          Fullname = usernames,
          # usernames was defined in this function
          # userconfig_datatable
          # $location_groupR does not include 'None'
          LocationNames = dM$location_groupR
        ),
        callback.update = userconfig.update.callback,
        callback.insert = userconfig.insert.callback,
        callback.delete = userconfig.delete.callback
      )
  })

  shiny::observeEvent(userconfig_list_change(), {
    invisible(dM$UserConfig)
    # this will provoke a change in $UserConfigR,
    # which will in turn provoke a change in $UserConfigLicenseR
    # and then update the userconfig_edited DT table
  })

  return(reactive({
    userconfig_list_change()
  }))
  # increments each time a callback changes UserConfig
}
