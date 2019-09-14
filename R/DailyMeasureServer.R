##### Define server logic #####################################################

#' Shiny app server function
#'
#' @param input required for shiny server
#' @param output required for shiny server
#' @param session required for shiny server
#'
#' @return None
#'
#' @include calculation_definitions.R
#' @include utils-pipe.R
#'
#' needed for simple encode/decode
DailyMeasureServer <- function(input, output, session) {

  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {

    dM$close()
    # close $config_db and $emr_db

    shiny::stopApp()
  })

  # create the dMeasure object
  dM <- dMeasure::dMeasure$new()

  # read config files

  ##### Configuration file ######################################################

  observeEvent(dM$configuration_file_pathR(), ignoreNULL = TRUE, {
    dM$open_configuration_db()
    # connects to SQLite configuration database, using either DBI or pool
    # generates the SQLite configuration database if needed
    # and updates old SQLite configuration databases with necessary fields
    dM$read_configuration_db()
    # reads server definitions, location definitions, user attributes etc..
    newdb <- dM$BPdatabaseChoice_new()
    if (newdb != dM$BPdatabaseChoice) {
      shinytoastr::toastr_info(
        "Opening link to Best Practice", closeButton = TRUE,
        position = "bottom-left", title = "Best Practice database")
      opened_base <- dM$open_emr_db()
      if (opened_base == "None") {
        shinytoastr::toastr_error(
          "Error opening Best Practice database",
          closeButton = TRUE, position = "bottom-left",
          timeOut = 10000) # stays open ten seconds
      } else {
        shinytoastr::toastr_success(
          "Linking to Best Practice database successful!",
          closeButton = TRUE,
          position = "bottom-left",
          title = "Best Practice database")
      }
    }
  })
  invisible(dM$configuration_file_path)
  # this will also set $configuration_file_pathR


  # 'helper' functions for input panel

  # only adjust appointment view after dates are 'submitted' using 'submit' button
  shiny::observeEvent(input$update_date, ignoreNULL = FALSE, {
    tryCatch({
      dM$choose_date(date_from = as.Date(input$date1),
                     date_to = as.Date(input$date2)) # also update the dMeasure object
    },
    warning = function(w) {shinytoastr::toastr_warning(message = w$message,
                                                       position = "bottom-left")})
  }) # initialize on first run, after that only update if 'update' button used

  date_today <- shiny::observeEvent(input$update_date_today, {
    # 'today' button. change date range to today, and click the 'update' button
    shiny::updateDateInput(session, 'date1', value = Sys.Date())
    shiny::updateDateInput(session, 'date2', value = Sys.Date())
    # change date range to today
    shinyjs::click('update_date')
    # and click the 'update' button

    # work more reliably if doubled?
    # change date range to today
    shinyjs::click('update_date')

  })

  output$locationList <- shiny::renderUI({
    shiny::selectInput(inputId = 'location', label = 'Practice location',
                       choices = dM$location_list, selected = 'All')
    # list of locations available in appointment picker
    # $location_list returns all locations in configuration, and add 'All'
  })

  # dMeasure object also has $view_restrictions field,
  # which contains a list of restriction, and the view (also tabname)
  # to hide
  #
  # if a view restriction is active, then by default users
  # can only see patients in their own appointment book for
  # the specified topic
  # this restriction does not apply if the user has the
  # 'Global' attribute for the topic in the user's attribute list
  #
  # as of 11th July 2019, $view_restrictions looks like...
  #
  # view_restrictions <- list(
  #  list(restriction = "GlobalActionView",
  #       tabs_to_hide = list("immunization", "cancerscreen")),
  #  list(restriction = "GlobalBillView",
  #       tabs_to_hide = list("billings")),
  #  list(restriction = "GlobalCDMView",
  #       tabs_to_hide = list("cdm"))
  # )

  # list of clinicians shown depends on 'practice location' chosen
  clinician_choice_list <- shiny::reactiveVal()
  #
  shiny::observeEvent(c(dM$dbversion, input$location,
                        dM$UserRestrictions, input$sidebartabs), {
                          # respond to database initialization or change in input choice
                          # respond to change in UserRestrictions and which sidebartab is selected
                          shiny::validate(
                            shiny::need(input$location, "Locations not available")
                          )

                          clinician_list <- dM$clinician_list(input$sidebartabs, input$location)
                          # find list of clinicians which can be selected to be viewed
                          #  filter by current $location setting
                          #  and also the view (input$sidebartabs) if a view restriction is activated
                          #  depending on user attributes/permissions and authentication/login status
                          #  this also sets dM$clinician_choice_list

                          clinician_choice_list(clinician_list)

                          dM$choose_clinicians(input$clinicians, input$sidebartabs)
                          # change the clinician chosen list in dMeasure R6 object
                          # will form 'intersection' between current choices (input$clinicians)
                          # and what is permissible with the view 'input$sidebartabs'
                        })

  output$clinicianList <- shiny::renderUI({
    choice_list <- clinician_choice_list()
    chosen_list <- input$clinicians # retain previous selections
    shiny::checkboxGroupInput('clinicians', label = 'Clinician',
                              choices = choice_list, selected = chosen_list)
  })

  shiny::observeEvent(input$clinicians, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # cannot ignoreNULL because sometimes an empty list will be chosen
    dM$choose_clinicians(choices = input$clinicians)
    # alter dMeasure object according to user input
    # (or perhaps 'toggle' button below)
  })

  toggle_clinicians <- shiny::observeEvent(input$toggle_clinician_list, {
    if (input$toggle_clinician_list == 0) {return(NULL)}
    else if (input$toggle_clinician_list%%2 == 1) {
      shiny::updateCheckboxGroupInput(session, 'clinicians',
                                      selected = clinician_choice_list())
      # toggle all clinicians selected
    } else {
      shiny::updateCheckboxGroupInput(session, 'clinicians',
                                      selected = character(0))
      # no clinicians selected
    }
  })

  shiny::observeEvent(input$contact_type, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # cannot ignoreNULL because sometimes an empty list will be chosen
    dM$contact_type <- input$contact_type
    # alter dMeasure object according to user input
  })
  shiny::observeEvent(dM$contact_typeR(), {
    shinyWidgets::updatePickerInput(session, inputId = "contact_type",
                                    selected = dM$contact_type)
  })

  shiny::observeEvent(input$min_contact, ignoreInit = TRUE, {
    dM$contact_min <- input$min_contact
    # alter dMeasure object according to user input
  })
  shiny::observeEvent(dM$contact_minR(), {
    shinyWidgets::updateSliderTextInput(session, inputId = "min_contact",
                                        selected = dM$contact_min)
  })

  shiny::observeEvent(input$appointment_status, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # cannot ignoreNULL because sometimes an empty list will be chosen
    dM$appointment_status <- input$appointment_status
    # alter dMeasure object according to user input
  })
  shiny::observeEvent(dM$appointment_statusR(), {
    shinyWidgets::updatePickerInput(session, inputId = "appointment_status",
                                    selected = dM$appointment_status)
  })

  shiny::observeEvent(input$visit_type, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # cannot ignoreNULL because sometimes an empty list will be chosen
    dM$visit_type <- input$visit_type
    # alter dMeasure object according to user input
  })
  shiny::observeEvent(dM$visit_typeR(), {
    shinyWidgets::updatePickerInput(session, inputId = "visit_type",
                                    selected = dM$visit_type)
  })

  # Immunization functions

  vax_table_results <- callModule(vax_datatable, "vax_dt", dM)

  # Bowel cancer screening

  callModule(cancerscreen_datatable, "cancerscreen_dt", dM)

  # call the module to generate the table
  callModule(billings_datatable, "billings_dt", dM)

  # chronic disease management table
  cdm_table_results <- callModule(cdm_datatable, "cdm_dt", dM)

  # administration and result management tab
  admin_table_results <- callModule(administration, "admin_dt", dM)

  # Practice Incentive Program (PIP) Quality Improvement (QI) measures
  qim_results <- callModule(qim, "qim", dM)

  # appointment list

  callModule(appointments_datatable, "appointments_dt", dM)

  output$test_dt <-
    DT::renderDT({
      DT::datatable(
        data.frame(a=c(2,3,68),
                   b=c('<span class="huge green positive ui tag label"><span data-tooltip="check me" data-variation="huge">
                                       721
                                       </span></span>
                                       <span class="huge green positive ui tag label">723</span><span class="ui tag label">10990</span>',
                       '<div class="huge ui negative button" data-tooltip="waiting ... "><i class="wheelchair loading icon"></i>
                                       2715</div>',
                       '<div class="huge ui button positive" data-variation="wide" data-html="<h1>
                                       Cheese factory
                                       </h1><font size=\'+0\'><b>Lots and lots</b> of information. make sure everything is <ins>complete</ins> on year after ... 12/Jan/2019</font>">GPMP</div>'
                   )),
        options = list(initComplete = DT::JS(semantic_popupJS)),
        escape = FALSE,
        fillContainer = FALSE)})


  # configuration file location tab

  output$configuration_file_details <- renderText({
    paste('Configuration file location: "', dM$configuration_file_pathR(), '"')
  })

  volumes <- c(shinyFiles::getVolumes()(), base = '.', home = Sys.getenv("USERPROFILE"))

  shinyFiles::shinyFileChoose(
    input, id = 'choose_configuration_file',
    session = session,
    roots = volumes,
    filetypes = c('sqlite'), # only files ending in '.sqlite'
    hidden = TRUE # the default is that configuration files have '.' hidden prefix
  )

  observeEvent(input$choose_configuration_file, ignoreNULL = TRUE, {
    if (!is.integer(input$choose_configuration_file)) {
      # if input$choose_configuration_file is an integer,
      # it is just the 'click' event on the filechoose button
      inFile <- shinyFiles::parseFilePaths(volumes, input$choose_configuration_file)
      file_name <- paste(inFile$datapath)
      dM$configuration_file_path <- file_name
      # this dMeasure method will also update the YAML configuration file
    }
  })

  shinyFiles::shinyFileSave(
    input, id = 'create_configuration_file',
    session = session,
    roots = volumes,
    hidden = TRUE
  )

  observeEvent(input$create_configuration_file, ignoreNULL = TRUE, {
    if (!is.integer(input$create_configuration_file)) {
      # if input$choose_configuration_file is an integer,
      # it is just the 'click' event on the filechoose button
      inFile <- shinyFiles::parseSavePath(volumes, input$create_configuration_file)
      file_name <- paste(inFile$datapath)
      dM$configuration_file_path <- file_name
      # this dMeasure method will create the .sqlite file and
      # also update the YAML configuration file
      dM$open_configuration_db()
      # this will initialize the .sqlite configuration file and open it
      dM$read_configuration_db()
    }
  })

  # database configuration tab
  serverconfig_change <- callModule(servers_datatable, "servers_dt", dM)
  # returns $count

  # logging configuration tab
  loggingconfig_change <- callModule(logging_datatable, "logging_dt", dM)
  # returns $count

  # location configuration tab
  location_list_change <- callModule(locations_datatable, "locations_dt", dM)

  shiny::observeEvent(c(location_list_change(), dM$location_listR()), {
    # change in location_listR (by GUI editor in locations_data module) prompts
    # change in location list filter (for providers) and location_list_names (for user config)
    shiny::updateSelectInput(session, inputId = 'location',
                             choices = dM$location_listR())
  })

  userconfig_change <- callModule(userconfig_datatable, "userconfig_dt", dM)

  ###### user configuration of their own password #######################
  callModule(passwordConfig_server, "password_config", dM)

  shiny::observeEvent(c(dM$identified_user(), dM$UserRestrictions()), {
    shiny::validate(
      shiny::need(dM$identified_user(), "No user information")
    )
    if ("ServerAdmin" %in% unlist(dM$UserRestrictions()$Restriction)) {
      # only some users allowed to see/change server settings and logging
      if ("ServerAdmin" %in% unlist(dM$identified_user()$Attributes)) {
        shiny::showTab("tab_config", "ServerPanel")
        shiny::showTab("tab_config", "LoggingPanel")
      } else {
        shiny::hideTab("tab_config", "ServerPanel")
        shiny::hideTab("tab_config", "LoggingPanel")
      }
    } else {
      shiny::showTab("tab_config", "ServerPanel")
      shiny::showTab("tab_config", "LoggingPanel")
    }
    if ("UserAdmin" %in% unlist(dM$UserRestrictions()$Restriction)) {
      # only some users allowed to see/change user settings
      if ("UserAdmin" %in% unlist(dM$identified_user()$Attributes)) {
        shiny::showTab("tab_config", "LocationsPanel")
        # also change ability to view locations panel
        shiny::showTab("tab_config", "UsersPanel")
      } else {
        shiny::hideTab("tab_config", "LocationsPanel")
        shiny::hideTab("tab_config", "UsersPanel")
      }
    } else {
      shiny::showTab("tab_config", "LocationsPanel")
      shiny::showTab("tab_config", "UsersPanel")
    }
    if (nrow(dM$identified_user()) > 0) {
      # user has been identified
      shiny::showTab("tab_config", "PasswordPanel")
      # only 'identified' and configured users can set a password
    } else {
      # no user identified
      shiny::hideTab("tab_config", "PasswordPanel")
    }
  })

  ####### User login #######################################################
  shiny::observeEvent(c(dM$identified_user(), dM$UserRestrictions()), {
    # need to know that user identification has occurred
    # and that restrictions have been read
    shiny::validate(
      shiny::need(dM$identified_user(), "No user information")
    )
    if ("RequirePasswords" %in% unlist(dM$UserRestrictions()$Restriction) &
        dM$authenticated == FALSE & nrow(dM$identified_user()) > 0) {
      # passwords are required, not yet authenticated
      # and information about users has been read in
      if (nrow(dM$identified_user()) > 0) {
        # user has been identified
        if (dM$empty_password()) {
          # empty or NA password, then asking for new password
          shiny::showModal(shiny::modalDialog(
            title="New password",
            shiny::tagList(
              paste("Password required for user ",
                    dM$identified_user()$Fullname, "."),
              shiny::HTML("You need to set a password<br><br>"),
              shiny::passwordInput("password1",
                                   label = "Enter Password", value = ""),
              shiny::br(),
              shiny::passwordInput("password2",
                                   label = "Confirm Password", value = "")
            ),
            footer = shiny::tagList(shiny::actionButton("confirmNewPassword",
                                                        "Confirm"))
          ))
        } else {
          shiny::showModal(shiny::modalDialog(
            title="Password required",
            shiny::tagList(
              paste("Password required for user ",
                    dM$identified_user()$Fullname, "."),
              shiny::HTML("<br><br>Please enter your password!<br>
              Click the 'Enter' button after typing in your password.<br><br>
              This is not (or shouldn't be!) your Windows or Best Practice password<br><br>"),
              shiny::passwordInput("password", label = "Password", value = "")
            ),
            footer = shiny::tagList(shiny::actionButton("confirmPassword",
                                                        "Enter"))
          ))
        }
      } else {
        # no user identified! but password required
        # will need to stop
        shiny::showModal(shiny::modalDialog(
          title="Password required",
          shiny::tagList(
            "User not recognized!",
            shiny::br(),
            "Please contact your systems administrator."
          ),
          footer = tagList()
        ))
      }
    }
  })

  shiny::observeEvent(input$confirmNewPassword, {
    if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                closeButton = TRUE)
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long")
    } else {
      dM$password.set(newpassword = input$password1)
      # will also set dM$authenticated to TRUE
      shiny::removeModal()
      shinytoastr::toastr_success(message = "Password set and Successful login",
                                  title = "Welcome back!",
                                  position = "bottom-left")
    }
  })

  input_password_CR <- reactiveVal(0)

  shiny::observeEvent(c(input$confirmPassword), {
    shiny::validate(
      shiny::need(nchar(input$password) > 0, "No password entered")
    )

    if (!tryCatch(dM$user_login(input$password),
                  error = function(e) {return(FALSE)})) {
      # dM$user_login returns $authenticated if successful log-in i.e. TRUE
      # otherwise returns an error
      shinytoastr::toastr_error("Wrong password",
                                position = "bottom-left",
                                closeButton = TRUE)
    } else {
      # successful login
      shiny::removeModal()
      shinytoastr::toastr_success(message = paste("Successful login for",
                                                  dM$identified_user()$Fullname),
                                  title = "Welcome back!",
                                  position = "bottom-left")
    }
  })

  ###### Render user information on top-right header ##########################
  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = dM$identified_user()$Fullname,
      src = 'icons/user-avatar.svg', # this depends on addResourcePath in zzz.R
      subtitle = Sys.info()[["user"]], # not necessarily an identified user
      shiny::fluidRow(
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            text = paste0(
              unlist(dM$identified_user()$Location),
              collapse = ", "),
            right_border = TRUE,
            margin_bottom = TRUE)
        ),
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            text = paste0(
              unlist(dM$identified_user()$Attributes),
              collapse = ", "),
            right_border = FALSE,
            margin_bottom = TRUE)
        )
      )
    )
  })

}
