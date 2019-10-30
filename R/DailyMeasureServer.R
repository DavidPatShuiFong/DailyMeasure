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
#' @include datatables_definitions.R
#' @include utils-pipe.R
#'
#' needed for simple encode/decode
DailyMeasureServer <- function(input, output, session) {

  print(.bcdyz.option)

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

  # create dMeasureQIM objects
  QIMmodule <- requireNamespace('dMeasureQIM', quietly = TRUE)
  # is module (package) available?
  if (QIMmodule) {
    dMQIM <- dMeasureQIM::dMeasureQIM$new(dM)
    dMQIMappt <- dMeasureQIM::dMeasureQIM$new(dM) # a second QIM module
    dMQIMappt$qim_contact <- FALSE # second module uses appointment list, not contact list
  }
  Billingsmodule <- requireNamespace('dMeasureBillings', quietly = TRUE)
  # is module (package) available?
  if (Billingsmodule) {
    dMBillings <- dMeasureBillings::dMeasureBillings$new(dM)
  }
  CDMmodule <- requireNamespace('dMeasureCDM', quietly = TRUE)
  # is module (package) available?
  if (Billingsmodule & CDMmodule) {
    # needs both modules!
    dMCDM <- dMeasureCDM::dMeasureCDM$new(dM, dMBillings)
  }

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

  shiny::observeEvent(input$min_contact, ignoreInit = TRUE, {
    dM$contact_min <- input$min_contact
    # alter dMeasure object according to user input
  })

  shiny::observeEvent(input$appointment_status, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # cannot ignoreNULL because sometimes an empty list will be chosen
    dM$appointment_status <- input$appointment_status
    # alter dMeasure object according to user input
  })

  shiny::observeEvent(input$visit_type, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # cannot ignoreNULL because sometimes an empty list will be chosen
    dM$visit_type <- input$visit_type
    # alter dMeasure object according to user input
  })

  ##### main body panel ###############################################################


  shinytabItems <-
    c(list(shinydashboard::tabItem(
      tabName = "appointments",
      fluidRow(column(width = 12, align = "center", h2("Appointments"))),
      fluidRow(column(width = 12, appointments_datatableUI("appointments_dt")))
    )),
    list(shinydashboard::tabItem(
      tabName = "immunization",
      fluidRow(column(width = 12, align = "center", h2("Immunization"))),
      fluidRow(column(width = 12, vax_datatableUI("vax_dt")))
    )),
    list(shinydashboard::tabItem(
      tabName = "cancerscreen",
      fluidRow(column(width = 12, align = "center", h2("Cancer screening"))),
      fluidRow(column(width = 12, cancerscreen_datatableUI("cancerscreen_dt")))
    )))

  # no PIP Quality Improvement Measure, billings, or CDM tabs, these are inserted dynamically
  # in the server section if the dMeasureQIM module/package is available

  #####################################################################################

  # Immunization functions

  vax_table_results <- callModule(vax_datatable, "vax_dt", dM)

  # Bowel cancer screening

  callModule(cancerscreen_datatable, "cancerscreen_dt", dM)

  if (Billingsmodule == TRUE) {
    output$BillingsMenu <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(.list = list(
        shinydashboard::menuItem("Billings",
                                 tabName = "billings", icon = shiny::icon("receipt"))
      ))
    }) # if QIMmodule is FALSE, then output$PIPqimMenu will be left undefined
    shinytabItems <- c(shinytabItems,
                       list(shinydashboard::tabItem(
                         tabName = "billings",
                         fluidRow(column(width = 12, align = "center", h2("Billings"))),
                         fluidRow(column(width = 12, billings_datatableUI("billings_dt")))
                       )))
    # call the module to generate the table
    callModule(billings_datatable, "billings_dt", dMBillings)
  }

  if (CDMmodule == TRUE & Billingsmodule == TRUE) {
    output$CDMMenu <- shinydashboard::renderMenu({
      shinydashboard::menuItem("CDM items",
                               tabName = "cdm", icon = shiny::icon("file-medical-alt"))
    }) # if QIMmodule is FALSE, then output$PIPqimMenu will be left undefined
    shinytabItems <- c(shinytabItems,
                       list(shinydashboard::tabItem(
                         tabName = "cdm",
                         shiny::fluidRow(column(width = 12, align = "center",
                                                h2("Chronic Disease Management items"))),
                         shiny::fluidRow(column(width = 12,
                                                cdm_datatableUI("cdm_dt")))
                       )))
    # chronic disease management table
    cdm_table_results <- callModule(cdm_datatable, "cdm_dt", dMCDM)
  }

  # administration and result management tab
  admin_table_results <- callModule(administration, "admin_dt", dM)

  if (QIMmodule == TRUE) {
    # only if QIM module/package is available
    output$PIPqimMenu <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(.list = list(
        shinydashboard::menuItem("PIP Quality Improvement",
                                 tabName = "qim", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("QIM Appointment",
                                 tabName = "qimAppt", icon = shiny::icon("chart-line"))
      ))
    }) # if QIMmodule is FALSE, then output$PIPqimMenu will be left undefined
    # Practice Incentive Program (PIP) Quality Improvement (QI) measures
    # add PIP QIM tab items to the tabItem vector
    shinytabItems <- c(shinytabItems,
                       list(shinydashboard::tabItem(
                         tabName = "qim",
                         shiny::fluidRow(column(width = 12, align = "center",
                                                h2("Quality Improvement Measure Reporting"))),
                         shiny::fluidRow(column(width = 12,
                                                qim_UI("qim")))
                       )),
                       list(shinydashboard::tabItem(
                         tabName = "qimAppt",
                         shiny::fluidRow(column(width = 12, align = "center",
                                                h2("Quality Improvement Measure Appointment View"))),
                         shiny::fluidRow(column(width = 12,
                                                qim_UI("qimAppt")))
                       ))
    )
    qim_results <- callModule(qim, "qim", dMQIM, contact = TRUE)
    # Practice Incentive Program (PIP) Quality Improvement (QI) measures
    # appointment view
    qim_results_appt <- callModule(qim, "qimAppt", dMQIMappt, contact = FALSE)
  }

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



  ##### final definition of tabItems #################################################

  shinytabItems <- c(shinytabItems,
                     list(shinydashboard::tabItem(
                       tabName = "administration",
                       #shiny::fluidRow(column(width = 12, align = "center",
                       #                       h2("Administration"))),
                       shiny::fluidRow(column(width = 12,
                                              administration_UI("admin_dt")))
                     )),
                     list(shinydashboard::tabItem(
                       tabName = "configuration",
                       shiny::fluidRow(
                         shinydashboard::tabBox(
                           id = "tab_config",
                           title = "Configuration",
                           width = 12,
                           height = "85vh",
                           shiny::tabPanel(
                             # sqlite configuration file location
                             # this is stored in a YAML file
                             # allows a 'local' user to use a remote configuration file
                             title = "Configuration file",
                             value = "ConfigLocation",
                             shiny::column(
                               width=12,
                               shiny::wellPanel(
                                 textOutput('configuration_file_details')
                                 # location of sqlite configuration file
                               ),
                               shiny::wellPanel(
                                 shinyFiles::shinyFilesButton(
                                   "choose_configuration_file",
                                   label = "Choose configuration file",
                                   title = "Choose configuration file (must end in '.sqlite')",
                                   multiple = FALSE),
                                 shinyFiles::shinySaveButton(
                                   "create_configuration_file",
                                   label = "Create configuration file",
                                   title = "Create configuration file (must end in '.sqlite')",
                                   filetype = list(sqlite = c('sqlite'))),
                                 shiny::helpText(paste("Choose location of an existing configuration file,",
                                                       "or create a new configuration file"))
                               ))
                           ),
                           shiny::tabPanel(
                             # Microsoft SQL server details
                             title = "Microsoft SQL Server details",
                             value = "ServerPanel",
                             shiny::column(width = 12,
                                           servers_datatableUI("servers_dt"))
                           ),
                           shiny::tabPanel(
                             # Microsoft SQL server details
                             title = "Logging details",
                             value = "LoggingPanel",
                             shiny::column(width = 12,
                                           logging_datatableUI("logging_dt"))
                           ),
                           shiny::tabPanel(
                             # Practice locations or groups
                             title = "Practice locations/groups",
                             value = "LocationsPanel",
                             shiny::column(width = 12,
                                           locations_datatableUI("locations_dt"))
                           ),
                           shiny::tabPanel(
                             # User settings and permissions
                             title = "User settings and permissions",
                             value = "UsersPanel",
                             shiny::column(width = 12,
                                           userconfig_datatableUI("userconfig_dt"))
                           ),
                           shiny::tabPanel(
                             # User password
                             title = "User Password Setting",
                             value = "PasswordPanel",
                             shiny::column(width = 12,
                                           passwordConfig_UI("password_config"))
                           )
                         )
                       ))),
                     list(shinydashboard::tabItem(
                       tabName = "test",
                       shiny::fluidRow(shiny::column(width = 12, align = "center",
                                                     h2("Test frame"))),
                       shiny::fluidRow(shiny::column(width = 12,
                                                     DT::DTOutput("test_dt")))
                     )))

  output$tabItems <- renderUI({
    do.call(tabItems, shinytabItems)
  })
  # initially the appointments tabitem is not defined!
  shinydashboard::updateTabItems(session, "sidebartabs", "appointments")
  # so need to re-render

  ##### configuration file location tab ##############################################

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
      src = 'icons/doctor.svg', # this depends on addResourcePath in zzz.R
      # doctor.svg or user-icon.svg
      # Icons made by <a href="https://www.flaticon.com/authors/freepik" title="Freepik">Freepik</a>
      # from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a></div>
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
        )),
      shiny::fluidRow(
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "GPstat!",
            text = paste("v",packageVersion("DailyMeasure")),
            right_border = TRUE,
            margin_bottom = TRUE)
        ),
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "dMeasure",
            text = paste("v", packageVersion("dMeasure")),
            right_border = FALSE,
            margin_bottom = TRUE)
        )
      ),
      shiny::fluidRow(
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "Billings module",
            text = paste("v", ifelse(Billingsmodule,
                                     as.character(packageVersion("dMeasureBillings")),
                                     "None")),
            right_border = TRUE,
            margin_bottom = TRUE)
        ),
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "CDM module",
            text = paste("v", ifelse(CDMmodule,
                                     as.character(packageVersion("dMeasureCDM")),
                                     "None")),
            right_border = FALSE,
            margin_bottom = TRUE)
        )
      )
    )
  })
}
