##### Define server logic #####################################################

#' @import dbplyr
#' @import tidyr
#' @import shiny
#' @import dplyr
NULL

#' Shiny app server function
#'
#' @param input required for shiny server
#' @param output required for shiny server
#' @param session required for shiny server
#'
#' @return None
#'
#' @include calculation_definitions.R
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

  # User configuration file path
  # (this config file contains a pointer to the .sqlite configuration file path)

  configuration_file_path <- reactiveVal()

  dbversion <- reactiveVal(0)
  # increments each time EMR database is opened

  BPdatabase <- reactiveVal(
    value = data.frame(id = integer(),
                       Name = character(),
                       Address = character(),
                       Database = character(),
                       UserID = character(),
                       dbPassword = character(),
                       stringsAsFactors = FALSE))
  BPdatabaseChoice <- reactiveVal(value = character())
  # database choice will be the same as the 'Name' of the chosen entry in BPdatabase
  PracticeLocations <- reactiveVal(
    value = data.frame(id = integer(),
                       Name = character(),
                       Description = character(),
                       stringsAsFactors = FALSE))
  # id needed for editing this dataframe later
  # need default value for practice location filter interface initialization
  UserConfig <- reactiveVal(
    value = data.frame(id = integer(),
                       Fullname = character(), AuthIdentity = character(),
                       Location = character(),
                       Attributes = character(),
                       Password = character(),
                       stringsAsFactors = FALSE))

  UserRestrictions <- reactiveVal(
    value = data.frame(uid = integer(),
                       Restriction = character(),
                       stringsAsFactors = FALSE))
  # this lists the 'enabled' restrictions,
  #  relevant to the 'Attributes' field of 'UserConfig'
  # without the restriction, all users have the 'permission'
  #  for the 'non-specified' action
  # use 'uid' rather than 'id', because 'id' is later used to identify the restrictions...

  observeEvent(dM$configuration_file_path(), ignoreNULL = TRUE, {
    dM$open_configuration_db()
    # connects to SQLite configuration database, using either DBI or pool
    # generates the SQLite configuration database if needed
    # and updates old SQLite configuration databases with necessary fields
    dM$read_configuration_db()
    # reads server definitions, location definitions, user attributes etc..
  })

  ### database initialization

  observeEvent(BPdatabaseChoice(), ignoreInit = TRUE, {
    print(paste("ChosenServerName:", BPdatabaseChoice()))

    if (BPdatabaseChoice() == "None") {
      dM$BPdatabaseChoice <- BPdatabaseChoice()
      # the '$BPdatabaseChoice' is an active field.
      # setting to "None" will close any currently open databases
    } else if (!is.null(BPdatabaseChoice())) {
      shinytoastr::toastr_info(
        "Opening link to Best Practice", closeButton = TRUE,
        position = "bottom-center", title = "Best Practice database")

      dM$BPdatabaseChoice <- BPdatabaseChoice()
      # setting $BPdatabaseChoice 'active field' will try to
      # open the requested database
      if (dm$BPdatabaseChoice == "None") {
        # failed to open database, has been re-set to "None"
        shinytoastr::toastr_error(
          paste0(e), title = "Error opening Best Practice database",
          closeButton = TRUE, position = "bottom-center",
          timeOut = 0) # stays open until clicked
        # SweetAlert from shinyWidgets not working as of June/2019
        # (including in shinyWidget's gallery)
        # shinyWidgets::sendSweetAlert(
        #  session = session,
        #  title = "Error opening database",
        #  text = e,
        #  type = "error")
      } else {
        shinytoastr::toastr_success(
          "Linking to Best Practice database successful!",
          closeButton = TRUE,
          position = "bottom-center",
          title = "Best Practice database")
      }
    }
  })

  UserFullConfig <- reactiveVal(NULL)
  # place-holder only there is no code to modify this at the ment
  # dM contains an active field '$UserFullConfig'
  # contains user names attached to configuration information
  # contains ALL user names
  # UserConfig() contains just names who have been configured

  # 'helper' functions for input panel

  # only adjust appointment view after dates are 'submitted' using 'submit' button
  date_a <- eventReactive(input$update_date, {
    dM$choose_date(date_from = input$date1) # also update the dMeasure object
    input$date1
  }, ignoreNULL = FALSE) # initialize on first run, after that only update if 'update' button used
  date_b <- eventReactive(input$update_date, {
    dM$choose_date(date_to = input$date2) # also update the dMeasure object
    input$date2
  }, ignoreNULL = FALSE)

  date_today <- observeEvent(input$update_date_today, {
    # 'today' button. change date range to today, and click the 'update' button
    updateDateInput(session, 'date1', value = Sys.Date())
    updateDateInput(session, 'date2', value = Sys.Date())
    # change date range to today
    click('update_date')
    # and click the 'update' button
  })

  output$locationList <- renderUI({
    selectInput(inputId = 'location', label = 'Practice location',
                choices = dM$location_list(), selected = 'All')
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
  clinician_choice_list <- reactiveVal()
  #
  observeEvent(c(dM$dbversion, input$location, dM$UserRestrictions, input$sidebartabs), {
    # respond to database initialization or change in input choice
    # respond to change in UserRestrictions and which sidebartab is selected
    shiny::validate(
      shiny::need(input$location, "Locations not available")
    )

    clinician_list <- dM$clinician_list(input$sidebartabs)
    # find list of clinicians which can be selected to be viewed
    #  filter by current $location setting
    #  and also the view (input$sidebartabs) if a view restriction is activated
    #  depending on user attributes/permissions and authentication/login status
    #  this also sets dM$clinician_choice_list

    clinician_choice_list(clinician_list)
  })

  output$clinicianList <- renderUI({
    choice_list <- clinician_choice_list()
    chosen_list <- input$clinicians # retain previous selections
    checkboxGroupInput('clinicians', label = 'Clinician',
                       choices = choice_list, selected = chosen_list)
  })

  observeEvent(input$clinicians, {
    dM$choose_clinicians(input$clinicians)
    # alter dMeasure object according to user input
    # (or perhaps 'toggle' button below)
  })

  toggle_clinicians <- observeEvent(input$toggle_clinician_list, {
    if (input$toggle_clinician_list == 0) {return(NULL)}
    else if (input$toggle_clinician_list%%2 == 1) {
      updateCheckboxGroupInput(session, 'clinicians', selected = clinician_choice_list())
      # toggle all clinicians selected
    } else {
      updateCheckboxGroupInput(session, 'clinicians', selected = character(0))
      # no clinicians selected
    }
  })

  # Immunization functions

  vax_table_results <- callModule(vax_datatable, "vax_dt", dM)

  # Bowel cancer screening

  callModule(cancerscreen_datatable, "cancerscreen_dt", dM)

  # call the module to generate the table
  callModule(billings_datatable, "billings_dt", dM)

  # chronic disease management table
  cdm_table_results <- callModule(cdm_datatable, "cdm_dt", dM)

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
    paste('Configuration file location: "', dM$configuration_file_path(), '"')
  })

  volumes <- c(shinyFiles::getVolumes()(), base = '.', home = Sys.getenv("USERPROFILE"))

  shinyFiles::shinyFileChoose(
    input, id = 'choose_configuration_file',
    session = session,
    roots = volumes,
    filetypes = c('sqlite') # only files ending in '.sqlite'
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

  # database configuration tab
  serverconfig_change <- callModule(servers_datatable, "servers_dt", dM)
  # returns $count

  # location configuration tab
  location_list_change <- callModule(locations_datatable, "locations_dt", dM)

  observeEvent(c(location_list_change(), dM$location_listR), {
    # change in location_listR (by GUI editor in locations_data module) prompts
    # change in location list filter (for providers) and location_list_names (for user config)
    updateSelectInput(session, inputId = 'location', choices = dM$location_list())
    location_list_names(PracticeLocations() %>% select(Name) %>% collect() %>% unlist(use.names = FALSE))
  })

  userconfig_change <- callModule(userconfig_datatable, "userconfig_dt", dM)

  ###### user configuration of their own password #######################
  callModule(passwordConfig_server, "password_config", dM)

  observeEvent(c(dM$identified_user(), dM$UserRestrictions()), {
    shiny::validate(
      shiny::need(dM$identified_user(), "No user information")
    )
    if ("ServerAdmin" %in% unlist(dM$UserRestrictions()$Restriction)) {
      # only some users allowed to see/change server settings
      if ("ServerAdmin" %in% unlist(dM$identified_user()$Attributes)) {
        showTab("tab_config", "ServerPanel")
      } else {
        hideTab("tab_config", "ServerPanel")
      }
    } else {
      showTab("tab_config", "ServerPanel")
    }
    if ("UserAdmin" %in% unlist(dM$UserRestrictions()$Restriction)) {
      # only some users allowed to see/change user settings
      if ("UserAdmin" %in% unlist(dM$identified_user()$Attributes)) {
        showTab("tab_config", "LocationsPanel")
        # also change ability to view locations panel
        showTab("tab_config", "UsersPanel")
      } else {
        hideTab("tab_config", "LocationsPanel")
        hideTab("tab_config", "UsersPanel")
        browser()
      }
    } else {
      showTab("tab_config", "LocationsPanel")
      showTab("tab_config", "UsersPanel")
    }
    if (nrow(dM$identified_user()) > 0) {
      # user has been identified
      showTab("tab_config", "PasswordPanel")
      # only 'identified' and configured users can set a password
    } else {
      # no user identified
      hideTab("tab_config", "PasswordPanel")
    }
  })

  ####### User login #######################################################
  observeEvent(c(dM$identified_user(), dM$UserRestrictions()), {
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
          showModal(modalDialog(
            title="New password",
            tagList(
              paste("Password required for user ",
                    dM$identified_user()$Fullname, "."),
              HTML("You need to set a password<br><br>"),
              passwordInput("password1",
                            label = "Enter Password", value = ""),
              br(),
              passwordInput("password2",
                            label = "Confirm Password", value = "")
            ),
            footer = tagList(actionButton("confirmNewPassword", "Confirm"))
          ))
        } else {
          showModal(modalDialog(
            title="Password required",
            tagList(
              paste("Password required for user ",
                    dM$identified_user()$Fullname, "."),
              HTML("<br><br>Please enter your password!<br>
              Click the 'Enter' button after typing in your password.<br><br>
              This is not (or shouldn't be!) your Windows or Best Practice password<br><br>"),
              passwordInput("password", label = "Password", value = "")
            ),
            footer = tagList(actionButton("confirmPassword", "Enter"))
          ))
        }
      } else {
        # no user identified! but password required
        # will need to stop
        showModal(modalDialog(
          title="Password required",
          tagList(
            "User not recognized!",
            br(),
            "Please contact your systems administrator."
          ),
          footer = tagList()
        ))
      }
    }
  })

  observeEvent(input$confirmNewPassword, {
    if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                closeButton = TRUE)
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long")
    } else {
      dM$set_password(newpassword = input$password1)
      # will also set dM$authenticated to TRUE
      removeModal()
      shinytoastr::toastr_success(message = "Password set and Successful login",
                                  title = "Welcome back!",
                                  position = "bottom-left")
    }
  })

  input_password_CR <- reactiveVal(0)

  observeEvent(c(input$confirmPassword), {
    shiny::validate(
      shiny::need(nchar(input$password) > 0, "No password entered")
    )

    if (tryCatch(dM$user_login(input$password),
                 error = function(e) {return(FALSE)})) {
      # dM$user_login returns $authenticated if successful log-in i.e. TRUE
      # otherwise returns and error
      shinytoastr::toastr_error("Wrong password",
                                closeButton = TRUE)
    } else {
      # successful login
      removeModal()
      shinytoastr::toastr_success(message = paste("Successful login for",
                                                  dM$identified_user()$Fullname),
                                  title = "Welcome back!",
                                  position = "bottom-left")
    }
  })

  ###### Render user information on top-right header ##########################
  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = identified_user()$Fullname,
      src = 'icons/user-avatar.svg', # this depends on addResourcePath in zzz.R
      subtitle = Sys.info()[["user"]], # not necessarily an identified user
      fluidRow(
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
