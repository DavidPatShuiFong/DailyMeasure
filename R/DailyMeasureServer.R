# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

##### Define server logic #####################################################

sessionCount <- reactiveValues(count = 0) # initially no sessions opened
# strangely, doesn't work if not a reactive

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
#' @include introduction.R
#'
#' needed for simple encode/decode
#' @export
DailyMeasureServer <- function(input, output, session) {
  if (!exists(".bcdyz.option")) {
    .bcdyz.option <<- list(demonstration = FALSE)
  }
  print(.bcdyz.option) # this can be passed from a calling function shiny::runApp()

  shiny::isolate(sessionCount$count <- sessionCount$count + 1)
  print(paste("Session Count:", shiny::isolate(sessionCount$count)))

  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    dM$close()
    # close $config_db and $emr_db

    isolate(sessionCount$count <- sessionCount$count - 1)
    print(paste("Session Count:", isolate(sessionCount$count)))

    if (isolate(sessionCount$count) == 0) {
      shiny::stopApp() # last session closed, so stop the App
    }
  })

  # create the dMeasure object
  dM <- dMeasure::dMeasure$new()

  # create dMeasureQIM objects
  QIMmodule <- requireNamespace("dMeasureQIM", quietly = TRUE)
  # is module (package) available?
  if (QIMmodule) {
    dMQIMrept <- dMeasureQIM::dMeasureQIM$new(dM) # 'report' view of module
    dMQIMappt <- dMeasureQIM::dMeasureQIM$new(dM) # a second QIM module
    dMQIMappt$qim_contact <- FALSE # second module uses appointment list, not contact list
    dMQIMappt$qim_demographicGroup <- c("") # by default, the second module does not show QIM aggregate groups
  }
  Billingsmodule <- requireNamespace("dMeasureBillings", quietly = TRUE)
  # is module (package) available?
  if (Billingsmodule) {
    dMBillings <- dMeasureBillings::dMeasureBillings$new(dM)
  }
  CDMmodule <- requireNamespace("dMeasureCDM", quietly = TRUE)
  # is module (package) available?
  if (Billingsmodule & CDMmodule) {
    # needs both modules!
    dMCDM <- dMeasureCDM::dMeasureCDM$new(dM, dMBillings)
  }
  Custommodule <- requireNamespace("dMeasureCustom", quietly = TRUE)
  if (Custommodule) {
    dMCustom <- dMeasureCustom::dMeasureCustom$new(dM)
  }
  Medicationmodule <- requireNamespace("dMeasureMedication", quietly = TRUE)
  if (Medicationmodule) {
    dMMedication <- dMeasureMedication::dMeasureMedication$new(dM)
  }
  # read config files

  ##### Configuration file ######################################################

  shiny::observeEvent(dM$configuration_file_pathR(),
    ignoreNULL = TRUE, {
      dM$open_configuration_db()
      # connects to SQLite configuration database, using either DBI or pool
      # generates the SQLite configuration database if needed
      # and updates old SQLite configuration databases with necessary fields
      dM$read_configuration_db()
      if (Custommodule &&
          exists("read_configuration_db",
                 where = asNamespace("dMeasureCustom"),
                 mode = "function")
      ) {
        # if dMCustom has a read_configuration_db function, then use it
        dMCustom$read_configuration_db()
      }
      # reads server definitions, location definitions, user attributes etc..
      if (dM$config_db$is_open()) {
        newdb <- dM$BPdatabaseChoice_new()
        if (newdb != dM$BPdatabaseChoice) {
          shinytoastr::toastr_info(
            "Opening link to Best Practice",
            closeButton = TRUE,
            position = "bottom-left", title = "Best Practice database"
          )
          opened_base <- dM$open_emr_db()
          if (opened_base == "None") {
            shinytoastr::toastr_error(
              "Error opening Best Practice database",
              closeButton = TRUE, position = "bottom-left",
              timeOut = 10000
            ) # stays open ten seconds
          } else {
            shinytoastr::toastr_success(
              "Linking to Best Practice database successful!",
              closeButton = TRUE,
              position = "bottom-left",
              title = "Best Practice database"
            )
          }
        }
      } else {
        shinytoastr::toastr_error(
          "Error opening configuration file",
          closeButton = TRUE, position = "bottom-left",
          timeOut = 10000
        ) # stays open ten seconds
      }
    }
  )
  invisible(dM$configuration_file_path)
  # this will also set $configuration_file_pathR


  # 'helper' functions for input panel

  # only adjust appointment view after dates are 'submitted' using 'submit' button
  shiny::observeEvent(
    input$update_date,
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    {
      tryCatch({
        dM$choose_date(
          date_from = as.Date(input$date1),
          date_to = as.Date(input$date2)
        ) # also update the dMeasure object
      },
        warning = function(w) {
          shinytoastr::toastr_warning(
            message = w$message,
            position = "bottom-left"
          )
        }
      )
    }
  )

  date_today <- shiny::observeEvent(input$update_date_today, {
    # 'today' button. change date range to today, and click the 'update' button
    shiny::updateDateInput(session, "date1", value = Sys.Date(), max = Sys.Date())
    shiny::updateDateInput(session, "date2", value = Sys.Date(), min = Sys.Date())
    # change date range to today
    shinyjs::click("update_date")
    # and click the 'update' button

    # work more reliably if doubled?
    # change date range to today
    shinyjs::click("update_date")
  })

  output$daterange <- shiny::renderUI({
    # creating during 'server' run might force the dates
    # to actually be Sys.Date()?
    # this might only be an issue where a server has
    # 'pre-created' the UI on first-run
    shiny::tagList(
      shiny::dateInput(
        inputId = "date1",
        label = "From:", format = "D dd/M/yyyy",
        min = Sys.Date() - 9000,
        value = Sys.Date()
      ),
      shiny::dateInput(
        inputId = "date2",
        label = "To:", format = "D dd/M/yyyy",
        max = Sys.Date() + 180,
        value = Sys.Date()
      )
    )
  })

  shiny::observeEvent(
    c(input$date1,
      input$date2),
    ignoreInit = TRUE,
    ignoreNULL = FALSE,
    {
      shiny::req(input$date1, input$date2)
      # this event can be provoked during an incomplete date input,
      # so need to check 'valid' date with `shiny::req`
      shinyjqui::jqui_effect(
        "#update_date_wrapper",
        effect = "bounce",
        options = list(distance = 1)
      )
      if (input$date1 > dM$date_b) {
        shiny::updateDateInput(session, "date1", value = dM$date_b)
      }
      if (input$date2 < dM$date_a) {
        shiny::updateDateInput(session, "date2", value = dM$date_a)
      }
    })

  shiny::observeEvent(
    dM$date_aR(),
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      # change 'date_from' in response to $date_a
      if (input$date1 != dM$date_a) {
        shiny::updateDateInput(session, "date1", value = dM$date_a)
      }
    }
  )

  shiny::observeEvent(
    dM$date_bR(),
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      # change 'date_to' in response to $date_a
      if (input$date2 != dM$date_b) {
        shiny::updateDateInput(session, "date2", value = dM$date_b)
      }
    }
  )

  output$last_visit <- shiny::renderUI({
    shiny::tagList(
      shiny::h5("Most recent contact"),
      shiny::dateInput(
        inputId = "min_date",
        label = "From:", format = "D dd/M/yyyy",
        min = Sys.Date() - 6000, max = Sys.Date(),
        value = Sys.Date() - 6000
      ),
      shiny::dateInput(
        inputId = "max_date",
        label = "To:", format = "D dd/M/yyyy",
        min = Sys.Date() - 6000, max = Sys.Date() + 180,
        value = Sys.Date()
      ),
      shiny::div(
        id = "update_lastvisit_wrapper",
        # wrapper needed for shinyjqui shake effect
        shiny::actionButton("update_lastvisit", "Update",
          shiny::icon("refresh"),
          class = "btn btn-primary"
        )
      )
    )
  })

  shiny::observeEvent(
    c(input$min_date,
      input$max_date),
    ignoreInit = FALSE,
    ignoreNULL = FALSE,
    {
      shinyjqui::jqui_effect(
        "#update_lastvisit_wrapper",
        effect = "bounce",
        options = list(distance = 1)
      )
      shiny::updateDateInput(session, "min_date", max = input$max_date)
      shiny::updateDateInput(session, "max_date", min = input$min_date)
    })

  shiny::observeEvent(
    dM$contact_minDateR(),
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      # change 'min_date' in response to $min_dateR
      if (input$min_date != dM$contact_minDate) {
        shiny::updateDateInput(session, "min_date", value = dM$contact_minDate)
        shiny::updateDateInput(session, "max_date", min = dM$contact_minDate)
      }
    }
  )

  shiny::observeEvent(
    dM$contact_maxDateR(),
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      # change 'max_date' in response to $max_dateR
      if (input$max_date != dM$contact_maxDate) {
        shiny::updateDateInput(session, "max_date", value = dM$contact_maxDate)
        shiny::updateDateInput(session, "min_date", max = dM$contact_maxDate)
      }
    }
  )

  # only adjust appointment view after dates are 'submitted' using 'submit' button
  shiny::observeEvent(
    input$update_lastvisit,
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    {
      dM$contact_minDate <- input$min_date
      dM$contact_maxDate <- input$max_date
    }
  )

  output$locationList <- shiny::renderUI({
    shiny::selectInput(
      inputId = "location", label = "Practice location",
      choices = dM$location_list, selected = "All"
    )
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
  shiny::observeEvent(c(
    dM$dbversion, input$location,
    dM$UserRestrictions, input$sidebartabs
  ), {
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
    shiny::checkboxGroupInput("clinicians",
      label = "Clinician",
      choices = choice_list, selected = chosen_list
    )
  })

  shiny::observeEvent(input$update_clinicians, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # cannot ignoreNULL because sometimes an empty list will be chosen
    dM$choose_clinicians(choices = input$clinicians)
    # alter dMeasure object according to user input
    # (or perhaps 'toggle' button below)
  })

  shiny::observeEvent(
    c(input$clinicians),
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      shinyjqui::jqui_effect(
        "#update_clinicians_wrapper",
        effect = "bounce",
        options = list(distance = 1)
      )
    }
  )

  toggle_clinicians <- shiny::observeEvent(input$toggle_clinician_list, {
    if (input$toggle_clinician_list == 0) {
      return(NULL)
    }
    else if (input$toggle_clinician_list %% 2 == 1) {
      shiny::updateCheckboxGroupInput(session, "clinicians",
        selected = clinician_choice_list()
      )
      # toggle all clinicians selected
    } else {
      shiny::updateCheckboxGroupInput(session, "clinicians",
        selected = character(0)
      )
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
    c(
      list(shinydashboard::tabItem(
        tabName = "appointments",
        fluidRow(column(width = 12, align = "center", h2("Appointments"))),
        fluidRow(column(width = 12, shiny::div(
          id = "appointments_datatable_wrapper", # for rintrojs
          appointments_datatableUI("appointments_dt")
        )))
      )),
      list(shinydashboard::tabItem(
        tabName = "immunization",
        fluidRow(column(width = 12, align = "center", h2("Immunization"))),
        fluidRow(column(width = 12, shiny::div(
          id = "immunization_datatable_wrapper", # for rintrojs
          vax_datatableUI("vax_dt")
        )))
      )),
      list(shinydashboard::tabItem(
        tabName = "cancerscreen",
        fluidRow(column(width = 12, align = "center", h2("Cancer screening"))),
        fluidRow(column(width = 12, shiny::div(
          id = "cancerscreen_datatable_wrapper",
          cancerscreen_datatableUI("cancerscreen_dt")
        )))
      ))
    )

  # no PIP Quality Improvement Measure, billings, or CDM tabs, these are inserted dynamically
  # in the server section if the dMeasureQIM module/package is available

  #####################################################################################

  # Immunization functions
  vax_table_results <- callModule(vax_datatable, "vax_dt", dM)

  # Cancer screening
  callModule(cancerscreen_datatable, "cancerscreen_dt", dM)

  if (Billingsmodule == TRUE) {
    output$BillingsMenu <- shinydashboard::renderMenu({
      dMeasureBillings::shinydashboardmenuItem()
    }) # if Billingsmodule is FALSE, then output$BillingsMenu will be left undefined
    shinytabItems <- c(shinytabItems, dMeasureBillings::dMeasureShinytabItems())
    # call the module to generate the table
    billings_table_results <- callModule(
      dMeasureBillings::datatableServer,
      "billings_dt", dMBillings
    )
  }

  if (CDMmodule == TRUE & Billingsmodule == TRUE) {
    output$CDMMenu <- shinydashboard::renderMenu({
      dMeasureCDM::shinydashboardmenuItem()
    }) # if CDMmodule or Billingsmodule is FALSE, then output$CDMMenu will be left undefined
    shinytabItems <- c(shinytabItems, dMeasureCDM::dMeasureShinytabItems())
    # chronic disease management table
    cdm_table_results <- callModule(
      dMeasureCDM::datatableServer,
      "cdm_dt", dMCDM
    )
  }

  if (Custommodule == TRUE) {
    output$CustomMenu <- shinydashboard::renderMenu({
      dMeasureCustom::shinydashboardmenuItem()
    })
    shinytabItems <- c(shinytabItems, dMeasureCustom::dMeasureShinytabItems())
    # chronic disease management table
    custom_table_results <- callModule(
      dMeasureCustom::datatableServer,
      "custom_dt", dMCustom
    )
  }
  if (Medicationmodule == TRUE) {
    output$MedicationMenu <- shinydashboard::renderMenu({
      dMeasureMedication::shinydashboardmenuItem()
    })
    shinytabItems <- c(shinytabItems, dMeasureMedication::dMeasureShinytabItems())
    # chronic disease management table
    medication_table_results <- callModule(
      dMeasureMedication::datatableServer,
      "medication_dt", dMMedication
    )
  }

  # Conditions
  callModule(conditions, "conditions_dt", dM)
  # administration and result management tab
  admin_table_results <- callModule(administration, "admin_dt", dM)
  # about
  callModule(about, "about_dt", dM)

  if (QIMmodule == TRUE) {
    # only if QIM module/package is available
    output$PIPqimMenu <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(.list = list(
        shinydashboard::menuItem("PIP Quality Improvement",
          tabName = "qimRept", icon = shiny::icon("chart-line")
        ),
        shinydashboard::menuItem("QIM Appointment",
          tabName = "qimAppt", icon = shiny::icon("chart-line")
        )
      ))
    })
    # if QIMmodule is FALSE, then output$PIPqimMenu will be left undefined

    # Practice Incentive Program (PIP) Quality Improvement (QI) measures
    # add PIP QIM tab items to the tabItem vector
    shinytabItems <- c(
      shinytabItems,
      list(shinydashboard::tabItem(
        tabName = "qimRept",
        shiny::fluidRow(column(
          width = 12, align = "center",
          h2("Quality Improvement Measure Reporting")
        )),
        shiny::fluidRow(column(
          width = 12,
          qim_UI("qimRept")
        ))
      )),
      list(shinydashboard::tabItem(
        tabName = "qimAppt",
        shiny::fluidRow(column(
          width = 12, align = "center",
          h2("Quality Improvement Measure Appointment View")
        )),
        shiny::fluidRow(column(width = 12, qim_UI("qimAppt")))
      ))
    )
    qim_results_rept <- callModule(qim, "qimRept", dMQIMrept, contact = TRUE) # 'report' view
    # Practice Incentive Program (PIP) Quality Improvement (QI) measures
    # appointment view
    qim_results_appt <- callModule(qim, "qimAppt", dMQIMappt, contact = FALSE)
  }

  # appointment list
  callModule(appointments_datatable, "appointments_dt", dM)

  output$test_dt <-
    DT::renderDT({
      DT::datatable(
        data.frame(
          a = c(2, 3, 68),
          b = c(
            '<span class="huge green positive ui tag label"><span data-tooltip="check me" data-variation="huge">
                                       721
                                       </span></span>
                                       <span class="huge green positive ui tag label">723</span><span class="ui tag label">10990</span>',
            '<div class="huge ui negative button" data-tooltip="waiting ... "><i class="wheelchair loading icon"></i>
                                       2715</div>',
            '<div class="huge ui button positive" data-variation="wide" data-html="<h1>
                                       Cheese factory
                                       </h1><font size=\'+0\'><b>Lots and lots</b> of information. make sure everything is <ins>complete</ins> on year after ... 12/Jan/2019</font>">GPMP</div>'
          )
        ),
        options = list(initComplete = DT::JS(semantic_popupJS)),
        escape = FALSE,
        fillContainer = FALSE
      )
    })

  ##### configuration tabItem ########################################################

  configuration_tabPanels <- list(
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
        width = 12,
        shiny::wellPanel(
          textOutput("configuration_file_details")
          # location of sqlite configuration file
        ),
        shiny::wellPanel(
          {
            if (.bcdyz.option$demonstration) {
              shiny::span(shiny::p(),
                shiny::strong("Demonstration mode : Configuration file choice disabled"),
                style = "color:red", shiny::p()
              )
            }
            else {}
          }, {
            x <- shinyFiles::shinyFilesButton(
              "choose_configuration_file",
              label = "Choose configuration file",
              title = "Choose configuration file (must end in '.sqlite')",
              multiple = FALSE
            )
            # disabled if demonstration mode
            if (.bcdyz.option$demonstration) {
              shinyjs::disabled(x)
            } else {
              x
            }
          }, {
            x <- shinyFiles::shinySaveButton(
              "create_configuration_file",
              label = "Create (and choose) configuration file",
              title = "Create (and choose) configuration file (must end in '.sqlite')",
              filetype = list(sqlite = c("sqlite"))
            )
            # disabled if demonstration mode
            if (.bcdyz.option$demonstration) {
              shinyjs::disabled(x)
            } else {
              x
            }
          },
          shiny::helpText(
            paste(
              "Choose location of an existing configuration file,",
              "or create a new configuration file."
            ),
            shiny::br(), shiny::br(),
            paste(
              "It is strongly recommended that if a different",
              "configuration file is chosen, or a new configuration",
              "file is created,"
            ),
            shiny::br(),
            paste(
              "that the user exit(/close) GPstat!",
              "and then (re-)start GPstat!"
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      # Microsoft SQL server details
      title = "Microsoft SQL Server details",
      value = "ServerPanel",
      shiny::column(
        width = 12,
        servers_datatableUI("servers_dt")
      )
    ),
    shiny::tabPanel(
      # Microsoft SQL server details
      title = "Logging details",
      value = "LoggingPanel",
      shiny::column(
        width = 12,
        logging_datatableUI("logging_dt")
      )
    ),
    shiny::tabPanel(
      # Practice locations or groups
      title = "Practice locations/groups",
      value = "LocationsPanel",
      shiny::column(
        width = 12,
        locations_datatableUI("locations_dt")
      )
    ),
    shiny::tabPanel(
      # User settings and permissions
      title = "User settings and permissions",
      value = "UsersPanel",
      shiny::column(
        width = 12,
        userconfig_datatableUI("userconfig_dt")
      )
    ),
    shiny::tabPanel(
      # User password
      title = "User Password Setting",
      value = "PasswordPanel",
      shiny::column(
        width = 12,
        passwordConfig_UI("password_config")
      )
    )
  )

  if (Custommodule) {
    if (
      exists(
        "dMeasureConfigurationTabPanelItem",
        where = asNamespace("dMeasureCustom"),
        mode = "function"
      )
    ) {
      configuration_tabPanels <- append(
        configuration_tabPanels,
        list(
          dMeasureCustom::dMeasureConfigurationTabPanelItem()
        )
      )
    }
  }

  configuration_tabItem <-
    list(
      shinydashboard::tabItem(
        tabName = "configuration",
        shiny::fluidRow(
          do.call(shinydashboard::tabBox, configuration_tabPanels)
        )
      )
    )


  ##### final definition of tabItems #################################################

  shinytabItems <- c(
    shinytabItems,
    list(shinydashboard::tabItem(
      tabName = "conditions",
      shiny::fluidRow(column(
        width = 12,
        conditions_UI("conditions_dt")
      ))
    )),
    list(shinydashboard::tabItem(
      tabName = "administration",
      # shiny::fluidRow(column(width = 12, align = "center",
      #                       h2("Administration"))),
      shiny::fluidRow(column(
        width = 12,
        administration_UI("admin_dt")
      ))
    )),
    configuration_tabItem,
    list(shinydashboard::tabItem(
      tabName = "about",
      fluidRow(column(width = 12, about_UI("about_dt")))
    )),
    list(shinydashboard::tabItem(
      tabName = "test",
      shiny::fluidRow(shiny::column(
        width = 12, align = "center",
        h2("Test frame")
      )),
      shiny::fluidRow(shiny::column(
        width = 12,
        DT::DTOutput("test_dt")
      ))
    ))
  )

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

  volumes <- c(shinyFiles::getVolumes()(), base = ".", home = Sys.getenv("USERPROFILE"))

  shinyFiles::shinyFileChoose(
    input,
    id = "choose_configuration_file",
    session = session,
    roots = volumes,
    filetypes = c("sqlite"), # only files ending in '.sqlite'
    hidden = TRUE # the default is that configuration files have '.' hidden prefix
  )

  shiny::observeEvent(input$choose_configuration_file, ignoreNULL = TRUE, {
    if (!is.integer(input$choose_configuration_file)) {
      # if input$choose_configuration_file is an integer,
      # it is just the 'click' event on the filechoose button
      inFile <- shinyFiles::parseFilePaths(volumes, input$choose_configuration_file)
      file_name <- paste(inFile$datapath)
      dM$configuration_file_path <- file_name
      # this dMeasure method will also update the YAML configuration file
      shinytoastr::toastr_warning(
        message = paste(
          "New configuration file chosen.",
          "Recommend GPstat! is re-started!"
        ),
        position = "bottom-left",
        closeButton = TRUE,
        timeOut = 0
      ) # keep open until closed
    }
  })

  shinyFiles::shinyFileSave(
    input,
    id = "create_configuration_file",
    session = session,
    roots = volumes,
    hidden = TRUE
  )

  shiny::observeEvent(input$create_configuration_file, ignoreNULL = TRUE, {
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
      if (Custommodule &&
          exists("read_configuration_db",
                 where = asNamespace("dMeasureCustom"),
                 mode = "function")
      ) {
        # if dMCustom has a read_configuration_db function, then use it
        dMCustom$read_configuration_db()
      }
      shinytoastr::toastr_warning(
        message = paste(
          "New configuration file created and chosen.",
          "Recommend GPstat! is re-started!"
        ),
        position = "bottom-left",
        closeButton = TRUE,
        timeOut = 0
      ) # keep open until closed
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
    shiny::updateSelectInput(session,
      inputId = "location",
      choices = dM$location_listR()
    )
  })

  userconfig_change <- callModule(userconfig_datatable, "userconfig_dt", dM)

  ###### custom module configuration ##############################

  if (Custommodule) {
    if (
      exists(
        "dMeasureConfigurationTabPanel",
        where = asNamespace("dMeasureCustom"),
        mode = "function"
      )
    ) {
      callModule(
        dMeasureCustom::dMeasureConfigurationTabPanel,
        "dMeasureCustom_config_dt",
        dMCustom
      )
    }
  }

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
            title = "New password",
            shiny::tagList(
              paste(
                "Password required for user ",
                dM$identified_user()$Fullname, "."
              ),
              shiny::HTML("You need to set a password<br><br>"),
              shiny::passwordInput("password1",
                label = "Enter Password", value = ""
              ),
              shiny::br(),
              shiny::passwordInput("password2",
                label = "Confirm Password", value = ""
              )
            ),
            footer = shiny::tagList(shiny::actionButton(
              "confirmNewPassword",
              "Confirm"
            ))
          ))
        } else {
          shiny::showModal(shiny::modalDialog(
            title = "Password required",
            shiny::tagList(
              paste(
                "Password required for user ",
                dM$identified_user()$Fullname, "."
              ),
              shiny::HTML("<br><br>Please enter your password!<br>
              Click the 'Enter' button after typing in your password.<br><br>
              This is not (or shouldn't be!) your Windows or Best Practice password<br><br>"),
              shiny::passwordInput("password", label = "Password", value = "")
            ),
            footer = shiny::tagList(shiny::actionButton(
              "confirmPassword",
              "Enter"
            ))
          ))
        }
      } else {
        # no user identified! but password required
        # will need to stop
        shiny::showModal(shiny::modalDialog(
          title = "Password required",
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
        closeButton = TRUE
      )
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long")
    } else {
      dM$password.set(newpassword = input$password1)
      # will also set dM$authenticated to TRUE
      shiny::removeModal()
      shinytoastr::toastr_success(
        message = "Password set and Successful login",
        title = "Welcome back!",
        position = "bottom-left"
      )
    }
  })

  input_password_CR <- reactiveVal(0)

  shiny::observeEvent(c(input$confirmPassword), {
    shiny::validate(
      shiny::need(nchar(input$password) > 0, "No password entered")
    )

    if (!tryCatch(dM$user_login(input$password),
      error = function(e) {
        return(FALSE)
      }
    )) {
      # dM$user_login returns $authenticated if successful log-in i.e. TRUE
      # otherwise returns an error
      shinytoastr::toastr_error("Wrong password",
        position = "bottom-left",
        closeButton = TRUE
      )
    } else {
      # successful login
      shiny::removeModal()
      shinytoastr::toastr_success(
        message = paste(
          "Successful login for",
          dM$identified_user()$Fullname
        ),
        title = "Welcome back!",
        position = "bottom-left"
      )
    }
  })

  ###### Guides ###############################################################

  shiny::observeEvent(input$guide_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps = steps_overview_df(),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })

  shiny::observeEvent(input$appointments_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps = steps_appointment_df(),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })

  shiny::observeEvent(input$immunization_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps = steps_immunization_df(),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })

  shiny::observeEvent(input$cancerscreen_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps = steps_cancerscreen_df(),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })

  shiny::observeEvent(input$billings_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps = dMeasureBillings::steps_introduction_df("#billings_datatable_wrapper"),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })
  shiny::observeEvent(input$cdm_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps = dMeasureCDM::steps_introduction_df("#cdm_datatable_wrapper"),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })
  shiny::observeEvent(input$conditions_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps =
          steps_conditions_df(
            eval(parse(
              text =
                paste0(
                  "input$`",
                  shiny::NS("conditions_dt")
                  ("tab_conditions"),
                  "`"
                )
            ))
          ),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })
  shiny::observeEvent(input$qimRept_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps =
          dMeasureQIM::steps_introduction_df(
            paste0(
              "#",
              shiny::NS("qimRept")
              ("qim_datatable_wrapper")
            ),
            eval(parse(
              text =
                paste0(
                  "input$`",
                  shiny::NS("qimRept")
                  ("tab_qim"),
                  "`"
                )
            )),
            appointment_view = FALSE
          ),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })
  shiny::observeEvent(input$qimAppt_overview, {
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    # above opens the right side-bar
    # see https://stackoverflow.com/questions/
    #  58012484/activate-deactivate-tab-in-the-rightsidebar-of-a-shinydashboardplus-at-click-on
    rintrojs::introjs(session,
      options = list(
        steps =
          dMeasureQIM::steps_introduction_df(
            paste0(
              "#",
              shiny::NS("qimAppt")
              ("qim_datatable_wrapper")
            ),
            eval(parse(
              text =
                paste0(
                  "input$`",
                  shiny::NS("qimAppt")
                  ("tab_qim"),
                  "`"
                )
            )),
            appointment_view = TRUE
          ),
        showStepNumbers = FALSE,
        skipLabel = "Quit"
      ),
      events = list(onbeforechange = I("rintrojs.callback.switchTabs(targetElement)"))
    )
  })

  ###### Render user information on top-right header ##########################
  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = dM$identified_user()$Fullname,
      src = "icons/doctor.svg", # this depends on addResourcePath in zzz.R
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
              collapse = ", "
            ),
            right_border = TRUE,
            margin_bottom = TRUE
          )
        ),
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            text = paste0(
              unlist(dM$identified_user()$Attributes),
              collapse = ", "
            ),
            right_border = FALSE,
            margin_bottom = TRUE
          )
        )
      ),
      shiny::fluidRow(
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "GPstat!",
            text = paste("v", packageVersion("DailyMeasure")),
            right_border = TRUE,
            margin_bottom = TRUE
          )
        ),
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "dMeasure",
            text = paste("v", packageVersion("dMeasure")),
            right_border = FALSE,
            margin_bottom = TRUE
          )
        )
      ),
      shiny::fluidRow(
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "Billings module",
            text = paste("v", ifelse(Billingsmodule,
              as.character(packageVersion("dMeasureBillings")),
              "None"
            )),
            right_border = TRUE,
            margin_bottom = TRUE
          )
        ),
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            header = "CDM module",
            text = paste("v", ifelse(CDMmodule,
              as.character(packageVersion("dMeasureCDM")),
              "None"
            )),
            right_border = FALSE,
            margin_bottom = TRUE
          )
        )
      )
    )
  })

  shiny::observeEvent(dM$check_subscription_datechange_trigR(),
    ignoreInit = TRUE, {
      # warning generated if dates have been changed as
      # the result of subscription check
      no_subscription <- paste(setdiff(
        dM$clinicians,
        dM$UserFullConfig %>>%
          dplyr::filter(Fullname %in% dM$clinicians &
            !is.na(LicenseDate) &
            LicenseDate >= Sys.Date()) %>>%
          dplyr::pull(Fullname)
      ), collapse = ", ")
      shinytoastr::toastr_warning(
        message = paste(
          "A chosen user has no subscription for chosen date range.",
          "If a chosen user has no subscription, the selected date range",
          "needs to be at least ", abs(dM$check_subscription_datechange_trigR()),
          "days old.",
          shiny::br(), shiny::br(),
          "Chosen users without subscription: ",
          no_subscription
        ),
        position = "bottom-left",
        closeButton = TRUE,
        timeOut = 0
      ) # keep open until closed
    }
  )
}
