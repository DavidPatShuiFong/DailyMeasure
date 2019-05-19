# DailyMeasure
# (C) David Fong, 2019


library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus) # version 0.6.0+ preferred (development version as of Feb/2019)
library(shinyWidgets)
library(shinytoastr) # notifications
library(shinyFiles) # file-picker. currently depends on development version 0.7.2

library(tidyverse)
library(dbplyr)  # database interaction for dplyr
library(DBI)     # R database interface
library(odbc)    # the database backend handler for Microsoft SQL Server
library(RSQLite) # the database backend handler for SQLite
library(pool)    # database pool

library(configr)    # config file read/write
library(base64enc)  # simple obfuscation for passwords
library(DT)         # pretty-print tables/tibbles
library(lubridate)  # time handling library

# source("./modules/dtedit.R")
library(DTedit)     # datatable edit wrapper.
# install with devtools::install_github('DavidPatShuiFong/DTedit')
# substantially based on jbryer/DTedit

# function setup

# fomantic (semantic.ui) string functions

semantic_tag <- function(tag, colour="", popuptext = NA, popuphtml = NA) {
  # returns a vector of tags. user-defined colour and popuptext (tooltip) or popuphtml (HTMl tooltip)
  # note that 'data-variation' is only available in the fomantic version of semantic.ui
  # as of writing, semantic.ui does not allow variation in text-size of javascript-free tags
  paste0('<span class="huge ', colour, ' ui tag label"',
         ifelse(!is.na(popuphtml),
                paste0('data-variation="wide" data-position = "left center" data-html="',
                       popuphtml,
                       '"', sep=""),
                ''),
         '> ',
         ifelse(!is.na(popuptext),
                paste0('<span data-tooltip = "',
                       popuptext,
                       '" data-variation = "wide huge" data-position = "left center">', sep=""),
                ''),
         tag,
         ifelse(!is.na(popuptext), '</span>', ''),
         ' </span>', sep = "")
  # paste0 is vectorized version of 'paste'
}

semantic_button <- function(button, colour="", popuptext = NA, popuphtml = NA) {
  # returns a vector of buttons.
  # user-defined colour and popuptext (tooltip) or popuphtml (HTML tooltip)
  # note that 'data-variation' is only available in the fomantic version of semantic.ui
  # as of writing, semantic.ui does not allow variation in text-size of javascript-free tags
  paste0('<span class="huge ', colour, ' ui button"',
         ifelse(!is.na(popuphtml),
                paste0('data-variation="wide" data-position = "left center" data-html="',
                       popuphtml,
                       '"', sep=""),
                ''),
         '> ',
         ifelse(!is.na(popuptext),
                paste0('<span data-tooltip = "',
                       popuptext,
                       '" data-variation = "wide huge" data-position = "left center">', sep=""),
                ''),
         button,
         ifelse(!is.na(popuptext), '</span>', ''),
         ' </span>', sep = "")
  # paste0 is vectorized version of 'paste'
}

## datatables functions and definitions

semantic_popupJS <- c("window.onload = function() {$('.ui.button') .popup({on: 'hover'});
                      $('.ui.tag.label') .popup({on: 'hover'})
                      }")

# (1) necessary for semantic/fomantic JS popups. included directly in datatables options
# (2) provide padding for export/print buttons
datatable_styled <- function(data, fillContainer = TRUE,
                             extensions = c('Buttons', 'Scroller', 'Responsive'),
                             dom = 'frltiBp',
                             buttons = c('copyHtml5', 'csvHtml5', 'excel', 'pdf', 'print'),
                             initComplete = JS(semantic_popupJS),
                             paging = FALSE,
                             scrollY = "60vh",
                             # 60% of window height, otherwise will just a few rows in size
                             ...) {
  options <- list(dom = dom, buttons = buttons, initComplete = initComplete,
                  paging = paging, scrollY = scrollY)
  datatable(data, fillContainer = fillContainer, extensions = extensions, options = options, ... )
}
# by default, have export/print buttons, only render what is visible
# javascript code to attach labels to semantic/fomantic JS popups
# no pagination

# 'helper' functions for calculation

calc_age <- function(birthDate, refDate = Sys.Date()) {
  # Calculate age at a given reference date
  # Create an interval between the date of birth and the enrollment date;
  # intervals are specific to the two dates. Periods give the actual length
  # of time between those dates, so convert to period and extract the year.
  # written by 'mmparker' https://gist.github.com/mmparker/7254445

  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  period$year
}

hrmin <- function(t) {
  # converts seconds to a 'time' starting from midnight
  # t : value in seconds
  # returns 24-hour time of form '14:15' (hh:mm)
  td <- seconds_to_period(t)
  sprintf('%02d:%02d', td@hour, td@minute)
}

##### UI modules
source("./modules/DailyMeasureUImodules.R")

##### Define UI for application ######################
ui <- dashboardPagePlus(

  header = dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "bars",
    title = "Daily Measure"
  ),

  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebartabs",
      menuItem("Zostavax", tabName = "zostavax"),
      menuItem("Bowel Cancer Screening", tabName = "fobt"),
      menuItem("Billings", tabName = "billings"),
      menuItem("CDM items", tabName = "cdm"),
      menuItem("Appointments", tabName = "appointments"),
      menuItem("Configuration", tabName = "configuration"),
      menuItem("Test", tabName = "test")
    )
  ),

  # Sidebar with a slider input for number of bins
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id = 1,
      title = "Appointment Details",
      icon = "desktop",
      active = TRUE,

      # appointment date range
      wellPanel(
        dateInput('date1', label = 'From:', format='D dd/M/yyyy',
                  min = Sys.Date()-4000, max = Sys.Date()+180),
        dateInput('date2', label = 'To:', format='D dd/M/yyyy',
                  min = Sys.Date()-4000, max = Sys.Date()+180),
        # range of dates, by default will be 'today'
        actionButton('update_date', 'Update', icon('refresh'), class = 'btn btn-primary'),
        # date range not activated until the 'Update' button is clicked
        helpText("After adjusting the date range, click the 'Update' button",
                 "to adjust the viewed appointment date range"),
        tags$div(title = "View today's appointments",
                 actionButton('update_date_today', 'Today', icon('calendar'), class = 'btn btn-info'))
        # manually change date range to 'today'
      ),

      # clinician list
      wellPanel(
        uiOutput('locationList'),
        # list of practice sites
        uiOutput('clinicianList'),
        # list of clinicians at the currently chosen practice site
        tags$div(title = "Select/De-select all clinicians",
                 # toggle all listed clinicians on, or off
                 actionButton('toggle_clinician_list', 'Select All/None', icon('check-square'), class = 'btn btn-primary'))
      )

    )
  ),

  dashboardBody(
    useSweetAlert(),
    useToastr(),
    tags$head(
      # stylesheets from fomantic.ui (a fork of semantic.ui)
      # Note that this is a specially edited version of semantic.css that is provided with fomantic
      # The 'popup' component does not work without some code
      # provided in the initial header of semantic.css
      # However, I have removed a lot of margin/padding/font re-definition
      # that is included in the header,
      # which disturbs the layout of shiny/flexdashboard
      tags$link(rel = "stylesheet", type = "text/css", href = "./fomantic_components.css"),
      # defining additional fomantic JS popup initialization in the header does not work.
      # Popup initialization does work inside DataTables
      # use of 'full' fomantic.js interferes with the popups from DTedit
      # minimum JS required for JS popups is popup.js and transition.js
      tags$script(src = "./popup.js"),
      tags$script(src = "./transition.js"),
      # Pushes the export/save buttons for datatables to the right
      # and provide padding on the top
      tags$style(HTML(".dataTables_wrapper .dt-buttons { float:none;
                      text-align:right;
                      padding-top:7px;
                      }"))
    ),

    tabItems(
      tabItem(tabName = "zostavax",
              fluidRow(column(width = 12, align = "center", h2("Zostavax"))),
              fluidRow(column(width = 12, DTOutput("zostavax_dt")))
      ),
      tabItem(tabName = "fobt",
              fluidRow(column(width = 12, align = "center", h2("Bowel cancer screening"))),
              fluidRow(column(width = 12, DTOutput("fobt_dt")))
      ),
      tabItem(tabName = "billings",
              fluidRow(column(width = 12, align = "center", h2("Billings"))),
              fluidRow(column(width = 12, DTOutput("billings_dt")))
      ),
      tabItem(tabName = "cdm",
              fluidRow(column(width = 12, align = "center", h2("Chronic Disease Management items"))),
              fluidRow(column(width = 12, DTOutput("cdm_dt")))
      ),
      tabItem(tabName = "appointments",
              fluidRow(column(width = 12, align = "center", h2("Appointments"))),
              fluidRow(column(width = 12, DTOutput("appointments_dt")))
      ),
      tabItem(tabName = "configuration",
              fluidRow(
                tabBox(
                  id = "tab_config",
                  title = "Configuration",
                  width = 12,
                  height = "85vh",
                  tabPanel(
                    # sqlite configuration file location
                    # this is stored in a YAML file
                    # allows a 'local' user to use a remote configuration file
                    title = "Configuration file",
                    column(width=12,
                           wellPanel(
                             textOutput('configuration_file_details') # location of sqlite configuration file
                           ),
                           wellPanel(
                             shinyFilesButton(
                               'choose_configuration_file',
                               label = 'Choose configuration file',
                               title = "Choose configuration file (must end in '.sqlite')",
                               multiple = FALSE),
                             # actionButton('choose_configuration_file',
                             # 'Choose configuration file', icon('refresh'),
                             #              class = 'btn btn-primary'),
                             actionButton('create_configuration_file', 'Create configuration file',
                                          class = 'btn btn-primary'),
                             helpText("Choose location of an existing configuration file,
                                      or create a new configuration file")
                           ))
                  ),
                  tabPanel(
                    # Microsoft SQL server details
                    title = "Microsoft SQL Server details",
                    column(width=12,
                           servers_datatableUI("servers_dt"))
                  ),
                  tabPanel(
                    # Practice locations or groups
                    title = "Practice locations/groups",
                    column(width=12,
                           locations_datatableUI("locations_dt"))
                  ),
                  tabPanel(
                    # User settings and permissions
                    title = "User settings and permissions",
                    column(width=12,
                           userconfig_datatableUI("userconfig_dt"))
                  )
                )
              )),
      tabItem(tabName = "test",
              fluidRow(column(width = 12, align = "center", h2("Test frame"))),
              fluidRow(column(width = 12, DTOutput("test_dt")))
      )
    )
  )
)

##### Configuration file ######################################################

if (is.yaml.file('./DailyMeasure_cfg.yaml')) {
  # if config file exists and is a YAML-type file
  local_config <- read.config("./DailyMeasure_cfg.yaml") #  config in local location
} else {
  # local config file does not exist. possibly first-run
  local_config <- list()
  local_config$config_file <- c("./DailyMeasure_cfg.sqlite")
  # main configuration file, could be set to 'common location'
  # write the (minimalist) local config file
  write.config(local_config, file.path = "./DailyMeasure_cfg.yaml", write.type = "yaml")
}

##### Define server logic #####################################################
server <- function(input, output, session) {

  # read config files

  # local_config <- reactiveValues(config_file = character())
  config_pool <- reactiveVal()
  configuration_file_path <- reactiveVal()
  BPdatabase <- reactiveVal(value = data.frame(id = integer(),
                                               Name = character(),
                                               Address = character(),
                                               Database = character(),
                                               UserID = character(),
                                               dbPassword = character(),
                                               stringsAsFactors = FALSE))
  BPdatabaseChoice <- reactiveVal(value = character())
  # database choice will be the same as the 'Name' of the chosen entry in BPdatabase
  PracticeLocations <- reactiveVal(value = data.frame(id = integer(),
                                                      Name = character(),
                                                      Description = character(),
                                                      stringsAsFactors = FALSE))
  # id needed for editing this dataframe later
  # need default value for practice location filter interface initialization
  UserConfig <- reactiveVal(value = data.frame(id = integer(),
                                               Fullname = character(), AuthIdentity = character(),
                                               Location = character(),
                                               Attributes = character(),
                                               stringsAsFactors = FALSE))

  configuration_file_path(local_config$config_file)

  observeEvent(configuration_file_path(), ignoreNULL = TRUE, {
    if (file.exists(isolate(configuration_file_path()))) {
      # open config database file
      config_pool(tryCatch(dbPool(RSQLite::SQLite(),
                                  dbname = isolate(configuration_file_path())),
                           error = function(e) {NULL}))
    } else {
      # if the config database doesn't exist, then create it (note create = TRUE option)
      config_pool(tryCatch(dbPool(RSQLite::SQLite(),
                                  dbname = isolate(configuration_file_path()),
                                  create = TRUE),
                           error = function(e) {NULL}))
    }

    initialize_data_table <- function(config_pool, tablename, variable_list) {
      # make sure the table in the database has all the right variable headings
      # allows 'update' of old databases
      #
      # input - config_pool : 'pool' reactive of database
      # input - tablename : name of table
      # input - variable_list : list of variable headings, with variable type
      #   e.g. list(c("id", "integer"), c("Name", "character"))
      #
      # alters table in database directly
      #
      # returns - nothing

      tablenames <- config_pool() %>% dbListTables()

      if (tablename %in% tablenames) {
        # if table exists in config_pool database
        columns <- config_pool() %>% tbl(tablename) %>% colnames()
        # list of column (variable) names
        data <- config_pool() %>% tbl("Server") %>% collect()
        # get a copy of the table's data
      } else {
        # table does not exist, needs to be created
        columns <- NULL
        data <- data.frame(NULL)
      }

      changed <- FALSE
      # haven't changed anything yet

      for (a in variable_list) {
        if (!(a[[1]] %in% columns)) {
          # if a required variable name is not in the table
          data <- data %>%
            mutate(!!a[[1]] := vector(a[[2]], nrow(data)))
          # use of !! and := to dynamically specify a[[1]] as a column name
          # potentially could use data[,a[[1]]] <- ...
          changed <- TRUE
        }
      }
      if (changed == TRUE) {
        dbWriteTable(config_pool(), tablename, data, overwrite = TRUE)
      }

    }

    # check that tables exist in the config file
    # also create new columns (variables) as necessary
    initialize_data_table(config_pool, "Server",
                          list(c("id", "integer"),
                               c("Name", "character"),
                               c("Address", "character"),
                               c("Database", "character"),
                               c("UserID", "character"),
                               c("dbPassword", "character")))
    # initialize_data_table will create table and/or ADD 'missing' columns to existing table

    initialize_data_table(config_pool, "ServerChoice",
                          list(c("id", "integer"),
                               c("Name", "character")))
    # there should only be (at most) one entry in this table!
    # with id '1', and a 'Name' the same as the chosen entry in table "Server"

    initialize_data_table(config_pool, "Location",
                          list(c("id", "integer"),
                               c("Name", "character"),
                               c("Description", "character")))

    initialize_data_table(config_pool, "Users",
                          list(c("id", "integer"),
                               c("Fullname", "character"),
                               c("AuthIdentity", "character"),
                               c("Location", "character"),
                               c("Attributes", "character")))
  })

  ### database initialization

  observeEvent(emrpool(), ignoreNULL = TRUE, {
    # if emrpool is initialized to a database,
    # then initialize tables
    if (is.environment(emrpool())) { # emrpool has been defined at least once
      if (dbIsValid(emrpool())) {  # and is still a valid database object (e.g. not disconnected)
        print("Re-initializing databases")
        initialize_tables(emrpool) # if pool is successfully initialized
      }
    }
  })

  observeEvent(BPdatabaseChoice(), {
  	print(paste("ChosenServerName:", BPdatabaseChoice()))

    # close existing database connection
    if (is.environment(emrpool())) {
      if (dbIsValid(emrpool())) {
        # if emrpool() is defined as a database, then close it
        poolClose(emrpool())
      }
    }
    if (BPdatabaseChoice() == "None") {
      # do nothing
    } else if (!is.null(BPdatabaseChoice())) {
      server <- BPdatabase() %>% filter(Name == BPdatabaseChoice()) %>% collect()
      print("Initializing EMR database")
      toastr_info("Opening link to Best Practice", title = "Best Practice database")
      emrpool(tryCatch(dbPool(odbc::odbc(), driver = "SQL Server",
                              server = server$Address, database = server$Database,
                              uid = server$UserID, pwd = server$dbPassword),
                       error = function(e) {
                         sendSweetAlert(
                           session = session,
                           title = "Error opening database",
                           text = e,
                           type = "error")
                       }
      ))

    }

    if (!is.environment(emrpool()) || !dbIsValid(emrpool())) {
      # || 'short-circuits' the evaluation, so if not an environment,
      # then dbIsValid() is not evaluated (will return an error if emrpool() is NULL)

      # either database not opened, or has just been closed
      db$users <- NULL
      db$patients <- NULL
      db$investigations <- NULL
      db$appointments <- NULL
      db$immunizations <- NULL
      db$preventive_health <- NULL
      db$correspondenceIn <- NULL
      db$reportValues <- NULL
      db$invoices <- NULL
      db$services <- NULL
      db$history <- NULL
      clinician_choice_list(NULL)
      BPdatabaseChoice("None") # set choice of database to 'None'
    } else {
      toastr_success("Linking to Best Practice database successful!",
                     title = "Best Practice database")
    }
  }, ignoreInit = TRUE)

  ### configuration database changes

  observeEvent(config_pool(), ignoreNULL = TRUE, {
    BPdatabase(isolate(config_pool()) %>% tbl("Server") %>% collect())
    BPdatabaseChoice((isolate(config_pool()) %>% tbl("ServerChoice") %>%
                        filter(id == 1) %>% select("Name") %>% collect())[[1]])
    PracticeLocations(isolate(config_pool()) %>% tbl("Location"))
    UserConfig(isolate(config_pool()) %>% tbl("Users") %>%
                 # in UserConfig, there can be multiple Locations/Attributes per user
                 collect() %>% mutate(Location = str_split(Location, ";"),
                                      Attributes = str_split(Attributes, ";")))
  })

  ### emr database variables
  emrpool <- reactiveVal()
  # the database pool of the electronic medical record
  # (Best Practice)
  db <- reactiveValues() # the database tables
  db$dbversion <- 0

  # change database tables whenever database is initialized
  # this is called by an observeEvent when emrpool() changes
  initialize_tables <- function(emrpool) {

    db$users <- emrpool() %>%
      # output - Fullname, UserID, Surname, Firstname, LocationName, Title, ProviderNo
      tbl(in_schema('dbo', 'BPS_Users')) %>%
      select(c('UserID', 'Surname', 'Firstname', 'LocationName', 'Title', 'ProviderNo')) %>%
      collect() %>%     # forces database to be read (instead of subsequent 'lazy' read)
      mutate(Title = trimws(Title), Firstname = trimws(Firstname), Surname = trimws(Surname)) %>%
      mutate(Fullname = paste(Title, Firstname, Surname, sep = ' ')) %>%
      left_join(UserConfig(), by = 'Fullname')   # add user details including practice locations

    db$patients <- emrpool() %>%
      tbl(in_schema('dbo', 'BPS_Patients'))

    db$investigations <- emrpool() %>%
      # output - InternalID, Collected (Date), TestName
      tbl(in_schema('dbo', 'BPS_Investigations')) %>%
      select(c('InternalID', 'Collected', 'TestName'))
    # as of Jan/2019, the odbc engine for MSSQL can't handle the full ('Select *') Investigations table
    # due to some type of bug/standards non-compliance. also can handle the History table. need to
    # 'Select' out just a few columns.

    db$appointments <- emrpool() %>%
      # Patient, InternalID, AppointmentDate, AppointmentTime, Provider, Status
      tbl(in_schema('dbo', 'BPS_Appointments')) %>%
      select(c('Patient', 'InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider', 'Status'))

    db$immunizations <- emrpool() %>%
      # InternalID, GivenDate, VaccineName, VaccineID
      tbl(in_schema('dbo', 'BPS_Immunisations')) %>%
      select(c('InternalID', 'GivenDate', 'VaccineName', 'VaccineID'))

    db$preventive_health <- emrpool() %>%
      # INTERNALID, ITEMID (e.g. not for Zostavax remindders)
      tbl(in_schema('dbo', 'PreventiveHealth')) %>%
      select(c('INTERNALID', 'ITEMID'))

    db$correspondenceIn <- emrpool() %>%
      # InternalID, CorrespondenceDate, Subject, Detail
      tbl(in_schema('dbo', 'BPS_CorrespondenceIn')) %>%
      select(c('InternalID', 'CorrespondenceDate', 'Subject', 'Detail'))

    db$reportValues <- emrpool() %>%
      # InternalID, ReportDate, ResultName, LoincCode
      tbl(in_schema('dbo', 'BPS_ReportValues')) %>%
      select(c('InternalID', 'ReportDate', 'ResultName', 'LoincCode'))

    db$invoices <- emrpool() %>%
      # InternalID, INVOICEID, INVOICEDATE
      tbl(in_schema('dbo', 'INVOICES')) %>%
      select(c('InternalID', 'INVOICEID', 'INVOICEDATE'))

    db$services <- emrpool() %>%
      tbl(in_schema('dbo', 'BPS_SERVICES'))

    db$history <- emrpool() %>%
      # InternalID, Year, Condition, ConditionID, Status
      tbl(in_schema('dbo', 'BPS_History')) %>%
      select(c('InternalID', 'Year', 'Condition', 'ConditionID', 'Status'))

    db$dbversion <- isolate(db$dbversion)+1
    print(paste("dbversion:", db$dbversion))
  }

  # 'helper' functions for input panel

  # only adjust appointment view after dates are 'submitted' using 'submit' button
  date_a <- eventReactive(input$update_date, {
    input$date1
  }, ignoreNULL = FALSE) # initialize on first run, after that only update if 'update' button used
  date_b <- eventReactive(input$update_date, {
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

  # list of locations available in appointment picker
  # returns all locations in configuration, and add 'All'
  location_list <- function() {
    locations <- data.frame(Name = c('All'))
    # add 'All' to (unique) locations list
    if (!is.null(isolate(PracticeLocations()))) {
      # if there are any practice locations defined, then add their names
      locations <- rbind(locations,
                         as.data.frame(isolate(PracticeLocations()) %>% select(c("Name"))))
      # no protection against non-unique PracticeLocation names!
    }
    return(locations$Name)
  }

  output$locationList <- renderUI({
    selectInput(inputId = 'location', label = 'Practice location',
                choices = location_list(), selected = 'All')
  })

  # list of clinicians shown depends on 'practice location' chosen
  clinician_choice_list <- reactiveVal()

  observeEvent(
    c(db$dbversion, input$location),
    {
      if (!is.null(input$location)) { # only if initialized
        clinician_choice_list(
          if (isolate(input$location) == 'All') {
            db$users$Fullname
          }
          else {
            subset(db$users, Location == input$location)$Fullname
            # initially, $Location might include a lot of NA,
            # so db$users[db$users$Location == input$location,] will also return NAs
          }
        )
        # note that 'ifelse' only returns result in the same 'shape' as the comparison statement
      }
    })

  output$clinicianList <- renderUI({
    choice_list <- clinician_choice_list()
    chosen_list <- input$clinicians # retain previous selections
    checkboxGroupInput('clinicians', label = 'Clinician',
                       choices = choice_list, selected = chosen_list)
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

  # Zostavax functions
  zostavax_vax_list <- reactive({
    appointments_list() %>%
      filter(Age >= 70 & Age <= 80) %>% # from age 70 to 80 years inclusive
      left_join(db$immunizations %>%
                  # those who have had the zostavax vaccine
                  filter((VaccineName %LIKE% "%zostavax%") | (VaccineID == 103)),
                copy = TRUE) %>%
      left_join(db$preventive_health %>%
                  # those who have been removed from the reminder system for Zostavax
                  filter(ITEMID == 15), by = c('InternalID' = 'INTERNALID'),
                copy = TRUE) %>%
      collect() %>%
      mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>%
      mutate(GivenDate = if_else(GivenDate <= AppointmentDate, GivenDate, as.Date(NA))) %>%
      # only include immunizations given up to date of appointment,
      # if there are any immunizations at all
      # note that 'if_else' is vectorize,
      # demanding same datatype for TRUE/FALSE alternatives
      # 'ifelse' does not preserve date type in this circumstance
      mutate(zostavaxtag =
               semantic_tag(paste0(' Zostavax '),
                            colour =
                              if_else(is.na(GivenDate),
                                      if_else(is.na(ITEMID), c('red'), c('purple')),
                                      c('green')),
                            # red if not given, purple if removed from herpes zoster vax reminders
                            # and green if has had the vax
                            popuphtml =
                              paste0("<h4>",
                                     if_else(is.na(ITEMID),
                                             paste0('Date : ', format(GivenDate)),
                                             'Removed from herpes zoster immunization reminders'),
                                     "</h4>")))
  })

  output$zostavax_dt <- renderDT({
    datatable_styled(zostavax_vax_list() %>%
                       select(c('Patient', 'AppointmentDate', 'AppointmentTime',
                                'Provider', 'DOB', 'Age', 'zostavaxtag')),
                     escape = c(7),
                     colnames = c('Zostavax' = 'zostavaxtag'))
  })

  # Bowel cancer screening
  bowel_cancer_screen_terms <-
    c("(VALUES('%FOB%'), ('%OCCULT%'), ('%FAECAL HUMAN HAEMOGLOBIN%'),
       ('%OCB NATIONAL SCREENING%'), ('%FHB%'), ('%FAECAL BLOOD%'),
       ('%FAECAL IMMUNOCHEMICAL TEST%'), ('%FAECAL HAEMOGLOBIN%'),
       ('%COLONOSCOPY%'), ('%COLONOSCOPE%')) AS tests(fobtnames)")

  fobt_investigation_query <-
    paste('SELECT InternalID, Collected, TestName FROM dbo.BPS_Investigations
           INNER JOIN', bowel_cancer_screen_terms,
          'ON TestName LIKE tests.fobtnames')
  # SQL code to find investigations which could be bowel cancer screening items

  fobt_letter_subject_query <-
    paste('SELECT InternalID, CorrespondenceDate, Subject FROM dbo.BPS_CorrespondenceIn
           INNER JOIN', bowel_cancer_screen_terms,
          'ON Subject LIKE tests.fobtnames')

  fobt_letter_detail_query <-
    paste('SELECT InternalID, CorrespondenceDate, Detail FROM dbo.BPS_CorrespondenceIn
           INNER JOIN', bowel_cancer_screen_terms,
          'ON Detail LIKE tests.fobtnames')

  fobt_result_query <-
    paste("SELECT InternalID, ReportDate, ResultName FROM dbo.BPS_ReportValues
            WHERE LoincCode IN ('2335-8','27396-1','14563-1','14564-9','14565-6',
                         	      '12503-9','12504-7','27401-9','27925-7','27926-5',
	                              '57905-2','56490-6','56491-4','29771-3')")

  screen_fobt_list <- reactive({
    appointments_list() %>%
      filter(Age >= 50 & Age <=75) # from age 50 to 75 years inclusive
  })

  screen_fobt_ix <- reactive({
    left_join(screen_fobt_list(),
              bind_rows(inner_join(screen_fobt_list(),
                                   dbGetQuery(emrpool(), fobt_investigation_query) %>%
                                     collect() %>%
                                     rename(TestDate = Collected),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list(),
                                   dbGetQuery(emrpool(), fobt_letter_subject_query) %>%
                                     collect() %>%
                                     rename(TestDate = CorrespondenceDate, TestName = Subject),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list(),
                                   dbGetQuery(emrpool(), fobt_letter_detail_query) %>%
                                     collect() %>%
                                     rename(TestDate = CorrespondenceDate, TestName = Detail),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list(),
                                   dbGetQuery(emrpool(), fobt_result_query) %>% collect() %>%
                                     rename(TestDate = ReportDate, TestName = ResultName),
                                   by = 'InternalID')
              ) %>%
                mutate(TestDate = as.Date(substr(TestDate, 1, 10))) %>%
                # remove time from date
                group_by(InternalID) %>%
                # group by patient ID (need most recent investigation for each patient)
                filter(TestDate == max(TestDate, na.rm = TRUE))
              # only keep the latest(/recent) dated investigation
    )
  })

  output$fobt_dt <- renderDT({
    datatable_styled(screen_fobt_ix() %>%
                       mutate(OutOfDateTest =
                                case_when(is.na(TestDate) ~ 1,
                                          # if no date (no detected test)
                                          interval(TestDate, AppointmentDate)>years(2) ~ 2,
                                          # if old
                                          TRUE ~ 3)) %>%   # if up-to-date
                       replace_na(list(TestName = 'FOBT')) %>%
                       mutate(fobttag =
                                semantic_tag(
                                  TestName,
                                  colour = c('red', 'yellow', 'green')[OutOfDateTest],
                                  popuphtml = paste0("<h4>Date : ", TestDate, "</h4>"))) %>%
                       select(c('Patient', 'AppointmentDate', 'AppointmentTime',
                                'Provider', 'DOB', 'Age', 'fobttag')),
                     escape = c(7),
                     colnames = c('FOBT' = 'fobttag')
    )
  }, server = FALSE)

  # collects ALL billings for patients who have displayed appointments
  appointments_billings <- reactive({
    appointments_list() %>%
      left_join(db$services, by = c('InternalID' = 'INTERNALID'), copy=TRUE) %>%
      collect() %>%
      mutate(SERVICEDATE = as.Date(substr(SERVICEDATE, 1, 10)))
  })

  # filter to billings which are done on the same day as displayed appointments
  appointments_billings_sameday <- reactive({
    appointments_billings() %>%
      filter(SERVICEDATE == AppointmentDate) %>%
      # billings done on the same day as displayed appointments
      select(c('InternalID', 'AppointmentDate', 'AppointmentTime',
               'Provider', 'MBSITEM', 'DESCRIPTION')) %>%
      # need to preserve ApppointmentTime and Provider
      # in the case where there are multiple apppointments
      # for the patient in the same time period/day and providers
      mutate(MBSITEM =
               semantic_button(MBSITEM,
                               colour = 'green',
                               popuphtml = paste0('<h4>', AppointmentDate,
                                                  "</h3><p><font size=\'+0\'>",
                                                  DESCRIPTION, '</p>'))) %>%
      # change MBSITEMS into fomantic/semantic tags
      group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>%
      # gathers item numbers on the same day into a single row
      summarise(Billings = paste(MBSITEM, collapse = "")) %>%
      ungroup()
  })

  output$billings_dt <- renderDT({
    datatable_styled(appointments_filtered_time() %>%
                       left_join(appointments_billings_sameday(),
                                 by = c('InternalID', 'AppointmentDate',
                                        'AppointmentTime', 'Provider')) %>%
                       select(c('Patient', 'AppointmentDate', 'AppointmentTime',
                                'Provider', 'Status', 'Billings')),
                     escape = c(6) # only interpret HTML for last column
    )
  }, server = TRUE)

  # CDM items

  cdm_item <- data.frame(
    code = c(721, 723, 732, 703, 705, 707, 2517, 2521, 2525, 2546, 2552, 2558, 2700, 2701, 2715, 2717),
    name = c('GPMP', 'TCA', 'GPMP R/V', 'HA', 'HA', 'HA', 'DiabetesSIP', 'DiabetesSIP', 'DiabetesSIP',
             'AsthmaSIP', 'AsthmaSIP', 'AsthmaSIP', 'MHCP', 'MHCP', 'MHCP', 'MHCP')
  )

  # filter to CDM item billed prior to (or on) the day of displayed appointments
  # only show most recent billed item in each category

  appointments_billings_cdm <- reactive({
    appointments_billings() %>%
      filter(MBSITEM %in% cdm_item$code) %>% # only chronic disease management items
      filter(SERVICEDATE <= AppointmentDate) %>% # only items billed before the appointment day
      select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider',
               'SERVICEDATE', 'MBSITEM', 'DESCRIPTION')) %>%
      mutate(MBSNAME = cdm_item$name[match(MBSITEM, cdm_item$code)]) %>%
      rbind(diabetes_list_cdm()) %>%
      group_by(InternalID, AppointmentDate, AppointmentTime, Provider, MBSNAME) %>%
      # group by patient, apppointment and CDM type (name)
      filter(SERVICEDATE == max(SERVICEDATE, na.rm = TRUE)) %>% # only keep most recent service
      ungroup() %>%
      mutate(mbstag =
               semantic_tag(MBSNAME,
                            colour =
                              if_else(SERVICEDATE == -Inf,
                                      'red', # invalid date is '-Inf', means item not claimed yet
                                      if_else(interval(SERVICEDATE, AppointmentDate)<=years(1),
                                              'green',
                                              'yellow')),
                            popuphtml =
                              paste0("<h4>Date : ", SERVICEDATE,
                                     "</h4><h6>Item : ", MBSITEM,
                                     "</h6><p><font size=\'+0\'>", DESCRIPTION, "</p>")
               )) %>%
      group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>%
      # gathers item numbers on the same day into a single row
      summarise(cdm = paste(mbstag, collapse = "")) %>%
      ungroup()
  })

  output$cdm_dt <- renderDT({
    datatable_styled(appointments_filtered_time() %>%
                       inner_join(appointments_billings_cdm(),
                                  by = c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
                       select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'cdm')),
                     colnames = c('Patient', 'Appointment Date', 'Appointment Time', 'Provider', 'CDM items'),
                     escape = c(5)) # only interpret HTML for last column
  },
  server = TRUE)

  # Diabetes

  # Best Practice Diabetes code
  diabetes_codes <- c(3, 775, 776, 778, 774, 7840, 11998)

  # Returns InternalID
  diabetes_list <- reactive({
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% diabetes_codes),
                 by = c('InternalID')) %>%
      select('InternalID')
  })

  diabetes_list_cdm <- reactive({
    a <- appointments_list() %>%
      inner_join(diabetes_list(), by = 'InternalID', copy = TRUE) %>%
      select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
      mutate(MBSNAME = c('DiabetesSIP'), DESCRIPTION = c('History : Diabetes'),
             SERVICEDATE = as.Date(-Inf, origin = '1970-01-01'), MBSITEM = NA) %>%
      unique()
    # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
    # setting invalid date to NA is not good for later comparisons,
    # where max(... , na.rm=TRUE) needs to be used

    b <- a %>% mutate(MBSNAME = c('GPMP'))
    # people with diabetes also qualify for GPMP. duplicate list with 'GPMP' MBSNAME
    rbind(a, b)
  })

  # Appointment list

  appointments_filtered <- reactive({
    # find appointments with chosen date range and providers
    validate (
      need(input$clinicians, 'Choose at least one clinician appointment to view'),
      need(date_a(), 'Invalid date range'),
      need(date_b(), 'Invalid date range')
    )
    db$appointments %>%
      filter(AppointmentDate >= local(date_a()) & AppointmentDate <= local(date_b())) %>%
      filter(Provider %in% input$clinicians)
    # note that dbplyr does not evaluate manipulated expressions, hence the use of 'local()'
    # a database filter on an empty list after %in% will result in an error message
  })

  appointments_filtered_time <- reactive({
    # changes times to more R (and visually) friendly formats
    appointments_filtered() %>%
      collect() %>% # force read of database required before mutations
      mutate(AppointmentTime = hrmin(AppointmentTime),
             AppointmentDate = as.Date(substr(AppointmentDate,1,10))) %>%
      arrange(AppointmentDate, AppointmentTime)
  })

  appointments_list <- reactive({
    # add date of birth to appointment list
    appointments_filtered_time() %>%
      left_join(db$patients, by = 'InternalID', copy = TRUE) %>%
      # need patients database to access date-of-birth
      select(c('Patient', 'InternalID', 'AppointmentDate',
               'AppointmentTime', 'Provider', 'DOB')) %>%
      mutate(DOB = as.Date(substr(DOB, 1, 10))) %>%
      mutate(Age = calc_age(DOB, AppointmentDate))
  })

  output$appointments_dt <- renderDT({datatable_styled(
    appointments_filtered_time() %>%
      select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'Status')))
  },
  server = FALSE)

  output$test_dt <-
    renderDT({datatable(data.frame(a=c(2,3,68),
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
                        options = list(initComplete = JS(semantic_popupJS)),
                        escape = FALSE,
                        fillContainer = FALSE)})


  # configuration file location tab

  output$configuration_file_details <- renderText({
    paste('Configuration file location: "', configuration_file_path(), '"')
  })

  volumes <- c(getVolumes()(), base = '.', home = Sys.getenv("USERPROFILE"))

  shinyFileChoose(input, id = 'choose_configuration_file',
                  session = session,
                  roots = volumes,
                  filetypes = c('sqlite') # only files ending in '.sqlite'
  )

  observeEvent(ignoreNULL = TRUE,input$choose_configuration_file, {
    if (!is.integer(input$choose_configuration_file)) {
      # if input$choose_configuration_file is an integer,
      # it is just the 'click' event on the filechoose button
      inFile <- parseFilePaths(volumes, input$choose_configuration_file)
      file_name <- paste(inFile$datapath)
      configuration_file_path(file_name)
    }
  })

  observe({
    # change local_config when configuration path changes
    local_config$config_file <<- configuration_file_path() # need to use <<- operator
  })

  # database configuration tab

  serverconfig_change <- callModule(servers_datatable, "servers_dt",
                                    BPdatabase, BPdatabaseChoice, emrpool, config_pool)
  # returns $count

  server_list_names <- reactiveVal(isolate(BPdatabase()) %>%
                                     select(Name) %>% collect() %>% unlist(use.names = FALSE))
  # just the names of the servers (no details)

  observeEvent(c(serverconfig_change$count(),BPdatabase()), {
    # change in server list (by GUI editor in server module) prompts
    server_list_names(BPdatabase() %>% select(Name) %>% collect() %>% unlist(use.names = FALSE))
  })

  # location configuration tab
  location_list_change <- callModule(locations_datatable, "locations_dt",
                                     PracticeLocations, UserConfig, config_pool)

  location_list_names <- reactiveVal(isolate(PracticeLocations()) %>%
                                       select(Name) %>% collect() %>% unlist(use.names = FALSE))
  # just the names of the practice locations (not the ID or description)

  observeEvent(c(location_list_change(), PracticeLocations()), {
    # change in location_list (by GUI editor in locations_data module) prompts
    # change in location list filter (for providers) and location_list_names (for user config)
    updateSelectInput(session, inputId = 'location', choices = location_list())
    location_list_names(PracticeLocations() %>% select(Name) %>% collect() %>% unlist(use.names = FALSE))
  })

  userconfig_change <- callModule(userconfig_datatable, "userconfig_dt",
                                  UserConfig, location_list_names, db$users, config_pool)

}

##### Run the application ###########################################
shinyApp(ui = ui, server = server)

