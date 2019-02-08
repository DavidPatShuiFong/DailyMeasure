# DailyMeasure
# (C) David Fong

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

library(tidyverse)
library(dbplyr) # database interaction for dplyr
library(DBI)    # R database interface
library(odbc)   # the database backend handler for Microsoft SQL Server
library(pool)   # database pool

library(configr)    # config file read/write
library(base64enc)  # simple obfuscation for passwords
library(DT)         # pretty-print tables/tibbles
library(lubridate)  # time handling library
library(DTedit)     # datatable edit wrapper. install with devtools::install_github('jbryer/DTedit')


# read config files

if (is.yaml.file('./DailyDash_cfg.yaml')) {
  # if config file exists and is a YAML-type file
  config <- read.config('./DailyDash_cfg.yaml')
} else {
  config <- list()
  
  config$server <- c('127.0.0.1\\BPSINSTANCE') # Note that 'true' server name includes a single backslash
  # in this string, the backslash is 'escaped' into a double backslash
  config$database <- c('BPSSamples')
  filename <- 'BestPracticeSQL_password.txt'
  config$dbpassword <- rawToChar(base64decode(readChar(filename,file.info(filename)$size)))
  
  # config$server <-  character()       # e.g. '127.0.0.1\\BPSINSTANCE'
  # config$database <-  character()     # e.g. 'BPSSamples' for samples database
  config$userid <-  c('bpsrawdata')   # database user ID
  # config$dbpassword <-  character()   # password for the database user
  config$practice_locations <-  data.frame(Name = character(), Description = character(),
                                           stringsAsFactors = FALSE) 
  # list of practice locations, or practitioner groups
  config$users <- data.frame(Fullname = character(),
                             AuthIdentity = character(),  # authentication identity
                             Location = character(),
                             Expert = logical(),         # expert options available
                             Globalview = logical(),     # able to view all users
                             Admin = logical(),          # able to change major system settings
                             stringsAsFactors = FALSE)  
}

# initial database setup

pool <- tryCatch(dbPool(odbc::odbc(), driver = "SQL Server",
                        server = config$server, database = config$database,
                        uid = config$userid, pwd = config$dbpassword),
                 error = function(e) {NULL}
)

# database table setup
# initially set to NULL

users <- NULL
locations <- NULL
patients <- NULL
investigations <- NULL
appointments <- NULL
immunizations <- NULL
preventive_health <- NULL
correspondenceIn <- NULL
reportValues <- NULL
invoices <- NULL
services <- NULL
history <- NULL
locations <- NULL


# function setup

# list of locations available in appointment picker
# returns all locations in configuration, and add 'All'
location_list <- function() {
  locations <- data.frame(Name = c('All'))
  # add 'All' to (unique) locations list
  if (!is.null(config$practice_locations)) {
    locations <- rbind(locations, config$practice_locations %>% select(c('Name')) %>% unique())
  }
  return(locations$Name)
}


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
  # returns a vector of buttons. user-defined colour and popuptext (tooltip) or popuphtml (HTML tooltip)
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
                             extensions = c('Buttons', 'Scroller'),
                             options=list(dom = 'frltiBp',
                                          buttons = c('copyHtml5', 'csvHtml5', 'excel', 'pdf', 'print'),
                                          initComplete = JS(semantic_popupJS),
                                          paging = FALSE),
                             ...) {
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


# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
  
  header = dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "bars",
    title = "Daily Measure"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Zostavax", tabName = "zostavax"),
      menuItem("Bowel Cancer Screening", tabName = "fobt"),
      menuItem("Billings", tabName = "billings"),
      menuItem("CDM items", tabName = "cdm"),
      menuItem("Appointments", tabName = "appointments"),
      menuItem("Test", tabName = "test")
    )
  ),
  
  dashboardBody(
    tags$head(
      # stylesheets from fomantic.ui (a fork of semantic.ui)
      # Note that this is a specially edited version of semantic.css that is provided with fomantic
      # The 'popup' component does not work without some code provided in the initial header of semantic.css
      # However, I have removed a lot of margin/padding/font re-definition that is included in the header,
      # which disturbs the layout of shiny/flexdashboard
      tags$link(rel = "stylesheet", type = "text/css", href = "./fomantic_components.css"),
      # defining additional fomantic JS popup initialization in the header does not work.
      # Popup initialization does work inside DataTables
      tags$script(src = "./fomantic.js"),
      # Pushes the export/save buttons for datatables to the right
      # and provide padding on the top
      tags$style(HTML(".dataTables_wrapper .dt-buttons { float:none;  
                      text-align:right;
                      padding-top:7px;
                      }"))
    ),
    
    tabItems(
      tabItem(tabName = "zostavax",
              h2("Zostavax"),
              fluidRow(DTOutput("zostavax_dt"))
              ),
      tabItem(tabName = "fobt",
              h2("Bowel cancer screening"),
              DTOutput("fobt_dt")
      ),
      tabItem(tabName = "billings",
              h2("Billings"),
              DTOutput("billings_dt")
      ),
      tabItem(tabName = "cdm",
              h2("Chronic Disease Management items"),
              DTOutput("cdm_dt")
      ),
      tabItem(tabName = "appointments",
              h2("Appointments"),
              DTOutput("appointments_dt")
      ),
      tabItem(tabName = "test",
              DTOutput("test_dt")
      )
    )
  ),
  
  # Application title

  # Sidebar with a slider input for number of bins 
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id=1,
      title = "Appointment Details",
      icon = "calendar-alt",
      active = TRUE,
      
      # appointment date range
      wellPanel(
        useShinyjs(rmd = TRUE),
        
        dateInput('date1', label = 'From:', format='D dd/M/yyyy', min = Sys.Date()-4000, max = Sys.Date()+180),
        dateInput('date2', label = 'To:', format='D dd/M/yyyy', min = Sys.Date()-4000, max = Sys.Date()+180),
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
        selectInput(inputId = 'location', label = 'Practice location', choices = location_list(), selected = 'All'),
        # list of practice sites
        uiOutput('clinicianList'),
        # list of clinicians at the currently chosen practice site
        tags$div(title = "Select/De-select all clinicians",
                 # toggle all listed clinicians on, or off
                 actionButton('toggle_clinician_list', 'Select All/None', icon('check-square'), class = 'btn btn-primary'))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  db <- reactiveValues()
  db$version <- 0
  
  # change this whenever database is initialized
  initialize_databases <- function(pool) {
    
    db$users <- pool %>% 
      # Fullname, UserID, Surname, Firstname, LocationName, Title, ProviderNo
      tbl(in_schema('dbo', 'BPS_Users')) %>% 
      select(c('UserID', 'Surname', 'Firstname', 'LocationName', 'Title', 'ProviderNo')) %>%
      collect() %>%     # forces database to be read (instead of subsequent 'lazy' read)
      mutate(Title = trimws(Title), Firstname = trimws(Firstname), Surname = trimws(Surname)) %>%
      mutate(Fullname = paste(Title, Firstname, Surname, sep = ' ')) %>%
      left_join(config$users, by = 'Fullname')   # add user details including practice locations
    
    db$patients <- pool %>%
      tbl(in_schema('dbo', 'BPS_Patients'))
    
    db$investigations <- pool %>%
      # InternalID, Collected (Date), TestName
      tbl(in_schema('dbo', 'BPS_Investigations')) %>%
      select(c('InternalID', 'Collected', 'TestName'))
    # as of Jan/2019, the odbc engine for MSSQL can't handle the full ('Select *') Investigations table
    # due to some type of bug/standards non-compliance. also can handle the History table. need to
    # 'Select' out just a few columns.
    
    db$appointments <- pool %>%
      # Patient, InternalID, AppointmentDate, AppointmentTime, Provider, Status
      tbl(in_schema('dbo', 'BPS_Appointments')) %>%
      select(c('Patient', 'InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider', 'Status'))
    
    db$immunizations <- pool %>%
      # InternalID, GivenDate, VaccineName, VaccineID
      tbl(in_schema('dbo', 'BPS_Immunisations')) %>%
      select(c('InternalID', 'GivenDate', 'VaccineName', 'VaccineID'))
    
    db$preventive_health <- pool %>%
      # INTERNALID, ITEMID (e.g. not for Zostavax remindders)
      tbl(in_schema('dbo', 'PreventiveHealth')) %>%
      select(c('INTERNALID', 'ITEMID'))
    
    db$correspondenceIn <- pool %>%
      # InternalID, CorrespondenceDate, Subject, Detail
      tbl(in_schema('dbo', 'BPS_CorrespondenceIn')) %>%
      select(c('InternalID', 'CorrespondenceDate', 'Subject', 'Detail'))
    
    db$reportValues <- pool %>%
      # InternalID, ReportDate, ResultName, LoincCode
      tbl(in_schema('dbo', 'BPS_ReportValues')) %>%
      select(c('InternalID', 'ReportDate', 'ResultName', 'LoincCode'))
    
    db$invoices <- pool %>%
      # InternalID, INVOICEID, INVOICEDATE
      tbl(in_schema('dbo', 'INVOICES')) %>%
      select(c('InternalID', 'INVOICEID', 'INVOICEDATE'))
    
    db$services <- pool %>%
      tbl(in_schema('dbo', 'BPS_SERVICES'))
    
    db$history <- pool %>%
      # InternalID, Year, Condition, ConditionID, Status
      tbl(in_schema('dbo', 'BPS_History')) %>%
      select(c('InternalID', 'Year', 'Condition', 'ConditionID', 'Status'))
    
    db$dbversion <- isolate(db$dbversion+1)
    
  }
  
  ### database initialization
  
  if (is.environment(pool)) {
    initialize_databases(pool) # if pool is successfully initialized
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
  
  # list of clinicians shown depends on 'practice location' chosen
  clinician_choice_list <- reactiveVal()
  
  observeEvent(c(
    db$dbversion,
    input$location),
    {
      if (!is.null(input$location)) { # only if initialized
        clinician_choice_list(
          if (isolate(input$location) == 'All')
          {isolate(db$users$Fullname)} else
          {db$users[db$users$Location == input$location,]$Fullname})
        # note that 'ifelse' only returns result in the same 'shape' as the comparison statement
      }
    })
  
  output$clinicianList <- renderUI({
    choice_list <- clinician_choice_list()
    chosen_list <- input$clinicians # retain previous selections
    checkboxGroupInput('clinicians', label = 'Clinician', choices = choice_list, selected = chosen_list)
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
                  filter((VaccineName %LIKE% "%zostavax%") | (VaccineID == 103)), copy = TRUE) %>%
      left_join(db$preventive_health %>%
                  # those who have been removed from the reminder system for Zostavax
                  filter(ITEMID == 15), by = c('InternalID' = 'INTERNALID'), copy = TRUE) %>%
      collect() %>%
      mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>%
      mutate(GivenDate = if_else(GivenDate <= AppointmentDate, GivenDate, as.Date(NA))) %>%
      # only include immunizations given up to date of appointment, if there are any immunizations at all
      # note that 'if_else' is vectorize, demanding same datataype for TRUE/FALSE alternatives
      # 'ifelse' does not preserve date type in this circumstance
      mutate(zostavaxtag = semantic_tag(paste0(' Zostavax '),
                                        colour = if_else(is.na(GivenDate),
                                                         if_else(is.na(ITEMID), c('red'), c('purple')),
                                                         c('green')),
                                        # red if not given, purple if removed from herpes zoster vax reminders
                                        # and green if has had the vax
                                        popuphtml = paste0("<h4>",
                                                           if_else(is.na(ITEMID),
                                                                   paste0('Date : ', format(GivenDate)),
                                                                   'Removed from herpes zoster immunization reminders'),
                                                           "</h4>")))
  })
  
  output$zostavax_dt <- renderDT({datatable_styled(zostavax_vax_list() %>%
                                                     select(c('Patient', 'AppointmentDate', 'AppointmentTime',
                                                              'Provider', 'DOB', 'Age', 'zostavaxtag')),
                                                   escape = c(7),
                                                   colnames = c('Zostavax' = 'zostavaxtag'))
  })
  
  # Bowel cancer screening
  bowel_cancer_screen_terms <- c("(VALUES('%FOB%'), ('%OCCULT%'), ('%FAECAL HUMAN HAEMOGLOBIN%'),
                               ('%OCB NATIONAL SCREENING%'), ('%FHB%'), ('%FAECAL BLOOD%'),
                                 ('%FAECAL IMMUNOCHEMICAL TEST%'), ('%FAECAL HAEMOGLOBIN%'),
                                 ('%COLONOSCOPY%'), ('%COLONOSCOPE%')) AS tests(fobtnames)")
  
  fobt_investigation_query <- paste('SELECT InternalID, Collected, TestName FROM dbo.BPS_Investigations
                                    INNER JOIN', bowel_cancer_screen_terms,
                                    'ON TestName LIKE tests.fobtnames')
  # SQL code to find investigations which could be bowel cancer screening items
  
  fobt_letter_subject_query <- paste('SELECT InternalID, CorrespondenceDate, Subject FROM dbo.BPS_CorrespondenceIn
                                     INNER JOIN', bowel_cancer_screen_terms,
                                     'ON Subject LIKE tests.fobtnames')
  
  fobt_letter_detail_query <- paste('SELECT InternalID, CorrespondenceDate, Detail FROM dbo.BPS_CorrespondenceIn
                                    INNER JOIN', bowel_cancer_screen_terms,
                                    'ON Detail LIKE tests.fobtnames')
  
  fobt_result_query <- paste("SELECT InternalID, ReportDate, ResultName FROM dbo.BPS_ReportValues 
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
                                   dbGetQuery(pool, fobt_investigation_query) %>% collect() %>%
                                     rename(TestDate = Collected),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list(),
                                   dbGetQuery(pool, fobt_letter_subject_query) %>% collect() %>%
                                     rename(TestDate = CorrespondenceDate, TestName = Subject),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list(),
                                   dbGetQuery(pool, fobt_letter_detail_query) %>% collect() %>%
                                     rename(TestDate = CorrespondenceDate, TestName = Detail),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list(),
                                   dbGetQuery(pool, fobt_result_query) %>% collect() %>%
                                     rename(TestDate = ReportDate, TestName = ResultName),
                                   by = 'InternalID')
              ) %>%
                mutate(TestDate = as.Date(substr(TestDate, 1, 10))) %>%  # remove time from date
                group_by(InternalID) %>% # group by patient ID (need most recent investigation for each patient)
                filter(TestDate == max(TestDate, na.rm = TRUE))  # only keep the latest(/recent) dated investigation
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
                                semantic_tag(TestName,
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
      filter(SERVICEDATE == AppointmentDate) %>%    # billings done on the same day as displayed appointments
      select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider', 'MBSITEM', 'DESCRIPTION')) %>%
      # need to preserve ApppointmentTime and Provider in the case where there are multiple apppointments
      # for the patient in the same time period/day and providers
      mutate(MBSITEM = semantic_button(MBSITEM,
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
                                 by = c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
                       select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'Status', 'Billings')),
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
      mutate(mbstag = semantic_tag(MBSNAME,
                                   colour = if_else(SERVICEDATE == -Inf,
                                                    'red', # invalid date is '-Inf', means item not claimed yet
                                                    if_else(interval(SERVICEDATE, AppointmentDate)<=years(1),
                                                            'green',
                                                            'yellow')),
                                   popuphtml = paste0("<h4>Date : ", SERVICEDATE,
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
      mutate(AppointmentTime = hrmin(AppointmentTime), AppointmentDate = as.Date(substr(AppointmentDate,1,10))) %>%
      arrange(AppointmentDate, AppointmentTime)
  })
  
  appointments_list <- reactive({
    # add date of birth to appointment list
    appointments_filtered_time() %>%
      left_join(db$patients, by = 'InternalID', copy = TRUE) %>%   # need patients database to access date-of-birth
      select(c('Patient', 'InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider', 'DOB')) %>% 
      mutate(DOB = as.Date(substr(DOB, 1, 10))) %>%
      mutate(Age = calc_age(DOB, AppointmentDate))
  })
  
  output$appointments_dt <- renderDT({datatable_styled(
    appointments_filtered_time() %>%
      select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'Status')))
  },
  server = FALSE)
  
  output$test_dt <- renderDT({datatable(data.frame(a=c(2,3,68),
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

