#' Shiny app server function

##### Define server logic #####################################################
DailyMeasureServer <- function(input, output, session) {

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
      toastr_info("Opening link to Best Practice", closeButton = TRUE,
                  position = "top-center", title = "Best Practice database")
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
                     closeButton = TRUE,
                     position = "top-center",
                     title = "Best Practice database")
    }
  }, ignoreInit = TRUE)

  ### configuration database changes

  observeEvent(config_pool(), ignoreNULL = TRUE, {
    BPdatabase(config_pool() %>% tbl("Server") %>% collect())
    BPdatabaseChoice((config_pool() %>% tbl("ServerChoice") %>%
                        filter(id == 1) %>% select("Name") %>% collect())[[1]])
    PracticeLocations(config_pool() %>% tbl("Location"))
    UserConfig(config_pool() %>% tbl("Users") %>%
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

    db$vaccine_disease <- emrpool() %>%
      # vaccineIDs linked to diseases
      # e.g. diseasecode 7+30 are for influenza vaccines
      tbl(in_schema("bpsdrugs.dbo", "VACCINE_DISEASE")) %>%
      select("VACCINEID", "DISEASECODE")

    db$preventive_health <- emrpool() %>%
      # INTERNALID, ITEMID (e.g. not for Zostavax remindders)
      tbl(in_schema('dbo', 'PreventiveHealth')) %>%
      select(InternalID = INTERNALID, ITEMID)

    db$correspondenceIn <- emrpool() %>%
      # InternalID, CorrespondenceDate, Subject, Detail
      tbl(in_schema('dbo', 'BPS_CorrespondenceIn')) %>%
      select(InternalID, CorrespondenceDate, Subject, Detail)

    db$reportValues <- emrpool() %>%
      # InternalID, ReportDate, ResultName, LoincCode
      tbl(in_schema('dbo', 'BPS_ReportValues')) %>%
      select(InternalID, ReportDate, ResultName, LoincCode)

    db$invoices <- emrpool() %>%
      # InternalID, INVOICEID, INVOICEDATE
      tbl(in_schema('dbo', 'INVOICES')) %>%
      select(InternalID, INVOICEID, INVOICEDATE)

    db$services <- emrpool() %>%
      tbl(in_schema('dbo', 'BPS_SERVICES')) %>%
      select(InternalID = INTERNALID, ServiceDate = SERVICEDATE,
             MBSItem = MBSITEM, Description = DESCRIPTION)

    db$history <- emrpool() %>%
      # InternalID, Year, Condition, ConditionID, Status
      tbl(in_schema('dbo', 'BPS_History')) %>%
      select(InternalID, Year, Condition, ConditionID, Status)

    db$observations <- emrpool() %>%
      tbl(in_schema("dbo", "OBSERVATIONS")) %>%
      select(InternalID = INTERNALID, DATANAME, DATACODE, DATAVALUE, OBSDATE)

    db$currentrx <- emrpool() %>%
      tbl(in_schema("dbo", "CURRENTRX")) %>%
      select(InternalID = INTERNALID, PRODUCTID, DRUGNAME, RXSTATUS)
    # RXSTATUS appears to be 1 if 'long-term' and 2 if 'short-term'

    db$obgyndetail <- emrpool() %>%
      tbl(in_schema("dbo", "OBSGYNDETAIL")) %>%
      select(InternalID = INTERNALID, NOMINALLMP, LASTPAPDATE, LASTPAPRESULT, BREASTFEEDING,
             MammogramStatus, LastMammogramDate, MammogramResult)

    db$pregnancies <- emrpool() %>%
      tbl(in_schema("dbo", "PREGNANCIES")) %>%
      select(InternalID = INTERNALID, EDCBYDATE, ACTUALLMP, NOMINALLMP, ENDDATE)

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

  # Immunization functions

  vax_table_results <- callModule(vax_datatable, "vax_dt",
                                  appointments_list, db,
                                  diabetes_list, asthma_list,
                                  atsi_list,
                                  malignancy_list, hiv_list,
                                  haemoglobinopathy_list, asplenic_list,
                                  transplant_list, cardiacdisease_list,
                                  trisomy21_list, bmi30_list, chroniclungdisease_list, neurologic_list,
                                  chronicliverdisease_list, chronicrenaldisease_list, pregnant_list)

  # Bowel cancer screening

  callModule(cancerscreen_datatable, "cancerscreen_dt",
             appointments_list, emrpool)

  # Billings for patients who have displayed appointments

  # collects ALL billings for patients who have displayed appointments
  # used by billings view, and CDM billings view
  appointments_billings <- reactive({
    appointments_list() %>%
      left_join(db$services, by = "InternalID", copy=TRUE) %>%
      collect() %>%
      mutate(ServiceDate = as.Date(substr(ServiceDate, 1, 10)))
  })

  # call the module to generate the table
  callModule(billings_datatable, "billings_dt",
             appointments_billings, db)

  # Appointment list

  appointments_filtered <- reactive({
    # find appointments with chosen date range and providers
    validate (
      need(input$clinicians, 'Choose at least one clinician\'s appointment to view'),
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

  # chronic disease management table
  cdm_table_results <- callModule(cdm_datatable, "cdm_dt",
                                  appointments_billings, appointments_filtered,
                                  appointments_filtered_time, appointments_list,
                                  diabetes_list, asthma_list,
                                  db$history)

  ### Diabetes sub-code

  diabetes_list <- reactive({
    # Best Practice Diabetes code
    diabetes_codes <- c(3, 775, 776, 778, 774, 7840, 11998)

    # Returns vector of InternalID of patients who have diabetes
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% diabetes_codes),
                 by = c('InternalID')) %>%
      pull(InternalID) %>%
      unique()
  })

  ### Asthma sub-code

  asthma_list <- reactive({
    # Best Practice Asthma code
    asthma_codes <- c(281, 285, 283, 284, 282)

    # Returns vector of InternalID of patients who have asthma
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% asthma_codes),
                 by = c('InternalID')) %>%
      pull(InternalID) %>%
      unique()
  })

  atsi_list <- reactive({
    # Best Practice Aboriginal or Torres Strait Islander codes
    atsi_codes <- c("Aboriginal", "Torres Strait Islander",
                    "Aboriginal/Torres Strait Islander")

    # returns vector of InternalID of patients who are
    # Aboriginal or Torres Strait Islander as recorded in patient into
    appointments_filtered() %>%
      inner_join(db$patients %>%
                   filter(Ethnicity %in% atsi_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  malignancy_list <- reactive({
    # Best Practice codes including for many cancers, carcinomas, lymphomas, leukaemias
    malignancy_codes <- c(463, 478, 485, 7845, 449, 6075, 453, 456, 473, 490, 11927,
                          445, 444, 446, 447, 448, 451, 457, 458, 459, 460, 462, 469, 454,
                          472, 474, 477, 480, 481, 482, 486, 487, 488, 489, 11911, 491,
                          492, 9391, 7751, 483, 8027, 470, 471, 476, 8261, 2475, 6835,
                          6827, 6817, 6818, 6813, 6824, 6830, 6820, 6822, 6819, 6815, 6828,
                          6826, 6821, 6833, 6831, 6823, 6834, 6825, 6832, 6829, 6814, 3221,
                          4975, 2273, 2287, 4976, 5604, 5599, 5602, 5600, 5609, 5601, 5603,
                          5608, 5607, 329, 2350, 2222, 5054, 2223, 6541, 2224, 2225, 2226,
                          6003, 5480, 2230, 452, 3215, 7005, 2173, 2174, 2175, 2176, 2177,
                          2178, 2179, 1440)

    # returns vector of InternalID of patients who
    # have a recorded malignancy
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% malignancy_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  hiv_list <- reactive({
    # Best Practice codes for HIV
    hiv_codes <- c(1727)

    # returns vector of InternalID of patients who
    # have HIV
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% hiv_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  haemoglobinopathy_list <- reactive({
    # Best Practice codes for haemoglobinopathy
    haemoglobinopathy_codes <- c(205, 208, 209, 210)

    # returns vector of InternalID of patients who
    # have haemoglobinopathy
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% haemoglobinopathy_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  asplenic_list <- reactive({
    # Best Practice codes for asplenia
    asplenic_codes <- c(3958, 5805, 6493, 3959)

    # returns vector of InternalID of patients who
    # are asplenic
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% asplenic_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  transplant_list <- reactive({
    # Best Practice codes for transplants (not corneal or hair)
    transplant_codes <- c(4160, 3691, 3814, 3826, 12026, 3765, 3989)
    # bone marrow, heart, liver, lung, pancreas, renal, thymus

    # returns vector of InternalID of patients who
    # have had transplants
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% transplant_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  cardiacdisease_list <- reactive({
    # Best Practice codes for cardiac disease
    transplant_codes <- c(7810, 226, 227, 228, 2376, 2377, 2378, 2379, 2380, 2381,
                          2382, 3576, 3577, 3578, 3579, 1534, 2556, 6847, 7847,
                          1347, 2376, 2377, 2378, 2379, 2380, 2381, 2382, 7847, 6847, 2556)
    # cyanotic congenital heart disease, ischaemic heart disease, AMI and congestive failure

    # returns vector of InternalID of patients who
    # have had cardiac disease
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% transplant_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  trisomy21_list <- reactive({
    # Best Practice codes for trisomy 21
    trisomy21_codes <- c(836)

    # returns vector of InternalID of patients who
    # have trisomy 21
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% trisomy21_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  bmi30_list <- reactive({
    # returns vector of InternalID of patients who
    # have bmi 30 or more (obesity)
    appointments_filtered() %>% collect() %>%
      inner_join(db$observations %>%
                   filter(DATACODE == 9), # this is BMI. also in DATANAME, but different spellings/cases
                 by = "InternalID", copy = TRUE) %>%
      filter(OBSDATE <= AppointmentDate) %>% # observation done before the appointment time
      group_by(InternalID, AppointmentDate, AppointmentTime) %>%
      slice(which.max(OBSDATE)) %>% # choose the observation with the most recent observation date
      # unfortunately, as the code stands, this generates a vector which is not appointment date specific
      # if a range of appointment dates has been chosen
      ungroup() %>%
      filter(DATAVALUE >= 30) %>% # those with BMI >= 30
      pull(InternalID) %>%
      unique()
  })

  chroniclungdisease_list <- reactive({
    # Best Practice codes for trisomy 21
    cld_codes <- c(598, 4740, 414, 702)
    # returns vector of InternalID of patients who
    # have lung disease such as bronchiectasis, cystic fibrosis, COPD/COAD
    # asthma is in a separate list

    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% cld_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  neurologic_list <- reactive({
    # Best Practice codes for neurology
    neuro_codes <- c(2351, 963, 965, 966, 968, 969, 971, 6604,
                     2022, 2630, 3093)
    # multiple sclerosis, epilepsy, spinal cord injury, paraplegia, quadriplegia

    # returns vector of InternalID of patients who
    # have neurologic disease
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% neuro_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  chronicliverdisease_list <- reactive({
    # Best Practice codes for chronic liver disease
    cld_codes <- c(11763, 584, 81)
    # liver disease (BP doesn't have 'chronic liver disease'!), cirrhosis, alcoholism

    # returns vector of InternalID of patients who
    # have chronic liver disease
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% cld_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  chronicrenaldisease_list <- reactive({
    # Best Practice codes for chronic liver disease
    crf_codes <- c(662, 258, 3132, 662, 2486, 2487, 6379, 2489, 7469, 1274,
                   7502, 7503, 7504, 7505, 7506, 2882)
    # chronic renal failure, renal impairment, dialysis

    # returns vector of InternalID of patients who
    # have chronic renal disease
    appointments_filtered() %>%
      inner_join(db$history %>%
                   filter(ConditionID %in% crf_codes),
                 by = c("InternalID")) %>%
      pull(InternalID) %>%
      unique()
  })

  pregnant_list <- reactive({
    # returns vector of InternalID of patients who
    # are pregnant

    appointments_filtered() %>% collect() %>%
      inner_join(db$pregnancies %>%
                   filter(is.null(ENDDATE)),
                 by = "InternalID", copy = TRUE) %>%
      filter((EDCBYDATE > AppointmentDate) & (EDCBYDATE < (AppointmentDate+months(9)))) %>%
      pull(InternalID) %>%
      unique()
  })

  # appointment list

  callModule(appointments_datatable, "appointments_dt",
             appointments_filtered_time)

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
                                  UserConfig, location_list_names, db, config_pool)

  output$user <- renderUser({
    dashboardUser(
      name = UserConfig()$Fullname[UserConfig()$AuthIdentity == Sys.info()[["user"]]],
      src = "./assets/icons/user-avatar.svg", # note the lack of "./www/..."
      subtitle = Sys.info()[["user"]],
      fluidRow(
        dashboardUserItem(
          width = 6,
          descriptionBlock(
            text = paste0(
              unlist(UserConfig()$Location[UserConfig()$AuthIdentity == Sys.info()[["user"]]]),
              collapse = ", "),
            right_border = TRUE,
            margin_bottom = TRUE)
        ),
        dashboardUserItem(
          width = 6,
          descriptionBlock(
            text = paste0(
              unlist(UserConfig()$Attributes[UserConfig()$AuthIdentity == Sys.info()[["user"]]]),
              collapse = ", "),
            right_border = FALSE,
            margin_bottom = TRUE)
        )
      )
    )
  })

}
