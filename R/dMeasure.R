#' dMeasure class
#' @title dMeasure class
#' @doctype class
#' @description case-finding in EMR (Best Practice) a
#' @field yaml_config_filepath - filepath of YAML configuration
#' @field sql_config_filepath - filepath of SQL configuration (NULL is not connected)
#' @field local_config - in-memory copy of YAML configuration
#'
#' @section Methods:
#' \describe{
#' \item{\strong{read_configuration_filepaths} : read (or initiate) YAML/SQL DB filepaths}
#' \item{\strong{open_configuration_db} : open SQL configuration database}
#' \item{\strong{read_configuration_db} : read SQL configuration database}
#' }
#'
#' @examples
#'
#' @export
dMeasure <-
  R6::R6Class("dMeasure",
              public = list(

                ##### Configuration file location ########################
                ## fields
                yaml_config_filepath = NULL,
                sql_config_filepath = NULL,
                local_config = NULL,

                # methods
                read_configuration_filepaths = function () {
                  # read configuration filepaths
                  # by default, the YAML configuration is either in the working
                  # directory (where a local installation of R lives),
                  # or the user's home directory
                  #
                  # this method will set $yaml_config_filepath and $sql_config_filepath
                  # it will read the YAML configuration filepath, which if already
                  # existing might contain the 'real' location of the $sql_config_filepath
                  #
                  # finally sets $configuration_file_path to 'real' SQL filepath
                  if (grepl("Program Files", normalizePath(R.home()))) {
                    # this is a system-wide install
                    self$yaml_config_filepath <- "~/.DailyMeasure_cfg.yaml"
                    self$sql_config_filepath <- "~/.DailyMeasure_cfg.sqlite"
                    # store in user's home directory
                  } else {
                    # this is a 'local' user install, not a system-wide install
                    # e.g. C:/Users/MyName/AppData/Programs/...
                    # as opposed to 'C:/Program Files/...'
                    self$yaml_config_filepath <- "./DailyMeasure_cfg.yaml"
                    self$sql_config_filepath <- "./DailyMeasure_cfg.sqlite"
                    # this file can be stored in the AppData folder, out of sight of the user
                  }

                  if (configr::is.yaml.file(self$yaml_config_filepath)) {
                    # if config file exists and is a YAML-type file
                    self$local_config <- configr::read.config(self$yaml_config_filepath)
                    # config in local location
                  } else {
                    # local config file does not exist. possibly first-run
                    self$local_config <- list()
                    self$local_config$config_file <- self$sql_config_filepath
                    # main configuration file, could be set to 'common location'
                    # write the (minimalist) local config file
                    configr::write.config(
                      self$local_config,
                      file.path = self$yaml_config_filepath,
                      write.type = "yaml"
                    )
                  }
                  self$configuration_file_path <- self$local_config$config_file
                  invisible(self)
                },

                ##### Configuration details - databases, locations, users ###########

                ## Fields
                config_db = NULL,
                # later dbConnection::dbConnection$new() connection to database
                # using either DBI or pool
                configuration_file_path = character(),
                BPdatabase = data.frame(id = integer(),
                                        Name = character(),
                                        Address = character(),
                                        Database = character(),
                                        UserID = character(),
                                        dbPassword = character(),
                                        stringsAsFactors = FALSE),
                BPdatabaseChoice = character(),
                # database choice will be the same as the 'Name' of
                # the chosen entry in BPdatabase
                PracticeLocations = data.frame(id = integer(),
                                               Name = character(),
                                               Description = character(),
                                               stringsAsFactors = FALSE),
                # id needed for editing this dataframe later
                # need default value for practice location filter
                # interface initialization
                UserConfig = data.frame(id = integer(),
                                        Fullname = character(), AuthIdentity = character(),
                                        Location = character(),
                                        Attributes = character(),
                                        Password = character(),
                                        stringsAsFactors = FALSE),

                UserRestrictions = data.frame(uid = integer(),
                                              Restriction = character(),
                                              stringsAsFactors = FALSE),
                # this lists the 'enabled' restrictions,
                #  relevant to the 'Attributes' field of 'UserConfig'
                # without the restriction, all users have the 'permission'
                #  for the 'non-specified' action
                # use 'uid' rather than 'id', because 'id' is
                # later used to identify the restrictions...

                ## methods
                open_configuration_db = function (
                  # this will open the SQL connection to
                  # the configuration from the SQL configuration file
                  #
                  # this function will also check the SQL database
                  # is compliant. new tables are added, and old ones
                  # are checked to see if all required fields/columns
                  # are present. if a field/column is missing, then
                  # the missing field/column is added.
                  #
                  # accepts two parameters
                  #  configuration_file_path (location of SQL configuration)
                  #  the configuration DB object (R6 dbConnection object)
                  # both these parameters have defaults, which may have
                  # been set by previous calls
                  #
                  configuration_file_path = self$configuration_file_path,
                  config_db = self$config_db) {

                  # on first call, self$config_db could be NULL
                  if (is.null(config_db)) {
                    self$config_db <- dbConnection::dbConnection$new()
                    config_db <- self$config_db
                    # new R6 object which generalizes database connections
                  }

                  if (file.exists(configuration_file_path)) {
                    # open config database file
                    config_db$connect(RSQLite::SQLite(),
                                      dbname = self$configuration_file_path)
                  } else {
                    # if the config database doesn't exist,
                    # then create it (note create = TRUE option)
                    config_db$connect(RSQLite::SQLite(),
                                      dbname = self$configuration_file_path)
                    # create = TRUE not a valid option?
                    # always tries to create file if it doesn't exist
                  }

                  initialize_data_table = function(config_db, tablename, variable_list ) {
                    # make sure the table in the database has all the right variable headings
                    # allows 'update' of old databases
                    #
                    # input - config_db : R6 object of configuration database
                    # input - tablename : name of table
                    # input - variable_list : list of variable headings, with variable type
                    #   e.g. list(c("id", "integer"), c("Name", "character"))
                    #
                    # alters table in database directly
                    #
                    # returns - nothing

                    tablenames <- config_db$conn() %>% DBI::dbListTables()

                    if (tablename %in% tablenames) {
                      # if table exists in config_db database
                      columns <- config_db$conn() %>% dplyr::tbl(tablename) %>% colnames()
                      # list of column (variable) names
                      data <- config_db$conn() %>% dplyr::tbl(tablename) %>% dplyr::collect()
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
                      DBI::dbWriteTable(config_db$conn(), tablename, data, overwrite = TRUE)
                    }
                  }


                  if (!is.null(self$config_db$conn())) {
                    # check that tables exist in the config file
                    # also create new columns (variables) as necessary
                    initialize_data_table(config_db, "Server",
                                          list(base::c("id", "integer"),
                                               base::c("Name", "character"),
                                               base::c("Address", "character"),
                                               base::c("Database", "character"),
                                               base::c("UserID", "character"),
                                               base::c("dbPassword", "character")))
                    # initialize_data_table will create table and/or ADD 'missing' columns to existing table

                    initialize_data_table(config_db, "ServerChoice",
                                          list(base::c("id", "integer"),
                                               base::c("Name", "character")))
                    # there should only be (at most) one entry in this table!
                    # with id '1', and a 'Name' the same as the chosen entry in table "Server"

                    initialize_data_table(config_db, "Location",
                                          list(base::c("id", "integer"),
                                               base::c("Name", "character"),
                                               base::c("Description", "character")))

                    initialize_data_table(config_db, "Users",
                                          list(base::c("id", "integer"),
                                               base::c("Fullname", "character"),
                                               base::c("AuthIdentity", "character"),
                                               base::c("Location", "character"),
                                               base::c("Password", "character"),
                                               base::c("Attributes", "character")))

                    initialize_data_table(config_db, "UserRestrictions",
                                          list(base::c("uid", "integer"),
                                               base::c("Restriction", "character")))
                    # list of restrictions for users
                    # use of 'uid' rather than 'id'
                    # (this relates to the 'Attributes' field in "Users")
                  }
                  invisible(self)
                },
                read_configuration_db = function(config_db = self$config_db) {
                  # read the SQL configuration database
                  # accept one parameter, which is the R6 object to the open SQL database
                  # default is the internally stored value

                  self$BPdatabase <- config_db$conn() %>%
                    dplyr::tbl("Server") %>% dplyr::collect()
                  self$BPdatabaseChoice <-
                    (config_db$conn() %>% dplyr::tbl("ServerChoice") %>%
                       dplyr::filter(id == 1) %>% dplyr::select("Name") %>%
                       dplyr::collect())[[1]]
                  self$PracticeLocations <- config_db$conn() %>%
                    dplyr::tbl("Location")
                  self$UserConfig <- config_db$conn() %>%
                    dplyr::tbl("Users") %>%
                    # in UserConfig, there can be multiple Locations/Attributes per user
                    dplyr::collect() %>%
                    dplyr::mutate(Location = stringi::stri_split(Location, regex = ";"),
                                  Attributes = stringi::stri_split(Attributes, regex = ";"))
                  self$UserRestrictions <- config_db$conn() %>%
                    dplyr::tbl("UserRestrictions") %>% dplyr::collect()
                  invisible(self)
                },

                ##### Electronic Medical Record (EMR) database configuration ######

                ## fields
                emr_db = NULL, # later will be R6 object containing database object
                db = list(dbVersion = 0), # later will be the EMR databases.
                # $dbversion is number of EMR database openings

                ## methods
                open_emr_db = function(BPdatabaseChoice = NULL,
                                       emr_db = NULL) {
                  # opens the EMR database
                  # 'BPdatabaseChoice' - the chosen database from the config_db list
                  # 'emr_db' - R6 database object. this might not be initially defined

                  # if no arguments passed, the defaults are what is stored in
                  # this object
                  if (is.null(BPdatabaseChoice)) {
                    BPdatabaseChoice = self$BPdatabaseChoice
                  }
                  if (is.null(emr_db)) {
                    emr_db = self$emr_db
                  }

                  browser()

                  print(paste("ChosenServerName:", BPdatabaseChoice))

                  if (is.null(emr_db)) {
                    self$emr_db <- dbConnection::dbConnection$new()
                    emr_db <- self$emr_db
                    # on first run, self$emr_db may be NULL
                  }
                  # close existing database connection
                  # safe to call $close() if no database is open
                  emr_db$close()

                  if (BPdatabaseChoice == "None") {
                    # do nothing
                  } else if (!is.null(BPdatabaseChoice)) {
                    server <- self$BPdatabase %>%
                      dplyr::filter(Name == BPdatabaseChoice) %>%
                      dplyr::collect()
                    print("Opening EMR database")
                    emr_db$connect(odbc::odbc(), driver = "SQL Server",
                                   server = server$Address, database = server$Database,
                                   uid = server$UserID, pwd = simple_decode(server$dbPassword))
                  }

                  if (is.null(emr_db$conn()) || !DBI::dbIsValid(emr_db$conn())) {
                    # || 'short-circuits' the evaluation, so if not an environment,
                    # then dbIsValid() is not evaluated (will return an error if emr_db$conn() is NULL)

                    # either database not opened, or has just been closed
                    self$db$users <- NULL
                    self$db$patients <- NULL
                    self$db$investigations <- NULL
                    self$db$appointments <- NULL
                    self$db$immunizations <- NULL
                    self$db$preventive_health <- NULL
                    self$db$correspondenceIn <- NULL
                    self$db$reportValues <- NULL
                    self$db$services <- NULL
                    self$db$history <- NULL
                    self$clinician_choice_list <- NULL
                    self$BPdatabaseChoice <- "None" # set choice of database to 'None'

                  } else {
                    # successfully opened database
                    self$initialize_emr_tables(emr_db)
                  }
                },
                initialize_emr_tables = function(emr_db = self$emr_db) {
                  # initialize the tables of the EMR database
                  # parameter - R6 object connecting to EMR database
                  print("Re-initializing databases")

                  self$db$users <- emr_db$conn() %>%
                    # this is a function! a collect() is later called prior to mutate/join,
                    # (as a result is no longer a 'lazy eval') and cannot be evaluated just once.
                    # output - Fullname, UserID, Surname, Firstname, LocationName, Title, ProviderNo
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Users')) %>%
                    dplyr::select(c('UserID', 'Surname', 'Firstname',
                                    'LocationName', 'Title', 'ProviderNo'))

                  self$db$patients <- emr_db$conn() %>%
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Patients'))

                  self$db$investigations <- emr_db$conn() %>%
                    # output - InternalID, Collected (Date), TestName
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Investigations')) %>%
                    dplyr::select(c('InternalID', 'Collected', 'TestName'))
                  # as of Jan/2019, the odbc engine for MSSQL can't handle the
                  # full ('Select *') Investigations table
                  # due to some type of bug/standards non-compliance.
                  # also can handle the History table. need to
                  # 'Select' out just a few columns.

                  self$db$appointments <- emr_db$conn() %>%
                    # Patient, InternalID, AppointmentDate, AppointmentTime, Provider, Status
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Appointments')) %>%
                    dplyr::select(c('Patient', 'InternalID',
                                    'AppointmentDate', 'AppointmentTime',
                                    'Provider', 'Status'))

                  self$db$immunizations <- emr_db$conn() %>%
                    # InternalID, GivenDate, VaccineName, VaccineID
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Immunisations')) %>%
                    dplyr::select(c('InternalID', 'GivenDate', 'VaccineName', 'VaccineID'))

                  self$db$vaccine_disease <- emr_db$conn() %>%
                    # vaccineIDs linked to diseases
                    # e.g. diseasecode 7+30 are for influenza vaccines
                    dplyr::tbl(dbplyr::in_schema("bpsdrugs.dbo", "VACCINE_DISEASE")) %>%
                    dplyr::select("VACCINEID", "DISEASECODE")

                  self$db$preventive_health <- emr_db$conn() %>%
                    # INTERNALID, ITEMID (e.g. not for Zostavax remindders)
                    dplyr::tbl(dbplyr::in_schema('dbo', 'PreventiveHealth')) %>%
                    dplyr::select('InternalID' = 'INTERNALID', 'ITEMID')

                  self$db$correspondenceIn <- emr_db$conn() %>%
                    # InternalID, CorrespondenceDate, Subject, Detail
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_CorrespondenceIn')) %>%
                    dplyr::select('InternalID', 'CorrespondenceDate', 'Subject', 'Detail')

                  self$db$reportValues <- emr_db$conn() %>%
                    # InternalID, ReportDate, ResultName, LoincCode
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_ReportValues')) %>%
                    dplyr::select('InternalID', 'ReportDate', 'ResultName', 'LoincCode')

                  self$db$services <- emr_db$conn() %>%
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_SERVICES')) %>%
                    dplyr::select('InternalID' = 'INTERNALID', 'ServiceDate' = 'SERVICEDATE',
                                  'MBSItem' = 'MBSITEM', 'Description' = 'DESCRIPTION')

                  self$db$history <- emr_db$conn() %>%
                    # InternalID, Year, Condition, ConditionID, Status
                    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_History')) %>%
                    dplyr::select('InternalID', 'Year',
                                  'Condition', 'ConditionID', 'Status')

                  self$db$observations <- emr_db$conn() %>%
                    dplyr::tbl(dbplyr::in_schema("dbo", "OBSERVATIONS")) %>%
                    dplyr::select('InternalID' = 'INTERNALID', 'DATANAME',
                                  'DATACODE', 'DATAVALUE', 'OBSDATE')

                  self$db$currentrx <- emr_db$conn() %>%
                    dplyr::tbl(dbplyr::in_schema("dbo", "CURRENTRX")) %>%
                    dplyr::select('InternalID' = 'INTERNALID', 'PRODUCTID',
                                  'DRUGNAME', 'RXSTATUS')
                  # RXSTATUS appears to be 1 if 'long-term' and 2 if 'short-term'

                  self$db$obgyndetail <- emr_db$conn() %>%
                    dplyr::tbl(dbplyr::in_schema("dbo", "OBSGYNDETAIL")) %>%
                    dplyr::select('InternalID' = 'INTERNALID', 'NOMINALLMP',
                                  'LASTPAPDATE', 'LASTPAPRESULT', 'BREASTFEEDING',
                                  'MammogramStatus', 'LastMammogramDate', 'MammogramResult')

                  self$db$pregnancies <- emr_db$conn() %>%
                    dplyr::tbl(dbplyr::in_schema("dbo", "PREGNANCIES")) %>%
                    dplyr::select('InternalID' = 'INTERNALID', 'EDCBYDATE',
                                  'ACTUALLMP', 'NOMINALLMP', 'ENDDATE')

                  self$db$dbversion <- self$db$dbversion + 1
                  print(paste("dbversion:", self$db$dbversion))
                }

              ))
