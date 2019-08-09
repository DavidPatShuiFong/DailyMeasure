#' Shiny app UI object
#'
#' Creates the shiny application user interface
#'
#' @include module_Appointments.R module_Immunization.R module_CancerScreen.R
#' @include module_Billings.R module_CDM.R
#' @include module_Configuration_users.R module_Configuration_location.R
#' @include module_Configuration_password.R module_Configuration_server.R
#' requires all moduleUI definitions to be defined
#'
#' @return dashboardPagePlus object
#'
DailyMeasureUI <- function() {
  shinydashboardPlus::dashboardPagePlus(

    header = shinydashboardPlus::dashboardHeaderPlus(
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "address-card",
      title = shiny::tagList(
        shiny::span(class = "logo-lg", "Daily Measure"),
        icon = icon("heartbeat")),
      shinydashboardPlus::userOutput("user")
    ),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "sidebartabs",
        shinydashboard::menuItem("Appointments",
                                 tabName = "appointments", icon = shiny::icon("calendar-check")),
        shinydashboard::menuItem("Immunization",
                                 tabName = "immunization", icon = shiny::icon("syringe")),
        shinydashboard::menuItem("Cancer Screening",
                                 tabName = "cancerscreen", icon = icon("x-ray")),
        shinydashboard::menuItem("Billings",
                                 tabName = "billings", icon = shiny::icon("receipt")),
        shinydashboard::menuItem("CDM items",
                                 tabName = "cdm", icon = shiny::icon("file-medical-alt")),
        shinydashboard::menuItem("Administration",
                                 tabName = "administration", icon = shiny::icon("microscope")),
        shinydashboard::menuItem("Configuration",
                                 tabName = "configuration", icon = shiny::icon("wrench"))
        # menuItem("Test", tabName = "test")
      )
    ),

    # Sidebar with a slider input for number of bins
    rightsidebar = shinydashboardPlus::rightSidebar(
      shinyjs::useShinyjs(), # this is needed to enable the 'click' of 'update_date' by 'Today'
      background = "dark",
      shinydashboardPlus::rightSidebarTabContent(
        id = 1,
        title = "Appointment Details",
        icon = "users",
        active = TRUE,

        # clinician list
        shiny::wellPanel(
          shiny::uiOutput('locationList'),
          # list of practice sites
          shiny::uiOutput('clinicianList'),
          # list of clinicians at the currently chosen practice site
          shiny::tags$div(title = "Select/De-select all clinicians",
                          # toggle all listed clinicians on, or off
                          shiny::actionButton('toggle_clinician_list', 'Select All/None',
                                              shiny::icon('check-square'), class = 'btn btn-primary'))
        )
      ),
      shinydashboardPlus::rightSidebarTabContent(
        id = 2,
        title = "Selected date range",
        icon = "calendar-alt",

        # appointment date range
        shiny::wellPanel(
          shiny::dateInput('date1', label = 'From:', format='D dd/M/yyyy',
                           min = Sys.Date()-6000, max = Sys.Date()+180,
                           value = Sys.Date()),
          shiny::dateInput('date2', label = 'To:', format='D dd/M/yyyy',
                           min = Sys.Date()-6000, max = Sys.Date()+180,
                           value = Sys.Date()),
          # range of dates, by default will be 'today'
          shiny::actionButton('update_date', 'Update',
                              shiny::icon('refresh'), class = 'btn btn-primary'),
          # date range not activated until the 'Update' button is clicked
          shiny::helpText("After adjusting the date range, click the 'Update' button",
                          "to adjust the viewed appointment date range"),
          shiny::tags$div(title = "View today's appointments",
                          shiny::actionButton('update_date_today', 'Today',
                                              shiny::icon('calendar'), class = 'btn btn-info'))
          # manually change date range to 'today'
        )
      )
    ),
    title = "Daily Measure",
    body = shinydashboard::dashboardBody(
      shinyWidgets::useSweetAlert(),
      shinytoastr::useToastr(),
      shinyjs::useShinyjs(),
      tags$head(
        # stylesheets from fomantic.ui (a fork of semantic.ui)
        # Note that this is a specially edited version of semantic.css that is provided with fomantic
        # The 'popup' component does not work without some code
        # provided in the initial header of semantic.css
        # However, I have removed a lot of margin/padding/font re-definition
        # that is included in the header,
        # which disturbs the layout of shiny/flexdashboard
        includeCSS(system.file('www', 'fomantic_components.css', package = "DailyMeasure")),
        # defining additional fomantic JS popup initialization in the header does not work.
        # Popup initialization does work inside DataTables
        # use of 'full' fomantic.js interferes with the popups from DTedit
        # minimum JS required for JS popups is popup.js and transition.js
        includeScript(system.file('www', 'popup.js', package = 'DailyMeasure')),
        includeScript(system.file('www', 'transition.js', package = 'DailyMeasure')),
        # Pushes the export/save buttons for datatables to the right
        # and provide padding on the top
        tags$style(HTML(".dataTables_wrapper .dt-buttons { float:none;
                      text-align:right;
                      padding-top:7px;
                      }"))
      ),

      tabItems(
        tabItem(
          tabName = "appointments",
          fluidRow(column(width = 12, align = "center", h2("Appointments"))),
          fluidRow(column(width = 12, appointments_datatableUI("appointments_dt")))
        ),
        tabItem(
          tabName = "immunization",
          fluidRow(column(width = 12, align = "center", h2("Immunization"))),
          fluidRow(column(width = 12, vax_datatableUI("vax_dt")))
        ),
        tabItem(
          tabName = "cancerscreen",
          fluidRow(column(width = 12, align = "center", h2("Cancer screening"))),
          fluidRow(column(width = 12, cancerscreen_datatableUI("cancerscreen_dt")))
        ),
        tabItem(
          tabName = "billings",
          fluidRow(column(width = 12, align = "center", h2("Billings"))),
          fluidRow(column(width = 12, billings_datatableUI("billings_dt")))
        ),
        shinydashboard::tabItem(
          tabName = "cdm",
          shiny::fluidRow(column(width = 12, align = "center",
                                 h2("Chronic Disease Management items"))),
          shiny::fluidRow(column(width = 12,
                                 cdm_datatableUI("cdm_dt")))
        ),
        shinydashboard::tabItem(
          tabName = "administration",
          shiny::fluidRow(column(width = 12, align = "center",
                                 h2("Administration"))),
          shiny::fluidRow(column(width = 12,
                                 administration_UI("admin_dt")))
        ),
        shinydashboard::tabItem(
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
          )),
        shinydashboard::tabItem(
          tabName = "test",
          shiny::fluidRow(shiny::column(width = 12, align = "center",
                                        h2("Test frame"))),
          shiny::fluidRow(shiny::column(width = 12,
                                        DT::DTOutput("test_dt")))
        )
      )
    )
  )
}
