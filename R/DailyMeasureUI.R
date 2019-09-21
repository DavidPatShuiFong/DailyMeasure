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
        icon = icon("heartbeat"),
        shiny::div(style = "display:inline-block",
                   shiny::HTML('<p style = "font-family:Verdana">  GP&thinsp;stat!</p>'))),
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
        shinydashboard::menuItemOutput("BillingsMenu"),
        shinydashboard::menuItemOutput("CDMMenu"),
        shinydashboard::menuItemOutput("PIPqimMenu"),
        # dynamically created Billigs, CDM, PIP quality improvement menu items. could be blank!
        # will be blank unless dMeasureQIM module/package is available
        shinydashboard::menuItem("Administration",
                                 tabName = "administration", icon = shiny::icon("microscope")),
        shinydashboard::menuItem("Configuration",
                                 tabName = "configuration", icon = shiny::icon("wrench"),
                                 selected = TRUE
                                 # this is a dummy entry and will be re-selected in the server
                                 )
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
        )),
      shinydashboardPlus::rightSidebarTabContent(
        id = 3,
        title = "Contact details",
        icon = "handshake",
        shiny::wellPanel(
          shinyWidgets::pickerInput(
            inputId = "contact_type",
            label = "Contact types",
            choices = c("Appointments", "Visits", "Services"),
            selected = c("Services"),
            options = list(style = "btn-primary",
                           `actions-box` = TRUE),
            multiple = TRUE
          ),
          shinyWidgets::sliderTextInput(
            inputId = "min_contact",
            label = "Minimum number of contacts",
            choices = c(1:10),
            grid = TRUE,
            selected = 1
          )
        ),
        shiny::wellPanel(
          shinyWidgets::pickerInput(
            inputId = "appointment_status",
            label = "Appointment status shown",
            choices = c("Booked", "Waiting", "With doctor",
                        "At billing", "Completed"),
            selected = c("With doctor", "At billing", "Completed"),
            # all 'completed' choices initially selected
            options = list(style = "btn-primary",
                           `actions-box` = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(
            inputId = "visit_type",
            label = "Visit types shown",
            choices = c("Surgery", "Home", "Non Visit", "Hospital",
                        "RACF", "Telephone",
                        "SMS", "Email", "Locum Service", "Out of Office",
                        "Other", "Hostel",
                        "Telehealth"),
            selected = c("Surgery", "Home", "Hospital",
                         "RACF", "Locum Service", "Out of Office",
                         "Hostel", "Telehealth"),
            # consult choices initially selected
            options = list(style = "btn-primary",
                           `actions-box` = TRUE),
            multiple = TRUE
          )
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
      shiny::uiOutput("tabItems")
      # tabItems are dynamically generated in the server
      # according to what modules/packages are available
    )
  )
}
