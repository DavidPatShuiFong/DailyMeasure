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
#' @export
DailyMeasureUI <- function() {
  shinydashboardPlus::dashboardPagePlus(
    rintrojs::introjsUI(),

    header = shinydashboardPlus::dashboardHeaderPlus(
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "address-card",
      title = shiny::tagList(
        icon = icon("heartbeat"),
        shiny::div(style = "display:inline-block",
                   shiny::HTML('<p style = "font-family:Verdana">  GP&thinsp;stat!</p>'))),
      left_menu = shiny::tagList(
        shinyWidgets::dropdownButton(
          shiny::tags$h3("Introductory tutorials"),
          shiny::br(),
          "Step-by-step walkthroughs of GPstat! features and controls.",
          shiny::br(),
          shiny::fluidRow(column(12, align = "center",
                                 shiny::actionButton("guide_overview", "Overview",
                                                     icon = icon("map-marked-alt", lib = "font-awesome"),
                                                     width = "9em"))),
          shiny::br(),
          list("Don't forget to check the '", shiny::icon("info"), " About' tab on the left-sidebar! ",
               "Includes video demonstrations and technical documentation."),
          shiny::br(),
          "Tutorials (if any) listed below are context sensitive, depending on currently chosen view.",
          shiny::br(),
          shiny::fluidRow(column(12, align = "center",
                                 shiny::conditionalPanel(
                                   condition = 'input.sidebartabs == "appointments"',
                                   shiny::actionButton("appointments_overview", "Appointments",
                                                       icon = icon("calendar-check", lib = "font-awesome"),
                                                       width = "9em")),
                                 shiny::conditionalPanel(
                                   condition = 'input.sidebartabs == "immunization"',
                                   shiny::actionButton("immunization_overview", "Immunization",
                                                       icon = icon("syringe", lib = "font-awesome"),
                                                       width = "9em")))),
          circle = TRUE, status = "info", size = "sm",
          icon = shiny::div(id = "graduation-cap-icon", shiny::icon("graduation-cap")), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Tutorials"),
          inputId = "tutorials")
      ),
      shinydashboardPlus::userOutput("user")
    ),
    sidebar = shinydashboard::dashboardSidebar(
      shiny::div(id = "sidebarMenu-wrapper", # this is needed for rintrojs
                 shinydashboard::sidebarMenu(
                   id = "sidebartabs",
                   shinydashboard::menuItem("Appointments",
                                            tabName = "appointments",
                                            icon = shiny::icon("calendar-check")),
                   shinydashboard::menuItem("Immunization",
                                            tabName = "immunization",
                                            icon = shiny::icon("syringe")),
                   shinydashboard::menuItem("Cancer Screening",
                                            tabName = "cancerscreen",
                                            icon = icon("x-ray")),
                   shinydashboard::menuItemOutput("BillingsMenu"),
                   shinydashboard::menuItemOutput("CDMMenu"),
                   shinydashboard::menuItemOutput("PIPqimMenu"),
                   shinydashboard::menuItemOutput("CustomMenu"),
                   # dynamically created Billings, CDM, PIP quality improvement and Custom
                   # menu items. could be blank!
                   # e.g. will be blank unless dMeasureQIM module/package is available
                   shinydashboard::menuItem("Conditions",
                                            tabName = "conditions",
                                            icon = shiny::icon("fingerprint")),
                   shinydashboard::menuItem("Administration",
                                            tabName = "administration",
                                            icon = shiny::icon("microscope")),
                   shinydashboard::menuItem("Configuration",
                                            tabName = "configuration",
                                            icon = shiny::icon("wrench"),
                                            selected = TRUE
                                            # this is a dummy entry and will be re-selected in the server
                   ),
                   shinydashboard::menuItem("About",
                                            tabName = "about",
                                            icon = shiny::icon("info"))
                   # menuItem("Test", tabName = "test")
                 ))
    ),

    # Sidebar with a slider input for number of bins
    rightsidebar = shinydashboardPlus::rightSidebar(
      shiny::tags$head(shiny::tags$style(shiny::HTML(
        ".control-sidebar-tabs {display:none;}
        .tabbable > .nav > li > a:hover {background-color: #333e43; color:white}
        .tabbable > .nav > li[class=active] > a   {background-color: #222d32;  color:white}"))),
      # removes empty space at top of rightsidebar
      # https://stackoverflow.com/questions/59289622/
      #  remove-the-dark-space-at-the-top-of-the-right-sidebar-in-a-shinydashboardplus
      # change tabpanel tab colouring on hover and active to be
      # more like shinydashboardPlus::rightSidebarTabContent
      # https://stackoverflow.com/questions/35025145/
      #  background-color-of-tabs-in-shiny-tabpanel
      # https://stackoverflow.com/questions/47798850/
      #  change-background-color-of-tabpanel-when-it-is-active-or-hover-over-in-shiny
      shinyjs::useShinyjs(), # this is needed to enable the 'click' of 'update_date' by 'Today'
      background = "dark",
      shiny::tabsetPanel(
        shiny::tabPanel(
          shiny::icon("users"),
          value = "rightsidebar-appointment",
          shiny::br(), shiny::h4("Appointment Details"), shiny::br(),
          # clinician list
          shiny::div(id = "rightsidebar-appointment-wrapper",
                     shiny::wellPanel(
                       shiny::uiOutput('locationList'),
                       # list of practice sites
                       shiny::uiOutput('clinicianList'),
                       # list of clinicians at the currently chosen practice site
                       shiny::tags$div(title = "Select/De-select all clinicians",
                                       # toggle all listed clinicians on, or off
                                       shiny::actionButton('toggle_clinician_list',
                                                           'Select All/None',
                                                           shiny::icon('check-square'),
                                                           class = 'btn btn-primary'))
                     ))
        ),
        shiny::tabPanel(
          shiny::icon("calendar-alt"),
          value = "rightsidebar-date",
          shiny::br(),shiny::h4("Selected date range"), shiny::br(),
          # appointment date range
          shiny::div(id = "rightsidebar-date-wrapper",
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
                                       shiny::actionButton('update_date_today',
                                                           'Today',
                                                           shiny::icon('calendar'),
                                                           class = 'btn btn-info'))
                       # manually change date range to 'today'
                     ))),
        shiny::tabPanel(
          shiny::icon("handshake"),
          value = "rightsidebar-contact",
          shiny::br(), shiny::h4("Contact details"), shiny::br(),
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
                          "At billing", "Invoiced", "Completed"),
              selected = c("With doctor", "At billing", "Invoiced", "Completed"),
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
      )
    ),
    title = "Daily Measure",

    body = shinydashboard::dashboardBody(
      shinyWidgets::useSweetAlert(),
      shinytoastr::useToastr(),
      shinyjs::useShinyjs(),
      shinyEffects::setPulse(id = "graduation-cap-icon", duration = 1, iteration = 50),
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
        shiny::tags$style(HTML(".dataTables_wrapper .dt-buttons { float:none;
                      text-align:right;
                      padding-top:7px;
                      }")),
        shiny::tags$style("@import url(https://use.fontawesome.com/releases/v5.12.0/css/all.css);")
        # currently shiny (up to 1.4.0, uses fontawsome v5.3.1)
        # if no internet access, then some icons will be missing e.g. 'baby'
      ),
      shiny::uiOutput("tabItems")
      # tabItems are dynamically generated in the server
      # according to what modules/packages are available
    )
  )
}
