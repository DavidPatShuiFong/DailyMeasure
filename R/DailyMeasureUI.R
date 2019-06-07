#' @include module_Appointments.R module_Immunization.R module_CancerScreen.R
#' @include module_Billings.R module_CDM.R module_Configuration.R
NULL
# requires all moduleUI definitions to be defined

#' Shiny app UI object

##### Define UI for application ######################
# create the shiny application user interface
DailyMeasureUI <- function() {
	shinydashboardPlus::dashboardPagePlus(

		header = shinydashboardPlus::dashboardHeaderPlus(
			enable_rightsidebar = TRUE,
			rightSidebarIcon = "calendar-alt",
			title = tagList(
				span(class = "logo-lg", "Daily Measure"),
				icon = icon("heartbeat")),
			shinydashboardPlus::userOutput("user")
		),
		sidebar = shinydashboard::dashboardSidebar(
			shinydashboard::sidebarMenu(
				id = "sidebartabs",
				menuItem("Appointments", tabName = "appointments", icon = icon("calendar-check")),
				menuItem("Immunization", tabName = "immunization", icon = icon("syringe")),
				menuItem("Cancer Screening", tabName = "cancerscreen", icon = icon("x-ray")),
				menuItem("Billings", tabName = "billings", icon = icon("receipt")),
				menuItem("CDM items", tabName = "cdm", icon = icon("file-medical-alt")),
				menuItem("Configuration", tabName = "configuration", icon = icon("wrench"))
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

		shinydashboard::dashboardBody(
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
				tabItem(tabName = "appointments",
								fluidRow(column(width = 12, align = "center", h2("Appointments"))),
								fluidRow(column(width = 12, appointments_datatableUI("appointments_dt")))
				),
				tabItem(tabName = "immunization",
								fluidRow(column(width = 12, align = "center", h2("Immunization"))),
								fluidRow(column(width = 12, vax_datatableUI("vax_dt")))
				),
				tabItem(tabName = "cancerscreen",
								fluidRow(column(width = 12, align = "center", h2("Cancer screening"))),
								fluidRow(column(width = 12, cancerscreen_datatableUI("cancerscreen_dt")))
				),
				tabItem(tabName = "billings",
								fluidRow(column(width = 12, align = "center", h2("Billings"))),
								fluidRow(column(width = 12, billings_datatableUI("billings_dt")))
				),
				tabItem(tabName = "cdm",
								fluidRow(column(width = 12, align = "center", h2("Chronic Disease Management items"))),
								fluidRow(column(width = 12, cdm_datatableUI("cdm_dt")))
				),
				tabItem(tabName = "configuration",
								fluidRow(
									shinydashboard::tabBox(
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
														 	shinyFiles::shinyFilesButton(
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
								fluidRow(column(width = 12, DT::DTOutput("test_dt")))
				)
			)
		)
	)
}
