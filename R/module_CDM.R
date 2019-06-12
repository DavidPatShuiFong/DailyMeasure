##### CDM (chronic disease management) modules ##########################################

#' Chronic Disease Management (CDM) module - UI function
#'
#' Display CDM status and opportunities within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
cdm_datatableUI <- function(id) {
	ns <- NS(id)

	tagList(
		fluidRow(
			column(4,
						 shinyWidgets::switchInput(
						 	inputId = ns("printcopy_view"),
						 	label = "<i class=\"fas fa-print\"></i> </i><i class=\"far fa-copy\"></i>  Print and Copy View",
						 	labelWidth = "100%")
			),
			column(2, offset = 6, # note that total 'column' width = 12
						 uiOutput(ns("cdm_item_choice"))
			)
		),
		shinycssloaders::withSpinner(
		  DT::DTOutput(ns("cdm_table")),
		  type = 8,
		  hide.element.when.recalculating = FALSE,
		  proxy.height = NULL)
	)
}

#' Chronic disease management list module - server
#'
#' chronic disease management items claimed, pending or unclaimed for appointment list
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param appointments_billings reactive. joining appointments and billings
#' @param appointments_filtered reactive
#' @param appointments_filtered_time reactive. filtered appointments list
#' @param appointments_list reactive. same as appointments_filtered_time, but with DOB and Age added
#' @param diabetes_list vector of diabetic patients with appointments
#' @param asthma_list vector of asthma patients with apponitments
#' @param dbhistory table of history items, lazy evaluation
#' @param appointments_filtered_time reactive list of appointments attached to all billings
#' @param db access to database tables from Best Practice EMR
#'
#' @include fomantic_definitions.R
#'
#' @return none
cdm_datatable <- function(input, output, session,
													appointments_billings, appointments_filtered,
													appointments_filtered_time, appointments_list,
													diabetes_list, asthma_list,
													dbHistory) {
	ns <- session$ns

	# MBS (medicare benefits schedule) item numbers for CDM
	cdm_item <- data.frame(
		code = c(721, 723, 732, 703, 705, 707, 2517, 2521, 2525, 2546, 2552, 2558, 2700, 2701, 2715, 2717),
		name = c('GPMP', 'TCA', 'GPMP R/V', 'HA', 'HA', 'HA', 'DiabetesSIP', 'DiabetesSIP', 'DiabetesSIP',
						 'AsthmaSIP', 'AsthmaSIP', 'AsthmaSIP', 'MHCP', 'MHCP', 'MHCP', 'MHCP')
	)

	cdm_item_names <- as.character(unique(cdm_item$name)) # de-factored

	output$cdm_item_choice <- renderUI({
		shinyWidgets::dropdown(
			input_id = "choice_dropdown",
			shinyWidgets::checkboxGroupButtons(
			  inputId = ns("cdm_chosen"), label = "CDM items shown",
			  choices = cdm_item_names, selected = cdm_item_names,
			  # all choices initially selected
			  status = "primary",
			  checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
			icon = icon("gear"),
			label = "CDM items shown"
		)
	})

	# filter to CDM item billed prior to (or on) the day of displayed appointments
	# only show most recent billed item in each category

	appointments_billings_cdm <- reactive({
		validate(
			need(appointments_billings(), "No billed appointments in chosen range"),
			need(nrow(appointments_billings())>0, "No billed appointments in chosen range")
		)
		appointments <- appointments_billings() %>%
			filter(MBSItem %in% cdm_item$code) %>%
			# only chronic disease management items
			filter(ServiceDate <= AppointmentDate) %>%
			# only items billed before the appointment day
			select(InternalID, AppointmentDate, AppointmentTime, Provider,
						 ServiceDate, MBSItem, Description) %>%
			mutate(MBSName = cdm_item$name[match(MBSItem, cdm_item$code)])

		if ("GPMP R/V" %in% input$cdm_chosen) {
			gpmprv <- appointments %>%
				# GPMP R/V tags.
				# unlike other items, this is on a 3 month schedule, and can follow
				# an item 'other' than itself (e.g. it can follow a GPMP or TCA)
				#
				# only show if a GPMP R/V is due (greater than three months since gpmp or tca or gpmp r/v)
				# or if GPMP R/V is the most recent of gpmp/tca/gpmp r/v
				#
				# green if 'up-to-date' (GPMP R/V is the most recent, and less than 3/months)
				# yellow if 'done, but old' (GPMP R/V is the most recent, and more than 3/months)
				# red if 'not done' (GPMP/TCA most recent, and more than three)
				filter(MBSName %in% c("GPMP", "TCA", "GPMP R/V")) %>%
				# r/v only applies if gpmp/tca or r/v already claimed
				group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>%
				# group by appointment
				slice(which.max(ServiceDate)) %>%
				ungroup() %>%
				# (one) item with latest servicedate
				filter((MBSName == "GPMP R/V") | lubridate::interval(ServiceDate, AppointmentDate)>months(3)) %>%
				# minimum 3-month gap since claiming previous GPMP/TCA,
				# or most recent claim is a GPMP R/V
				mutate(mbstag =
							 	semantic_tag("GPMP R/V", # semantic/fomantic buttons
							 							 colour =
							 							 	if_else(MBSName %in% c("GPMP", "TCA"),
							 							 					'red',
							 							 					# no GPMP R/V since the last GPMP/TCA
							 							 					if_else(lubridate::interval(ServiceDate, AppointmentDate)<months(3),
							 							 									# GPMP R/V. Less than or more than 3 months?
							 							 									'green',
							 							 									'yellow')),
							 							 popuphtml =
							 							 	paste0("<h4>Date : ", ServiceDate,
							 							 				 "</h4><h6>Item : ", MBSItem,
							 							 				 "</h6><p><font size=\'+0\'>", Description, "</p>")),
							 mbstag_print = paste0("GPMP R/V", " ", # printable version of information
							 											if_else(MBSName %in% c("GPMP", "TCA"),
							 															paste0("(", MBSName, ": ", ServiceDate, ") Overdue"),
							 															if_else(lubridate::interval(ServiceDate, AppointmentDate)<months(3),
							 																			paste0("(", ServiceDate, ")"),
							 																			paste0("(", ServiceDate, ") Overdue"))
							 											)
							 )
				)
		} else {
			gpmprv <- NULL
		}

		appointments <- appointments %>%
			filter(!(MBSName == "GPMP R/V")) %>% # GPMP R/V will be added back in as a 'tagged' version
			rbind(diabetes_list_cdm()) %>%
			rbind(asthma_list_cdm()) %>%
			rbind(aha75_list_cdm()) %>%
			filter(MBSName %in% input$cdm_chosen) %>%
			group_by(InternalID, AppointmentDate, AppointmentTime, Provider, MBSName) %>%
			# group by patient, apppointment and CDM type (name)
			filter(ServiceDate == max(ServiceDate, na.rm = TRUE)) %>%
			# only keep most recent service
			ungroup()

		appointments <- appointments %>%
			mutate(mbstag =
						 	semantic_tag(MBSName, # semantic/fomantic buttons
						 							 colour =
						 							 	if_else(
						 							 	  ServiceDate == -Inf,
						 							 	  'red',
						 							 	  # invalid date is '-Inf', means item not claimed yet
						 							 	  if_else(
						 							 	    lubridate::interval(ServiceDate, AppointmentDate)<lubridate::years(1),
						 							 	    'green',
						 							 	    'yellow')),
						 							 popuphtml =
						 							   paste0("<h4>Date : ", ServiceDate,
						 							          "</h4><h6>Item : ", MBSItem,
						 							          "</h6><p><font size=\'+0\'>", Description, "</p>")),
						 	mbstag_print = paste0(MBSName, " ", # printable version of information
						 	                      if_else(
						 	                        ServiceDate == -Inf,
						 	                        '',
						 	                        paste0("(", ServiceDate, ")",
						 	                               if_else(
						 	                                 lubridate::interval(ServiceDate, AppointmentDate)<lubridate::years(1),
						 	                                 "",
						 	                                 " Overdue")))
						 	)
			) %>%
		  rbind(gpmprv) %>% # add in GPMP reviews
			group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>%
			# gathers item numbers on the same day into a single row
			summarise(cdm = paste(mbstag, collapse = ""),
								cdm_print = paste(mbstag_print, collapse = ", ")) %>%
			ungroup()

		appointments
	})

	### AHA 75 (annual health assessment for those aged 75 years and above)
	aha75_list_cdm <- reactive({
		appointments_list() %>%
			filter(Age >= 75) %>%
			select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
			mutate(MBSName = c('HA'), Description = c('Age 75 years or older'),
						 ServiceDate = as.Date(-Inf, origin = '1970-01-01'), MBSItem = NA) %>%
			unique()
	})

	### Diabetes CDM list

	diabetes_list_cdm <- reactive({

		a <- appointments_list() %>%
			filter(InternalID %in% diabetes_list()) %>%
			select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
			mutate(MBSName = c('DiabetesSIP'), Description = c('History : Diabetes'),
						 ServiceDate = as.Date(-Inf, origin = '1970-01-01'), MBSItem = NA) %>%
			unique()
		# invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
		# setting invalid date to NA is not good for later comparisons,
		# where max(... , na.rm=TRUE) needs to be used

		b <- a %>% mutate(MBSName = "GPMP")
		# people with diabetes also qualify for GPMP. duplicate list with 'GPMP' MBSName
		rbind(a, b)
	})

	### asthma list CDM

	asthma_list_cdm <- reactive({

		a <- appointments_list() %>%
		  filter(InternalID %in% asthma_list()) %>%
			select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
			mutate(MBSName = c('AsthmaSIP'), Description = c('History : Asthma'),
						 ServiceDate = as.Date(-Inf, origin = '1970-01-01'), MBSItem = NA) %>%
			unique()
		# invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
		# setting invalid date to NA is not good for later comparisons,
		# where max(... , na.rm=TRUE) needs to be used

		b <- a %>% mutate(MBSName = c('GPMP'))
		# people with asthma also qualify for GPMP. duplicate list with 'GPMP' MBSName
		rbind(a, b)
	})

	### create tag-styled datatable (or 'printable' datatable)

	cdm_styled_datatable <- reactive({
		if (!is.null(appointments_billings_cdm()) & !is.null(appointments_filtered_time())) {
			if (input$printcopy_view == TRUE) {
				# printable/copyable view
				datatable_styled(appointments_filtered_time() %>%
												 	inner_join(appointments_billings_cdm(),
												 						 by = c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
												 	select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'cdm_print')),
												 colnames = c('Patient', 'Appointment Date', 'Appointment Time', 'Provider', 'CDM items'))
			} else {
				# fomantic/semantic tag view
				datatable_styled(appointments_filtered_time() %>%
												 	inner_join(appointments_billings_cdm(),
												 						 by = c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
												 	select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'cdm')),
												 colnames = c('Patient', 'Appointment Date', 'Appointment Time', 'Provider', 'CDM items'),
												 dom = 'frltip', # no copy/print buttons
												 escape = c(5)) # only interpret HTML for last column
			}
		}
	})

	output$cdm_table <- DT::renderDT({
		cdm_styled_datatable()
	},
	server = TRUE)
}
