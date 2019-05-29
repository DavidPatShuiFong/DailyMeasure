##### information modules ###############################################################


##### CDM (chronic disease management) modules ##########################################

cdm_datatableUI <- function(id) {
	ns <- NS(id)

	tagList(
		fluidRow(
			column(4,
						 switchInput(
						 	inputId = ns("printcopy_view"),
						 	label = "<i class=\"fas fa-print\"></i> </i><i class=\"far fa-copy\"></i>  Print and Copy View",
						 	labelWidth = "100%")
			),
			column(2, offset = 6, # note that total 'column' width = 12
						 dropdown(
						 	uiOutput(ns("cdm_item_choice")),
						 	icon = icon("gear"),
						 	label = "CDM items shown"
						 )
			)
		),
		DTOutput(ns("cdm_table"))
	)
}

cdm_datatable <- function(input, output, session,
													appointments_billings, appointments_filtered,
													appointments_filtered_time, appointments_list,
													diabetes_list, asthma_list,
													dbHistory) {
	# chronic disease management items claimed, pending or unclaimed for appointment list
	# input - input, output, session (as required by modules)
	# input - appointments_billings - reactive. joining appointments and billings
	# input - appointments_filtered - reactive
	# input - appointments_filtered_time - reactive. filtered appointments list
	# input - appointments_list - reactive. same as appointments_filtered_time, but with DOB and Age added
	# input - diabetes_list - vector of diabetic patients with appointments
	# input - asthma_list - vector of asthma patients with apponitments
	# input - dbhistory - table of history items, lazy evaluation
	# output - none
	ns <- session$ns

	# fomantic/semantic UI definitions
	source("./modules/fomantic_definitions.R")

	# MBS (medicare benefits schedule) item numbers for CDM
	cdm_item <- data.frame(
		code = c(721, 723, 732, 703, 705, 707, 2517, 2521, 2525, 2546, 2552, 2558, 2700, 2701, 2715, 2717),
		name = c('GPMP', 'TCA', 'GPMP R/V', 'HA', 'HA', 'HA', 'DiabetesSIP', 'DiabetesSIP', 'DiabetesSIP',
						 'AsthmaSIP', 'AsthmaSIP', 'AsthmaSIP', 'MHCP', 'MHCP', 'MHCP', 'MHCP')
	)

	cdm_item_names <- as.character(unique(cdm_item$name)) # de-factored

	output$cdm_item_choice <- renderUI({
		checkboxGroupButtons(inputId = ns("cdm_chosen"), label = "CDM items shown",
												 choices = cdm_item_names, selected = cdm_item_names,
												 # all choices initially selected
												 status = "primary",
												 checkIcon = list(yes = icon("ok", lib = "glyphicon")))
	})

	# filter to CDM item billed prior to (or on) the day of displayed appointments
	# only show most recent billed item in each category

	cdm_selected <- reactiveVal(cdm_item_names)
	# use instead of input$cdm_chosen directly because
	# input$cdm_chosen is not defined until the dropdown button is selected!
	observeEvent(input$cdm_chosen, ignoreNULL = FALSE, {
		cdm_selected(input$cdm_chosen)
	})

	appointments_billings_cdm <- reactiveVal(NULL)

	observeEvent(c(appointments_billings(), diabetes_list_cdm(), asthma_list_cdm(),
								 cdm_selected()), {
								 	appointments <- appointments_billings() %>%
								 		filter(MBSITEM %in% cdm_item$code) %>%
								 		# only chronic disease management items
								 		filter(SERVICEDATE <= AppointmentDate) %>%
								 		# only items billed before the appointment day
								 		select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider',
								 						 'SERVICEDATE', 'MBSITEM', 'DESCRIPTION')) %>%
								 		mutate(MBSNAME = cdm_item$name[match(MBSITEM, cdm_item$code)])

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
								 		filter(MBSNAME %in% c("GPMP", "TCA", "GPMP R/V")) %>%
								 		# r/v only applies if gpmp/tca or r/v already claimed
								 		group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>%
								 		# group by appointment
								 		slice(which.max(SERVICEDATE)) %>%
								 		ungroup() %>%
								 		# (one) item with latest servicedate
								 		filter((MBSNAME == "GPMP R/V") | interval(SERVICEDATE, AppointmentDate)>months(3)) %>%
								 		# minimum 3-month gap since claiming previous GPMP/TCA,
								 		# or most recent claim is a GPMP R/V
								 		mutate(mbstag =
								 					 	semantic_tag("GPMP R/V", # semantic/fomantic buttons
								 					 							 colour =
								 					 							 	if_else(MBSNAME %in% c("GPMP", "TCA"),
								 					 							 					'red',
								 					 							 					# no GPMP R/V since the last GPMP/TCA
								 					 							 					if_else(interval(SERVICEDATE, AppointmentDate)<months(3),
								 					 							 									# GPMP R/V. Less than or more than 3 months?
								 					 							 									'green',
								 					 							 									'yellow')),
								 					 							 popuphtml =
								 					 							 	paste0("<h4>Date : ", SERVICEDATE,
								 					 							 				 "</h4><h6>Item : ", MBSITEM,
								 					 							 				 "</h6><p><font size=\'+0\'>", DESCRIPTION, "</p>")),
								 					 mbstag_print = paste0("GPMP R/V", " ", # printable version of information
								 					 											if_else(MBSNAME %in% c("GPMP", "TCA"),
								 					 															paste0("(", MBSNAME, ": ", SERVICEDATE, ") Overdue"),
								 					 															if_else(interval(SERVICEDATE, AppointmentDate)<months(3),
								 					 																			paste0("(", SERVICEDATE, ")"),
								 					 																			paste0("(", SERVICEDATE, ") Overdue"))
								 					 											)
								 					 )
								 		)

								 	appointments <- appointments %>%
								 		filter(!(MBSNAME == "GPMP R/V")) %>% # GPMP R/V will be added back in as a 'tagged' version
								 		rbind(diabetes_list_cdm()) %>%
								 		rbind(asthma_list_cdm()) %>%
								 		rbind(aha75_list_cdm()) %>%
								 		filter(MBSNAME %in% cdm_selected()) %>%
								 		group_by(InternalID, AppointmentDate, AppointmentTime, Provider, MBSNAME) %>%
								 		# group by patient, apppointment and CDM type (name)
								 		filter(SERVICEDATE == max(SERVICEDATE, na.rm = TRUE)) %>%
								 		# only keep most recent service
								 		ungroup()

								 	appointments <- appointments %>%
								 		mutate(mbstag =
								 					 	semantic_tag(MBSNAME, # semantic/fomantic buttons
								 					 							 colour =
								 					 							 	if_else(SERVICEDATE == -Inf,
								 					 							 					'red',
								 					 							 					# invalid date is '-Inf', means item not claimed yet
								 					 							 					if_else(interval(SERVICEDATE, AppointmentDate)<years(1),
								 					 							 									'green',
								 					 							 									'yellow')),
								 					 							 popuphtml =
								 					 							 	paste0("<h4>Date : ", SERVICEDATE,
								 					 							 				 "</h4><h6>Item : ", MBSITEM,
								 					 							 				 "</h6><p><font size=\'+0\'>", DESCRIPTION, "</p>")),
								 					 mbstag_print = paste0(MBSNAME, " ", # printable version of information
								 					 											if_else(SERVICEDATE == -Inf,
								 					 															'',
								 					 															paste0("(", SERVICEDATE, ")",
								 					 																		 if_else(interval(SERVICEDATE, AppointmentDate)<years(1),
								 					 																		 				"", " Overdue")))
								 					 )
								 		) %>%
								 		rbind(gpmprv) %>% # add in GPMP reviews
								 		group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>%
								 		# gathers item numbers on the same day into a single row
								 		summarise(cdm = paste(mbstag, collapse = ""),
								 							cdm_print = paste(mbstag_print, collapse = ", ")) %>%
								 		ungroup()
								 	appointments_billings_cdm(appointments)
	})

	### AHA 75 (annual health assessment for those aged 75 years and above)
	aha75_list_cdm <- reactive({
		appointments_list() %>%
			filter(Age >= 75) %>%
			select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
			mutate(MBSNAME = c('HA'), DESCRIPTION = c('Age 75 years or older'),
						 SERVICEDATE = as.Date(-Inf, origin = '1970-01-01'), MBSITEM = NA) %>%
			unique()
	})

	### Diabetes CDM list

	diabetes_list_cdm <- reactive({

		a <- appointments_list() %>%
			filter(InternalID %in% diabetes_list()) %>%
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

	### asthma list CDM

	asthma_list_cdm <- reactive({

		a <- appointments_list() %>%
		  filter(InternalID %in% asthma_list()) %>%
			select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>%
			mutate(MBSNAME = c('AsthmaSIP'), DESCRIPTION = c('History : Asthma'),
						 SERVICEDATE = as.Date(-Inf, origin = '1970-01-01'), MBSITEM = NA) %>%
			unique()
		# invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
		# setting invalid date to NA is not good for later comparisons,
		# where max(... , na.rm=TRUE) needs to be used

		b <- a %>% mutate(MBSNAME = c('GPMP'))
		# people with asthma also qualify for GPMP. duplicate list with 'GPMP' MBSNAME
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

	output$cdm_table <- renderDT({
		cdm_styled_datatable()
	},
	server = TRUE)
}
