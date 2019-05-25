##### information modules ###############################################################


##### Immunization modules ##########################################

vax_datatableUI <- function(id) {
	ns <- NS(id)

	tagList(
		fluidRow(
			column(4,
						 switchInput(
						 	inputId = ns("printcopy_view"),
						 	label = "<i class=\"fas fa-print\"></i> </i><i class=\"far fa-copy\"></i>  Print and Copy View",
						 	labelWidth = "100%")
			)
#			column(2, offset = 6, # note that total 'column' width = 12
#						 dropdown(
#						 	uiOutput(ns("vax_item_choice")),
#						 	icon = icon("gear"),
#						 	label = "CDM items shown"
#						 )
#			)
		),
		DTOutput(ns("vax_table"))
	)
}

vax_datatable <- function(input, output, session,
													appointments_list, db) {
	# vaccinations done, pending or never done for appointment list
	# input - input, output, session (as required by modules)
	# input - appointments_list - reactive. same as appointments_filtered_time, but with DOB and Age added
	# input - db - EMR database
	# output - none
	ns <- session$ns

	# fomantic/semantic UI definitions
	source("./modules/fomantic_definitions.R")

	vax_list <- reactiveVal(NULL)

	observeEvent(appointments_list(), ignoreNULL = TRUE, {
		l <- appointments_list() %>%
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
		vax_list(l)
	})

	styled_vax_list <- reactive({
		if (!is.null(vax_list()) & !is.null(appointments_list())) {
			datatable_styled(vax_list() %>%
											 	select(c('Patient', 'AppointmentDate', 'AppointmentTime',
											 					 'Provider', 'DOB', 'Age', 'zostavaxtag')),
											 escape = c(7),
											 colnames = c('Zostavax' = 'zostavaxtag'))
		}
	})

	output$vax_table <- renderDT({
		styled_vax_list()
	})
}
