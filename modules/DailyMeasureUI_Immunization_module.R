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
			),
			column(2, offset = 6, # note that total 'column' width = 12
						 dropdown(
						 	uiOutput(ns("vax_item_choice")),
						 	icon = icon("gear"),
						 	label = "Vaccination items shown"
						 )
			)
		),
		DTOutput(ns("vax_table"))
	)
}

zostavax_list <- function(appointments_list, db) {
  # return datatable of appointments where Zostavax is recommended (might already be given)
  #  Patient, InternalID, AppointmentDate, ApppointmentTime, Provider, DOB, Age
  #  vaxtag, vaxtag_print (these two are the 'semantic' tags and printable tags)
  # input - appointment_list - reactive of appointment list
  # input - db - access to Best Practice EMR database

  appointments_list() %>%
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
    mutate(vaxtag =
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
                                           if_else(is.na(GivenDate),
                                                   "Age 70 to 79 years",
                                                   paste0('Date : ', format(GivenDate))),
                                           'Removed from herpes zoster immunization reminders'),
                                   "</h4>")),
           vaxtag_print =
             paste0("Zostavax", " ", # printable version of information
                    if_else(is.na(GivenDate),
                            if_else(is.na(ITEMID), "(Due)", "(Removed from herpes zoster immunization reminders)"),
                            paste0("(Given : ", format(GivenDate), ")"))
             )
    ) %>%
    select(c("Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
             "DOB", "Age", "vaxtag", "vaxtag_print"))
}

influenza_list <- function(appointments_list, db,
													 diabetes_list, asthma_list,
													 atsi_list) {
  # return datatable of appointments where influenza is recommended (might already be given)
  #  Patient, InternalID, AppointmentDate, ApppointmentTime, Provider, DOB, Age
  #  vaxtag, vaxtag_print (these two are the 'semantic' tags and printable tags)
  # input - appointment_list - reactive of appointment list
  # input - db - access to Best Practice EMR database
  # input - reactive - vector of patients with diabetes

  lprevious <- appointments_list() %>%
    # those who have had influenza vaccines in the past
    left_join(db$immunizations %>% collect() %>%
                # those who have had the influenza vaccine
                filter(VaccineID %in%
                         unlist(db$vaccine_disease %>%
                                  filter(DISEASECODE %in% c(7,30)) %>%
                                  select("VACCINEID") %>%
                                  collect(), use.names = FALSE)),
              # there are many, many influenza vaccine IDs, but these can be found
              # via the db$vaccine_disease database
              copy = TRUE) %>%
    mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>%
    filter(GivenDate <= AppointmentDate) %>%
    # only include immunizations given up to date of appointment,
    # if there are any immunizations at all
    # note that 'if_else' is vectorize,
    # demanding same datatype for TRUE/FALSE alternatives
    # 'ifelse' does not preserve date type in this circumstance
    group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>%
    # group by appointment
    slice(which.max(GivenDate)) %>%
    ungroup() %>%
    # (one) item with latest vaccinedate (prior to appointmentdate)
    mutate(Reason = paste0("Given : ", GivenDate)) %>%
    select(c("Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
             "DOB", "Age",
             "GivenDate", "Reason"))

  l65 <- appointments_list() %>%
    filter(Age>=65) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Age 65 years or greater")

  l5 <- appointments_list() %>%
  	mutate(AgeInMonths = calc_age_months(DOB, AppointmentDate)) %>%
  	filter(AgeInMonths >= 6 & AgeInMonths < 60) %>%
  	mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
  				 Reason = "Age 6 months to 4 years inclusive") %>%
  	select(-AgeInMonths)

  ldiabetes <- appointments_list() %>%
    filter(InternalID %in% diabetes_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Diabetes")

  latsi <- appointments_list() %>%
  	filter(InternalID %in% atsi_list()) %>%
  	mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
  				 Reason = "Aboriginal or Torres Strait Islander")

  lasthma <- appointments_list() %>%
  	filter(InternalID %in% asthma_list()) %>%
  	mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
  				 Reason = "Asthma")

  l <- rbind(lprevious, l65, l5, latsi, ldiabetes, lasthma) %>%
    group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider, DOB, Age) %>%
    summarise(GivenDate = max(GivenDate),
              Reason = paste0(Reason, collapse = ", ")) %>% # join unique Reasons together
    ungroup() %>%
    left_join(db$preventive_health %>%
                # those who have been removed from the reminder system for influenza
                filter(ITEMID == 1), by = c('InternalID' = 'INTERNALID'),
              copy = TRUE) %>%
    collect() %>%
    mutate(vaxtag =
             semantic_tag(paste0(' Influenza '),
                          colour =
                            if_else(is.na(GivenDate) |
                                      (GivenDate == as.Date(-Inf, origin = '1970-01-01')),
                                    if_else(is.na(ITEMID),
                                            c('red'), c('purple')),
                                    if_else(year(GivenDate) == year(AppointmentDate),
                                            c('green'), c("yellow"))),
                          # red if not given, purple if removed from flu vax reminders
                          # and green if has had the vax this year. yellow if 'old' vax
                          popuphtml =
                            paste0("<h4>",
                                   if_else(is.na(ITEMID),
                                           as.character(Reason), # co-erce to character (it could be empty)
                                           'Removed from influenza immunization reminders'),
                                   "</h4>")),
           vaxtag_print =
             paste0("Influenza", " ", # printable version of information
                    if_else(is.na(GivenDate),
                            if_else(is.na(ITEMID),
                                    paste0("(", as.character(Reason), ")"),
                                    "(Removed from influenza immunization reminders)"),
                            paste0("(", Reason, ")",
                                   if_else(is.na(GivenDate) | (GivenDate == -Inf), # no previous vax
                                           " Due",
                                           if_else(year(GivenDate) == year(AppointmentDate),
                                                   "", " Due")))) # ?vax given this year
             )
    ) %>%
    select(c("Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
             "DOB", "Age", "vaxtag", "vaxtag_print"))

  return(l)
}

vax_datatable <- function(input, output, session,
													appointments_list, db,
													diabetes_list, asthma_list,
													atsi_list) {
	# vaccinations done, pending or never done for appointment list
	# input - input, output, session (as required by modules)
	# input - appointments_list - reactive. same as appointments_filtered_time, but with DOB and Age added
	# input - db - EMR database
  # input - diabetes_list, asthma_list - condition lists
	# input - atsi_list - Aboriginal or Torres Strait islander
	# output - none
	ns <- session$ns

	# fomantic/semantic UI definitions
	source("./modules/fomantic_definitions.R")
	# age calculation functions
	source("./modules/calculation_definitions.R")

	vax_names <- c("Zostavax", "Influenza")

	output$vax_item_choice <- renderUI({
	  checkboxGroupButtons(inputId = ns("vax_chosen"), label = "Vaccination items shown",
	                       choices = vax_names, selected = vax_names,
	                       # all choices initially selected
	                       status = "primary",
	                       checkIcon = list(yes = icon("ok", lib = "glyphicon")))
	})

	# filter to vax

	vax_selected <- reactiveVal(vax_names)
	# use instead of input$vax_chosen directly because
	# input$vax_chosen is not defined until the dropdown button is selected!
	observeEvent(input$vax_chosen, ignoreNULL = FALSE, ignoreInit = TRUE, {
		# change value of vax_selected to t he value of input$vax_chosen
		# cannot ignoreNULL because all vax could be de-selected
		# ignoreInit because it is not chosen
		vax_selected(input$vax_chosen)
	})

	vax_list <- reactiveVal(NULL)

	observeEvent(c(appointments_list(), vax_selected()), {
	  validate(
	    need(appointments_list(), "No appointments in chosen range"),
	    need(nrow(appointments_list())>0, "No appointments in chosen range")
	  )

	  vlist <- NULL
	  # Zostavax (herpes zoster 'shingles' vaccine)
	  if ("Zostavax" %in% vax_selected())
	  {vlist <- rbind(vlist, zostavax_list(appointments_list, db))}
	  # influenza
	  if ("Influenza" %in% vax_selected())
	  {vlist <- rbind(vlist, influenza_list(appointments_list, db,
	  																			diabetes_list, asthma_list,
	  																			atsi_list))}

	  if (is.null(vlist)) {
	    vax_list(NULL)
	  } else {
	    vax_list(vlist %>%
	               group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
	                        DOB, Age) %>%
	               # gathers vaccination notifications on the same appointment into a single row
	               summarise(vaxtag = paste(vaxtag, collapse = ""),
	                         vaxtag_print = paste(vaxtag_print, collapse = ", ")) %>%
	               ungroup())
	  }
	})

	styled_vax_list <- reactive({
		#if (!is.null(vax_list()) & !is.null(appointments_list())) {
	  validate(
	    need(appointments_list(), "No appointments in selected range"),
	    need(vax_list(), "Choose at least one vaccination to display")
	  )
	  dummy <- vax_list()

	  if (input$printcopy_view == TRUE) {
	    # printable/copyable view
	    datatable_styled(vax_list() %>%
	                       select(c('Patient', 'AppointmentDate', 'AppointmentTime',
	                                'Provider', 'DOB', 'Age', 'vaxtag_print')),
	                     escape = c(7),
	                     colnames = c('Vaccination' = 'vaxtag_print'))
	  } else {
	    # fomantic/semantic tag view
	    datatable_styled(vax_list() %>%
	                       select(c('Patient', 'AppointmentDate', 'AppointmentTime',
	                                'Provider', 'DOB', 'Age', 'vaxtag')),
	                     escape = c(7),
	                     colnames = c('Vaccination' = 'vaxtag'))
	  }
#		}
	})

	output$vax_table <- renderDT({
		styled_vax_list()
	})
}
