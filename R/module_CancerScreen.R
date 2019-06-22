##### cancer screening modules ##########################################

#' Cancer screen User Interface module
#'
#' Datatable with list of patients and cancer screening opportunities.
#' Includes 'printable' toggle, and selectable cancer screening dropdown.
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
cancerscreen_datatableUI <- function(id) {
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
						 uiOutput(ns("cancerscreen_choice"))
			)
		),
		shinycssloaders::withSpinner(
		  DT::DTOutput(ns("cancerscreen_table")),
		  type = 8,
		  hide.element.when.recalculating = FALSE,
		  proxy.height = NULL)
	)
}

##### cancer sub-functions ######


##### bowel cancer (FOBT) definitions ######

bowel_cancer_screen_terms <-
	c("(VALUES('%FOB%'), ('%OCCULT%'), ('%FAECAL HUMAN HAEMOGLOBIN%'),
      ('%OCB NATIONAL SCREENING%'), ('%FHB%'), ('%FAECAL BLOOD%'),
      ('%FAECAL IMMUNOCHEMICAL TEST%'), ('%FAECAL HAEMOGLOBIN%'),
      ('%COLONOSCOPY%'), ('%COLONOSCOPE%')) AS tests(fobtnames)")

fobt_investigation_query <-
	paste('SELECT InternalID, Collected, TestName FROM dbo.BPS_Investigations
          INNER JOIN', bowel_cancer_screen_terms,
				'ON TestName LIKE tests.fobtnames')
# SQL code to find investigations which could be bowel cancer screening items

fobt_letter_subject_query <-
	paste('SELECT InternalID, CorrespondenceDate, Subject FROM dbo.BPS_CorrespondenceIn
           INNER JOIN', bowel_cancer_screen_terms,
				'ON Subject LIKE tests.fobtnames')

fobt_letter_detail_query <-
	paste('SELECT InternalID, CorrespondenceDate, Detail FROM dbo.BPS_CorrespondenceIn
           INNER JOIN', bowel_cancer_screen_terms,
				'ON Detail LIKE tests.fobtnames')

fobt_result_query <-
	paste("SELECT InternalID, ReportDate, ResultName FROM dbo.BPS_ReportValues
            WHERE LoincCode IN ('2335-8','27396-1','14563-1','14564-9','14565-6',
                         	      '12503-9','12504-7','27401-9','27925-7','27926-5',
	                              '57905-2','56490-6','56491-4','29771-3')")

#' Bowel cancer screening list
#'
#' @param appointments_list reactive, list of appointments to search
#' @param emr_db R6 object, accesss to Best Practice (EMR) database
#'
#' @return list of appointments (with patient details)
#'
fobt_list <- function(appointments_list, emr_db) {

	screen_fobt_list <-appointments_list() %>%
		filter(Age >= 50 & Age <=75) # from age 50 to 75 years inclusive

	screen_fobt_ix <-
		left_join(screen_fobt_list,
							bind_rows(inner_join(screen_fobt_list,
							                     DBI::dbGetQuery(emr_db$conn(), fobt_investigation_query) %>%
							                       collect() %>%
							                       rename(TestDate = Collected),
							                     by = 'InternalID'),
												inner_join(screen_fobt_list,
																	 DBI::dbGetQuery(emr_db$conn(), fobt_letter_subject_query) %>%
																	   collect() %>%
																	   rename(TestDate = CorrespondenceDate, TestName = Subject),
																	 by = 'InternalID'),
												inner_join(screen_fobt_list,
												           DBI::dbGetQuery(emr_db$conn(), fobt_letter_detail_query) %>%
												             collect() %>%
												             rename(TestDate = CorrespondenceDate, TestName = Detail),
																	 by = 'InternalID'),
												inner_join(screen_fobt_list,
												           DBI::dbGetQuery(emr_db$conn(), fobt_result_query) %>% collect() %>%
												             rename(TestDate = ReportDate, TestName = ResultName),
												           by = 'InternalID')
							) %>%
								mutate(TestDate = as.Date(substr(TestDate, 1, 10))) %>%
								# remove time from date
								group_by(InternalID) %>%
								# group by patient ID (need most recent investigation for each patient)
								# only keep the latest(/recent) dated investigation
								filter(TestDate == max(TestDate, na.rm = TRUE))) %>%
		mutate(OutOfDateTest =
					 	case_when(is.na(TestDate) ~ 1,
					 						# if no date (no detected test)
					 						lubridate::interval(TestDate, AppointmentDate)>lubridate::years(2) ~ 2,
					 						# if old
					 						TRUE ~ 3)) %>%   # if up-to-date
		replace_na(list(TestName = 'FOBT')) %>%
		mutate(screentag =
					 	semantic_tag(
					 		TestName,
					 		colour = c('red', 'yellow', 'green')[OutOfDateTest],
					 		popuphtml = paste0("<h4>Date : ", TestDate, "</h4>")),
					 screentag_print =
					 	paste0(TestName,
					 				 case_when(OutOfDateTest == 1 ~ " (Never Done) ",
					 				 					OutOfDateTest == 2 ~ " (OVERDUE) ",
					 				 					OutOfDateTest == 3 ~ " "),
					 				 if_else(OutOfDateTest != 1,
					 				 				paste0("(Date:", TestDate, ")"),
					 				 				""))) %>%
		select(Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
					 DOB, Age, screentag, screentag_print)

	return(screen_fobt_ix)
}

##### server side #######

#' Cancer screening module server
#'
#' Chronic disease management items claimed, pending or unclaimed for appointment list
#'
#' @param input (as required by modules)
#' @param output (as required by modules)
#' @param session (as required by modules)
#' @param appointments_list reactive list of appointments to search
#' @param emr_db R6 object, access to Electronic Medical Record database
#'
#' @return None
#'
#' @include fomantic_definitions.R
#' requires fomantic/semantic definitions
cancerscreen_datatable <- function(input, output, session,
																	 appointments_list, emr_db) {

	ns <- session$ns

	# MBS (medicare benefits schedule) item numbers for CDM
	cancerscreen_names <- c("Bowel cancer")

	output$cancerscreen_choice <- renderUI({
		shinyWidgets::dropdown(
			# placing the drop-down in the render UI (as opposed to module UI)
			# allows input$cancerscreen_chosen to be defined at time of rendering
			# (otherwise, it is not defined until the drop-down is actually opened!)
			inputid = "choice_dropdown",
			shinyWidgets::checkboxGroupButtons(
			  inputId = ns("cancerscreen_chosen"), label = "Cancer Screen items shown",
			  choices = cancerscreen_names, selected = cancerscreen_names,
			  # all choices initially selected
			  status = "primary",
			  checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
			icon = icon("gear"),
			label = "Cancer screening shown"
		)
	})

	cancerscreen_list <- reactiveVal(NULL)

	cancerscreen_list <- reactive({
		validate(
			need(appointments_list(), "No appointments in chosen range"),
			need(nrow(appointments_list())>0, "No appointments in chosen range")
		)

		screenlist <- NULL
		# Zostavax (herpes zoster 'shingles' vaccine)
		if ("Bowel cancer" %in% input$cancerscreen_chosen)
		{screenlist <- rbind(screenlist, fobt_list(appointments_list, emr_db))}

		if (!is.null(screenlist)) {
			screenlist <- screenlist %>%
				group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
								 DOB, Age) %>%
				# gathers vaccination notifications on the same appointment into a single row
				summarise(screentag = paste(screentag, collapse = ""),
									screentag_print = paste(screentag_print, collapse = ", ")) %>%
				ungroup()
		}
		screenlist
	})

	styled_cancerscreen_list <- reactive({
		validate(
			need(appointments_list(), "No appointments in selected range"),
			need(cancerscreen_list(), "Choose at least one screening to display")
		)
		dummy <- cancerscreen_list()

		if (input$printcopy_view == TRUE) {
			# printable/copyable view
			datatable_styled(cancerscreen_list() %>%
											 	select(c('Patient', 'AppointmentDate', 'AppointmentTime',
											 					 'Provider', 'DOB', 'Age', 'screentag_print')),
											 colnames = c('Screening' = 'screentag_print'))
		} else {
			# fomantic/semantic tag view
			datatable_styled(cancerscreen_list() %>%
											 	select(c('Patient', 'AppointmentDate', 'AppointmentTime',
											 					 'Provider', 'DOB', 'Age', 'screentag')),
											 escape = c(7),
											 dom = 'frltip', # no copy/print buttons
											 colnames = c('Screening' = 'screentag'))
		}
	})

	output$cancerscreen_table <- DT::renderDT({
		styled_cancerscreen_list()
	})
}
