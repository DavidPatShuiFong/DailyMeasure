##### CDM (chronic disease management) modules ##########################################

#' Chronic Disease Management (CDM) module - UI function
#'
#' Display CDM status and opportunities within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
cdm_datatableUI <- function(id) {
	ns <- shiny::NS(id)

	shiny::tagList(
	  shiny::fluidRow(
	    shiny::column(4,
	                  shinyWidgets::switchInput(
	                    inputId = ns("printcopy_view"),
	                    label = "<i class=\"fas fa-print\"></i> </i><i class=\"far fa-copy\"></i>  Print and Copy View",
	                    labelWidth = "100%")
	    ),
	    shiny::column(2, offset = 6, # note that total 'column' width = 12
	                  shiny::uiOutput(ns("cdm_item_choice"))
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

	appointments_billings_cdm <- eventReactive(c(dM$appointments_billingsR(),
	                                             input$cdm_chosen,
	                                             input$printcopy_view), {
		validate(
			need(dM$appointments_billingsR(), "No billed appointments in chosen range"),
			need(nrow(dM$appointments_billingsR())>0, "No billed appointments in chosen range")
		)
		appointments <- dM$appointments_billings_cdm(cdm_chosen = input$cdm_chosen,
		                                             lazy = FALSE,
		                                             screentag = !input$printcopy_view,
		                                             screentag_pring = input$princopy_view)

		return(appointments)
	})

	### AHA 75 (annual health assessment for those aged 75 years and above)
	aha75_list_cdm <- eventReactive(dM$appointments_listR(), {
	  return(dm$aha75_list_cdm)
	})

	### Diabetes CDM list

	diabetes_list_cdm <- eventReactive(dM$appointments_listR(), {
	  return(dm$diabetes_list_cdm)
	})

	### asthma list CDM

	asthma_list_cdm <- eventReactive(dM$appointments_listR(), {
	  return(dM$asthma_list_cdm)
	})

	### create tag-styled datatable (or 'printable' datatable)

	cdm_styled_datatable <- reactive({
	  if (!is.null(appointments_billings_cdm()) &
	      !is.null(dM$appointments_filtered_timeR())) {
	    if (input$printcopy_view == TRUE) {
	      # printable/copyable view
	      datatable_styled(dM$appointments_filtered_timeR() %>>%
	                         dplyr::inner_join(appointments_billings_cdm(),
	                                    by = c('InternalID', 'AppointmentDate',
	                                           'AppointmentTime', 'Provider')) %>>%
	                         dplyr::select(c('Patient', 'AppointmentDate', 'AppointmentTime',
	                                         'Provider', 'cdm_print')),
	                       colnames = c('Patient', 'Appointment Date', 'Appointment Time',
	                                    'Provider', 'CDM items'))
			} else {
				# fomantic/semantic tag view
			  datatable_styled(dM$appointments_filtered_timeR() %>>%
			                     dplyr::inner_join(appointments_billings_cdm(),
			                                       by = c('InternalID', 'AppointmentDate',
			                                              'AppointmentTime', 'Provider')) %>>%
												 	select(c('Patient', 'AppointmentDate', 'AppointmentTime',
												 	         'Provider', 'cdm')),
												 colnames = c('Patient', 'Appointment Date', 'Appointment Time',
												              'Provider', 'CDM items'),
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
