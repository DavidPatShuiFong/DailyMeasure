##### Billings module ##########################################

#' Appointments module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
billings_datatableUI <- function(id) {
	ns <- shiny::NS(id)

	shiny::tagList(
	  shiny::fluidRow(
	    shiny::column(4,
	                  shinyWidgets::switchInput(
	                    inputId = ns("printcopy_view"),
	                    label = paste("<i class=\"fas fa-print\"></i> </i>",
	                                  "<i class=\"far fa-copy\"></i>",
	                                  "  Print and Copy View", sep = ""),
	                    labelWidth = "100%")
			)
		),
		shinycssloaders::withSpinner(
		  DT::DTOutput(ns("billings_table")),
		  type = 8,
		  hide.element.when.recalculating = FALSE,
		  proxy.height = NULL)
	)
}

##### server side ##########################################

#' appointment list module - server
#'
#' list of appointments and billings
#' within selected range of dates and providers
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @include fomantic_definitions.R
#'
#' @return none
#'
billings_datatable <- function(input, output, session, dM) {
	ns <- session$ns

	# MBS (medicare benefits schedule) item numbers for CDM

	# filter to billings which are done on the same day as displayed appointments
	appointments_billings_sameday <- shiny::eventReactive(dM$appointments_filteredR(), {
	  return(dM$appointments_billings_sameday(screentag = TRUE,
	                                          screentag_print = TRUE))
	  #	returns Patient, InternalID, AppointmentDate, AppointmentTime,
	  #	        Provider, MBSItem, Description, billingtag, billingtag_print
	  # need to preserve ApppointmentTime and Provider
	  # in the case where there are multiple apppointments
	  # for the patient in the same time period/day and providers
	})

	billings_list <- shiny::reactive({
	  shiny::validate(
	    need(appointments_billings_sameday(), "No appointments in chosen range"),
	    need(nrow(appointments_billings_sameday())>0, "No appointments in chosen range")
	  )
	  
	  billingslist <- NULL
	  billingslist <- rbind(billingslist, appointments_billings_sameday())

	  if (is.null(billingslist)) {
	    # no valid appointments
	  } else {
	    billingslist <- billingslist %>>%
	      dplyr::group_by(Patient, InternalID, AppointmentDate,
	                      AppointmentTime, Provider) %>>%
	      # gathers vaccination notifications on the same appointment into a single row
	      dplyr::summarise(billingtag = paste(billingtag, collapse = ""),
	                       billingtag_print = paste(billingtag_print, collapse = ", ")) %>>%
	      dplyr::ungroup()
	  }
	  billingslist
	})

	styled_billings_list <- reactive({
	  shiny::validate(
	    need(billings_list(), "No appointments in selected range")
	  )

	  if (input$printcopy_view == TRUE) {
	    # printable/copyable view
	    datatable_styled(billings_list() %>>%
	                       dplyr::select(Patient, AppointmentDate, AppointmentTime,
	                                     Provider, billingtag_print),
	                     colnames = c('Billings' = 'billingtag_print'))
	  } else {
	    # fomantic/semantic tag view
	    datatable_styled(billings_list() %>%
	                       dplyr::select(Patient, AppointmentDate, AppointmentTime,
	                                     Provider, billingtag),
	                     escape = c(5),
	                     dom = 'frltip', # no copy/print buttons
	                     colnames = c('Billings' = 'billingtag'))
	  }
	})

	# test code for having a section of code which only responds to buttons pushed
	# in the 'main server function/UI' when the 'Billings' tab is selected
	#
	# in that case, the calling code would look like this...
	# (probably actually better observe 'date_a' and 'date_b' rather than input$update_date
	# because the 'today' button also modifies the date)
	#
	#	callModule(billings_datatable, "billings_dt",
	#	           appointments_billings, db,
	#	           reactive(input$sidebartabs), "billings",
	#	           reactive(input$update_date), clinician_choice_list)
	#
	# the header of the server part of the module would look like this...
	#
	#	billings_datatable <- function(input, output, session,
	#	                               appointments_billings, db,
	#	                               input_sidebartabs, menuitemtabname,
	#	                               input_update_date, clinician_choice_list) {
	#
	# and the observing function would look like this...
	#
	#	observeEvent(c(appointments_billings(),
	#	               input_sidebartabs(),
	#	               input_update_date(), clinician_choice_list()),
	#	             {
	#	               print(paste0("Triggered Billings update, tab=", input_sidebartabs()))
	#	               if (input_sidebartabs() == menuitemtabname){
	#	                 browser()
	#	               }
	#	})

	output$billings_table <- DT::renderDT({
	  styled_billings_list()
	})
}
