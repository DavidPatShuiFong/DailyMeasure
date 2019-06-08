##### Appointments module ##########################################

#' Appointments module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
appointments_datatableUI <- function(id) {

	ns <- shiny::NS(id)

	tagList(
	  # print-view only (no semantic/fomantic buttons)
	  shinycssloaders::withSpinner(
	    DT::DTOutput(ns("appointments_table")),
	    type = 8,
	    hide.element.when.recalculating = FALSE,
	    proxy.height = NULL)
	)
}

##### server side #####################

#' appointment list module - server
#'
#' list of appointments within selected range of dates and providers
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param appointments_filtered_time - reactive list of appointments
#'
#' @return none
appointments_datatable <- function(input, output, session,
                               appointments_filtered_time) {
	ns <- session$ns

	# fomantic/semantic UI definitions not required

	# appointment list
	output$appointments_dt <- DT::renderDT({datatable_styled(
		appointments_filtered_time() %>%
			select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'Status')))
	},
	server = FALSE)


	styled_appointments_list <- reactive({
	  validate(
	    need(appointments_filtered_time(), "No appointments in selected range"),
	    need(nrow(appointments_filtered_time()) > 0, "No appointments in selected range")
	  )

		datatable_styled(appointments_filtered_time() %>%
										 	select(Patient, AppointmentDate, AppointmentTime,
										 				 Provider, Status))
	})

	output$appointments_table <- DT::renderDT({
	  styled_appointments_list()
	})
}
