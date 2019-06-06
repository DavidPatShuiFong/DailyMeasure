#' Appointments module

##### Appointments module ##########################################

appointments_datatableUI <- function(id) {

	ns <- shiny::NS(id)

	tagList(
	  # print-view only (no semantic/fomantic buttons)
	  withSpinner(DT::DTOutput(ns("appointments_table")),
	              type = 8,
	              hide.element.when.recalculating = FALSE,
	              proxy.height = NULL)
	)
}

##### appointments sub-functions ######

##### server side #######

appointments_datatable <- function(input, output, session,
                               appointments_filtered_time) {
  # billings items claimed
  # input - input, output, session (as required by modules)
  # input - appointments_filtered_time - reactive list of appointments
	# output - none
	ns <- session$ns

	# fomantic/semantic UI definitions not required

	# appointment list
	output$appointments_dt <- renderDT({datatable_styled(
		appointments_filtered_time() %>%
			select(c('Patient', 'AppointmentDate', 'AppointmentTime', 'Provider', 'Status')))
	},
	server = FALSE)


	styled_appointments_list <- reactive({
	  validate(
	    need(appointments_filtered_time(), "No appointments in selected range")
	  )

		datatable_styled(appointments_filtered_time() %>%
										 	select(Patient, AppointmentDate, AppointmentTime,
										 				 Provider, Status))
	})

	output$appointments_table <- renderDT({
	  styled_appointments_list()
	})
}
