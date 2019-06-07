##### Billings module ##########################################

billings_datatableUI <- function(id) {
	ns <- NS(id)

	tagList(
		fluidRow(
			column(4,
						 switchInput(
						 	inputId = ns("printcopy_view"),
						 	label = "<i class=\"fas fa-print\"></i> </i><i class=\"far fa-copy\"></i>  Print and Copy View",
						 	labelWidth = "100%")
			)
		),
		withSpinner(DT::DTOutput(ns("billings_table")),
		            type = 8,
		            hide.element.when.recalculating = FALSE,
		            proxy.height = NULL)
	)
}

##### billing sub-functions ######

##### server side #######

billings_datatable <- function(input, output, session,
                               appointments_billings, db) {
  # billings items claimed
  # input - input, output, session (as required by modules)
  # input - appointments_billings - reactive list of appointments attached to all billings
  # input - access to database tables from Best Practice EMR
  # input - calling_input (reactive) and menuitemtabname (string)
  #  - 'input' of calling environment. is billings tab currently chosen? or just selected?
  # input - inputupdatedate (reactive button) and clinician_choice_list (reactiveVal)
	# output - none
	ns <- session$ns

	# MBS (medicare benefits schedule) item numbers for CDM

	# billings_names <- ""
	#	output$billings_choice <- renderUI({
	#	  checkboxGroupButtons(inputId = ns("billings_chosen"), label = "billings shown",
	#	                       choices = cancerscreen_names, selected = billings_names,
	#	                       # all choices initially selected
	#	                       status = "primary",
	#	                       checkIcon = list(yes = icon("ok", lib = "glyphicon")))
	#	})

	# billings_selected <- reactiveVal(billings_names)
	# use instead of input$billings_chosen directly because
	# input$billings_chosen is not defined until the dropdown button is selected!
	#observeEvent(input$billings_chosen, ignoreNULL = FALSE, ignoreInit = TRUE, {
		# cannot ignoreNULL because all  items could be de-selected
		# ignoreInit is true because input$cancerscreen_chosen is not defined initially
	#	billings_selected(input$billings_chosen)
	#})

	# filter to billings which are done on the same day as displayed appointments
	appointments_billings_sameday <- reactive({
	  appointments_billings() %>%
	    filter(ServiceDate == AppointmentDate) %>%
	    # billings done on the same day as displayed appointments
	    select(Patient, InternalID, AppointmentDate, AppointmentTime,
	           Provider, MBSItem, Description) %>%
	    # need to preserve ApppointmentTime and Provider
	    # in the case where there are multiple apppointments
	    # for the patient in the same time period/day and providers
	    mutate(billingtag =
	             semantic_button(MBSItem,
	                             colour = 'green',
	                             popuphtml = paste0('<h4>', AppointmentDate,
	                                                "</h3><p><font size=\'+0\'>",
	                                                Description, '</p>')),
	           billingtag_print = MBSItem)
	    # change MBSITEMS into fomantic/semantic tags
	})

	billings_list <- reactive({
	  validate(
	    need(appointments_billings_sameday(), "No appointments in chosen range"),
	    need(nrow(appointments_billings_sameday())>0, "No appointments in chosen range")
	  )

	  billingslist <- NULL
	  billingslist <- rbind(billingslist, appointments_billings_sameday())

	  if (is.null(billingslist)) {
	    # no valid appointments
	  } else {
	    billingslist <- billingslist %>%
	      group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider) %>%
	      # gathers vaccination notifications on the same appointment into a single row
	      summarise(billingtag = paste(billingtag, collapse = ""),
	                billingtag_print = paste(billingtag_print, collapse = ", ")) %>%
	      ungroup()
	  }
	  billingslist
	})

	styled_billings_list <- reactive({
	  validate(
	    need(billings_list(), "No appointments in selected range")
	  )
	  print("Creating styled billing list")

	  if (input$printcopy_view == TRUE) {
	    # printable/copyable view
	    datatable_styled(billings_list() %>%
	                       select(Patient, AppointmentDate, AppointmentTime,
	                              Provider, billingtag_print),
	                     colnames = c('Billings' = 'billingtag_print'))
	  } else {
	    # fomantic/semantic tag view
	    datatable_styled(billings_list() %>%
	                       select(Patient, AppointmentDate, AppointmentTime,
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

	output$billings_table <- renderDT({
	  styled_billings_list()
	})
}
