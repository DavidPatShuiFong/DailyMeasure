##### cancer screening modules ##########################################

cancerscreen_datatableUI <- function(id) {
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
						 	uiOutput(ns("cancerscreen_choice")),
						 	icon = icon("gear"),
						 	label = "Cancer screening shown"
						 )
			)
		),
		DTOutput(ns("cancerscreen_table"))
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

fobt_list <- function(appointments_list, emrpool) {
  # Bowel cancer screening
  # input - appointments_list - reactive, list of appointments
  # input - emrpool - accesss to Best Practice (EMR) database

  screen_fobt_list <-appointments_list() %>%
      filter(Age >= 50 & Age <=75) # from age 50 to 75 years inclusive

  screen_fobt_ix <-
    left_join(screen_fobt_list,
              bind_rows(inner_join(screen_fobt_list,
                                   dbGetQuery(emrpool(), fobt_investigation_query) %>%
                                     collect() %>%
                                     rename(TestDate = Collected),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list,
                                   dbGetQuery(emrpool(), fobt_letter_subject_query) %>%
                                     collect() %>%
                                     rename(TestDate = CorrespondenceDate, TestName = Subject),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list,
                                   dbGetQuery(emrpool(), fobt_letter_detail_query) %>%
                                     collect() %>%
                                     rename(TestDate = CorrespondenceDate, TestName = Detail),
                                   by = 'InternalID'),
                        inner_join(screen_fobt_list,
                                   dbGetQuery(emrpool(), fobt_result_query) %>% collect() %>%
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
                       interval(TestDate, AppointmentDate)>years(2) ~ 2,
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

cancerscreen_datatable <- function(input, output, session,
                                   appointments_list, emrpool) {
  # chronic disease management items claimed, pending or unclaimed for appointment list
  # input - input, output, session (as required by modules)
  # input - appointments_list - reactive\
	# output - none
	ns <- session$ns

	# fomantic/semantic UI definitions
	source("./modules/fomantic_definitions.R")

	# MBS (medicare benefits schedule) item numbers for CDM
	cancerscreen_names <- c("Bowel cancer")

	output$cancerscreen_choice <- renderUI({
	  checkboxGroupButtons(inputId = ns("cancerscreen_chosen"), label = "Cancer Screen items shown",
	                       choices = cancerscreen_names, selected = cancerscreen_names,
	                       # all choices initially selected
	                       status = "primary",
	                       checkIcon = list(yes = icon("ok", lib = "glyphicon")))
	})

	cancerscreen_selected <- reactiveVal(cancerscreen_names)
	# use instead of input$cancerscreen_chosen directly because
	# input$cancerscreen_chosen is not defined until the dropdown button is selected!
	observeEvent(input$cancerscreen_chosen, ignoreNULL = FALSE, ignoreInit = TRUE, {
		# cannot ignoreNULL because all  items could be de-selected
		# ignoreInit is true because input$cancerscreen_chosen is not defined initially
		cancerscreen_selected(input$cancerscreen_chosen)
	})

	cancerscreen_list <- reactiveVal(NULL)

	observeEvent(c(appointments_list(), cancerscreen_selected()), {
	  validate(
	    need(appointments_list(), "No appointments in chosen range"),
	    need(nrow(appointments_list())>0, "No appointments in chosen range")
	  )

	  screenlist <- NULL
	  # Zostavax (herpes zoster 'shingles' vaccine)
	  if ("Bowel cancer" %in% cancerscreen_selected())
	  {screenlist <- rbind(screenlist, fobt_list(appointments_list, emrpool))}

	  if (is.null(screenlist)) {
	    cancerscreen_list(NULL)
	  } else {
	    cancerscreen_list(screenlist %>%
	               group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
	                        DOB, Age) %>%
	               # gathers vaccination notifications on the same appointment into a single row
	               summarise(screentag = paste(screentag, collapse = ""),
	                         screentag_print = paste(screentag_print, collapse = ", ")) %>%
	               ungroup())
	  }
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

	output$cancerscreen_table <- renderDT({
	  styled_cancerscreen_list()
	})
}
