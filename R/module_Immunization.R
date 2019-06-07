#! Immunization module

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
             uiOutput(ns("vax_item_choice"))
      )
    ),
    withSpinner(DT::DTOutput(ns("vax_table")),
                type = 8,
                hide.element.when.recalculating = FALSE,
                proxy.height = NULL)
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
                filter(ITEMID == 15), by = "InternalID",
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
                            if_else(is.na(ITEMID),
                                    "(DUE) (Age 70 to 79 years)",
                                    "(Removed from herpes zoster immunization reminders)"),
                            paste0("(Given : ", format(GivenDate), ")"))
             )
    ) %>%
    select(c("Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
             "DOB", "Age", "vaxtag", "vaxtag_print"))
}

influenza_list <- function(appointments_list, db,
                           diabetes_list, asthma_list,
                           atsi_list,
                           malignancy_list, hiv_list,
                           haemoglobinopathy_list, asplenic_list,
                           transplant_list, cardiacdisease_list, trisomy21_list,
                           bmi30_list, chroniclungdisease_list, neurologic_list,
                           chronicliverdisease_list, chronicrenaldisease_list,
                           pregnant_list) {
  # return datatable of appointments where influenza is recommended (might already be given)
  #  Patient, InternalID, AppointmentDate, ApppointmentTime, Provider, DOB, Age
  #  vaxtag, vaxtag_print (these two are the 'semantic' tags and printable tags)
  # input - appointment_list - reactive of appointment list
  # input - db - access to Best Practice EMR database
  # input - reactive - vector of patients with diabetes, asthma, who are ATSI
  # input - various immune compromising conditions
  # input - pregnant_list

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

  lprematurity <- appointments_list() %>%
    # pre-term infants
    mutate(AgeInMonths = calc_age_months(DOB, AppointmentDate)) %>%
    filter(AgeInMonths >= 6 & AgeInMonths < 24) %>%
    filter(InternalID %in%
             (db$history %>% filter(ConditionID == 2973) %>%
                pull(InternalID))) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Premature infant (if <37 weeks gestation)") %>%
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

  lmalignancy <- appointments_list() %>%
    filter(InternalID %in% malignancy_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Malignancy")

  lhiv <- appointments_list() %>%
    filter(InternalID %in% hiv_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "HIV")

  lhaemoglobinopathy <- appointments_list() %>%
    filter(InternalID %in% haemoglobinopathy_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Haemoglobinopathy")

  lasplenic <- appointments_list() %>%
    filter(InternalID %in% asplenic_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Asplenia")

  ltransplant <- appointments_list() %>%
    filter(InternalID %in% transplant_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Transplant recipient")

  lcardiac <- appointments_list() %>%
    filter(InternalID %in% cardiacdisease_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Heart disease")

  lbmi30 <- appointments_list() %>%
    filter(InternalID %in% bmi30_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "BMI>30")

  lchroniclung <- appointments_list() %>%
    filter(InternalID %in% chroniclungdisease_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Chronic lung disease")

  lneurology <- appointments_list() %>%
    filter(InternalID %in% neurologic_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Neurological disease")

  lchronicliver <- appointments_list() %>%
    filter(InternalID %in% chronicliverdisease_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Chronic liver disease")

  lrenaldisease <- appointments_list() %>%
    filter(InternalID %in% chronicrenaldisease_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "BMI>30")

  lchildaspirin <- appointments_list() %>%
    # children aged 6 months to 10 years on long-term aspirin
    # risk of Reye's syndrome after influenza infection
    mutate(AgeInMonths = calc_age_months(DOB, AppointmentDate)) %>%
    filter(AgeInMonths >= 6 & AgeInMonths <= 131) %>%
    filter(InternalID %in%
             (db$currentrx %>%
                filter(RXSTATUS == 1 & PRODUCTID %in%
                         c(99,8489,222,522,534,12254,545,546,547,549,548,550,554,551,552,553,555,
                           8726,11362,540,8060,8062,8061,8063,8064,541,8304,560,559,558,562,563,8071,
                           710,13262,1131,1148,11361,11327,1612,1613,1614,1619,11360,1917,16891,2328,
                           2340,2341,2342,2345,2344,11326,14681,2523,3531,16877,6827,6918,12519,
                           7651,7704)) %>%
                # RXSTATUS == 1 (long-term medication), many aspirin productIDs!
                pull(InternalID))
    ) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Child aged 6 months to 10 years on long-term aspirin") %>%
    select(c("Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
             "DOB", "Age",
             "GivenDate", "Reason"))

  lpregnant <- appointments_list() %>%
    filter(InternalID %in% pregnant_list()) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Pregnancy")

  lhomeless <- appointments_list() %>%
    # homeless infants
    filter(InternalID %in%
             (db$history %>% filter(ConditionID == 3017 & Status == "Active") %>%
                pull(InternalID))) %>%
    mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
           Reason = "Homeless")

  l <- rbind(lprevious, l65, l5, lprematurity, latsi, ldiabetes, lasthma,
             lmalignancy, lhiv, lhaemoglobinopathy, lasplenic, ltransplant,
             lcardiac, lbmi30, lchroniclung, lneurology, lrenaldisease, lchronicliver,
             lchildaspirin, lpregnant, lhomeless) %>%
    group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider, DOB, Age) %>%
    summarise(GivenDate = max(GivenDate),
              Reason = paste0(Reason, collapse = ", ")) %>% # join unique Reasons together
    ungroup() %>%
    left_join(db$preventive_health %>%
                # those who have been removed from the reminder system for influenza
                filter(ITEMID == 1), by = "InternalID",
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
                            paste0(if_else(is.na(GivenDate) | (GivenDate == -Inf), # no previous vax
                                           " (DUE) ",
                                           if_else(year(GivenDate) == year(AppointmentDate),
                                                   " ", " (DUE) ")),
                                   "(", Reason, ")"
                            )) # ?vax given this year
             )
    ) %>%
    select(c("Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
             "DOB", "Age", "vaxtag", "vaxtag_print"))

  return(l)
}

vax_datatable <- function(input, output, session,
                          appointments_list, db,
                          diabetes_list, asthma_list,
                          atsi_list,
                          malignancy_list, hiv_list, haemoglobinopathy_list, asplenic_list,
                          transplant_list, cardiacdisease_list, trisomy21_list,
                          bmi30_list, chroniclungdisease_list, neurologic_list,
                          chronicliverdisease_list, chronicrenaldisease_list,
                          pregnant_list) {
  # vaccinations done, pending or never done for appointment list
  # input - input, output, session (as required by modules)
  # input - appointments_list - reactive. same as appointments_filtered_time, but with DOB and Age added
  # input - db - EMR database
  # input - diabetes_list, asthma_list - condition lists
  # input - atsi_list - Aboriginal or Torres Strait islander
  # input - malignancy, hiv, haemoglobinopathy, asplenic - various immunocompromising conditions
  # input - transplant_list - those who have had transplants
  # output - none
  ns <- session$ns

  vax_names <- c("Zostavax", "Influenza")

  output$vax_item_choice <- renderUI({
    dropdown(
      inputid = "choice_dropdown",
      checkboxGroupButtons(inputId = ns("vax_chosen"), label = "Vaccination items shown",
                           choices = vax_names, selected = vax_names,
                           # all choices initially selected
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
      icon = icon("gear"),
      label = "Vaccination items shown"
    )
  })

  vax_list <- reactiveVal(NULL)

  vax_list <- reactive({
    validate(
      need(appointments_list(), "No appointments in chosen range"),
      need(nrow(appointments_list())>0, "No appointments in chosen range")
    )

    vlist <- NULL
    # Zostavax (herpes zoster 'shingles' vaccine)
    if ("Zostavax" %in% input$vax_chosen)
    {vlist <- rbind(vlist, zostavax_list(appointments_list, db))}
    # influenza
    if ("Influenza" %in% input$vax_chosen)
    {vlist <- rbind(vlist, influenza_list(appointments_list, db,
                                          diabetes_list, asthma_list,
                                          atsi_list,
                                          malignancy_list, hiv_list,
                                          haemoglobinopathy_list, asplenic_list,
                                          transplant_list, cardiacdisease_list, trisomy21_list,
                                          bmi30_list, chroniclungdisease_list, neurologic_list,
                                          chronicliverdisease_list, chronicrenaldisease_list,
                                          pregnant_list))}

    if (!is.null(vlist)) {
      vlist <- vlist %>%
                 group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
                          DOB, Age) %>%
                 # gathers vaccination notifications on the same appointment into a single row
                 summarise(vaxtag = paste(vaxtag, collapse = ""),
                           vaxtag_print = paste(vaxtag_print, collapse = ", ")) %>%
                 ungroup()
    }
    vlist
  })

  styled_vax_list <- reactive({
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
                       colnames = c('Vaccination' = 'vaxtag_print'))
    } else {
      # fomantic/semantic tag view
      datatable_styled(vax_list() %>%
                         select(c('Patient', 'AppointmentDate', 'AppointmentTime',
                                  'Provider', 'DOB', 'Age', 'vaxtag')),
                       escape = c(7),
                       dom = 'frltip', # no copy/print buttons
                       colnames = c('Vaccination' = 'vaxtag'))
    }
  })

  output$vax_table <- renderDT({
    styled_vax_list()
  })
}
