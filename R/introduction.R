# steps for rintrojs

steps_choose_provider_date_df <- function() {

  steps_df <-
    data.frame(element = "#rightsidebar-appointment-wrapper",
               intro = paste(shiny::tags$h5(shiny::icon("users"),
                                            " Appointments"),
                             shiny::br(),
                             "Choose providers who will be seen in",
                             "appointments lists.",
                             shiny::br(), shiny::br(),
                             "Provider list can be restricted by",
                             "practice location.",
                             shiny::br(), shiny::br(),
                             "Providers can be",
                             "assigned practice location(s) in",
                             shiny::icon("wrench"),
                             "Configuration - User Settings and Permissions.",
                             shiny::br(),
                             "Practice location(s) can be created/modified",
                             "in", shiny::icon("wrench"),
                             "Configuration - Practice Locations/Groups."),
               position = "left") %>>%
    rbind(data.frame(element = "#rightsidebar-date-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("calendar-alt"),
                                                  " Date selection"),
                                   shiny::br(),
                                   "Choose date range."),
                     position = "left"))
  return(steps_df)
}

steps_appointment_df <- function() {
  steps_df <-
    data.frame(element = as.character(NA),
               intro = c(paste(shiny::tags$h4("GPstat! Appointments"),
                               shiny::br(),
                               "View appointment list/status for chosen clinicians and dates.")),
               position = "auto",
               stringsAsFactors = FALSE) %>>%
    rbind(steps_choose_provider_date_df()) %>>%
    rbind(data.frame(element = "#appointments_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Appointments view"),
                                     shiny::br(),
                                     "List of appointments, according to currently selected clinicians and dates.",
                                     shiny::br(), shiny::br(),
                                     "By default shows : ", shiny::strong("Patient"), "(name),",
                                     shiny::strong("Appointment Date"), ", ",
                                     shiny::strong("Appointment Time"), ", ",
                                     shiny::strong("Provider"), "(clinician), Appointment",
                                     shiny::strong("Status"))),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#appointments_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Column visibility"),
                                     shiny::br(),
                                     "You can show/hide columns!", shiny::br(), shiny::br(),
                                     "Click the 'Column visibility'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can even try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#appointments_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Copy"),
                                     shiny::br(),
                                     "You can copy the table", shiny::br(),
                                     "into the clipboard.", shiny::br(), shiny::br(),
                                     "Click the 'Copy'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#appointments_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Print"),
                                     shiny::br(),
                                     "You can print the table.", shiny::br(), shiny::br(),
                                     "Click the 'Print'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#appointments_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Download/Export"),
                                     shiny::br(),
                                     "You can download the table", shiny::br(),
                                     "to Excel, PDF or 'CSV'.", shiny::br(), shiny::br(),
                                     "Click the 'Download'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "Note that 'Column Visibility'", shiny::br(),
                                     "changes which columns are", shiny::br(),
                                     "downloaded/exported.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#appointments_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Search"),
                                     shiny::br(),
                                     "You can search the table", shiny::br(),
                                     "for names, numbers etc..", shiny::br(),shiny::br(),
                                     "Use the 'Search'", shiny::br(),
                                     "dialog at the top-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto"))

  return(steps_df)
}

steps_immunization_df <- function() {
  steps_df <-
    data.frame(element = as.character(NA),
               intro = c(paste(shiny::tags$h4("GPstat! Immunization"),
                               shiny::br(),
                               "View immunization opportunities for chosen clinicians and dates.",
                               shiny::br(),
                               "Immunization opportunities can be",
                               "viewed for (usuallly future)",
                               "Appointments", shiny::icon("calendar-alt"), ",",
                               "or historical Contacts", shiny::icon("handshake"), ".")),
               position = "auto",
               stringsAsFactors = FALSE) %>>%
    rbind(steps_choose_provider_date_df()) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Appointments view"),
                                     shiny::br(),
                                     "List of immunization opportunities,",
                                     "according to currently selected clinicians, dates,",
                                     "and Appointment/Contact view.",
                                     shiny::br(), shiny::br(),
                                     "By default shows : ", shiny::strong("Patient"), "(name),",
                                     shiny::strong("Appointment Date"), ", ",
                                     shiny::strong("Appointment Time"), ", ",
                                     shiny::strong("Provider"), "(clinician), Appointment",
                                     shiny::strong("Status"))),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Column visibility"),
                                     shiny::br(),
                                     "You can show/hide columns!", shiny::br(), shiny::br(),
                                     "Click the 'Column visibility'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can even try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Copy"),
                                     shiny::br(),
                                     "You can copy the table", shiny::br(),
                                     "into the clipboard.", shiny::br(), shiny::br(),
                                     "Click the 'Copy'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Print"),
                                     shiny::br(),
                                     "You can print the table.", shiny::br(), shiny::br(),
                                     "Click the 'Print'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Download/Export"),
                                     shiny::br(),
                                     "You can download the table", shiny::br(),
                                     "to Excel, PDF or 'CSV'.", shiny::br(), shiny::br(),
                                     "Click the 'Download'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "Note that 'Column Visibility'", shiny::br(),
                                     "changes which columns are", shiny::br(),
                                     "downloaded/exported.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Search"),
                                     shiny::br(),
                                     "You can search the table", shiny::br(),
                                     "for names, numbers etc..", shiny::br(),shiny::br(),
                                     "Use the 'Search'", shiny::br(),
                                     "dialog at the top-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto"))

  return(steps_df)
}

steps_overview_df <- function() {
  steps_df <-
    data.frame(element = as.character(NA),
               intro = c(paste(shiny::tags$h4("GPstat! introduction"),
                               shiny::br(),
                               "A 'near-future' tool to opportunistically find opportunities",
                               "for screening and chronic disease management.")),
               position = "auto") %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = "Main menu",
                     position = "auto")) %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("calendar-check"),
                                                  " Appointments"),
                                   shiny::br(),
                                   "List of appointments with currently selected",
                                   "provider(s) and selected date range."),
                     position = "auto")) %>>%
    rbind(steps_choose_provider_date_df()) %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("syringe"),
                                                  " Immunization"),
                                   shiny::br(),
                                   "Immunization opportunities in currently list of",
                                   "appointments."),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("x-ray"),
                                                  " Cancer screening"),
                                   shiny::br(),
                                   "Cancer screening opportunities in current list of",
                                   "of appointments."),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("fingerprint"),
                                                  " Conditions"),
                                   shiny::br(),
                                   "Condition-specific review e.g. post-natal."),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("microscope"),
                                                  " Administration"),
                                   shiny::br(),
                                   "Allergy, family history recording. Result management."),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("wrench"),
                                                  " Configuration"),
                                   shiny::br(),
                                   "Best Practice server configuration.",
                                   "Configuration file location.",
                                   "User and subscription management. Log files."),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#sidebarMenu-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("info"),
                                                  " About"),
                                   shiny::br(),
                                   "Video tutorials and other documentation.",
                                   "Privacy statement. Licenses. Credits"),
                     position = "auto"))

  return(steps_df)
}