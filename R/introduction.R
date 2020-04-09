# steps for rintrojs

#' rintrojs steps for choosing provider and date
#'
#' @title steps_choose_clinician_date_df
#' @description returns a dataframe of rintrojs steps
#' @export
steps_choose_clinician_date_df <- function() {

  steps_df <-
    data.frame(element = "#rightsidebar-appointment-wrapper",
               intro = paste(shiny::tags$h5(shiny::icon("users"),
                                            " Appointments"),
                             shiny::br(),
                             "Choose clinicians who will be seen in",
                             "appointment or contact lists.",
                             shiny::br(), shiny::br(),
                             "Clinician list can be restricted by",
                             "practice location.",
                             shiny::br(), shiny::br(),
                             "Clinicians can be",
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

#' rintrojs steps for choosing contact specificatioin
#'
#' @title steps_choose_contact_details_df
#' @description returns a dataframe of rintrojs steps
#' @export
steps_choose_contact_details_df <- function() {
  steps_df <- data.frame(element = "#contact_type-wrapper",
                         intro = paste(shiny::tags$h5(shiny::icon("handshake"),
                                                      " Contact type"),
                                       shiny::br(),
                                       "Choose types of contact which are used in ",
                                       "'Contact view' or 'PIP Quality Improvement",
                                       shiny::br(), shiny::br(),
                                       "Choose from 'Appointment' (entry in appointment book),",
                                       "Visits (recording in progress notes) and",
                                       "Services (billings)."),
                         position = "left") %>>%
    rbind(data.frame(element = "#contact_type-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("handshake"),
                                                  " Minimum number of oontacts"),
                                   shiny::br(),
                                   "Minimum number of contacts (within the specified time period",
                                   "to be considered in the 'contacted list'.",
                                   shiny::br(), shiny::br(),
                                   "This can be used to define an 'active' patient e.g. three",
                                   "contacts within the last two years.",
                                   shiny::br(), shiny::br(),
                                   "The time period is specified with the '",
                                   shiny::icon("calendar-alt"), "Date Range' tab in",
                                   "this right side-bar."),
                     position = "left")) %>>%
    rbind(data.frame(element = "#appointment_visit-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("handshake"),
                                                  " Appointment status shown"),
                                   shiny::br(),
                                   "If 'Appointments' is chosen as a type of contact",
                                   "then choose the types of appointment status which",
                                   "are considered valid contacts.",
                                   shiny::br(), shiny::br(),
                                   "Options include 'Booked', 'Waiting', 'With Doctor',",
                                   "'At Billing', 'Invoiced' and 'Completed'.",
                                   shiny::br(), shiny::br(),
                                   "By default, 'Booked' and 'Waiting' are not considered",
                                   "valid contacts."),
                     position = "left")) %>>%
    rbind(data.frame(element = "#appointment_visit-wrapper",
                     intro = paste(shiny::tags$h5(shiny::icon("handshake"),
                                                  " Visit types shown"),
                                   shiny::br(),
                                   "If 'Visits' is chosen as a type of contact",
                                   "then choose the types of visits which",
                                   "are considered valid contacts.",
                                   shiny::br(), shiny::br(),
                                   "Options are numerous, and include 'Surgery', 'Home', 'Non Visit',",
                                   "'Telephone', 'SMS', 'Email', 'Telehealth' etc. etc.",
                                   shiny::br(), shiny::br(),
                                   "The default does", shiny::tags$em("not"), "include 'Non Visit' and",
                                   "'Telephone', but does include 'Telehealth'."),
                     position = "left"))
  return(steps_df)
}

#' rintrojs steps for describing datatable view helpers
#'
#' @title steps_datatable_helpers
#' @description returns a dataframe of rintrojs steps
#'
#' steps for a typical datatable display which includes
#' 1. column visibility
#' 2. print/copy view
#' 3. Copy button (only print/copy view)
#' 4. Download/export button (only print/copy view)
#' 5. search dialog
#'
#' @param element_name name of the highlighted table element (with 'extra' print/copy view toggle)
#' @export
steps_datatable_helpers <- function(element_name) {

  steps_df <- data.frame(element = element_name,
                         intro = c(paste(shiny::tags$h4("Column visibility"),
                                         shiny::br(),
                                         "You can show/hide columns!", shiny::br(), shiny::br(),
                                         "Click the 'Column visibility'", shiny::br(),
                                         "button at the bottom-right", shiny::br(),
                                         "of this table view.", shiny::br(), shiny::br(),
                                         "You can even try it now",
                                         emo::ji("smile"), "!")),
                         position = "auto") %>>%
    rbind(data.frame(element = element_name,
                     intro = c(paste(shiny::tags$h4("Print and Copy View"),
                                     shiny::br(),
                                     shiny::icon("print"), shiny::icon("copy"), shiny::br(),
                                     "Top-left of the table view.",
                                     shiny::br(), shiny::br(),
                                     "Enables copying, printing and", shiny::br(),
                                     "download/export", shiny::br(),
                                     "to Excel/PDF/'CSV'.", shiny::br(), shiny::br(),
                                     "Copy/Print/Download buttons", shiny::br(),
                                     "will be visible at the bottom-", shiny::br(),
                                     "right if 'Print and Copy'", shiny::br(),
                                     "is turned ON.", shiny::br(), shiny::br(),
                                     "You can even try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = element_name,
                     intro = c(paste(shiny::tags$h4("Copy"),
                                     shiny::br(),
                                     shiny::tags$em("(", shiny::icon("print"), shiny::icon("copy"),
                                                    "Print and Copy view only)"),
                                     shiny::br(), shiny::br(),
                                     "You can copy the table", shiny::br(),
                                     "into the clipboard.", shiny::br(), shiny::br(),
                                     "Click the 'Copy'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!",
                                     shiny::br(), shiny::br(),
                                     shiny::tags$em("(", shiny::icon("print"), shiny::icon("copy"),
                                                    "Print and Copy view only)"))),
                     position = "auto")) %>>%
    rbind(data.frame(element = element_name,
                     intro = c(paste(shiny::tags$h4("Print"),
                                     shiny::br(),
                                     shiny::tags$em("(", shiny::icon("print"), shiny::icon("copy"),
                                                    "Print and Copy view only)"),
                                     shiny::br(), shiny::br(),
                                     "You can print the table.", shiny::br(), shiny::br(),
                                     "Click the 'Print'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!",
                                     shiny::br(), shiny::br(),
                                     shiny::tags$em("(", shiny::icon("print"), shiny::icon("copy"),
                                                    "Print and Copy view only)"))),
                     position = "auto")) %>>%
    rbind(data.frame(element = element_name,
                     intro = c(paste(shiny::tags$h4("Download/Export"),
                                     shiny::br(),
                                     shiny::tags$em("(", shiny::icon("print"), shiny::icon("copy"),
                                                    "Print and Copy view only)"),
                                     shiny::br(), shiny::br(),
                                     "You can download the table", shiny::br(),
                                     "to Excel, PDF or 'CSV'.", shiny::br(), shiny::br(),
                                     "Click the 'Download'", shiny::br(),
                                     "button at the bottom-right", shiny::br(),
                                     "of this table view.", shiny::br(), shiny::br(),
                                     "Note that 'Column Visibility'", shiny::br(),
                                     "changes which columns are", shiny::br(),
                                     "downloaded/exported.", shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!",
                                     shiny::br(), shiny::br(),
                                     shiny::tags$em("(", shiny::icon("print"), shiny::icon("copy"),
                                                    "Print and Copy view only)"))),
                     position = "auto")) %>>%
    rbind(data.frame(element = element_name,
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

steps_appointment_df <- function() {
  steps_df <-
    data.frame(element = as.character(NA),
               intro = c(paste(shiny::tags$h4("GPstat! Appointments"),
                               shiny::br(),
                               "View appointment list/status for chosen clinicians and dates.")),
               position = "auto",
               stringsAsFactors = FALSE) %>>%
    rbind(steps_choose_clinician_date_df()) %>>%
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
                               shiny::br(), shiny::br(),
                               "Immunization opportunities can be",
                               "viewed for (usually future)",
                               "Appointments", shiny::icon("calendar-alt"), ",",
                               "or historical Contacts", shiny::icon("handshake"), ".")),
               position = "auto",
               stringsAsFactors = FALSE) %>>%
    rbind(steps_choose_clinician_date_df()) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Immunizations view"),
                                     shiny::br(),
                                     "List of immunization opportunities,",
                                     "according to currently selected clinicians, dates,",
                                     "and Appointment/Contact view.",
                                     shiny::br(), shiny::br(),
                                     "By default shows : ", shiny::strong("Patient"), "(name),",
                                     shiny::strong("DOB/Age"), " and ",
                                     shiny::strong("Vaccination"), " list.",
                                     shiny::br(), shiny::br(),
                                     "In Appointments", shiny::icon("calendar-alt"), "view also",
                                     "shows Appointment details e.g.", shiny::strong("AppointmentTime"),
                                     "and", shiny::strong("Provider"), " (clinician).",
                                     shiny::br(), shiny::br(),
                                     "In Contacts", shiny::icon("handshake"), "view shows",
                                     shiny::strong("ExternalID"), "(Patient ID), and ",
                                     shiny::strong("telephone contact details"))),
                     position = "auto")) %>>%
    rbind(steps_datatable_helpers("#immunization_datatable_wrapper")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Vaccination items shown"),
                                     shiny::br(),
                                     shiny::icon("gear"), shiny::br(),
                                     "Top-right of the table view.",
                                     shiny::br(), shiny::br(),
                                     "Choose vaccines to display.",
                                     shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Include up-to-date"),
                                     shiny::br(),
                                     "Top of the table view.",
                                     shiny::br(), shiny::br(),
                                     "Include vaccines which are",
                                     "'up-to-date' i.e. given previously",
                                     "within the recommended timeframe.",
                                     shiny::br(), shiny::br(),
                                     "You can try it now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(data.frame(element = "#immunization_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Appointment/Contact view"),
                                     shiny::br(),
                                     shiny::icon("calendar-alt"), shiny::icon("handshake"), shiny::br(),
                                     "Top/middle of the table view.",
                                     shiny::br(), shiny::br(),
                                     "Immunization opportunities can be",
                                     "viewed for (usually future)",
                                     "Appointments", shiny::icon("calendar-alt"), ",",
                                     "or historical Contacts", shiny::icon("handshake"), ".",
                                     shiny::br(), shiny::br(),
                                     "If using Contacts", shiny::icon("handshake"),"view",
                                     "then the", shiny::tags$em("types"), "of contact e.g.",
                                     "Appointment-book, Visits (progress notes) and/or Services (billings)",
                                     "and", shiny::tags$em("minimum number"), "of contact (within the",
                                     "specified time period) is chosen with the", shiny::tags$em("Contact details"),
                                     shiny::icon("handshake"), "tab in the right side-bar.",
                                     shiny::br(), shiny::br(),
                                     "You can try choosing between",
                                     shiny::icon("calendar-alt"), "Appointment view and",
                                     shiny::icon("handshake"), "Contact view now",
                                     emo::ji("smile"), "!")),
                     position = "auto")) %>>%
    rbind(steps_choose_contact_details_df())

  return(steps_df)
}

steps_cancerscreen_df <- function() {
  steps_df <-
    data.frame(element = as.character(NA),
               intro = c(paste(shiny::tags$h4("GPstat! Cancer screening"),
                               shiny::br(),
                               "View cancer screening opportunities for chosen clinicians and dates.",
                               shiny::br(), shiny::br(),
                               "Immunization opportunities can be",
                               "viewed for (usually future)",
                               "Appointments.")),
               position = "auto",
               stringsAsFactors = FALSE) %>>%
    rbind(steps_choose_clinician_date_df()) %>>%
    rbind(data.frame(element = "#cancerscreen_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Cancer screening view"),
                                     shiny::br(),
                                     "List of cancer screening opportunities,",
                                     "according to currently selected clinicians and dates.",
                                     shiny::br(), shiny::br(),
                                     "By default shows : ", shiny::strong("Patient"), "(name),",
                                     shiny::strong("DOB/Age"), " and ",
                                     shiny::strong("Screening"), " list.",
                                     shiny::br(), shiny::br(),
                                     "Also",
                                     "shows Appointment details e.g.", shiny::strong("AppointmentTime"),
                                     "and", shiny::strong("Provider"), " (clinician).")),
                     position = "auto")) %>>%
    rbind(steps_datatable_helpers("#cancerscreen_datatable_wrapper")) %>>%
    rbind(data.frame(element = "#cancerscreen_datatable_wrapper",
                     intro = c(paste(shiny::tags$h4("Cancer screen items shown"),
                                     shiny::br(),
                                     shiny::icon("gear"), shiny::br(),
                                     "Top-right of the table view.",
                                     shiny::br(), shiny::br(),
                                     "Choose cancer screening items to display.",
                                     shiny::br(), shiny::br(),
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
    rbind(steps_choose_clinician_date_df()) %>>%
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