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
      shiny::column(
        4,
        shinyWidgets::switchInput(
          inputId = ns("printcopy_view"),
          label = paste(
            "<i class=\"fas fa-print\"></i>",
            "<i class=\"far fa-copy\"></i>",
            "  Print and Copy View"
          ),
          labelWidth = "12em",
          width = "20em"
        )
      ),
      shiny::column(2,
        offset = 2,
        shiny::uiOutput(ns("billing_options"))
      ),
      shiny::column(3,
        offset = 1, # note that total 'column' width = 12
        shinyWidgets::switchInput(
          inputId = ns("allbillings_view"),
          label = paste("Show all day's billings"),
          labelWidth = "12em",
          width = "20em"
        )
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("billings_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL
    )
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
#' @param dMBillings dMeasureBillings R6 object
#'
#' @return none
#'
billings_datatable <- function(input, output, session, dMBillings) {
  ns <- session$ns

  output$billing_options <-  shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = ns("billing_options_dropdown"),
      icon = icon("gear"),
      label = "Inclusions/Exclusions",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("billing_options_chosen"),
        label = "Billing Options",
        choices = c("COVID-19 Bulk Billing Incentive"),
        selected = c("COVID-19 Bulk Billing Incentive"),
        # initially all chosen, which includes choices to
        #  'include' ATSI 35-44 years,
        # and 'exclude'
        #  those with known cardiovascular disease and age 75
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    )
  })

  billings_list <- shiny::eventReactive(
    c(
      dMBillings$billings_listR(),
      input$printcopy_view
    ), {
      shiny::validate(
        shiny::need(
          dMBillings$billings_listR(),
          "No appointments in chosen range"
        ),
        shiny::need(
          nrow(dMBillings$billings_listR()) > 0,
          "No appointments in chosen range"
        )
      )

      billingslist <- dMBillings$list_billings(
        lazy = TRUE,
        screentag = !input$printcopy_view,
        screentag_print = input$printcopy_view,
        rawbilling = TRUE
        # the 'raw' billings will be in the 'billings' column
      )

      return(billingslist)
    }
  )

  shiny::observeEvent(input$allbillings_view, ignoreNULL = TRUE, {
    dMBillings$own_billings <- !input$allbillings_view
  })

  check_for_covid19_bulkbilling <- function(InternalID, DOB, Date, Age, billings) {

    if (is.null(billings)) {
      return(as.character(NA))
      # wasn't billed, so can't add a bulk-billing incentive!
    }

    if (any(c(10990, 10991, 10981, 10982) %in% billings)) {
      return(as.character(NA))
      # already charged a bulk-billing incentive
    }

    if (Age >= 70) {
      return("Age 70+")
    }
    intID_Date <- data.frame(InternalID = InternalID, Date = Date)
    if (Age >= 50 &&
        length(dMBillings$dM$atsi_list(intID_Date))) {
      return("ATSI 50+")
    }
    if (length(dMBillings$dM$pregnant_list(intID_Date))) {
      return("Pregnant")
    }
    if (nrow(dMBillings$dM$postnatal_list(intID_Date,
      include_edc = TRUE, # 'guess' delivery of no known result
      days_min = 0, days_max = 365,
      outcome = c(0,1)))) { # unknown result or live birth
      # $postnatal_list returns a dataframe, not a vector
      return("Parent of child less than 12 months")
    }
    if (length(dMBillings$dM$diabetes_list(intID_Date))) {
      return("Diabetes")
    }
    if (length(dMBillings$dM$asthma_list(intID_Date))) {
      return("Asthma")
    }
    if (length(dMBillings$dM$hiv_list(intID_Date))) {
      return("HIV")
    }
    if (length(dMBillings$dM$malignancy_list(intID_Date))) {
      return("Malignancy")
    }
    if (length(dMBillings$dM$haemoglobinopathy_list(intID_Date))) {
      return("Haemoglobinopathy")
    }
    if (length(dMBillings$dM$asplenic_list(intID_Date))) {
      return("Asplenia")
    }
    if (length(dMBillings$dM$transplant_list(intID_Date))) {
      return("Transplant recipient")
    }
    if (length(dMBillings$dM$cardiacdisease_list(intID_Date))) {
      return("Heart disease")
    }
    if (length(dMBillings$dM$chroniclungdisease_list(intID_Date))) {
      return("Chronic lung disease")
    }
    if (length(dMBillings$dM$chronicliverdisease_list(intID_Date))) {
      return("Chronic liver disease")
    }
    if (length(dMBillings$dM$neurologic_list(intID_Date))) {
      return("Neurological disease")
    }
    if (length(dMBillings$dM$chronicrenaldisease_list(intID_Date))) {
      return("Chronic renal disease")
    }
    if (length(dMBillings$dM$bmi30_list(intID_Date))) {
      return("BMI>30")
    }

    # no conditions found
    return(as.character(NA))
  }

  styled_billings_list <- shiny::reactive({
    shiny::validate(
      shiny::need(billings_list(), "No appointments in selected range")
    )

    billings <- billings_list()

    if ("COVID-19 Bulk Billing Incentive" %in% input$billing_options_chosen) {
      billings <- billings %>>%
        dplyr::mutate(covid19bb = mapply(
          check_for_covid19_bulkbilling,
          InternalID, DOB, Date, Age, billings
        ))

      if (input$printcopy_view == TRUE) {
        billings <- billings %>>%
          dplyr::mutate(
            billingtag_print = dplyr::if_else(
              is.na(covid19bb),
              billingtag_print,
              paste(
                billingtag_print,
                paste0(", COVID-19 opportunity [", covid19bb, "]")
              )
            )
          )
      } else {
        billings <- billings %>>%
          dplyr::mutate(
            billingtag = dplyr::if_else(
              is.na(covid19bb),
              billingtag,
              paste0(
                billingtag,
                dMeasure::semantic_button(
                  "COVID-19 BB",
                  colour = "pink",
                  popuphtml = paste0(
                    "<p><font size = \'+0\'>",
                    covid19bb,
                    "</p>"
                  )
                )
              )
            )
          )
      }
    }

    if (input$printcopy_view == TRUE) {
      # printable/copyable view
      datatable_styled(billings %>>%
        dplyr::select(
          Patient, Date, AppointmentTime, Status, VisitType,
          Provider, billingtag_print
        ),
      colnames = c("Billings" = "billingtag_print")
      )
    } else {
      # fomantic/semantic tag view
      datatable_styled(billings %>%
        dplyr::select(
          Patient, Date, AppointmentTime, Status, VisitType,
          Provider, billingtag
        ),
      escape = c(5),
      copyHtml5 = NULL, printButton = NULL, # no copy/print buttons
      downloadButton = NULL,
      colnames = c("Billings" = "billingtag")
      )
    }
  })

  output$billings_table <- DT::renderDT({
    styled_billings_list()
  })
}
