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
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ), # make the horizontal rule darker!
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
        offset = 3,
        shiny::uiOutput(ns("billing_options"))
      ),
      shiny::column(3,
        offset = 0, # note that total 'column' width = 12
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

  output$billing_options <- shiny::renderUI({
    shinyWidgets::dropdown(
      input_id = ns("billing_options_dropdown"),
      icon = icon("gear"),
      label = "Billing options",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("billing_reminders_chosen"),
        label = "Billing Reminders",
        choices = c("COVID-19 Bulk Billing Incentive"),
        selected = c("COVID-19 Bulk Billing Incentive"),
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),
      shiny::hr(),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("billing_types_viewed"),
        label = "Billing types viewed",
        choices = c("Direct 'bulk' billing", "DVA", "WorkCover", "Other"),
        selected = c("Direct 'bulk' billing", "DVA", "WorkCover", "Other"),
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    )
  })

  payerCodePrevious <- 0:8
  # a 'semi-global' containing current billings types viewed
  # (all of them) in input$billing_types_viewed
  # used in 'billings_list' reactive to check if there is
  # a change in the input

  billings_list <- shiny::eventReactive(
    c(
      dMBillings$billings_listR(),
      input$billing_types_viewed,
      input$billing_reminders_chosen
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

      # returns list_billings
      # filtered by payerCode (e.g. bulk billing, DVA, WorkCover)
      # adds covid19bb if enabled, TRUE is possible bulk-bill incentive opportunity

      payerCode <- c()
      if ("Direct 'bulk' billing" %in% input$billing_types_viewed) {
        payerCode <- c(payerCode, 2)
      }
      if ("DVA" %in% input$billing_types_viewed) {
        payerCode <- c(payerCode, 3)
      }
      if ("WorkCover" %in% input$billing_types_viewed) {
        payerCode <- c(payerCode, 4)
      }
      if ("Other" %in% input$billing_types_viewed) {
        payerCode <- c(payerCode, 0, 1, 5:8)
      }
      if (length(payerCode) == length(payerCodePrevious) &&
        all(payerCode == payerCodePrevious)) {
        # need to compare length as well as each individual element
        # as there is a possibility of 're-cycling'
        # https://stackoverflow.com/questions/10374932/comparing-two-vectors-in-an-if-statement
        lazy <- TRUE
        # don't need to force re-calculation of billings list
        # as reactives have already taken care of recalculation
      } else {
        lazy <- FALSE
        # need to force a re-calculation of billings list
        # as payerCode is currently not a reactive
        payerCodePrevious <- payerCode
      }

      billingslist <- dMBillings$list_billings(
        lazy = lazy,
        payerCode = payerCode
      )

      if ("COVID-19 Bulk Billing Incentive" %in% input$billing_reminders_chosen) {
        billingslist <- billingslist %>>%
          dplyr::mutate(covid19bb = mapply(
            check_for_covid19_bulkbilling,
            InternalID, DOB, Date, Age, MBSItem
          ))
      }

      return(billingslist)
    }
  )

  shiny::observeEvent(input$allbillings_view, ignoreNULL = TRUE, {
    dMBillings$own_billings <- !input$allbillings_view
  })

  check_for_covid19_bulkbilling <- function(InternalID, DOB, Date, Age, MBSItem) {
    if (is.null(MBSItem)) {
      return(as.character(NA))
      # wasn't billed, so can't add a bulk-billing incentive!
    }

    if (any(c(10990, 10991, 10992, 10981, 10982) %in% MBSItem)) {
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
      outcome = c(0, 1)
    ))) { # unknown result or live birth
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

  styled_billings_list <- shiny::eventReactive(
    c(
      billings_list(),
      input$printcopy_view
    ), {
      shiny::validate(
        shiny::need(billings_list(), "No appointments in selected range")
      )

      billings <- billings_list()

      if (input$printcopy_view == TRUE) {
        billings <- billings %>>%
          dMeasureBillings::tag_billings_list(
            screentag_print = TRUE,
            screentag = FALSE
          )
      } else {
        billings <- billings %>>%
          dMeasureBillings::tag_billings_list(
            screentag_print = FALSE,
            screentag = TRUE
          )
      }

      if ("COVID-19 Bulk Billing Incentive" %in% input$billing_reminders_chosen) {
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
    }
  )

  output$billings_table <- DT::renderDT({
    styled_billings_list()
  })
}
