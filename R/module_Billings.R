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
      inputId = ns("billing_options_dropdown"),
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
      shiny::actionButton(
        inputId = ns("view_billing_types"),
        label = "Change billing types"
      )
    )
  })

  shiny::observeEvent(
    input$view_billing_types,
    ignoreInit = TRUE, {
      selected_billings <- character(0)
      if (2 %in% dMBillings$payerCode) {
        selected_billings <- c(selected_billings, "Direct 'bulk' billing")
      }
      if (3 %in% dMBillings$payerCode) {
        selected_billings <- c(selected_billings, "DVA")
      }
      if (4 %in% dMBillings$payerCode) {
        selected_billings <- c(selected_billings, "WorkCover")
      }
      if (5 %in% dMBillings$payerCode) {
        selected_billings <- c(selected_billings, "Other")
      }
      shiny::showModal(
        shiny::modalDialog(
          title = "Billing types",
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("billing_types_viewed"),
            label = "Billing types viewed",
            choices = c("Direct 'bulk' billing", "DVA", "WorkCover", "Other"),
            selected = selected_billings,
            status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
          ),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("billing_types_ok"), "OK")
          )
        )
      )
    }
  )
  shiny::observeEvent(
    # 'ok' for modal opened by 'view billing types' button
    input$billing_types_ok, {
      payerCode <- numeric(0)
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
        # 0 = unknown
        # 1 = private
        # 5 = private (head of family)
        # 8 = private (other)
      }
      if (!isTRUE(all.equal(sort(payerCode), sort(dMBillings$payerCode)))) {
        dMBillings$payerCode <- payerCode
      }
      shiny::removeModal()
    }
  )

  billings_list <- shiny::eventReactive(
    c(
      dMBillings$billings_listR(),
      input$billing_reminders_chosen
    ),
    ignoreInit = TRUE, {
      shiny::validate(
        shiny::need(
          dMBillings$billings_list,
          "No appointments in chosen range"
        ),
        shiny::need(
          nrow(dMBillings$billings_list) > 0,
          "No appointments in chosen range"
        )
      )

      # returns list_billings
      billingslist <- dMBillings$billings_list

      if ("COVID-19 Bulk Billing Incentive" %in% input$billing_reminders_chosen) {
        covid19bb <- check_for_covid19_bulkbilling(billingslist)
        billingslist <- billingslist %>>%
          dplyr::mutate(covid19bb = covid19bb)
      }

      return(billingslist)
    }
  )

  shiny::observeEvent(input$allbillings_view,
                      ignoreInit = TRUE,
                      ignoreNULL = TRUE, {
    dMBillings$own_billings <- !input$allbillings_view
  })

  check_for_covid19_bulkbilling <- function(df) {
    # checks for possibility of COVID-19 bulk-billing incentive
    # expects data.frame, but quickly converts to data.table
    # InternalID, DOB, Date, Age, MBSItem

    # executes substantially faster than previous mapply version
    # 2 seconds compared to 32 seconds on a sample database search

    dt <- data.table::as.data.table(data.table::copy(df))

    dt[, covid19bb := ""] # 'none' by default
    dt[is.na(MBSItem), covid19bb := NA]
    # if no MBSItems charged, can't charge a bulk-billing incentive payment!

    f <- function(x) {
      any(c(10990, 10991, 10992, 10981, 10982) %in% x)
    }
    dt[
      !is.na(covid19bb) & mapply(f, MBSItem),
      # forced to use mapply! unable to search directly %in% list items
      # during testing, this cost about 30% of total execution time!
      covid19bb := NA
    ]
    # not yet set to 'NA' and
    # already charged a bulk-billing incentive
    # if already charged a bulk-billing incentive item number, can't charge another

    dt[
      !is.na(covid19bb) & Age >= 70,
      covid19bb := dMeasure::paste2(
        # paste2, with 'na.rm = TRUE', will remove the empty string ""
        covid19bb, "Age 70+",
        sep = ", ", na.rm = TRUE
      )
    ]

    intID_date <- dt[
      Age >= 50 & !is.na(covid19bb), # age >= 50 and not yet 'assigned'
      c("InternalID", "Date")
    ] # choose InternalID and Date columns
    atsi_list <- dMBillings$dM$atsi_list(intID_date)
    dt[
      InternalID %in% atsi_list & !is.na(covid19bb),
      # only if InternalID in atsi_list AND not already assigned (e.g. to 'NA')
      covid19bb := dMeasure::paste2(
        covid19bb, "ATSI 50+",
        sep = ", ", na.rm = TRUE
      )
    ]
    # data.table accepts empty vector for atsi_list!

    pregnant_list <- dMBillings$dM$pregnant_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% pregnant_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Pregnant",
        sep = ", ", na.rm = TRUE
      )
    ]

    postnatal_list <- dMBillings$dM$postnatal_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")],
      include_edc = TRUE, # 'guess' delivery if no known result
      days_min = 0, days_max = 365,
      outcome = c(0, 1) # unknown result or live birth
    ) %>>% dplyr::pull(InternalID)
    # $postnatal_list returns a dataframe, not a vector
    dt[
      InternalID %in% postnatal_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Mother of child less than 12 months",
        sep = ", ", na.rm = TRUE
      )
    ]

    parent_list <- dMBillings$dM$parent_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")],
      months_min = 0, months_max = 12
    )
    dt[
      InternalID %in% parent_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Parent of child less than 12 months",
        sep = ", ", na.rm = TRUE
      )
      ]

    diabetes_list <- dMBillings$dM$diabetes_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% diabetes_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Diabetes",
        sep = ", ", na.rm = TRUE
      )
    ]

    asthma_list <- dMBillings$dM$asthma_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% asthma_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Asthma",
        sep = ", ", na.rm = TRUE
      )
    ]

    hiv_list <- dMBillings$dM$hiv_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% hiv_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "HIV",
        sep = ", ", na.rm = TRUE
      )
    ]


    malignancy_list <- dMBillings$dM$malignancy_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% malignancy_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Malignancy",
        sep = ", ", na.rm = TRUE
      )
    ]

    haemoglobinopathy_list <- dMBillings$dM$haemoglobinopathy_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% haemoglobinopathy_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Haemoglobinopathy",
        sep = ", ", na.rm = TRUE
      )
    ]

    asplenic_list <- dMBillings$dM$asplenic_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% asplenic_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Asplenia",
        sep = ", ", na.rm = TRUE
      )
    ]

    transplant_list <- dMBillings$dM$transplant_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% transplant_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Transplant recipient",
        sep = ", ", na.rm = TRUE
      )
    ]

    cardiacdisease_list <- dMBillings$dM$cardiacdisease_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% cardiacdisease_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Heart disease",
        sep = ", ", na.rm = TRUE
      )
    ]

    chroniclungdisease_list <- dMBillings$dM$chroniclungdisease_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% chroniclungdisease_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Chronic lung disease",
        sep = ", ", na.rm = TRUE
      )
    ]

    chronicliverdisease_list <- dMBillings$dM$chronicliverdisease_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% chronicliverdisease_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Chronic liver disease",
        sep = ", ", na.rm = TRUE
      )
    ]

    neurologic_list <- dMBillings$dM$neurologic_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% neurologic_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Neurological disease",
        sep = ", ", na.rm = TRUE
      )
    ]

    chronicrenaldisease_list <- dMBillings$dM$chronicrenaldisease_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% chronicrenaldisease_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "Chronic renal disease",
        sep = ", ", na.rm = TRUE
      )
    ]

    bmi30_list <- dMBillings$dM$bmi30_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")]
    )
    dt[
      InternalID %in% bmi30_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "BMI>30",
        sep = ", ", na.rm = TRUE
      )
    ]

    gpmp_list <- dMBillings$gpmp_list(
      dt[!is.na(covid19bb), c("InternalID", "Date")],
      months_min = 0,
      months_max = 12
    )
    dt[
      InternalID %in% gpmp_list & !is.na(covid19bb),
      covid19bb := dMeasure::paste2(
        covid19bb, "GPMP <12 months",
        sep = ", ", na.rm = TRUE
      )
      ]

    dt[covid19bb == "", covid19bb := NA] # still nothing found!

    return(dt %>>% dplyr::pull(covid19bb))
  }

  styled_billings_list <- shiny::eventReactive(
    c(
      billings_list(),
      input$printcopy_view
    ), {
      shiny::validate(
        shiny::need(
          billings_list(),
          "No appointments in selected range"
        )
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
        dt <- datatable_styled(billings %>>%
            dplyr::select(
              Patient, Date, AppointmentTime, Status, VisitType,
              Provider, billingtag_print
            ),
          colnames = c("Billings" = "billingtag_print")
        )
      } else {
        # fomantic/semantic tag view
        dt <- datatable_styled(billings %>%
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
      return(dt)
    }
  )

  output$billings_table <- DT::renderDT({
    styled_billings_list()
  })
}
