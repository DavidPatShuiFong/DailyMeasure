###### conditions modules ###################################################

#' about_UI - information module
#'
#' information module
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
about_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::tabBox(
      id = "tab_conditions",
      title = "About",
      width = 12,
      height = "85vh",
      shiny::tabPanel(
        title = "Documentation",
        width = 12,
        shiny::br(),
        about_documentation_UI(ns("documentation"))
      ),
      shiny::tabPanel(
        title = "Version",
        width = 12,
        shiny::br(),
        about_version_UI(ns("version"))
      ),
      shiny::tabPanel(
        title = "Contact",
        width = 12,
        shiny::br(),
        about_contact_UI(ns("contact"))
      )
    )
  )
}

about_documentation_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10, offset = 1,
                    shiny::h2("Installation and initial configuration of GPstat!"),
                    shiny::a(href = "http://rpubs.com/DavidFong/GPstatInstall",
                             target = "_blank", # open in new tab
                             "Installation and configuration description (RPubs)"), shiny::br(),
                    shiny::a(href = "https://youtu.be/i6jhy_wY0bM",
                             target = "_blank",
                             "Video demonstration of installation and configuration (Youtube)"),
                    shiny::br(), shiny::br(),
                    shiny::h2("Usage"),
                    shiny::a(href = "https://youtu.be/4zuYG4uU4-4",
                             target = "_blank",
                             "Video demonstration of features (Youtube)"), shiny::br(),
                    shiny::a(href = "http://rpubs.com/DavidFong/dMeasure",
                             target = "_blank",
                             "'dMeasure' backend documentation - highly technical! (RPubs)"), shiny::br(),
                    shiny::a(href = "http://rpubs.com/DavidFong/framinghamRiskEquation",
                             target = "_blank",
                             "'framinghamRiskEquation' : implementation of Framingham Risk Equation - highly technical! (RPubs)"),
                    shiny::br()
      )
    )
  )
}

about_version_UI <- function(id) {
  ns <- shiny::NS(id)

  BillingsVersion <- tryCatch(packageVersion("dMeasureBillings"),
                              error = function(cond) {return(NULL)})
  CDMVersion <- tryCatch(packageVersion("dMeasureCDM"),
                         error = function(cond) {return(NULL)})

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10, offset = 1,

                    shiny::fluidRow(
                      shinydashboardPlus::dashboardUserItem(
                        width = 6,
                        shinydashboardPlus::descriptionBlock(
                          header = "GPstat!",
                          text = paste("v",packageVersion("DailyMeasure")),
                          right_border = TRUE,
                          margin_bottom = TRUE)
                      ),
                      shinydashboardPlus::dashboardUserItem(
                        width = 6,
                        shinydashboardPlus::descriptionBlock(
                          header = "dMeasure",
                          text = paste("v", packageVersion("dMeasure")),
                          right_border = FALSE,
                          margin_bottom = TRUE)
                      )
                    ),
                    shiny::fluidRow(
                      shinydashboardPlus::dashboardUserItem(
                        width = 6,
                        shinydashboardPlus::descriptionBlock(
                          header = "Billings module",
                          text = paste("v", ifelse(!is.null(BillingsVersion),
                                                   as.character(BillingsVersion),
                                                   "None")),
                          right_border = TRUE,
                          margin_bottom = TRUE)
                      ),
                      shinydashboardPlus::dashboardUserItem(
                        width = 6,
                        shinydashboardPlus::descriptionBlock(
                          header = "CDM module",
                          text = paste("v", ifelse(!is.null(CDMVersion),
                                                   as.character(CDMVersion),
                                                   "None")),
                          right_border = FALSE,
                          margin_bottom = TRUE)
                      )
                    )
      )))
}

about_contact_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10, offset = 1,
                    tags$style(HTML("h1 {font-family: Arial;}")),
                    shiny::h2("Author"),
                    shiny::strong("Dr. David Fong"), shiny::br(),
                    shiny::em("MBSS FRACGP FARGP DipRANZCOG GradCertDrugAlcohol GradCertIntegrativeMedicine"),
                    shiny::br(),
                    "coHealth Kensington (lead doctor)", shiny::br(),
                    shiny::br(), shiny::br(),
                    shiny::h3("E-mail"),
                    "vkelim at bigpond dot com", shiny::br(),
                    "david dot fong at cohealth dot org dot au", shiny::br(),
                    shiny::br(), shiny::br(),
                    "\u0398\u03c5\u03b3\u03ac\u03c4\u03b7\u03c1 \u002e\u002e\u002e \u1f55\u03c0\u03b1\u03b3\u03b5 \u03b5\u1f30\u03c2 \u03b5\u1f30\u03c1\u03ae\u03bd\u03b7\u03bd\u002c \u03ba\u03b1\u1f76 \u1f34\u03c3\u03b8\u03b9 \u1f51\u03b3\u03b9\u1f74\u03c2 \u1f00\u03c0\u1f78 \u03c4\u1fc6\u03c2 \u03bc\u03ac\u03c3\u03c4\u03b9\u03b3\u03cc\u03c2 \u03c3\u03bf\u03c5",
                    shiny::br(),
                    shiny::uiOutput(ns("string1"))
      )
    )
  )
}

#' about server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @return none
about <- function(input, output, session, dM) {
  ns <- session$ns

  # data quality
  callModule(about_documentation_datatable, "documentation", dM)
  callModule(about_contact, "contact", dM)

}

#' about documentation - server
#'
#' list some documentation
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @return none
about_documentation_datatable <- function(input, output, session, dM) {
  ns <- session$ns
  NULL
}

about_contact <- function(input, output, session, dM) {
  ns <- session$ns

  output$string1 <- shiny::renderUI(({
    "Daughter ... go in peace and wholeness, be freed of your suffering (\u99ac\u592a\u798f\u97f3 5:34)"
  }))

}