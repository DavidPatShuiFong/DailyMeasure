# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
        title = "Credits",
        width = 12,
        shiny::br(),
        about_credits_UI(ns("credits"))
      ),
      shiny::tabPanel(
        title = "Privacy",
        width = 12,
        shiny::br(),
        about_privacy_UI(ns("privacy"))
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
      shiny::column(10,
        offset = 1,
        shiny::h2("Installation and initial configuration of GPstat!"),
        shiny::a(
          href = "http://rpubs.com/DavidFong/GPstatInstall",
          target = "_blank", # open in new tab
          "Installation and configuration description (RPubs)"
        ), shiny::br(),
        shiny::a(
          href = "https://youtu.be/i6jhy_wY0bM",
          target = "_blank",
          "Video demonstration of installation and configuration (Youtube)"
        ),
        shiny::br(), shiny::br(),
        shiny::h2("Usage"),
        shiny::a(
          href = "https://youtu.be/mTJzcycPkRU",
          target = "_blank",
          "Video demonstration of features (Youtube)"
        ), shiny::br(),
        shiny::a(
          href = "http://rpubs.com/DavidFong/dMeasure",
          target = "_blank",
          "'dMeasure' backend documentation - highly technical! (RPubs)"
        ), shiny::br(),
        shiny::a(
          href = "http://rpubs.com/DavidFong/framinghamRiskEquation",
          target = "_blank",
          "'framinghamRiskEquation' : implementation of Framingham Risk Equation - highly technical! (RPubs)"
        ),
        shiny::br()
      )
    )
  )
}

about_version_UI <- function(id) {
  ns <- shiny::NS(id)

  BillingsVersion <- tryCatch(packageVersion("dMeasureBillings"),
    error = function(cond) {
      return(NULL)
    }
  )
  CDMVersion <- tryCatch(packageVersion("dMeasureCDM"),
    error = function(cond) {
      return(NULL)
    }
  )

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10,
        offset = 1,

        shiny::fluidRow(
          shinydashboardPlus::dashboardUserItem(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              header = "GPstat!",
              text = paste("v", packageVersion("DailyMeasure")),
              right_border = TRUE,
              margin_bottom = TRUE
            )
          ),
          shinydashboardPlus::dashboardUserItem(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              header = "dMeasure",
              text = paste("v", packageVersion("dMeasure")),
              right_border = FALSE,
              margin_bottom = TRUE
            )
          )
        ),
        shiny::fluidRow(
          shinydashboardPlus::dashboardUserItem(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              header = "Billings module",
              text = paste("v", ifelse(!is.null(BillingsVersion),
                as.character(BillingsVersion),
                "None"
              )),
              right_border = TRUE,
              margin_bottom = TRUE
            )
          ),
          shinydashboardPlus::dashboardUserItem(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              header = "CDM module",
              text = paste("v", ifelse(!is.null(CDMVersion),
                as.character(CDMVersion),
                "None"
              )),
              right_border = FALSE,
              margin_bottom = TRUE
            )
          )
        )
      )
    )
  )
}

about_credits_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10,
        offset = 1,
        '"GPstat!/DailyMeasure" is written by David Fong.', shiny::br(),
        "YEAR: 2019", shiny::br(),
        "COPYRIGHT HOLDER: Dr David Pat Shui Fong", shiny::br(),
        "Proprietary. All rights reserved.", shiny::br(),
        shiny::br(),
        'Some individual components of DailyMeasure have "liberal" copy-left licenses applicable', shiny::br(),
        "e.g. DTedit has an LGPL license,", shiny::br(),
        "framinghamRiskEquation and dMeasureQIM have Mozilla Public Licenses (MPL 2.0).", shiny::br(),
        shiny::br(),
        "Credits are too many to list, but here goes...", shiny::br(),
        shiny::br(),
        "The ",
        shiny::a(
          href = "https://www.povertyactionlab.org/",
          target = "_blank", # open in new tab
          "Abdul-Latif Jameel Povery Action Lab (J-PAL)"
        ),
        " and ",
        shiny::a(
          href = "https://micromasters.mit.edu/dedp/",
          target = "_blank", # open in new tab
          "Data, Economics and Development Policy (MITx Micromasters) course"
        ), shiny::br(),
        "for providing inspiration and instruction regarding how data, maths and analysis (including using 'R') can be used to improve lives.", shiny::br(), shiny::br(),
        "The ",
        shiny::a(
          href = "https://www.coursera.org/specializations/jhu-data-science",
          target = "_blank", # open in new tab
          "John Hopkins University Data Science Specialization course"
        ),
        " (much of which was taught by data science practitioners in the field of public health)", shiny::br(),
        "for providing solid foundations and wide exposure to 'R' web-technologies which made it possible, and sometimes fun!,", shiny::br(),
        "for an enthusiast like me to develop GPstat!", shiny::br(),
        shiny::br(),
        shiny::a(
          href = "https://www.doctorscontrolpanel.com.au/e",
          target = "_blank", # open in new tab
          "Dr Anton Knieriemen's \"Doctor's Control Panel\""
        ),
        " for providing an example of point-of-care assessment of potential patient interventions and needs.", shiny::br(),
        "I once wrote a prototype of some of GPstat's functionality using DCP's ",
        "scripting language.", shiny::br(),
        shiny::br(),
        "My brother, Dr Lee Fong, for voice-over in some documentation and overall invaluable support.",
        shiny::br(), shiny::br(),
        shiny::pre(shiny::includeText(system.file("www", "LICENSE", package = "DailyMeasure")))
      )
    )
  )
}

about_privacy_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8,
        offset = 2,
        shiny::h2("Privacy statement"),
        shiny::br(),
        shiny::span(paste(
          "No patient information, identified, de-identified, aggregated or",
          "otherwise is stored locally (on the computers used by the clinic),",
          "unless explicitly saved/copied/printed by the user."
        )),
        shiny::br(), shiny::br(),
        shiny::span(paste(
          "No patient information, identified, de-identified, aggregated or",
          "otherwise is sent/transmitted across the network or Internet."
        )),
        shiny::br(), shiny::br(),
        shiny::span(paste(
          "Information about the users of the software, as stored in",
          "Best Practice and modified by the user, is stored locally",
          "(on the computers used by the clinic)."
        )),
        shiny::br(), shiny::br(),
        shiny::span(paste(
          "Information about the users of the software, and the name of the clinic",
          "is sent in encrypted form - using the Internet - to GPstat!/DailyMeasure",
          "registration/subscription databases *if* explicitly requested by the user.",
          "The information about the users includes",
          "provider number and user name, as is stored in the Best Practice database.",
          "The information is stored in those databases",
          "in encrypted form. Those databases will send back information to the",
          "user, in encrypted form, which includes the clinic name, user details",
          "and registration/subscription details."
        )),
        shiny::br(), shiny::br(),
        shiny::span(paste(
          "Information about GPstat!/DailyMeasure usage is stored locally",
          "(on the computeres used by the clinic) if logging features are enabled",
          "by the user. This information is not sent/transmitted across the network/Internet.",
          "By default, logging features are disabled."
        )),
        shiny::br(), shiny::br(),
        shiny::span(paste("All changes to privacy conditions will be described on this page."))
      )
    )
  )
}

about_contact_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10,
        offset = 1,
        tags$head(tags$style(
          type = "text/css",
          ".chinese_type {\u5fae\u8f6f\u96c5\u9ed1}",
          paste(
            ".chinese_type_large {font-family:FangSong, \u4eff\u5b8b, STFangSong, \u534e\u6587\u4eff\u5b8b,",
            "KaiTi, \u6977\u4f53, STKaiti, \u534e\u6587\u6977\u4f53, \u5fae\u8f6f\u96c5\u9ed1}"
          )
        )),
        shiny::h2("Author"),
        shiny::strong("Dr. David Fong"), shiny::br(),
        shiny::em("MBBS (UniMelb) FRACGP FARGP DipRANZCOG GradCertDrugAlcohol GradCertIntegrativeMedicine"),
        shiny::br(),
        shiny::em("Micromasters Data, Economics and Development Policy (MITx)"),
        shiny::br(),
        "coHealth Kensington (lead doctor)", shiny::br(),
        shiny::br(), shiny::br(),
        shiny::h3("E-mail"),
        "vkelim at bigpond dot com", shiny::br(),
        "david dot fong at cohealth dot org dot au", shiny::br(),
        shiny::br(), shiny::br(),
        shiny::span(
          style = "font-size:x-large",
          "\u0398\u03c5\u03b3\u03ac\u03c4\u03b7\u03c1 \u002e\u002e\u002e",
          "\u1f55\u03c0\u03b1\u03b3\u03b5 \u03b5\u1f30\u03c2 \u03b5\u1f30\u03c1\u03ae\u03bd\u03b7\u03bd\u002c",
          "\u03ba\u03b1\u1f76 \u1f34\u03c3\u03b8\u03b9 \u1f51\u03b3\u03b9\u1f74\u03c2 \u1f00\u03c0\u1f78",
          "\u03c4\u1fc6\u03c2 \u03bc\u03ac\u03c3\u03c4\u03b9\u03b3\u03cc\u03c2 \u03c3\u03bf\u03c5"
        ),
        shiny::br(),
        shiny::span(
          class = "chinese_type",
          style = "font-size:small",
          "Daughter ... go in peace and wholeness, be freed of your suffering (\u99ac\u592a\u798f\u97f3 5:34)"
        ),
        shiny::br(), shiny::br(),
        shiny::span(
          style = "font-size:large",
          "“Wen yu gibit enijing langa enibodi, God garra gibitbek langa yu.", shiny::br(),
          "En if yu gibit olabat detmatj, wal God garra gibitbek yu mowa,", shiny::br(),
          "en pipul garra gibit yu detmatj du.”"
        ), shiny::br(),
        shiny::span(
          style = "font-size:small",
          "“Give, and it will be given to you.",
          "A good measure, pressed down, shaken together and running over, will be poured into your lap.", shiny::br(),
          "For with the measure you use, it will be measured to you.”    ",
          shiny::span(
            class = "chinese_type",
            "(\u8def\u52a0\u798f\u97f3 6:38)"
          )
        ),
        shiny::br(), shiny::br(),
        shiny::span(
          style = "font-size:x-large", class = "chinese_type_large",
          "\u6211\u7948\u7977\u4f60\u4eec\u5c31\u80fd\u9886\u609f\u795e\u7684\u7231\u662f\u591a\u4e48\u7684\u957f\u9614\u9ad8\u6df1\u002c", shiny::br(),
          "\u5e76\u4e14\u77e5\u9053\u4ed6\u7684\u7231\u662f\u8d85\u8fc7\u4eba\u6240\u80fd\u7406\u89e3\u7684\u002c", shiny::br(),
          "\u4f7f\u4f60\u4eec\u88ab\u5145\u6ee1\u002c \u5f97\u7740\u795e\u7684\u4e00\u5207\u4e30\u76db\u002e   "
        ), shiny::br(),
        shiny::span(
          style = "font-size:small", class = "chinese_type",
          "I pray that you, being rooted and established in love, may have power to grasp", shiny::br(),
          "how wide and long and high and deep Love is, and to know this love that surpasses knowledge", shiny::br(),
          "that you may be filled to the measure of all fullness   (\u4ee5\u5f17\u6240\u66f8 3:17-19)"
        )
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
