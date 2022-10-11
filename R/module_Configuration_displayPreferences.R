# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' display preferenmces module - UI
#'
#' Allow user to set or change display preferences
#'
#' @param id as required by shiny modules
#'
displayPreferences_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      "Change Display Preferences",
      shiny::br(),
      shiny::br(),
      shiny::uiOutput(ns("selection")),
      {
        if (.bcdyz.option$demonstration) {
          shiny::span(shiny::p(), shiny::strong("Demonstration mode : Date format display changes disabled"),
                      style = "color:red", shiny::p()
          )
        }
        else {}
      }
    )
  )
}

#' password configuration module - server function
#'
#' Allow user to set or change their user password
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dM dMeasure R6 object
#'
#' @return count - increments with each GUI edit of user configuration database
displayPreferences_server <- function(input, output, session, dM) {
  ns <- session$ns

  output$selection <- shiny::renderUI({
    # drop-down list of servers (including 'None')
    x <- shiny::selectInput(
      inputId = ns("dateformat_chosen"),
      label = "Date format choice",
      choices = dM$dateformat_choices,
      selected = dM$dateformat_choice,
      width = "25%"
    )
    if (.bcdyz.option$demonstration) {
      x <- shinyjs::disabled(x) # if demonstration mode, disable server selection
    }
    x
  })

  shiny::observeEvent(input$dateformat_chosen, ignoreNULL = TRUE, {
    dM$dateformat_choice <- input$dateformat_chosen
    shinyFeedback::feedbackWarning(
      inputId = "dateformat_chosen",
      show = is.null(lubridate::guess_formats(input$dateformat_chosen, orders = "ymd")[[1]]),
      text = "'Sorting' of dates which are not YMD may produce unexpected results"
    )
  })

}
