#' password configuration module - UI
#'
#' Allow user to set or change their user password.
#' Only shown if password can be set, i.e. user has been identified.
#'
#' @param id as required by shiny modules
#'
passwordConfig_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      "Change User Password",
      shiny::br(), shiny::br(),
      {if (.bcdyz.option$demonstration)
      {shiny::span(shiny::p(), shiny::strong("Demonstration mode : Password change disabled"),
                   style = "color:red", shiny::p())}
        else {}},
      {x <- shiny::actionButton(ns("ChangePassword"), "Change Password", icon("unlock-alt"));
      # disabled if demonstration mode
      if (.bcdyz.option$demonstration) {shinyjs::disabled(x)} else {x}}
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
passwordConfig_server <- function(input, output, session, dM) {
  ns <- session$ns

  shiny::observeEvent(input$ChangePassword, {
    if (dM$empty_password()) {
      # empty or NA password, then asking for new password
      shiny::showModal(shiny::modalDialog(
        title="New password",
        shiny::tagList(
          shiny::passwordInput(ns("password1"), label = "Enter Password", value = ""),
          shiny::br(),
          shiny::passwordInput(ns("password2"), label = "Confirm Password", value = "")
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("confirmNewPassword"), "Confirm"),
          shiny::modalButton("Cancel")
        )
      ))
    } else {
      shiny::showModal(shiny::modalDialog(
        title="Change password",
        shiny::tagList(
          shiny::passwordInput(ns("passwordOld"), label = "Old Password", value = ""),
          shiny::br(),
          shiny::passwordInput(ns("password1"), label = "Enter Password", value = ""),
          shiny::br(),
          shiny::passwordInput(ns("password2"), label = "Confirm Password", value = "")
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("confirmChangePassword"), "Confirm"),
          shiny::modalButton("Cancel")
        )
      ))
    }
  })

  shiny::observeEvent(input$confirmNewPassword, {
    # setting new password. no previous password
    if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                position = "bottom-left",
                                closeButton = TRUE)
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long",
                                position = "bottom-left")
    } else {
      dM$password.set(input$password1)
      shiny::removeModal()
    }
  })

  shiny::observeEvent(input$confirmChangePassword, {
    # changing old password
    if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                position = "bottom-left",
                                closeButton = TRUE)
    } else {
      tryCatch({
        success = TRUE
        dM$password.set(newpassword = input$password1,
                        oldpassword = input$passwordOld)
      },
      error = function(e) {
        shinytoastr::toastr_error(e$message,
                                  position = "bottom-left",
                                  closeButton = TRUE)
        success = FALSE
      })
      if (success) {
        shiny::removeModal()
      }
    }
  })
}
