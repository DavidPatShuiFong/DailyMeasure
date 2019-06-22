#' password configuration module - UI
#'
#' Allow user to set or change their user password.
#' Only shown if password can be set, i.e. user has been identified.
#'
#' @param id as required by shiny modules
#'
passwordConfig_UI <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      "Change User Password",
      br(), br(),
      actionButton(ns("ChangePassword"), "Change Password", icon("unlock-alt"))
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
#' @param	UserConfig reactiveval, list of user config
#' @param LoggedInUser reactiveval, currently logged in user
#' @param config_db R6 object, access to configuration database
#'
#' @return count - increments with each GUI edit of user configuration database
passwordConfig_server <- function(input, output, session,
                                  UserConfig, LoggedInUser,
                                  config_db) {
  ns <- session$ns

  observeEvent(input$ChangePassword, {
    if (is.na(LoggedInUser()$Password) || (nchar(LoggedInUser()$Password) == 0)) {
      # empty or NA password, then asking for new password
      showModal(modalDialog(
        title="New password",
        tagList(
          passwordInput(ns("password1"), label = "Enter Password", value = ""),
          br(),
          passwordInput(ns("password2"), label = "Confirm Password", value = "")
        ),
        footer = tagList(actionButton(ns("confirmNewPassword"), "Confirm"),
                         modalButton("Cancel")
        )
      ))
    } else {
      showModal(modalDialog(
        title="Change password",
        tagList(
          passwordInput(ns("passwordOld"), label = "Old Password", value = ""),
          br(),
          passwordInput(ns("password1"), label = "Enter Password", value = ""),
          br(),
          passwordInput(ns("password2"), label = "Confirm Password", value = "")
        ),
        footer = tagList(actionButton(ns("confirmChangePassword"), "Confirm"),
                         modalButton("Cancel")
        )
      ))
    }
  })

  observeEvent(input$confirmNewPassword, {
    if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                closeButton = TRUE)
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long")
    } else {
      setPassword(input$password1, UserConfig, LoggedInUser, config_db$conn())
      # this function is found in calculation_definitions.R
      removeModal()
    }
  })

  observeEvent(input$confirmChangePassword, {
    if (!simple_tag_compare(input$passwordOld, LoggedInUser()$Password)) {
      shinytoastr::toastr_error("Old Password incorrect",
                                closeButton = TRUE)
    } else if (input$password1 != input$password2) {
      shinytoastr::toastr_error("Passwords must match",
                                closeButton = TRUE)
    } else if (nchar(input$password1) < 6) {
      shinytoastr::toastr_error("Password must be at least six (6) characters long")
    } else {
      setPassword(input$password1, UserConfig, LoggedInUser, config_db$conn())
      removeModal()
    }
  })
}
