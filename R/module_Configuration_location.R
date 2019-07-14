
##### locations - editable datatable module #######################################################

#' location configuration module - UI function
#'
#' Editable datatable with list of locations
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
locations_datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    DTedit::dteditUI(ns("locations"))
  )
}

#' location configuration module - server
#'
#' Editable datatable with list of locations
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param	dM dMeasure R6 object
#'
#' @return count - increments with each edit of server database
locations_datatable <- function(input, output, session, dM) {
  # returns location_list_change - increments with each GUI edit of location list
  # change in location_list_change to prompt change in selectable filter list of locations

  # callback functions for DTEdit
  ## locations

  locations_dt_viewcols <- c("id", "Name", "Description")
  # columns viewed in DTedit when adding/editing/removing locations
  # 'id' is likely not necessary for end-users

  location_list_change <- reactiveVal(0)
  observeEvent(location_list_change(), ignoreInit = TRUE, {
    invisible(dM$location_list())
    # this will also re-fresh $location_listR
  })

  ### callback definitions for DTedit location
  locations.insert.callback <- function(data, row) {
    # adding a new practice location
    if (length(grep(toupper(data[row, ]$Name),
                    toupper(as.data.frame(isolate(PracticeLocations()))$Name)))){
      # if the proposed new name is the same as one that already exists
      # (ignoring case). grep returns empty integer list if no match
      stop("New practice location name cannot be the same as existing names")
    } else if (is.null(data[row,]$Name)){
      stop("New practice location name cannot be 'empty'!")
    } else {

      dM$location.insert(as.list(data[row, ]))

      location_list_change(location_list_change() + 1)
      # this value returned by module

      return(dM$location.list())
    }
  }

  locations.update.callback <- function(data, olddata, row) {
    # change (update) a practice location

    if (length(grep(toupper(data[row, ]$Name),
                    toupper(data[-row,]$Name)))){
      # if the proposed new name is the same as one that already exists
      # (ignoring case). grep returns empty integer list if no match
      stop("New practice location name cannot be the same as existing names")
    } else if (is.null(data[row,]$Name)){
      stop("New practice location name cannot be 'empty'!")
    } else if ((olddata[row,]$Name %in% userlocations()) &
               (olddata[row,]$Name != data[row,]$Name)) {
      stop(paste0("Cannot change the name of '", olddata[row,]$Name,
                  "', this location is assigned to a user."))
    } else {

      dM$location.update(as.lsit(data[row, ]))

      location_list_change(location_list_change() + 1)
      # this value returned by module

      return(dM$server.list())
    }
  }
  locations.delete.callback <- function(data, row) {
    # delete a practice location
    if (data[row,]$Name %in% userlocations()) {
      stop(paste0("Cannot remove '", data[row,]$Name,
                  "', this location is assigned to a user."))
    } else {
      dM$location.delete(as.list(data[row,]))

      location_list_change(location_list_change() + 1) # this value returned by module
    }
    return(dM$location.list())
  }

  # depends on modularized version of DTedit
  locations_edited <- callModule(DTedit::dtedit, 'locations',
                                 thedataframe = dM$location_listR, # a reactiveval
                                 view.cols = locations_dt_viewcols, # no need to show 'id' in future
                                 edit.cols = c('Name', 'Description'),
                                 edit.label.cols = c('Practice Locations', 'Description'),
                                 show.copy = FALSE,
                                 input.types = c(Name = 'textInput', Description = 'textInput'),
                                 callback.update = locations.update.callback,
                                 callback.insert = locations.insert.callback,
                                 callback.delete = locations.delete.callback
  )

  return(reactive({location_list_change()}))
  # increments each time a callback changes PracticeLocations()
}
