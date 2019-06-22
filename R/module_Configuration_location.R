
##### locations - editable datatable module #######################################################

#' location configuration module - UI function
#'
#' Editable datatable with list of locations
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
locations_datatableUI <- function(id) {
  ns <- NS(id)

  tagList(
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
#' @param	PracticeLocations reactiveval, list of locations
#' @param UserConfig reactiveval, list of users who have been configured
#' @param config_db R6 object, access to configuration database
#'
#' @return count - increments with each edit of server database
locations_datatable <- function(input, output, session,
                                PracticeLocations, UserConfig, config_db) {
  # returns location_list_change - increments with each GUI edit of location list
  # change in location_list_change to prompt change in selectable filter list of locations

  # callback functions for DTEdit
  ## locations

  locations_dt_viewcols <- c("id", "Name", "Description")
  # columns viewed in DTedit when adding/editing/removing locations
  # 'id' is likely not necessary for end-users

  userlocations <- reactiveVal()
  # list of user names
  observeEvent(UserConfig(), {
    userlocations(UserConfig()$Location %>% unlist(use.names = FALSE))
    # extract from EMR database. note that this is NOT reactive to underlying change in EMR database
    # can't exclude names already configured, because this is also used when
    # editing a current user configuration
  })

  location_list_change <- reactiveVal(0)

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

      newid <- max(c(as.data.frame(PracticeLocations())$id, 0)) + 1
      # initially, PracticeLocations$id might be an empty set
      # so need to append a '0'
      data[row, ]$id <- newid

      query <- "INSERT INTO Location (id, Name, Description) VALUES (?, ?, ?)"
      data_for_sql <- as.list.data.frame(c(newid, data[row,]$Name, data[row,]$Description))

      config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method

      PracticeLocations(data) # update the dataframe in memory
      location_list_change(location_list_change() + 1)
      # this value returned by module

      return(PracticeLocations())
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
      query <- "UPDATE Location SET Name = ?, Description = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(data[row,]$Name, data[row,]$Description, data[row,]$id))

      config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method

      PracticeLocations(data)
      location_list_change(location_list_change() + 1)
      # this value returned by module

      return(PracticeLocations())
    }
  }
  locations.delete.callback <- function(data, row) {
    # delete a practice location
    if (data[row,]$Name %in% userlocations()) {
      stop(paste0("Cannot remove '", data[row,]$Name,
                  "', this location is assigned to a user."))
    } else {
      query <- "DELETE FROM Location WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(data[row,]$id))

      config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method

      PracticeLocations(data[-c(row),])
      location_list_change(location_list_change() + 1) # this value returned by module
    }
    return(PracticeLocations())
  }

  # depends on modularized version of DTedit
  locations_edited <- callModule(DTedit::dtedit, "locations",
                                 thedataframe = PracticeLocations, # pass a ReactiveVal
                                 view.cols = locations_dt_viewcols, # no need to show 'id' in future
                                 edit.cols = c("Name", "Description"),
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
