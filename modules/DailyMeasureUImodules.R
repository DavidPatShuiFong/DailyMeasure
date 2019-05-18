# modules for input/output

##### servers - editable datatable module #######################################################

servers_datatableUI <- function(id) {
	ns <- NS(id)
	
	tagList(
		wellPanel(
			uiOutput(ns("selection"))
		),
		dteditUI(ns("servers"))
	)
}

servers_datatable <- function(input, output, session, BPdatabase, BPdatabaseChoice, emrpool, config_pool) {
	# Practice locations/groups server part of module
	# input : BPdatabase reactiveval, list of servers
	# input : BPdatabaseChoice reactiveval, name of chosen server
	# input : emrpool reactiveval, current Best Practice (electronic medical record 'EMR') database pool in use
	# input : config_pool - reactiveval, access to configuration database
	# returns server_list_change$count - increments with each GUI edit of server list
	# change in server_list_change to prompt change in selectable filter list of locations
	ns <- session$ns
	
	servers_dt_viewcols <- c("id", "Name", "Address", "Database", "UserID", "dbPassword")
	# columns viewed in DTedit when adding/editing/removing servers
	# 'id' is likely not necessary for end-users
	servers_dt_editcols <- servers_dt_viewcols[!servers_dt_viewcols %in% c("id")]
	
	chosen_database <- reactiveVal(NULL) # ID of chosen database
	servers_list_change <- reactiveVal(0)
	
	servername_list <- reactiveVal(append("None", isolate(BPdatabase()$Name)))
	observeEvent(c(BPdatabase(), config_pool()), {
		servername_list(BPdatabase()$Name)
	})
	
	output$selection <- renderUI({
		selectInput(inputId = ns("server_chosen"), label = "Chosen Best Practice server",
								choices = servername_list(), selected = isolate(BPdatabaseChoice()))
	})
	
	observeEvent(BPdatabaseChoice(), {
		updateSelectInput(session, inputId = "server_chosen", selected = BPdatabaseChoice())
	})
	
	observeEvent(BPdatabase()$Name, {
		servername_list(append("None", BPdatabase()$Name))
	})
	
	observeEvent(input$server_chosen, {
		chosen_database(input$server_chosen) # this will be the server 'Name', a character string
		BPdatabaseChoice(input$server_chosen)
		if (nrow(config_pool() %>% tbl("ServerChoice") %>% filter(id ==1) %>% collect())) {
			# already an entry in the ServerChoice table
			query <- "UPDATE ServerChoice SET Name = ? WHERE id = ?"
			data_for_sql <- as.list.data.frame(c(input$server_chosen, 1))
		} else {
			# create a new entry
			query <- "INSERT INTO ServerChoice (id, Name) VALUES (?, ?)"
			data_for_sql <- as.list.data.frame(c(1, input$server_chosen))
		}
		
		connection <- poolCheckout(config_pool()) # can't write with the pool
		rs <- dbSendQuery(connection, query) # parameterized query can handle apostrophes etc.
		dbBind(rs, data_for_sql)
		# for statements, rather than queries, we don't need to dbFetch(rs)
		# update database
		dbClearResult(rs)
		poolReturn(connection)
		
	})
	
	### callback definitions for DTedit
	servers.insert.callback <- function(data, row) {
		# adding a new server description
		if (toupper(data[row,]$Name) %in% toupper(append(data[-row,]$Name, "None"))) {
			# if the proposed server is the same as one that already exists
			# (ignoring case)
			stop("New server name cannot be the same as existing names, or 'None'")
		} else if (str_length(data[row,]$Name) == 0 | str_length(data[row,]$Address) == 0 |
							 str_length(data[row,]$Database) == 0 | str_length(data[row,]$UserID) == 0 |
							 str_length(data[row,]$dbPassword) == 0) {
			stop("All entries must be described")
		} else {
			
			newid <- max(c(as.data.frame(BPdatabase())$id, 0)) + 1
			# initially, BPdatabase()$id might be an empty set, so need to append a '0'
			data[row, ]$id <- newid
			
			query <- "INSERT INTO Server (id, Name, Address, Database, UserID, dbPassword) VALUES (?, ?, ?, ?, ?, ?)"
			data_for_sql <- as.list.data.frame(c(newid, data[row,]$Name, data[row,]$Address,
																					 data[row,]$Database, data[row,]$UserID,
																					 data[row,]$dbPassword))
			
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # parameterized query can handle apostrophes etc.
			dbBind(rs, data_for_sql)
			# for statements, rather than queries, we don't need to dbFetch(rs)
			# update database
			dbClearResult(rs)
			poolReturn(connection)
			
			BPdatabase(data) # update the dataframe in memory
			servers_list_change(servers_list_change() + 1) # this value returned by module
			
			return(BPdatabase())
		}
	}
	
	servers.update.callback <- function(data, olddata, row) {
		# change (update) a server description
		
		if (toupper(data[row,]$Name) %in% toupper(append(data[-row,]$Name, "None"))) {
			# if the proposed server is the same as one that already exists
			# (ignoring case)
			stop("New server name cannot be the same as existing names, or 'None'") 
		} else if (toupper(data[row,]$Name) == toupper(BPdatabaseChoice())) {
			stop(paste0("Cannot edit '", data[row,]$Name, "', currently in use!"))
		} else if (toupper(data[row,]$Name == "NONE")) {
			stop("New server name cannot be 'None'!")
		} else if (str_length(data[row,]$Name) == 0 | str_length(data[row,]$Address) == 0 |
							 str_length(data[row,]$Database) == 0 | str_length(data[row,]$UserID) == 0 |
							 str_length(data[row,]$dbPassword) == 0) {
			stop("All entries must be described")
		} else {
			browser()
			query <- "UPDATE Server SET Name = ?, Address = ?, Database = ?, UserID = ?, dbPassword = ? WHERE id = ?"
			data_for_sql <- as.list.data.frame(c(data[row,]$Name, data[row,]$Address,
																					 data[row,]$Database, data[row,]$UserID,
																					 data[row,]$dbPassword, data[row,]$id))
			
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # update database
			dbBind(rs, data_for_sql)
			dbClearResult(rs)
			poolReturn(connection)
			
			BPdatabase(data)
			servers_list_change(servers_list_change() + 1) # this value returned by module
			
			return(BPdatabase())
		}
	}
	servers.delete.callback <- function(data, row) {
		# delete a server description
		if (toupper(data[row,]$Name) == toupper(BPdatabaseChoice())) {
			stop(paste0("Cannot remove '", data[row,]$Name, "', currently in use!"))
		} else {
			query <- "DELETE FROM Server WHERE id = ?"
			data_for_sql <- as.list.data.frame(c(data[row,]$id))
			
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # update database
			dbBind(rs, data_for_sql)
			dbClearResult(rs)
			poolReturn(connection)
			
			BPdatabase(data[-c(row),])
			servers_list_change(servers_list_change() + 1) # this value returned by module
		}
		return(BPdatabase())
	}
	# depends on modularized version of DTedit
	servers_edited <- callModule(dtedit, "servers",
															 thedataframe = BPdatabase, # pass a ReactiveVal
															 view.cols = servers_dt_viewcols, # no need to show 'id' in future
															 edit.cols = servers_dt_editcols,
															 input.types = c(Name = 'textInput', Address = 'textInput',
															 								Database = 'textInput', UserID = 'textInput',
															 								dbPassword = 'textInput'),
															 callback.update = servers.update.callback,
															 callback.insert = servers.insert.callback,
															 callback.delete = servers.delete.callback
	)
	
	return(list(
		count = reactive({servers_list_change()})
	))
	# increments each time a callback changes BPdatabase()
}

##### locations - editable datatable module #######################################################

locations_datatableUI <- function(id) {
	ns <- NS(id)
	
	tagList(
		dteditUI(ns("locations"))
	)
}

locations_datatable <- function(input, output, session, PracticeLocations, UserConfig, config_pool) {
	# Practice locations/groups server part of module
	# input : PracticeLocations() reactiveval, list of PracticeLocations
	# input : Users - access to Users database. avoid deleting location currently in 'use' by user
	# input : config_pool - reactiveval, access to configuration database
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
			# initially, PracticeLocations$id might be an empty set, so need to append a '0'
			data[row, ]$id <- newid
			
			query <- "INSERT INTO Location (id, Name, Description) VALUES (?, ?, ?)"
			data_for_sql <- as.list.data.frame(c(newid, data[row,]$Name, data[row,]$Description))
			
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # parameterized query can handle apostrophes etc.
			dbBind(rs, data_for_sql)
			# for statements, rather than queries, we don't need to dbFetch(rs)
			# update database
			dbClearResult(rs)
			poolReturn(connection)
			
			PracticeLocations(data) # update the dataframe in memory
			location_list_change(location_list_change() + 1) # this value returned by module
			
			return(PracticeLocations())
		}
	}
	
	locations.update.callback <- function(data, olddata, row) {
		# change (update) a practice location
		
		if (length(grep(toupper(data[row, ]$Name),
										toupper(as.data.frame(isolate(PracticeLocations()))$Name)))){
			# if the proposed new name is the same as one that already exists
			# (ignoring case). grep returns empty integer list if no match
			stop("New practice location name cannot be the same as existing names")
		} else {
			query <- "UPDATE Location SET Name = ?, Description = ? WHERE id = ?"
			data_for_sql <- as.list.data.frame(c(data[row,]$Name, data[row,]$Description, data[row,]$id))
			
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # update database
			dbBind(rs, data_for_sql)
			dbClearResult(rs)
			poolReturn(connection)
			
			PracticeLocations(data)
			location_list_change(location_list_change() + 1) # this value returned by module
			
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
			
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # update database
			dbBind(rs, data_for_sql)
			dbClearResult(rs)
			poolReturn(connection)
			
			PracticeLocations(data[-c(row),])
			location_list_change(location_list_change() + 1) # this value returned by module
		}
		return(PracticeLocations())
	}
	
	# depends on modularized version of DTedit
	locations_edited <- callModule(dtedit, "locations",
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

##### users config - editable datatable module ######################################################

userconfig_datatableUI <- function(id) {
	ns <- NS(id)
	
	tagList(
		dteditUI(ns("userconfigs"))
	)
}

userconfig_datatable <- function(input, output, session, UserConfig, LocationNames, Users, config_pool) {
	# User config, server part of module
	# input : UserConfig() reactiveval, list of user config
	# input : LocationNames - list of location names (not including ID or Description)
	# input : Users - dataframe of users. This is a 'lazy' evaluation database reference
	# input : config_pool - reactiveval, access to configuration database
	# returns userconfig_list_change - increments with each GUI edit of userconfig list
	
	# callback functions for DTEdit
	## locations
	
	userconfig_dt_viewcols <- c("id", "Fullname", "AuthIdentity", "Location",
															"Attributes")
	userconfig_dt_editcols <- userconfig_dt_viewcols[!userconfig_dt_viewcols %in% c("id")]
	user_attribute_types <- c("Expert", "GlobalView", "Admin")
	# columns viewed in DTedit when adding/editing/removing user config
	
	usernames <- reactiveVal()
	# list of user names
	observeEvent(UserConfig(), {
		if (!is.null(Users)) {
			usernames(Users %>% select(Fullname) %>% collect() %>% unlist(use.names = FALSE))
			# extract from EMR database. note that this is NOT reactive to underlying change in EMR database
			# can't exclude names already configured, because this is also used when
			# editing a current user configuration
		}
	})
	
	userconfig_list_change <- reactiveVal(0)
	
	### callback definitions for DTedit userconfig
	userconfig.insert.callback <- function(data, row) {
		# adding a new user configuration
		
		if (data[row,]$Fullname %in% data[-row,]$Fullname) {
			# if the proposed new name is the same as one that is configured elsewhere
			stop("This user is already configured")
		} else {
			newid <- max(c(as.data.frame(UserConfig())$id, 0)) + 1
			# initially, UserConfig()$id might be an empty set, so need to append a '0'
			data[row, ]$id <- newid
			
			query <- "INSERT INTO Users (id, Fullname, AuthIdentity, Location, Attributes) VALUES ($id, $fn, $au, $lo, $at)"
			data_for_sql <- list(id = newid, fn = data[row,]$Fullname, au = paste0(data[row,]$Authidentity, ""),
													 # $Location and $Attribute could both have multiple (or no) entries
													 lo = paste0(data[row,]$Location[[1]], collapse = ";"),
													 at = paste0(data[row,]$Attributes[[1]], collapse = ";"))
			
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # parameterized query can handle apostrophes etc.
			dbBind(rs, data_for_sql)
			# for statements, rather than queries, we don't need to dbFetch(rs)
			# update database
			dbClearResult(rs)
			poolReturn(connection)
			
			UserConfig(data) # update the dataframe in memory
			userconfig_list_change(userconfig_list_change() + 1) # this value returned by module
			
			return(UserConfig())
		}
	}
	
	userconfig.update.callback <- function(data, olddata, row) {
		# change (update) a user configuration
		
		if (data[row, ]$Fullname %in% data[-row,]$Fullname) {
			# if the proposed new name is the same as one that is configured elsewhere
			stop("This user is already configured")
		} else {
			query <- "UPDATE Users SET Fullname = ?, AuthIdentity = ?, Location = ?, Attributes = ? WHERE id = ?"
			data_for_sql <- as.list(c(data[row,]$Fullname, paste0(data[row,]$AuthIdentity, ""),
																paste0(data[row,]$Location[[1]], collapse = ";"),
																paste0(data[row,]$Attributes[[1]], collapse = ";"),
																data[row,]$id))
			connection <- poolCheckout(config_pool()) # can't write with the pool
			rs <- dbSendQuery(connection, query) # update database
			dbBind(rs, data_for_sql)
			dbClearResult(rs)
			poolReturn(connection)
			
			UserConfig(data)
			userconfig_list_change(userconfig_list_change() + 1) # this value returned by module
			
			return(UserConfig())
		}
	}
	
	userconfig.delete.callback <- function(data, row) {
		# delete a user configuration
		
		query <- "DELETE FROM Users WHERE id = ?"
		data_for_sql <- as.list.data.frame(c(data[row,]$id))
		
		connection <- poolCheckout(config_pool()) # can't write with the pool
		rs <- dbSendQuery(connection, query) # update database
		dbBind(rs, data_for_sql)
		dbClearResult(rs)
		poolReturn(connection)
		
		UserConfig(data[-c(row),])
		userconfig_list_change(userconfig_list_change() + 1) # this value returned by module
		
		return(UserConfig())
	}
	# depends on modularized version of DTedit
	userconfig_edited <- callModule(dtedit, "userconfigs",
																	thedataframe = UserConfig, # pass a ReactiveVal
																	view.cols = userconfig_dt_viewcols, # no need to show 'id' in future
																	edit.cols = userconfig_dt_editcols,
																	# edit.label.cols = ,
																	show.copy = FALSE,
																	input.types = c(Fullname = 'selectInputReactive',
																									AuthIdentity = 'textInput',
																									Location = 'selectInputMultipleReactive',
																									Attributes = 'selectInputMultiple'),
																	input.choices = c(Location = 'LocationNames',
																										Fullname = 'Fullname',
																										Attributes = list(user_attribute_types)),
																	input.choices.reactive = list(Fullname = usernames,
																																LocationNames = LocationNames),
																	callback.update = userconfig.update.callback,
																	callback.insert = userconfig.insert.callback,
																	callback.delete = userconfig.delete.callback
	)
	
	return(reactive({userconfig_list_change()}))
	# increments each time a callback changes UserConfig
}
