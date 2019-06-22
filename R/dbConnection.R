#' dbConnection class
#' @title dbConnection class
#' @doctype class
#' @description connect using DBI or pool
#' @field DBIconn - connection using DBI (NULL if not connected)
#' @field poolconn - connection using pool (NULL is not connected)
#' @field conn() - connection using whichever connection is available
#'
#' @section Methods:
#' \describe{
#' \item{\strong{connect} : connect to database}
#' \item{\strong{close} : close connection to database}
#' \item{\strong{dbSendQuery} : send query (statement) to connection}
#' }
#'
#' @examples
#' dbConnection$new()   # creates new object
#'
#' dbConnection$connect(usepool = TRUE, RSQLite::SQLite(),
#'                      dbname = "mydatabase.sqlite")
#'                      # sets $DBIconn or $poolcon to connection
#'                      # both set to NULL if fail/warn
#'
#' dbConnection$close() # closes all connections
#'
#' query <- "UPDATE Users SET Name = ? WHERE id = ?"
#' data_for_sql <- list(c("John"), id)
#' dbConnectioin$dbSendQuery(query, data_for_sql)
#'                      # send parametized 'SQL query'
#'                      # with 'data_for_sql'
#'
#' @export
dbConnection <-
  R6::R6Class("dbConnection",
              public = list(
                DBIconn = NULL, # connection using DBI
                poolconn = NULL, # connection using pool
                connect = function (usepool = TRUE, drv, # by default, try to open using pool
                                    ...
                ) {
                  # connect to database, using 'pool' package if available and desired
                  # accepts standard database opening parameters
                  # and 'usepool', whether to try to open the pool
                  #
                  if (usepool & requireNamespace("pool", quietly = TRUE)) {
                    # if use of pool is requested, and 'pool' package is available
                    tryCatch(self$poolconn <- pool::dbPool(drv, ...),
                             error = function(e) {NULL},
                             warning = function(w) {NULL})
                  } else {
                    tryCatch(self$DBIconn <- DBI::dbConnect(drv, ...),
                             error = function(e) {NULL},
                             warning = function(w) {NULL})
                  }
                  invisible(self)
                },
                close = function() {
                  # close any open connections
                  if (!is.null(self$DBIconn)) {
                    DBI::dbDisconnect(self$DBIconn)
                    self$DBIconn <- NULL
                  }
                  if (!is.null(self$poolconn)) {
                    pool::poolClose(self$poolconn)
                    self$poolconn <- NULL
                  }
                  invisible(self)
                },
                finalize = function() {
                  # object being destroyed/removed
                  # close all open connections
                  self$close()
                },
                conn = function(value) {
                  # return open connection
                  if (missing(value)) {
                    # called without argument, which is the default
                    if (!is.null(self$DBIconn)) {
                      return(self$DBIconn)
                    }
                    if (!is.null(self$poolconn)) {
                      return(self$poolconn)
                    }
                    return(NULL) # no open connections
                  } else {
                    stop("Can't set `$conn`, use $connect to open a database", call. = FALSE)
                  }
                },
                dbSendQuery = function(query, data_for_sql) {
                  # send SQL statement to active connection,
                  # eith DBI or pool
                  # @param query - the SQL query
                  # @data_for_sql - the data
                  if (!is.null(self$DBIconn)) {
                    rs <- DBI::dbSendQuery(self$DBIconn, query)
                    # parameterized query can handle apostrophes etc.
                    DBI::dbBind(rs, data_for_sql)
                    # for statements, rather than queries, we don't need to dbFetch(rs)
                    # update database
                    DBI::dbClearResult(rs)
                  }
                  if (!is.null(self$poolconn)) {
                    temp_connection <- pool::poolCheckout(self$poolconn)
                    # can't write with the pool
                    rs <- DBI::dbSendQuery(temp_connection, query)
                    # parameterized query can handle apostrophes etc.
                    DBI::dbBind(rs, data_for_sql)
                    # for statements, rather than queries, we don't need to dbFetch(rs)
                    # update database
                    DBI::dbClearResult(rs)
                    pool::poolReturn(temp_connection)
                  }
                  invisible(self)
                }
              ))
