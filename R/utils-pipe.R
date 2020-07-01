#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom data.table ":="
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

.datatable.aware = TRUE

#' Pipe operator pipeR
#'
#' @name %>>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom pipeR %>>%
#' @usage x \%>>\% expr
NULL
