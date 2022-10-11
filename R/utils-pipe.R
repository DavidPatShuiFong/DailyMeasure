# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
