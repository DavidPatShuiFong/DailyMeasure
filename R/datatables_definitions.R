## datatables functions and definitions

#' Javascript support code for semantic/fomantic JS popups
#'
#' necessary for semantic/fomantic JS popups. included directly in datatables options
#'
#' provide padding for export/print buttons
semantic_popupJS <- c("window.onload = function() {$('.ui.button') .popup({on: 'hover'});
                      $('.ui.tag.label') .popup({on: 'hover'})
                      }")

#' styled DT datatable
#'
#' by default, have export/print buttons, only render what is visible.
#' javascript code to attach labels to semantic/fomantic JS popups.
#' no pagination.
#'
#' @param data the datatable
#' @param fillContainer TRUE to automatically fill containing element
#' @param colvis column visibility button. set to NULL if not wanted
#' @param copyHtml5 copy button. set to NULL if not wanted
#' @param printButton print button. set to NULL if not wanted
#' @param downloadButton collection of download buttons. set to NULL if not wanted
#' @param drawCallback by default, execute JS code to enable semantic popups
#' @param paging 'pages' for long table. default is FALSE, otherwise hard to 'print'/copy a whole table
#' @param scrollY percentage of window height
#' @param deferRender don't render all rows on page immediately
#' @param scrollX left-right scrolling
#' @param fixedColumns fix left- or right-most columns in place
#' @param columnDefs allow customization of columns e.g. hiding by default
#'
#' @return DT datatable object
#' @export
datatable_styled <- function(data, fillContainer = TRUE,
                             extensions = c('Buttons', 'Scroller', 'Responsive'),
                             dom = 'frltiBp',
                             colvis = list(extend = 'colvis',
                                           collectionLayout = "four-column",
                                           # https://datatables.net/reference/button/collection
                                           # force 'dropdown' to drop-up
                                           # because this button is at bottom of page
                                           # another option is to add 'fixed' to collectionLayout
                                           # to result in 'centred' drop-down
                                           dropup = TRUE),
                             copyHtml5 = list(extend = 'copyHtml5',
                                              exportOptions = list(columns = ':visible')),
                             printButton = list(extend = 'print',
                                                exportOptions = list(columns = ':visible')),
                             downloadButton = list(extend = 'collection',
                                                   buttons = list(
                                                     list(extend = 'csvHtml5',
                                                          exportOptions = list(columns = ':visible'),
                                                          filename = 'DailyMeasure'),
                                                     list(extend = 'excel',
                                                          exportOptions = list(columns = ':visible'),
                                                          filename = 'DailyMeasure'),
                                                     list(extend = 'pdf',
                                                          exportOptions = list(columns = ':visible'),
                                                          filename = 'DailyMeasure')),
                                                   text = 'Download'),
                             # initComplete = DT::JS(semantic_popupJS),
                             drawCallback = DT::JS(semantic_popupJS),
                             `responsive-resize` = DT::JS(semantic_popupJS),
                             `responsive-display` = DT::JS(paste(
                               "function ( e, datatable, row, showHide, update )",
                               "{console.log( 'Details for row '+row.index()+' '",
                               "+(showHide ? 'shown' : 'hidden'))}")),
                             # responsive-display doesn't seem to work
                             paging = FALSE,
                             scrollY = "60vh",
                             # 60% of window height, otherwise will be just a few rows in size
                             deferRender = TRUE,
                             scrollX = FALSE,
                             fixedColumns = FALSE,
                             columnDefs = list(list()), # allows hiding columns by default
                             ...) {
  buttons <- list()
  j <- 1
  for (i in list(colvis, copyHtml5, printButton, downloadButton)) {
    if (!is.null(i)) {
      buttons[[j]] <- i
      j <- j + 1
    }
  }

  options <- list(dom = dom, buttons = buttons, drawCallback = drawCallback,
                  paging = paging, scrollY = scrollY, deferRender = deferRender, scrollX = scrollX,
                  fixedColumns = fixedColumns, columnDefs = columnDefs,
                  `responsive-resize` = `responsive-resize`,
                  `responsive-display` = `responsive-display`)
  DT::datatable(data, fillContainer = fillContainer, extensions = extensions, options = options, ... )
}
