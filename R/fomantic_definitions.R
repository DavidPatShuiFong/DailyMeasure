# fomantic (semantic.ui) string functions

#' Create semantic/fomantic tags with attached tooltips (text and HTML)
#'
#' User-defined colour and popuptext (tooltip) or popuphtml (HTMl tooltip)
#'
#' @param tag list of tag contents
#' @param colour list of colours
#' @param popuptext list of popuptexts
#' @param popuphtml (alternative) list of popup html
#'
#' @return vector of semantic/fomantic tags
semantic_tag <- function(tag, colour="", popuptext = NA, popuphtml = NA) {
  #
  paste0('<span class="huge ', colour, ' ui tag label"',
         ifelse(!is.na(popuphtml),
                paste0('data-variation="wide" data-position = "left center"',
                       'data-htmltagX="', # this is a 'dummy' attribute
                       # to help datatables order this column alphabetically!
                       tag, # will order this column alphabetically by 'tag'
                       '"',
                       'data-html="',
                       popuphtml,
                       '"', sep=""),
                # 'data-variation' is only available in the
                # fomantic version of semantic.ui
                # as of writing, semantic.ui does not allow
                # variation in text-size of javascript-free tag
                ''),
         '> ',
         ifelse(!is.na(popuptext),
                paste0('<span data-tooltip = "',
                       popuptext,
                       '" data-variation = "wide huge" data-position = "left center">', sep=""),
                ''),
         tag,
         ifelse(!is.na(popuptext), '</span>', ''),
         ' </span>', sep = "")
  # paste0 is vectorized version of 'paste'
}

#' Create semantic/fomantic buttons with attached tooltips (text and HTML)
#'
#' user-defined colour and popuptext (tooltip) or popuphtml (HTML tooltip)
#'
#' @param button list of buttons contents
#' @param colour list of colours
#' @param popuptext list of popup texts
#' @param popuphtml (alternative) list of popup html
#'
#' @return vector of semantic/fomantic buttons
semantic_button <- function(button, colour="", popuptext = NA, popuphtml = NA) {
  paste0('<span class="huge ', colour, ' ui button"',
         ifelse(!is.na(popuphtml),
                paste0('data-variation="wide" data-position = "left center" data-html="',
                       popuphtml,
                       '"', sep=""),
                # 'data-variation' is only available
                # in the fomantic version of semantic.ui
                # as of writing, semantic.ui does not allow variation
                # in text-size of javascript-free tags
                ''),
         '> ',
         ifelse(!is.na(popuptext),
                paste0('<span data-tooltip = "',
                       popuptext,
                       '" data-variation = "wide huge" data-position = "left center">', sep=""),
                ''),
         button,
         ifelse(!is.na(popuptext), '</span>', ''),
         ' </span>', sep = "")
  # paste0 is vectorized version of 'paste'
}

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
#' @param initComplete by default, execute JS code to enable semantic popups
#' @param paging set to FALSE, otherwise hard to 'print'/copy a whole table
#'
#' @return DT datatable object
#' @export
datatable_styled <- function(data, fillContainer = TRUE,
                             extensions = c('Buttons', 'Scroller', 'Responsive'),
                             dom = 'frltiBp',
                             buttons = list(list(extend = 'colvis',
                                                 collectionLayout = "four-column"),
                                            list(extend = 'copyHtml5',
                                                 exportOptions = list(columns = ':visible')),
                                            list(extend = 'print',
                                                 exportOptions = list(columns = ':visible')),
                                            list(extend = 'collection',
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
                                                 text = 'Download'
                                            )),
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
                             scrollX = FALSE,
                             fixedColumns = FALSE,
                             columnDefs = list(list()), # allows hiding columns by default
                             ...) {
  options <- list(dom = dom, buttons = buttons, drawCallback = drawCallback,
                  paging = paging, scrollY = scrollY, scrollX = scrollX,
                  fixedColumns = fixedColumns, columnDefs = columnDefs,
                  `responsive-resize` = `responsive-resize`,
                  `responsive-display` = `responsive-display`)
  DT::datatable(data, fillContainer = fillContainer, extensions = extensions, options = options, ... )
}
