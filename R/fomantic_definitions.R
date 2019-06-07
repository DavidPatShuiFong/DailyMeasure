# fomantic (semantic.ui) string functions

semantic_tag <- function(tag, colour="", popuptext = NA, popuphtml = NA) {
  # returns a vector of tags. user-defined colour and popuptext (tooltip) or popuphtml (HTMl tooltip)
  # note that 'data-variation' is only available in the fomantic version of semantic.ui
  # as of writing, semantic.ui does not allow variation in text-size of javascript-free tags
  paste0('<span class="huge ', colour, ' ui tag label"',
         ifelse(!is.na(popuphtml),
                paste0('data-variation="wide" data-position = "left center" data-html="',
                       popuphtml,
                       '"', sep=""),
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

semantic_button <- function(button, colour="", popuptext = NA, popuphtml = NA) {
  # returns a vector of buttons.
  # user-defined colour and popuptext (tooltip) or popuphtml (HTML tooltip)
  # note that 'data-variation' is only available in the fomantic version of semantic.ui
  # as of writing, semantic.ui does not allow variation in text-size of javascript-free tags
  paste0('<span class="huge ', colour, ' ui button"',
         ifelse(!is.na(popuphtml),
                paste0('data-variation="wide" data-position = "left center" data-html="',
                       popuphtml,
                       '"', sep=""),
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

semantic_popupJS <- c("window.onload = function() {$('.ui.button') .popup({on: 'hover'});
                      $('.ui.tag.label') .popup({on: 'hover'})
                      }")

# (1) necessary for semantic/fomantic JS popups. included directly in datatables options
# (2) provide padding for export/print buttons
datatable_styled <- function(data, fillContainer = TRUE,
                             extensions = c('Buttons', 'Scroller', 'Responsive'),
                             dom = 'frltiBp',
                             buttons = c('copyHtml5', 'csvHtml5', 'excel', 'pdf', 'print'),
                             initComplete = htmlwidgets::JS(semantic_popupJS),
                             paging = FALSE,
                             scrollY = "60vh",
                             # 60% of window height, otherwise will just a few rows in size
                             ...) {
  options <- list(dom = dom, buttons = buttons, initComplete = initComplete,
                  paging = paging, scrollY = scrollY)
  DT::datatable(data, fillContainer = fillContainer, extensions = extensions, options = options, ... )
}
# by default, have export/print buttons, only render what is visible
# javascript code to attach labels to semantic/fomantic JS popups
# no pagination
