# app/view/deconvolution_sidebar.R

box::use(
  shiny[NS],
  shinyFiles[shinyDirButton],
  bslib[sidebar],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    title = "File Upload",
    shinyDirButton(ns('folder'), 'Select folder', 'Select .raw Folder(s)', FALSE)
  )
}
