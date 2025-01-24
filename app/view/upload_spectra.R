# app/view/upload_spectra.R

box::use(
  bslib[sidebar],
  shiny[actionButton, fileInput, selectInput, textInput, NS],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    title = "File Upload",
    fileInput(
      ns("raw_input"),
      "Select Input File",
      multiple = FALSE,
      accept = c(".txt", ".tab")),
    fileInput(
      ns("mass_input"),
      "Select ExpMW File",
      multiple = FALSE,
      accept = c("text/tab-separated-values")),
    textInput(ns("protein_mass"), "Protein Mass", ""),
    selectInput(
      ns("units"),
      "Select units for Kobs/KI calculations",
      choices = c("\U003BCM - minutes", "M - seconds"), 
      selected = "\U003BCM - minutes"),
    actionButton(ns("run_conversion_function"), "Calculate Conversions")
  )
}