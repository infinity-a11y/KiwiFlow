# app/view/ki_kinact.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  shiny[actionButton, br, textInput, NS],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    title = "Parameter Settings",
    card(
      card_header(
        class = "bg-dark",
        "Set noise level (+/-)"
      ),
      card_body(
        textInput(ns("cmpd_label"), "Compound Labeling", "4"),
        textInput(ns("prot_peak"), "Protein Peak", "10") 
      )
    ),
    br(),
    card(
      card_header(
        class = "bg-dark",
        "Customize"
      ),
      card_body(
        textInput(ns("n_label"), "Considered number of labeling", "4"),
        textInput(ns("n_spectra"), "Considered number of spectra", "20")
      )
    ),
    br(),
    actionButton(ns("run_ki"), "Calculate KI/Kinact")
  )
}