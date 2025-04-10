# app/main.R

box::use(
  bslib,
  shiny[div, icon, moduleServer, NS, stopApp, tagList, tags, reactive],
  shinyjs[useShinyjs],
  waiter[useWaiter, waiter_hide, waiterShowOnLoad],
)

box::use(
  app / logic / dev_utils,
  app / view / conversion_main,
  app / view / conversion_sidebar,
  app / view / deconvolution_process,
  app / view / deconvolution_sidebar,
  app / view / ki_kinact_sidebar,
  app / view / log_view,
  app / view / log_sidebar,
  app / logic / logging[start_logging, write_log, close_logging]
)

suppressWarnings(library(logr))

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    dev_utils$add_dev_headers(),
    div(id = "blocking-overlay"),
    useWaiter(),
    waiterShowOnLoad(
      html = tags$div(
        style = "text-align: center;",
        tags$img(
          src = "static/logo_animated.svg",
          width = "400px",
          height = "400px"
        ),
        tags$div(
          style = paste0(
            "font-family: monospace; font-size: 50px; color: blac",
            "k; opacity: 0; animation: fadeIn 1s ease-in forwards",
            "; animation-delay: 1s;"
          ),
          "KiwiFlow"
        )
      )
    ),
    useShinyjs(),
    bslib$page_navbar(
      id = ns("tabs"),
      title = tags$div(
        tags$img(
          src = "static/logo.svg",
          height = "42rem",
          style = "margin-right: 5px; margin-top: -2px"
        ),
        tags$span(
          "KiwiFlow",
          style = "font-size: 21px; font-family: monospace;"
        )
      ),
      window_title = "KiwiFlow 0.1.0",
      underline = TRUE,
      bslib$nav_panel(
        title = "Deconvolution",
        bslib$page_sidebar(
          sidebar = deconvolution_sidebar$ui(
            ns("deconvolution_pars")
          ),
          bslib$card(deconvolution_process$ui(
            ns("deconvolution_process")
          ))
        )
      ),
      bslib$nav_panel(
        title = "Protein Conversion",
        class = "locked-panel",
        div(id = "overlay-message", "Module still in work ..."),
        bslib$page_sidebar(
          sidebar = conversion_sidebar$ui(ns("protein_conversion")),
          bslib$card(
            bslib$card_header("Conversion Table"),
            conversion_main$ui(ns("conversion_card"))
          )
        )
      ),
      bslib$nav_panel(
        title = "KI/Kinact",
        class = "locked-panel",
        div(id = "overlay-message", "Module still in work ..."),
        bslib$page_sidebar(
          sidebar = ki_kinact_sidebar$ui(ns("ki")),
          bslib$navset_card_tab(
            bslib$nav_panel(title = "Kobs Table"),
            bslib$nav_panel(title = "Kinact Table")
          )
        )
      ),
      bslib$nav_panel(
        title = "Logs",
        bslib$page_sidebar(
          sidebar = log_sidebar$ui(ns("log_sidebar")),
          bslib$card(
            log_view$ui(ns("logs"))
          )
        )
      ),
      bslib$nav_spacer(),
      bslib$nav_menu(
        title = "Links",
        align = "right",
        icon = icon("link"),
        bslib$nav_item(
          tags$a(
            tags$span(
              tags$i(class = "fa-brands fa-github me-1"),
              "GitHub"
            ),
            href = "https://github.com/infinity-a11y/MSFlow",
            target = "_blank",
            class = "nav-link"
          )
        ),
        bslib$nav_item(
          tags$a(
            tags$span(
              tags$img(
                src = "static/liora_logo.png",
                style = "height: 1em; margin-right: 5px;"
              ),
              "Liora Bioinformatics"
            ),
            href = "https://www.liora-bioinformatics.com/home",
            target = "_blank",
            class = "nav-link"
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Kill server on session end
    session$onSessionEnded(function() {
      write_log("Session closed")
      stopApp()
    })

    # Initiate logging
    start_logging()
    write_log("Session started")

    # Log view server
    active_tab_reactive <- reactive({
      input$tabs
    })
    log_buttons <- log_sidebar$server("log_sidebar")
    log_view$server("logs", active_tab_reactive, log_buttons)

    # Conversion server
    conversion_main$server("conversion_card")

    # Deconvolution sidebar server
    dirs <- deconvolution_sidebar$server("deconvolution_pars")

    # Deconvolution process server
    deconvolution_process$server("deconvolution_process", dirs)

    # Hide waiter
    Sys.sleep(2)
    waiter_hide()
  })
}
