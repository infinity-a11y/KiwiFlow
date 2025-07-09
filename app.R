#Rhino / shinyApp entrypoint. Do not edit.
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

rhino::app()

# start_app <- function ()
# {
#     entrypoint <- rhino:::read_config()$legacy_entrypoint
#     rhino:::configure_box()
#     rhino:::configure_static()
#     rhino:::configure_logger()
#     if (identical(entrypoint, "app_dir")) {
#         return(shiny::shinyAppDir("app"))
#     }
#     rhino:::make_app(rhino:::load_main(use_source = identical(entrypoint, "source"),
#         expect_shiny_module = is.null(entrypoint)))
# }

# start_app()