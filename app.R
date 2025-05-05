#Rhino / shinyApp entrypoint.

app_dir <- normalizePath(file.path("."))
python_path <- file.path(app_dir, "python_env", "python.exe")
if (!file.exists(python_path)) {
  stop("Python executable not found at: ", python_path)
}
Sys.setenv(RETICULATE_PYTHON = python_path)

reticulate::use_python(python_path, required = TRUE)
reticulate::py_run_string("import unidec") 

rhino::app()
