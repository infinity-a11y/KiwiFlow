Sys.setlocale("LC_NUMERIC", "C")

# Allow absolute module imports (relative to the app root).
options(box.path = getwd(), OutDec = ".")

# box.lsp languageserver external hook
if (nzchar(system.file(package = "box.lsp"))) {
  options(
    languageserver.parser_hooks = list(
      "box::use" = box.lsp::box_use_parser
    )
  )
}
