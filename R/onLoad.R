
#' Adds the content of inst/assets/ to flexpivot/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("flexpivot", system.file("assets", package = "flexpivot"))
}
