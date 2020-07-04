
#' Pivot table RStudio addin
#'
#' @param data a \code{data.frame}.
#'
#' @return No value.
#' @export
#'
#' @importFrom shiny fluidPage actionButton runGadget dialogViewer req observeEvent
#' @importFrom htmltools tags tagList singleton
#'
#' @examples
#' if (interactive()) {
#'
#'   library(flexpivot)
#'   data("nobel_laureates")
#'   pivot_addin(nobel_laureates)
#'
#' }
pivot_addin <- function(data) {

  if (!requireNamespace(package = "esquisse"))
    stop("Package 'esquisse' is required to run this function, please install it.", call. = FALSE)

  runGadget(
    app = pivot_addin_ui(variables = names(data)),
    server = pivot_addin_server(data),
    viewer = dialogViewer(
      dialogName = "Flexpivot",
      width = 1000,
      height = 750
    )
  )
}



pivot_addin_ui <- function(variables) {
  fluidPage(
    singleton(x = tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "flexpivot/css/styles.css")
    )),
    tags$div(
      class = "pivot-addin-title-box",
      tags$h1(shiny::icon("wrench"), "Pivot table builder", class = "pivot-addin-title"),
      tags$div(
        class = "pivot-addin-closebtn",
        actionButton(inputId = "close", label = "Close", class = "btn-sm")
      )
    ),
    tags$div(
      class = "pivot-addin-container",
      esquisse::dragulaInput(
        inputId = "vars",
        sourceLabel = "Variables",
        targetsLabels = c("row", "col"),
        choices = variables,
        replace = FALSE
      ),
      pivotOutput(outputId = "pivot")
    )
  )
}



pivot_addin_server <- function(data) {
  function(input, output, session) {

    output$pivot <- renderPivot({
      req(input$vars$target$row)
      pivot_table(
        data = data,
        rows = input$vars$target$row,
        cols = input$vars$target$col
      )
    })

    # Close addin
    observeEvent(input$close, shiny::stopApp())

  }
}





