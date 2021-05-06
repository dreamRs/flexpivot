
#' Pivot table RStudio addin
#'
#' @param data a \code{data.frame}.
#'
#' @return No value.
#' @export
#'
#' @importFrom shiny runGadget dialogViewer req observeEvent
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
pivot_addin <- function(data) { # nocov start

  runGadget(
    app = ui(pivot_ui(id = "pivot", variables = names(data))),
    server = function(input, output) {
      pivot_server(
        id = "pivot",
        data = data
      )
      observeEvent(input$close, shiny::stopApp())
    },
    viewer = dialogViewer(
      dialogName = "Flexpivot",
      width = 1000,
      height = 750
    )
  )
} # nocov end


#' @importFrom shiny bootstrapPage actionButton
#' @importFrom htmltools singleton tagList tags
ui <- function(...) {
  bootstrapPage(
    singleton(x = tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "flexpivot/css/styles.css")
    )),
    tags$div(
      class = "pivot-title-container",
      tags$h1("flexpivot", class = "pivot-title"),
      tags$div(
        class = "pull-right",
        actionButton(
          inputId = "close",
          label = NULL,
          icon = icon("times", class = "fa-lg"),
          class = "btn-sm",
          title = "Close Window"
        )
      )
    ),
    tags$div(
      class = "container-fluid",
      ...
    )
  )
}


#' @importFrom shiny NS fluidRow column
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom htmltools tags
pivot_ui <- function(id, variables) {
  ns <- NS(id)
  tags$div(
    class = "pivot-main-container",
    fluidRow(
      column(
        width = 6,
        pickerInput(
          inputId = ns("rows"),
          label = "Rows:",
          choices = variables,
          multiple = TRUE,
          options = pickerOptions(
            liveSearch = TRUE
          ),
          width = "100%"
        )
      ),
      column(
        width = 6,
        pickerInput(
          inputId = ns("cols"),
          label = "Column:",
          choices = variables,
          multiple = TRUE,
          options = pickerOptions(
            liveSearch = TRUE,
            maxOptions = 1
          ),
          width = "100%"
        )
      )
    ),
    pivotOutput(outputId = ns("pivot"))
  )
}


#' @importFrom shiny moduleServer req
pivot_server <- function(id, data) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$pivot <- renderPivot({
        req(input$rows)
        pivot_table(
          data = data,
          rows = input$rows,
          cols = input$cols
        )
      })

    }
  )
}





