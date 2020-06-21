

# Test addin --------------------------------------------------------------

library(shiny)
library(esquisse)
library(flextable)
library(flexpivot)


data("diamonds", package = "ggplot2")

ui <- fluidPage(
  tags$h3("flexpivot"),
  dragulaInput(
    inputId = "vars",
    sourceLabel = "Variables",
    targetsLabels = c("row", "col", "weight"),
    choices = names(diamonds),
    replace = FALSE
  ),
  # verbatimTextOutput("test")
  pivotOutput(outputId = "table")
)

server <- function(input, output, session) {

  output$test <- renderPrint({
    input$vars
  })

  pivot_r <- reactive({
    req(input$vars$target$row)

    pivot_table(
      data = diamonds,
      rows = input$vars$target$row,
      cols = input$vars$target$col
    )
  })

  output$table <- renderPivot({
    pivot_r()
  })

}

runGadget(
  app = ui,
  server = server,
  viewer = dialogViewer(dialogName = "", width = 1000, height = 750)
)

