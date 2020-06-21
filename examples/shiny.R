
library(shiny)
library(flexpivot)
data("diamonds", package = "ggplot2")

ui <- fluidPage(
  tags$h2("Pivot table in Shiny"),
  fluidRow(
    column(
      width = 6,
      selectInput(
        inputId = "row",
        label = "Row",
        choices = c("cut", "color", "clarity"),
        width = "100%"
      )
    ),
    column(
      width = 6,
      selectInput(
        inputId = "col",
        label = "Col",
        choices = c("cut", "color", "clarity"),
        selected = "color",
        width = "100%"
      )
    )
  ),
  pivotOutput("pivot")
)

server <- function(input, output, session) {

  output$pivot <- renderPivot({
    pivot_table(diamonds, input$row, input$col)
  })

}

if (interactive())
  shinyApp(ui, server)
