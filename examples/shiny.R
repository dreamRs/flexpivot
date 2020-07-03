library(shiny)
library(flexpivot)
data("nobel_laureates")

ui <- fluidPage(
  tags$h2("Pivot table in Shiny"),
  fluidRow(
    column(
      width = 6,
      selectInput(
        inputId = "row",
        label = "Row",
        choices = c("category", "gender", "birth_continent", "laureate_type"),
        width = "100%"
      )
    ),
    column(
      width = 6,
      selectInput(
        inputId = "col",
        label = "Col",
        choices = c("category", "gender", "birth_continent", "laureate_type"),
        selected = "gender",
        width = "100%"
      )
    )
  ),
  pivotOutput("pivot")
)

server <- function(input, output, session) {

  output$pivot <- renderPivot({
    pivot_table(nobel_laureates, input$row, input$col)
  }, background = "#A3BE8C")

}

if (interactive())
  shinyApp(ui, server)
