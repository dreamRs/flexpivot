
library(shiny)
library(htmltools)
library(shinyWidgets)
library(ggplot2)

pivotOutput <- function(outputId, width = "100%", ...) {
  tags$div(
    class = "pivot-table-container",
    dropMenu(
      actionButton(
        inputId = paste0(outputId, "_exports"),
        label = "Export",
        icon = icon("caret-down"),
        class = "btn-xs pull-right"
      ),
      placement = "bottom-end",
      downloadLink(
        outputId = paste0(outputId, "_export_pptx"),
        label = "Export to PowerPoint"
      ),
      tags$br(),
      downloadLink(
        outputId = paste0(outputId, "_export_docx"),
        label = "Export to Word"
      ),
      tags$br(),
      downloadLink(
        outputId = paste0(outputId, "_export_xlsx"),
        label = "Export to Excel"
      )
    ),
    tags$div(
      id = outputId, class = "shiny-html-output shiny-pivot-output",
      style = if (!is.null(width)) paste0("width:", validateCssUnit(width), ";"),
      style = "overflow: auto;",
      ...
    )
  )
}


renderPivot <- function(expr,
                        width = 1,
                        background = "#81A1C1",
                        border = "#FFFFFF",
                        fontSize = 14,
                        labels = pivot_labels(),
                        formatter = pivot_formatter(),
                        env = parent.frame(),
                        quoted = FALSE,
                        filename = "export-pivot") {
  installExprFunction(expr, "func", env, quoted)
  createRenderFunction(func, function(result, shinysession, name, ...) {
    if (is.null(result) || length(result) == 0)
      return(NULL)
    if (!inherits(result, "pivot_table"))
      stop("'expr' must return a pivot_table object!")
    pivot_ft <- pivot_format(
      pivot = result,
      background = background,
      border = border,
      fontSize = fontSize,
      labels = labels,
      formatter = formatter
    )
    shinysession$output[[paste0(name, "_export_pptx")]] <- downloadHandler(
      filename = function() {
        if (is.function(filename))
          filename <- filename()
        paste0(filename, ".pptx")
      },
      content = function(file) {
        export_pptx(pivot_ft, output = file)
      }
    )
    shinysession$output[[paste0(name, "_export_docx")]] <- downloadHandler(
      filename = function() {
        if (is.function(filename))
          filename <- filename()
        paste0(filename, ".docx")
      },
      content = function(file) {
        export_docx(pivot_ft, output = file)
      }
    )
    shinysession$output[[paste0(name, "_export_xlsx")]] <- downloadHandler(
      filename = function() {
        if (is.function(filename))
          filename <- filename()
        paste0(filename, ".xlsx")
      },
      content = function(file) {
        export_xlsx(result, output = file)
      }
    )
    pivot_ft <- set_table_properties(pivot_ft, layout = "autofit", width = width)
    pivot_ft <- htmltools_value(pivot_ft)
    rendered <- renderTags(pivot_ft)
    dependencies <- lapply(
      X = resolveDependencies(rendered$dependencies),
      FUN = createWebDependency
    )
    list(html = rendered$html, deps = dependencies)
  }, pivot2Output, list())
}



library(shiny)

ui <- fluidPage(
  tags$h3("Test"),
  pivot2Output("mypivot")
)

server <- function(input, output, session) {

  output$mypivot <- renderPivot2({
    pivot_table(diamonds, rows = "cut", cols = "color")
  })

}

shinyApp(ui, server)


