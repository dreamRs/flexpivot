
#' Pivot table Output in Shiny
#'
#' @param outputId Output variable to read from.
#' @param width If not \code{NULL}, must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param ... Other arguments to pass to the container tag function.
#'  This is useful for providing additional classes for the tag.
#'
#' @return An HTML output element that can be included in Shiny UI.
#' @export
#'
#' @importFrom htmltools tags validateCssUnit
#'
#' @name pivot-shiny
#'
#' @example examples/shiny.R
pivotOutput <- function(outputId, width = "100%", ...) {
  tags$div(
    id = outputId, class = "shiny-html-output shiny-pivot-output",
    style = if (!is.null(width)) paste0("width:", validateCssUnit(width), ";"),
    style = "overflow: auto;",
    ...
  )
}

#' @param expr An expression that generates a \code{\link{pivot_table}}.
#' @param width Value of the preferred width of the table in percent (\code{[0,1]}).
#' @inheritParams pivot_format
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @rdname pivot-shiny
#' @export
#'
#' @importFrom shiny exprToFunction renderUI
#' @importFrom flextable set_table_properties htmltools_value
renderPivot <- function(expr,
                        width = 1,
                        background = "#81A1C1",
                        border = "#FFFFFF",
                        fontSize = 14,
                        labels = pivot_labels(),
                        formatter = pivot_formatter(),
                        env = parent.frame(),
                        quoted = FALSE) {
  pivot_fun <- shiny::exprToFunction(expr, env, quoted)
  shiny::renderUI({
    pivot <- pivot_fun()
    if (!inherits(pivot, "pivot_table"))
      stop("'expr' must return a pivot_table object!")
    pivot_ft <- pivot_format(pivot)
    pivot_ft <- set_table_properties(pivot_ft, layout = "autofit", width = width)
    htmltools_value(pivot_ft)
  })
}
