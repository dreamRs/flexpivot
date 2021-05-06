
#' @title Pivot table Output in Shiny
#'
#' @description Display a \code{\link{pivot_table}} in Shiny:
#'  * pivotOutput / renderPivot: adds a button to download pivot table in PowerPoint, Word and Excel.
#'  * pivot2Output / renderPivot2: display only the pivot table.
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
pivot2Output <- function(outputId, width = "100%", ...) {
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
#' @importFrom shiny exprToFunction renderUI is.reactive
#' @importFrom flextable set_table_properties htmltools_value
renderPivot2 <- function(expr,
                         width = 1,
                         background = "#81A1C1",
                         color = "#FFFFFF",
                         border = "#FFFFFF",
                         font_size = 14,
                         font_name = NULL,
                         labels = pivot_labels(),
                         formatter = pivot_formatter(),
                         env = parent.frame(),
                         quoted = FALSE) {
  pivot_fun <- exprToFunction(expr, env, quoted)
  renderUI({
    pivot <- pivot_fun()
    if (inherits(pivot, "pivot_table")) {
      pivot <- pivot_format(
        pivot = pivot,
        background = background,
        border = border,
        font_size = font_size,
        font_name = font_name,
        labels = labels,
        formatter = formatter
      )
    }
    if (!inherits(pivot, "flexpivot")) {
      stop("renderPivot2: 'expr' must return a pivot_table or pivot_format object.", call. = FALSE)
    }

    if (is.reactive(width)) {
      width <- width()
    }
    pivot <- set_table_properties(pivot, layout = "autofit", width = width)
    htmltools_value(pivot)
  })
}


#' @param export Export labels, use \code{NULL} to disable all exports.
#' @rdname pivot-shiny
#' @export
#' @importFrom htmltools tags validateCssUnit tagList
#' @importFrom shiny actionButton icon downloadLink actionLink
#' @importFrom shinyWidgets dropMenu
pivotOutput <- function(outputId, width = "100%", export = export_labels(), ...) {
  tags$div(
    class = "pivot-table-container",
    html_dependency_clipboard(),
    tags$style(sprintf(
      "#%s_dropmenu {display: inline;}", paste0(outputId, "_exports")
    )),
    ...,
    if (!is.null(export)) {
      dropMenu(
        actionButton(
          inputId = paste0(outputId, "_exports"),
          label = export$export,
          icon = icon("caret-down"),
          class = "btn-xs pull-right"
        ),
        placement = "bottom-end",
        if (!is.null(export$clipboard)) {
          tagList(
            actionLink(
              inputId = paste0(outputId, "_clipboard"),
              label = tagList(
                tags$img(
                  src = "flexpivot/icons/clippy.svg",
                  style = "height: 20px; margin: 3px;"
                ),
                export$clipboard
              ),
              `data-clipboard-target` = paste0("#", outputId),
              class = "btn-pivot-table-clipboard"
            ),
            tags$br()
          )
        },
        if (!is.null(export$powerpoint)) {
          tagList(
            downloadLink(
              outputId = paste0(outputId, "_export_pptx"),
              label = tagList(
                tags$img(
                  src = "flexpivot/icons/microsoft-powerpoint.png",
                  style = "height: 20px; margin: 3px;"
                ),
                export$powerpoint
              )
            ),
            tags$br()
          )
        },
        if (!is.null(export$word)) {
          tagList(
            downloadLink(
              outputId = paste0(outputId, "_export_docx"),
              label = tagList(
                tags$img(
                  src = "flexpivot/icons/microsoft-word.png",
                  style = "height: 20px; margin: 3px;"
                ),
                export$word
              )
            ),
            tags$br()
          )
        },
        if (!is.null(export$excel)) {
          downloadLink(
            outputId = paste0(outputId, "_export_xlsx"),
            label = tagList(
              tags$img(
                src = "flexpivot/icons/microsoft-excel.png",
                style = "height: 20px; margin: 3px;"
              ),
              export$excel
            )
          )
        }
      )
    },
    tags$div(
      id = outputId, class = "shiny-html-output shiny-pivot-output",
      style = if (!is.null(width)) paste0("width:", validateCssUnit(width), ";"),
      style = "overflow: auto;"
    ),
    tags$script(sprintf(
      "var %s = new ClipboardJS('#%s');",
      paste0(gsub("[^[:alnum:]]", "", outputId), "_clipboard"),
      paste0(outputId, "_clipboard")
    ))
  )
}

#' @param clipboard,powerpoint,word,excel Labels to display in
#'  export menu, use \code{NULL} to disable specific format.
#' @rdname pivot-shiny
#' @export
export_labels <- function(export = "Export",
                          clipboard = "Copy to clipboard",
                          powerpoint = "Export to PowerPoint",
                          word = "Export to Word",
                          excel = "Export to Excel") {
  list(
    export = export,
    clipboard = clipboard,
    powerpoint = powerpoint,
    word = word,
    excel = excel
  )
}

#' @param label_value For Excel output, the label for variable containing the values.
#' @param filename A string of the filename to export WITHOUT extension, it will be added accordint to type of export.
#' @rdname pivot-shiny
#' @export
#' @importFrom shiny installExprFunction createRenderFunction downloadHandler
#'  createWebDependency is.reactive
#' @importFrom htmltools renderTags resolveDependencies
#' @importFrom flextable set_table_properties htmltools_value
renderPivot <- function(expr,
                        width = 1,
                        background = "#81A1C1",
                        color = "#FFFFFF",
                        border = "#FFFFFF",
                        font_size = 11,
                        font_name = NULL,
                        labels = pivot_labels(),
                        formatter = pivot_formatter(),
                        label_value = "value",
                        env = parent.frame(),
                        quoted = FALSE,
                        filename = "export-pivot") {
  pivot_fun <- exprToFunction(expr, env, quoted)
  createRenderFunction(pivot_fun, function(pivot, shinysession, name, ...) {
    if (is.null(pivot) || length(pivot) == 0)
      return(NULL)
    if (inherits(pivot, "pivot_table")) {
      pivot <- pivot_format(
        pivot = pivot,
        background = background,
        border = border,
        font_size = font_size,
        font_name = font_name,
        labels = labels,
        formatter = formatter
      )
    }
    if (!inherits(pivot, "flexpivot")) {
      stop("renderPivot: 'expr' must return a pivot_table or pivot_format object.", call. = FALSE)
    }

    shinysession$output[[paste0(name, "_export_pptx")]] <- downloadHandler(
      filename = function() {
        if (is.function(filename))
          filename <- filename()
        paste0(filename, ".pptx")
      },
      content = function(file) {
        export_pptx(
          x = pivot,
          output = file,
          background = background,
          border = border,
          font_size = font_size,
          font_name = font_name,
          labels = labels,
          formatter = formatter
        )
      }
    )
    shinysession$output[[paste0(name, "_export_docx")]] <- downloadHandler(
      filename = function() {
        if (is.function(filename))
          filename <- filename()
        paste0(filename, ".docx")
      },
      content = function(file) {
        export_docx(
          x = pivot,
          output = file,
          background = background,
          border = border,
          font_size = font_size,
          font_name = font_name,
          labels = labels,
          formatter = formatter
        )
      }
    )
    shinysession$output[[paste0(name, "_export_xlsx")]] <- downloadHandler(
      filename = function() {
        if (is.function(filename))
          filename <- filename()
        paste0(filename, ".xlsx")
      },
      content = function(file) {
        data <- attr(pivot, which = "data", exact = TRUE)
        export_xlsx(x = data, output = file, label_value = label_value)
      }
    )
    if (is.reactive(width)) {
      width <- width()
    }
    out <- set_table_properties(pivot, layout = "autofit", width = width)
    out <- htmltools_value(out)
    rendered <- renderTags(out)
    dependencies <- lapply(
      X = resolveDependencies(rendered$dependencies),
      FUN = createWebDependency
    )
    list(html = rendered$html, deps = dependencies)
  }, pivotOutput, list())
}




#' @importFrom htmltools htmlDependency
html_dependency_clipboard <- function() {
  htmlDependency(
    name = "clipboard",
    version = "2.0.6",
    package = "flexpivot",
    src = c(file = "assets/clipboard"),
    script = c("clipboard.min.js")
  )
}








