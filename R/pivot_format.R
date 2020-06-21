
#' Flextable format for `pivot_table`
#'
#' @param pivot a `pivot_table` object.
#' @param background Background color.
#' @param border Border color (applies to all table).
#' @param fontSize Font size (applies to all table).
#' @param labels Custom labels for statistics, see \code{\link{pivot_labels}}.
#' @param formatter Function to format content, see \code{\link{pivot_formatter}}.
#'
#' @return a `flextable` object.
#' @export
#'
#' @importFrom flextable flextable theme_zebra merge_v bg color bold fontsize padding width border
#' @importFrom officer fp_border
#' @importFrom data.table copy .SD first := setnames
#'
#' @example examples/pivot_format.R
pivot_format <- function(pivot,
                         background = "#81A1C1",
                         border = "#FFFFFF",
                         fontSize = 14,
                         labels = pivot_labels(),
                         formatter = pivot_formatter()) {
  if (!inherits(pivot, "pivot_table"))
    stop("'pivot' must be a 'pivot_table' object")
  pivot <- copy(pivot)
  rows <- attr(pivot, "rows", exact = TRUE)
  cols <- attr(pivot, "cols", exact = TRUE)
  if (!is.null(cols)) {

    # Apply formatter
    cols_vars <- setdiff(names(pivot), c(rows, "stats"))
    pivot[, (cols_vars) := lapply(.SD, as.character), .SDcols = cols_vars]
    pivot[stats == "n", (cols_vars) := lapply(.SD, function(x) {
      formatter$n(as.numeric(x))
    }), .SDcols = cols_vars]
    pivot[stats == "p", (cols_vars) := lapply(.SD, function(x) {
      formatter$p(as.numeric(x))
    }), .SDcols = cols_vars]
    pivot[stats == "p_col", (cols_vars) := lapply(.SD, function(x) {
      formatter$p_col(as.numeric(x))
    }), .SDcols = cols_vars]
    pivot[stats == "p_row", (cols_vars) := lapply(.SD, function(x) {
      formatter$p_row(as.numeric(x))
    }), .SDcols = cols_vars]

    # Apply labels
    pivot[stats == "n", stats := labels$n]
    pivot[stats == "p", stats := labels$p]
    pivot[stats == "p_col", stats := labels$p_col]
    pivot[stats == "p_row", stats := labels$p_row]
    setnames(pivot, "stats", labels$stats)

  } else {

    # Apply formatter
    cols_vars <- setdiff(names(pivot), rows)
    pivot[, (cols_vars) := lapply(.SD, as.character), .SDcols = cols_vars]
    if (hasName(pivot, "n")) {
      pivot[, n := formatter$n(as.numeric(n))]
    }
    if (hasName(pivot, "p")) {
      pivot[, p := formatter$p(as.numeric(p))]
    }
    if (hasName(pivot, "p_col")) {
      pivot[, p_col := formatter$p_col(as.numeric(p_col))]
    }
    if (hasName(pivot, "p_row")) {
      pivot[, p_row := formatter$p_row(as.numeric(p_row))]
    }

    # Apply labels
    setnames(
      x = pivot,
      old = c("n", "p", "p_row", "p_col"),
      new = c(labels$n, labels$p, labels$p_col, labels$p_row),
      skip_absent = TRUE
    )
  }

  if (!is.null(labels$rows)) {
    setnames(
      x = pivot,
      old = rows,
      new = labels$rows,
      skip_absent = TRUE
    )
  }

  ft <- flextable(pivot)
  ft <- theme_zebra(ft, odd_header = "transparent", even_header = "transparent", odd_body = "#ECEFF4")
  ft <- merge_v(ft, part = "body", j = seq_along(rows))
  ft <- bg(ft, j = seq_along(rows), bg = background, part = "body")
  ft <- bg(ft, bg = background, part = "header")
  ft <- color(ft, j = seq_along(rows), color = "#FFFFFF", part = "body")
  ft <- color(ft, color = "#FFFFFF", part = "header")
  ft <- bold(ft, j = seq_along(rows))
  ft <- fontsize(x = ft, size = fontSize, part = "all")
  ft <- padding(x = ft, padding = 10, part = "all")
  ft <- width(x = ft, width = 1.5)
  if (!is.null(cols)) {
    ft <- border(
      ft, i = as.formula(paste0(
        "~ ", labels$stats, " == '", pivot[[labels$stats]][1], "'"
      )),
      border.top = officer::fp_border(color = "#D8DEE9", width = 2), part = "body"
    )
  }
  if (!is.null(border))
    ft <- border(ft, border = officer::fp_border(color = "#FFFFFF"), part = "all")
  class(ft) <- c(class(ft), "flexpivot")
  return(ft)
}





#' Labels for \code{pivot_format}
#'
#' @param stats Name of statistics column.
#' @param n Count.
#' @param p Percentage.
#' @param p_col Column perc.
#' @param p_row Row perc.
#' @param rows Labels for variables use as rows.
#'
#' @return a \code{list} that can be use in \code{\link{pivot_format}}.
#' @export
#'
#' @examples
#' data("diamonds", package = "ggplot2")
#'
#' # With two variables
#' pivot_format(
#'   pivot = pivot_table(diamonds, rows = "cut", cols = "color"),
#'   labels = pivot_labels(
#'     stats = "Statistique",
#'     n = "N",
#'     p = "%",
#'     rows = c("Cut variable")
#'   )
#' )
pivot_labels <- function(stats = "Statistic",
                         n = "N",
                         p = "%",
                         p_col = "Col %",
                         p_row = "Row %",
                         rows = NULL) {
  list(
    stats = stats,
    n = n,
    p = p,
    p_col = p_col,
    p_row = p_row,
    rows = rows
  )
}





#' Formatters for \code{pivot_format}
#'
#' @param n Function, applied to n.
#' @param p Function, applied to p.
#' @param p_col Function, applied to p_col.
#' @param p_row Function, applied to p_row.
#'
#' @return a \code{list} of \code{function}s that can be use in \code{\link{pivot_format}}.
#' @export
#'
#' @example examples/pivot_formatter.R
pivot_formatter <- function(n = round,
                            p = function(x) {
                              paste0(round(x, 1), "%")
                            },
                            p_col = function(x) {
                              paste0(round(x, 1), "%")
                            },
                            p_row = function(x) {
                              paste0(round(x, 1), "%")
                            }) {
  list(
    n = n,
    p = p,
    p_col = p_col,
    p_row = p_row
  )
}




