
#' Flextable format for `pivot_table`
#'
#' @param pivot a `pivot_table` object.
#' @param background Background color.
#' @param border Border color (applies to all table).
#' @param labels Custom labels for statistics, see \code{\link{pivot_labels}}.
#'
#' @return a `flextable` object.
#' @export
#'
#' @importFrom flextable flextable theme_zebra merge_v bg color bold fontsize padding width border
#' @importFrom officer fp_border
#' @importFrom data.table first := setnames
#'
#' @example examples/pivot_format.R
pivot_format <- function(pivot, background = "#81A1C1", border = "#FFFFFF", labels = pivot_labels()) {
  if (!inherits(pivot, "pivot_table"))
    stop("'pivot' must be a 'pivot_table' object")
  rows <- attr(pivot, "rows")
  cols <- attr(pivot, "cols")
  if (hasName(pivot, "stats")) {
    pivot[stats == "n", stats := labels$n]
    pivot[stats == "p", stats := labels$p]
    pivot[stats == "p_col", stats := labels$p_col]
    pivot[stats == "p_row", stats := labels$p_row]
    setnames(pivot, "stats", labels$stats)
  } else {
    setnames(
      x = pivot,
      old = c("n", "p", "p_row", "p_col"),
      new = c(labels$n, labels$p, labels$p_col, labels$p_row),
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
  ft <- fontsize(x = ft, size = 15, part = "all")
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
  ft
}





#' Labels for \code{pivot_format}
#'
#' @param stats Name of statistics column.
#' @param n Count.
#' @param p Percentage.
#' @param p_col Column perc.
#' @param p_row Row perc.
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
#'   labels = pivot_labels(stats = "Statistique", n = "N", p = "%")
#' )
pivot_labels <- function(stats = "Statistic",
                         n = "Count",
                         p = "Percentage",
                         p_col = "Column perc.",
                         p_row = "Row perc.") {
  list(
    stats = stats,
    n = n,
    p = p,
    p_col = p_col,
    p_row = p_row
  )
}










