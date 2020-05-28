
#' Flex format for `pivot_table`
#'
#' @param pivot a `pivot_table` object.
#' @param background Background color.
#'
#' @return a `flextable` object.
#' @export
#'
#' @importFrom flextable flextable theme_zebra merge_v bg color bold fontsize padding width border
#' @importFrom officer fp_border
#' @importFrom data.table first
#'
#' @example examples/flex.R
flex <- function(pivot, background = "#81A1C1") {
  if (!inherits(pivot, "pivot_table"))
    stop("'pivot' must be a 'pivot_table' object")
  rows <- attr(pivot, "rows")
  cols <- attr(pivot, "cols")
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
        "~ stats == '", pivot$stats[1], "'"
      )),
      border.top = officer::fp_border(color = "#D8DEE9", width = 2), part = "body"
    )
  }
  ft
}
