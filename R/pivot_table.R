
#' Create a pivot table
#'
#' @param data a \code{data.frame}.
#' @param rows Character vector of variable(s) to use as rows.
#' @param cols Character vector of variable(s) to use as columns.
#' @param stats Statistic(s) to compute.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table := melt dcast
#' @importFrom stats as.formula
#'
#' @example examples/pivot_table.R
pivot_table <- function(data, rows, cols = NULL, stats = c("n", "p", "p_row", "p_col")) {
  stats <- match.arg(stats, several.ok = TRUE)
  data <- as.data.table(data)
  agg <- data[, list(n = .N), keyby = c(rows, cols)]
  agg[, p := round(n / sum(n, na.rm = TRUE) * 100, 2)]
  if (is.null(cols))
    return(agg[])
  agg[, p_row := round(n / sum(n, na.rm = TRUE) * 100, 2), by = c(rows)]
  agg[, p_col := round(n / sum(n, na.rm = TRUE) * 100, 2), by = c(cols)]
  agg[, (stats) := lapply(.SD, as.numeric), .SDcols = stats]
  agg <- melt(
    data = agg,
    id.vars = c(rows, cols),
    measure.vars = stats,
    variable.factor = FALSE,
    variable.name = "stats",
    verbose = FALSE
  )
  dcast(
    data = agg,
    formula = as.formula(paste(
      paste(c(rows, "stats"), collapse = " + "),
      paste(cols, collapse = " + "),
      sep = " ~ "
    )),
    value.var = "value",
    sep = "_|_",
    fill = 0
  )
}
