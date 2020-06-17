
#' Create a pivot table
#'
#' @param data a \code{data.frame}.
#' @param rows Character vector of variable(s) to use as rows.
#' @param cols Character vector of variable(s) to use as columns.
#' @param wt Character, variable to use as weights if any.
#' @param stats Statistic(s) to compute.
#' @param total Logical, add total or not.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table := setnames melt dcast setattr cube set .SD setorderv
#' @importFrom stats as.formula
#'
#' @example examples/pivot_table.R
pivot_table <- function(data,
                        rows,
                        cols = NULL,
                        wt = NULL,
                        stats = c("n", "p", "p_row", "p_col"),
                        total = TRUE) {
  stats <- match.arg(stats, several.ok = TRUE)
  data <- as.data.table(data)
  if (is.null(wt)) {
    data[, wt := 1]
  } else {
    if (!hasName(data, wt))
      stop("Invalid 'wt' column: must be an available column in data.", call. = FALSE)
    setnames(data, old = wt, new = "wt")
  }
  agg <- cube(x = data, j = list(n = sum(wt)), by = c(rows, cols), id = TRUE)
  setorderv(agg, cols = rows, na.last = TRUE)
  for (i in c(rows, cols)) {
    ind <- unlist(agg[, lapply(.SD, is.na), .SDcols = i], use.names = FALSE) &
      agg$grouping > 0
    if (isTRUE(total)) {
      set(x = agg, i = which(ind), j = i, value = "Total")
    } else {
      agg <- agg[-which(ind)]
    }
  }
  agg[, p := round(n / sum(n, na.rm = TRUE) * 100, 2), by = "grouping"]
  if (is.null(cols)) {
    agg[, grouping := NULL]
    setattr(agg, "class", c(class(agg), "pivot_table"))
    setattr(agg, "rows", rows)
    setattr(agg, "cols", cols)
    return(agg[])
  }
  agg[, p_row := round(n / sum(n, na.rm = TRUE) * 100, 2), by = c(rows, "grouping")]
  agg[, p_col := round(n / sum(n, na.rm = TRUE) * 100, 2), by = c(cols, "grouping")]
  agg[, (stats) := lapply(.SD, as.numeric), .SDcols = stats]
  agg <- melt(
    data = agg,
    id.vars = c(rows, cols),
    measure.vars = stats,
    variable.factor = FALSE,
    variable.name = "stats",
    verbose = FALSE
  )
  result <- dcast(
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

  setattr(result, "class", c(class(result), "pivot_table"))
  setattr(result, "rows", rows)
  setattr(result, "cols", cols)
  result[]
}


