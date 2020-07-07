
#' Create a pivot table
#'
#' @param data a \code{data.frame}.
#' @param rows Character vector of variable(s) to use as rows.
#' @param cols Character vector of variable(s) to use as columns.
#' @param wt Character, variable to use as weights if any.
#' @param stats Statistic(s) to compute.
#' @param total Logical, add total or not.
#' @param total_label Label to use fo total.
#' @param na_label Label to use for missing values.
#' @param complete Complete missing combination between
#'  \code{rows} if several and \code{cols} if any.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table is.data.table copy as.data.table := setnames
#'  melt dcast setattr cube set .SD setorderv chmatch frankv setcolorder fifelse
#' @importFrom stats as.formula
#'
#' @example examples/pivot_table.R
pivot_table <- function(data,
                        rows,
                        cols = NULL,
                        wt = NULL,
                        stats = c("n", "p", "p_row", "p_col"),
                        total = TRUE,
                        total_label = "Total",
                        na_label = "<missing>",
                        complete = TRUE) {
  stats <- match.arg(stats, several.ok = TRUE)
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- as.data.table(data)
  }
  rows_cols <- unique(c(rows, cols))
  for (variable in rows_cols) {
    set(x = data, i = which(is.na(data[[variable]])), j = variable, value = na_label)
  }
  rows_values <- get_levels(data, rows, na_label = na_label)
  if (is_valid(data, cols))
    cols_values <- get_levels(data, cols, na_label = na_label)
  if (is.null(wt)) {
    set(data, j = "wt_pivot_table", value = 1)
  } else {
    if (!hasName(data, wt))
      stop("Invalid 'wt' column: must be an available column in data.", call. = FALSE)
    setnames(data, old = wt, new = "wt_pivot_table")
  }

  agg <- cube(
    x = data,
    j = list(n = colSums(.SD, na.rm = TRUE)),
    .SDcols = "wt_pivot_table",
    by = rows_cols,
    id = TRUE
  )
  if (isTRUE(complete)) {
    agg <- rbind(
      complete(
        data = agg[grouping == 0],
        vars = rows_cols,
        fill = list(grouping = 0, n = 0)
      ),
      agg[grouping > 0]
    )
  }
  agg[, (rows_cols) := lapply(.SD, function(x) {
    if (!inherits(x, c("character", "factor"))) {
      x <- as.character(x)
    }
    x
  }), .SDcols = rows_cols]
  for (j in rows_cols) {
    ind <- is.na(agg[[j]]) & agg$grouping > 0
    if (isTRUE(total)) {
      set(x = agg, i = which(ind), j = j, value = total_label)
    } else {
      agg <- agg[-which(ind)]
    }
  }
  agg[, p := round(n / sum(n, na.rm = TRUE) * 100, 2), by = "grouping"]
  if (is.null(cols)) {
    agg[, grouping := NULL]
    for (row in rev(names(rows_values))) {
      odr <- chmatch(as.character(agg[[row]]), table = c(rows_values[[row]], total_label))
      odr <- frankv(odr, ties.method = "first")
      agg <- agg[order(odr)]
    }
    vars <- c(
      setdiff(names(agg), c("n", "p", "p_row", "p_col")),
      intersect(names(agg), stats)
    )
    agg <- agg[, .SD, .SDcols = vars]
    setattr(agg, "class", c(class(agg), "pivot_table"))
    setattr(agg, "rows", rows)
    setattr(agg, "cols", cols)
    return(agg[])
  }
  agg[, p_row := round(n / sum(n, na.rm = TRUE) * 100, 2), by = c(rows, "grouping")]
  agg[, p_col := round(n / sum(n, na.rm = TRUE) * 100, 2), by = c(cols, "grouping")]
  agg[, (stats) := lapply(.SD, as.numeric), .SDcols = stats]
  agg[, (stats) := lapply(.SD, function(x) {
    fifelse(is.nan(x), 0, x)
  }), .SDcols = stats]
  # browser()
  agg <- melt(
    data = agg,
    id.vars = rows_cols,
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
    fill = 0,
    drop = FALSE
  )
  for (row in rev(names(rows_values))) {
    odr <- chmatch(as.character(result[[row]]), table = c(rows_values[[row]], total_label))
    odr <- frankv(odr, ties.method = "first")
    result <- result[order(odr)]
  }
  colorder <- get_cols_order(cols_values, total = total, total_label = total_label)
  setcolorder(result, c(rows, "stats", colorder))
  setattr(result, "class", c(class(result), "pivot_table"))
  setattr(result, "rows", rows)
  setattr(result, "cols", cols)
  setattr(result, "cols_values", cols_values)
  result[]
}


