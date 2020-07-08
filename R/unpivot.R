
#' @title Unpivot
#'
#' @description From a \code{\link{pivot_table}} object put column(s) back as rows.
#'
#' @param x A \code{\link{pivot_table}} object.
#'
#' @return a \code{data.table}.
#' @export
#'
#' @importFrom data.table melt tstrsplit .SD set setcolorder
#'
#' @example examples/unpivot.R
unpivot <- function(x) {
  if (!inherits(x, "pivot_table")) {
    stop("unpivot must be used with output of pivot_table()", call. = FALSE)
  }
  cols <- attr(x, "cols", exact = TRUE)
  rows <- attr(x, "rows", exact = TRUE)
  stat <- attr(x, "stat", exact = TRUE)
  if (is.null(cols))
    return(x)
  x <- melt(
    data = x,
    id.vars = c(rows, stat),
    variable.factor = FALSE,
    variable.name = "var_pivot_col"
  )
  x[, (cols) := tstrsplit(.SD[[1]], split = "_|_", fixed = TRUE), .SDcols = "var_pivot_col"]
  set(x = x, j = "var_pivot_col", value = NULL)
  setcolorder(x, c(rows, cols, stat, "value"))
  return(x[])
}

