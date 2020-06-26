

# Utility functions -------------------------------------------------------

hasName <- function(x, name) {
  match(name, names(x), nomatch = 0L) > 0L
}

is_valid <- function(data, cols) {
  if (is.null(cols))
    return(FALSE)
  if (all(cols %in% names(data))) {
    return(TRUE)
  } else {
    stop("Not all cols specified are in data.", call. = FALSE)
  }
}

#' @importFrom data.table CJ :=
complete <- function(data, vars, fill = list()) {
  data <- data[do.call(CJ, c(list(unique = TRUE), mget(vars))), on = vars]
  if (length(fill) > 0 && all(nzchar(names(fill)))) {
    for (fillvar in names(fill)) {
      data[is.na(get(fillvar)), (fillvar) := fill[[fillvar]]]
    }
  }
  data[]
}
