

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

get_levels <- function(data, vars) {
  lapply(data[, .SD, .SDcols = vars], function(x) {
    if (inherits(x, "factor")) {
      levels(x)
    } else if (inherits(x, c("Date", "POSIXt", "numeric", "integer"))) {
      as.character(sort(unique(x)))
    } else {
      as.character(unique(x))
    }
  })
}

#' @importFrom data.table CJ transpose
get_cols_order <- function(cols_values, total = TRUE, total_label = "Total") {
  if (isTRUE(total))
    cols_values <- lapply(cols_values, append, value = total_label)
  cols <- do.call(CJ, c(cols_values, list(unique = TRUE, sorted = FALSE)))
  cols <- as.list(cols)
  vapply(X = transpose(cols), FUN = paste, collapse = "_|_", FUN.VALUE = character(1))
}

#' @importFrom data.table CJ :=
complete <- function(data, vars, fill = list()) {
  data <- data[do.call(CJ, lapply(
    X = mget(vars),
    FUN = function(var) {
      if (inherits(var, "factor")) {
        if (anyNA(var)) {
          factor(c(levels(var), NA_character_), levels = levels(var), ordered = is.ordered(var))
        } else {
          factor(levels(var), levels = levels(var), ordered = is.ordered(var))
        }
      } else {
        unique(var)
      }
    }
  )), on = vars]
  if (length(fill) > 0 && all(nzchar(names(fill)))) {
    for (fillvar in names(fill)) {
      data[is.na(get(fillvar)), (fillvar) := fill[[fillvar]]]
    }
  }
  data[]
}
