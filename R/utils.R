

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
