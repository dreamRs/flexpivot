

# Utility functions -------------------------------------------------------

hasName <- function(x, name) {
  match(name, names(x), nomatch = 0L) > 0L
}


