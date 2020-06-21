
#' @title Export pivot table
#'
#' @description Export formated pivot table to PowerPoint, Word or Excel document.
#'
#' @param x A \code{\link{pivot_table}} or \code{\link{pivot_format}} object.
#' @param output Path where to generate file.
#' @param to Output document: \code{"pptx"} (PowerPoint), \code{"docx"} (Word) or \code{"xlsx"} (Excel).
#' @param ... Arguments passed to \code{\link{pivot_format}} if \code{x} isn't formated yet.
#'
#' @return Path to output (invisibly).
#' @export
#'
#' @name export-pivot
#'
#' @example examples/export.R
export <- function(x, output, to = c("pptx", "docx", "xlsx"), ...) {
  to <- match.arg(to)
  if (identical(to, "pptx")) {
    export_pptx(x, output, ...)
  } else if (identical(to, "docx")) {
    export_docx(x, output, ...)
  }
}


#' @rdname export-pivot
#' @export
#' @importFrom flextable fontsize padding autofit
#' @importFrom officer read_pptx add_slide ph_with ph_location_type
export_pptx <- function(x, output, ...) {
  output <- normalizePath(output, mustWork = FALSE)
  if (inherits(x, "pivot_table")) {
    x <- pivot_format(x, ...)
  }
  if (!inherits(x, "flexpivot")) {
    stop("export: 'x' must be a pivot_table or pivot_format object.", call. = FALSE)
  }
  x <- fontsize(x, size = 11, part = "all")
  x <- padding(x = x, padding = 3, part = "all")
  doc <- read_pptx()
  doc <- add_slide(doc)
  doc <- ph_with(
    x = doc,
    value = autofit(x),
    location = ph_location_type(type = "body")
  )
  print(doc, target = output)
}


#' @rdname export-pivot
#' @export
#' @importFrom flextable fontsize padding autofit body_add_flextable
#' @importFrom officer read_docx
export_docx <- function(x, output, ...) {
  output <- normalizePath(output, mustWork = FALSE)
  if (inherits(x, "pivot_table")) {
    x <- pivot_format(x, ...)
  }
  if (!inherits(x, "flexpivot")) {
    stop("export: 'x' must be a pivot_table or pivot_format object.", call. = FALSE)
  }
  x <- fontsize(x, size = 11, part = "all")
  x <- padding(x = x, padding = 3, part = "all")
  doc <- read_docx()
  doc <- body_add_flextable(x = doc, value = autofit(x))
  print(doc, target = output)
}





