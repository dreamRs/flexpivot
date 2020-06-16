% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_format.R
\name{pivot_formatter}
\alias{pivot_formatter}
\title{Formatters for \code{pivot_format}}
\usage{
pivot_formatter(
  n = round,
  p = function(x) {     paste0(round(x, 1), "\%") },
  p_col = function(x) {     paste0(round(x, 1), "\%") },
  p_row = function(x) {     paste0(round(x, 1), "\%") }
)
}
\arguments{
\item{n}{Function, applied to n.}

\item{p}{Function, applied to p.}

\item{p_col}{Function, applied to p_col.}

\item{p_row}{Function, applied to p_row.}
}
\value{
a \code{list} of \code{function}s that can be use in \code{\link{pivot_format}}.
}
\description{
Formatters for \code{pivot_format}
}
\examples{

data("diamonds", package = "ggplot2")

# One variable
pt <- pivot_table(diamonds, rows = "cut")

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x), big.mark = " ")
  }
))


# Two variable as rows
pt <- pivot_table(diamonds, rows = c("cut", "color"))

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x), big.mark = " ")
  }
))



# One row, one column
pt <- pivot_table(diamonds, rows = "cut", cols = "color")

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x), big.mark = " ")
  }
))

}