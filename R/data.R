#' @title Nobel laureates
#'
#' @description Dataset containing the Nobel Prize winners and information about them.
#'
#' @format A data frame with 950 rows and 10 variables:
#' \describe{
#'   \item{id}{Unique id (by laureate)}
#'   \item{name}{Name of laureate}
#'   \item{gender}{Laureate's gender if person}
#'   \item{birth_date}{Birth date of Laureate if person}
#'   \item{birth_continent}{Laureate's continent of birth if person}
#'   \item{year}{The year the Nobel Prize was awarded}
#'   \item{category}{Nobel Prize category.}
#'   \item{award_portion}{Awarded portion of the Nobel}
#'   \item{laureate_type}{Organization or individual laureate}
#' }
#' @source Nobel Prize API \url{https://www.nobelprize.org/}
"nobel_laureates"
