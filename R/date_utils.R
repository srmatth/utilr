#' This Year
#'
#' Helper function to get the current year
#'
#' @return the current year
this_year <- function() {
  year <- lubridate::year(Sys.Date())
  return(year)
}

#' Next Year
#'
#' Helper function to get the next year
#'
#' @return the next year
next_year <- function() {
  year <- lubridate::year(Sys.Date()) + 1
  return(year)
}

#' This Month
#'
#' A helper function to get the current month
#'
#' @return the current month (as a number)
this_month <- function() {
  month <- lubridate::month(Sys.Date())
  return(month)
}
