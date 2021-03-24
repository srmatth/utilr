#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Inverted versions of in, is.null and is.na
#'
#' @export
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)
not_null <- Negate(is.null)
not_na <- Negate(is.na)

#' @title Snake case syntax for is.na and is.null
#'
#' @export
is_na <- is.na
is_null <- is.null

#' Removes the null from a vector
#'
#' @param x a vector to drop the nulls from
#'
#' @export
#'
#' @examples
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}
