% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_version.R
\name{prompt_user}
\alias{prompt_user}
\title{Simple Y/N Prompt}
\usage{
prompt_user(x)
}
\arguments{
\item{x}{A character vector.

For block styles, conditions, and questions, each element of the
vector becomes a line, and the result is processed by \code{\link[glue:glue]{glue::glue()}}.
For inline styles, each element of the vector becomes an entry in a
comma separated list.}
}
\description{
Simple Y/N Prompt
}
