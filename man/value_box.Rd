% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/value_box.R
\name{value_box}
\alias{value_box}
\title{Value Box}
\usage{
value_box(
  value,
  subtitle,
  icon = NULL,
  color = "#ffffff",
  background = "#2180BC",
  width = 4,
  href = NULL
)
}
\arguments{
\item{value}{the value to be displayed in the value box, typically a numeric}

\item{subtitle}{the text that will be displayed underneath the value,
typically a string}

\item{icon}{the icon that will appear on the right side of the value box,
from \code{icon()}}

\item{color}{the color of the text in hex format, generally '#FFFFFF'}

\item{background}{the color of the background in hex format}

\item{width}{the width of the value box (bootstrap grid system), defaults to 4}

\item{href}{an optional argument for adding a link to the value box}
}
\value{
html tags that can be placed inside
\code{shinydashboard::renderValueBox()}
}
\description{
This is a function that creates a value box but allows you to customize the
colors inside the value box.
}
\details{
This function build on the valueBox functions from the \code{{shinydashboard}} package.
Having color arguments allows the valueboxes in the Shiny Apps to be more in line
with the cla color scheme.
The function was adapted from
\href{https://community.rstudio.com/t/shinydashboard-custom-box-colors-to-match-brand/14147/5}{RStudioCommunity}
and was posted by the user \emph{efhopkins} on 2018-09-18.
}
\examples{
if (interactive()) {

  library(shiny)
  library(clashiny)
  library(shinydashboard)

  ui = dashboardPage(
    header = dashboardHeader(title = "Test Value Boxes"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      valueBoxOutput(outputId = "box1"),
      valueBoxOutput(outputId = "box2"),
      valueBoxOutput(outputId = "box3")
    )
  )

  server <- function(input, output, session) {
    output$box1 <- renderValueBox({
      value_box(
        value = scales::percent(.0123, accuracy = .01),
        subtitle = "This is a percent value",
        icon = icon("percent"),
        background = cla::cla_colors()$theme["orange"],
        color = "#FFFFFF"
      )
    })

    output$box2 <- renderValueBox({
      value_box(
        value = scales::dollar(134237898, accuracy = 100),
        subtitle = "This is a dollar value, with different text color",
        icon = icon("dollar-sign"),
        color = cla::cla_colors()$theme["blue"],
        background = cla::cla_colors()$theme["yellow"]
      )
    })

    output$box3 <- renderValueBox({
      value_box(
        value = scales::number(6100, accuracy = 1, big.mark = ","),
        subtitle = "This is a numeric value (number of employees?)",
        icon = icon("users"),
        background = cla::cla_colors()$theme["green"],
        color = "#FFFFFF"
      )
    })
  }

  shinyApp(ui, server)

}


}
