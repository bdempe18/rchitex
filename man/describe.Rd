% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe.R
\name{describe}
\alias{describe}
\title{Formats Summary Tables to Text, LaTeX, and HTML}
\usage{
describe(
  data,
  note = "",
  silent = F,
  path = NA,
  max_precision = 6,
  statistics = list(N = length, Mean = mean, `St. Dev` = stats::sd, Min = min, Max =
    max),
  md = NA,
  landscape = FALSE,
  label = "sumStats",
  title = "Summary statistics",
  header = TRUE,
  as_table = FALSE,
  flip = FALSE,
  ...
)
}
\arguments{
\item{data}{Dataframe of variables.}

\item{note}{Optional note displayed at bottom of table.}

\item{silent}{No text is outputted if true.}

\item{path}{File path to write tex file.}

\item{max_precision}{Maximum number of digits in each table cell.}

\item{statistics}{A list of functions to be applied to each column of the data frame or tibble.
Each function must be vectorized.}

\item{md}{Allows for outputting in either latex ("latex") or html ("html") for markdown formatting.
The Markdown chunk must be set to \code{results = "asis"}.}

\item{landscape}{If true, the Latex table will be landscaped.}

\item{label}{Latex label.}

\item{title}{Table title.}

\item{header}{Includes RCHITEX header as a Latex comment if true.}

\item{as_table}{True values wrap the underlying Latex Tabular object in a table.}

\item{flip}{Invert tables when set to TRUE.}
}
\value{
None
}
\description{
Given a dataframe and a list of summary statistics, \code{describe} outputs
a text table summarizing the data while simultaneously outputting the
equivalent LateX code.
}
\examples{
df <- data.frame("first" = c(4,5,6), "second" = c(7,5,3))
describe(df)

stats <- list(N=length, Average = mean, foo = function(x) x[1]+2)
describe(df, statistics=stats)

}
