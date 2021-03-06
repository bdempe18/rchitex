% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/append.R
\name{appendize}
\alias{appendize}
\title{Generates Latex Appendix}
\usage{
appendize(..., dir)
}
\arguments{
\item{...}{Models generated by \code{build}, \code{describe}, ggplot objects,
or ggplot objects wrapped in \code{\link{gg_tex}}.}

\item{dir}{Path to Latex file node where the appendix folder will be appended.}
}
\description{
Creates a LaTeX appendix of regression summaries and descriptive statistics.
  A directory containing all the relevant files
}
\examples{

data('freeny')
mod <- lm(y ~ lag.quarterly.revenue + price.index + income.level +
            market.potential, data = freeny)
mod2 <- lm(y ~ lag.quarterly.revenue + price.index + income.level,
          data = freeny)

tex1 <- build(mod, label='f1', silent=TRUE)
tex2 <- build(mod2, label='f2', silent=TRUE)
d <- paste(getwd(), 'rchitEx', sep='/')
append(list(tex1, tex2), d)
##...Latex files outputted to dir... ##

}
