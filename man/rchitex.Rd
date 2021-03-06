% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rchitex.R
\docType{package}
\name{rchitex}
\alias{rchitex}
\title{rchitex: streaming the transition from R exploration to article writing}
\description{
The rchitex package is designed to facilitate the creation
of LaTeX articles from R models. rchitex provides
three main functions, \code{\link{build}} for regression
models, \code{\link{describe}} for summary statistics,
and \code{\link{appendize}} and \code{\link{gg_tex}} for creating an entire LaTeX
appendix.
}
\details{
rchitex's tex outputting allows for elegant console data investigation.
Data can be summarized and outputted with a wide range of customizations.

Along with text output, rchitex supports latex and html output, making the
package optimal for use in Markdown files.
}
\section{Supported models}{


\describe{
  \item{\emph{stats}}{lm, glm}
  \item{\emph{plm}}{plm}
  \item{\emph{AER}}{ivreg, tobit}
}
}

\section{acknowledgements}{


 Thank to everyone who helped advise, test, criticize, and support the creation of
 this package, especially Erin Partin and Glen Kwende.
}

