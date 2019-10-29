#' rchitex: streaming the transition from R exploration to article writing
#'
#' The rchitex package is designed to facilitate the creation
#' of LaTeX articles from R models. rchitex provides
#' three main functions, \code{\link{build}} for regression
#' models, \code{\link{describe}} for summary statistics,
#' and \code{\link{appendize}} and \code{\link{gg_tex}} for creating an entire LaTeX
#' appendix.
#'
#' rchitex's tex outputting allows for elegant console data investigation.
#' Data can be summarized and outputted with a wide range of customizations.
#'
#' Along with text output, rchitex supports latex and html output, making the
#' package optimal for use in Markdown files.
#'
#' @section Supported models:
#'
#' \describe{
#'   \item{\emph{stats}}{lm, glm}
#'   \item{\emph{plm}}{plm}
#'   \item{\emph{AER}}{ivreg, tobit}
#' }
#'
#' @docType package
#' @name rchitex
NULL
