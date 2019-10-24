#' Wraps ggplot as Latex Objects
#'
#' gg_tex allows ggplots to be included in automated Latex appendixes constructing through \code{\link{appendize}}.
#'
#' @param ggOb A ggplot.
#' @param label Intended Latex label for plot figure.
#' @param caption Intended Latex title for plot figure.
#'
#' @examples
#' df <- data.frame("first" = c(4,5,6), "second" = c(7,5,3))
#' g <- ggplot(data=df) + geom_point(aes(x=first, y=second))
#' rchitex::appendize(gg_tex(g, label='ggFig', caption='My ggplot'), path=getwd())
#'
#' @export
gg_tex <- function(ggOb, label, caption = 'Figure') {
  g <- structure(list(options = list(label=label, caption=caption),
                 code = ggOb),
            class = c('rchitex', 'ggtex'))
  invisible(g)

}
