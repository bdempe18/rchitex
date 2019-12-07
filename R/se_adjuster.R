#' Simply adjustment of model standard errors
#'
#' Allows model standard errors to be adjusted
#'  prior to table output
#'  @param mod A (linear) model
#'  @return a modified version of the inputted model
#' @export
rse <- function(mod) {
  UseMethod('rse')
}

#' @export
rse.default <- function(mod) {
  se <- lmtest::coeftest(mod, vcov = sandwich::vcovHC(mod, "HC1"))
  #se <- list(est = se[,1],
  #           std = se[,2],
  #           t_val = se[,3],
  #           p_val = se[,4])
  mod$se <- se
  mod$model_type <- class(mod)
  class(mod) <- 'rse'

  mod
}

#' builder function
#' @param ... Shouldn't be used so arguments not specified
rse.function <- function(...) {
  stop('How did we get here?!')
}

#' @export
print.rse <- function(m) {
  print(structure(m, class = m$model_type))
}

#' @export
summary.rse <- function(m) {
  x <- summary(structure(m, class = m$model_type))
  x$coefficients <- m$se
  x
}
