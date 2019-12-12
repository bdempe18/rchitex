#' Simple adjustment of model standard errors
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
nobs.rse <- function(m) {
  nobs(structure(m, class = m$model_type))
}

#' @export
summary.rse <- function(m) {
  x <- summary(structure(m, class = m$model_type))
  x$coefficients <- m$se
  x
}

#' Simple adjustment of model standard errors
#' @param mod A (linear) model.
#' @param f Either a function to be transform a vector of standard errors
#'   or a vector of new standard errors to replace the model standard errors
#' @return a modified version of the inputted model
#' @export
adj_se <- function(mod, transformation) {
  UseMethod("adj_se")
}

#' @export
adj_se.default <- function(mod, transformation) {
  se <- summary(mod)$coefficients
  if (class(tranformation) == "function") se[,2] <- transformation(se[,2])
  else if (is.vector(transformation) & length(transformation) == length(se[,2])) {
    se[,2] <- transformation
  }
  # t-stat = beta_hat / se
  se[,3] <- se[,1] / se[,2]
  se[,4] <- pt(se[,3], df = mod$df.residual)
  mod$se <- se
  mod$model_type <- class(mod)
  class(mod) <- 'rse'

  mod
}

#' @export
print.adj_se <- function(m) {
  print(structure(m, class = m$model_type))
}

#' @export
nobs.adj_se <- function(m) {
  nobs(structure(m, class = m$model_type))
}

#' @export
summary.adj_se <- function(m) {
  x <- summary(structure(m, class = m$model_type))
  x$coefficients <- m$se
  x
}
