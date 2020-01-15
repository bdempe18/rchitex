#' Simple adjustment of model standard errors.
#'
#' Allows model standard errors to be adjusted
#'  prior to table output
#' @param m A linear model.
#' @return A custom S3 representation of the linear model with robust standard errors.
#' @export
rse <- function(m) {
  UseMethod('rse')
}

#' Simple adjustment of model standard errors.
#'
#' Allows model standard errors to be adjusted
#'  prior to table output
#' @param m A linear model.
#' @return A custom S3 representation of the linear model with robust standard errors.
#' @export
rse.default <- function(m) {
  se <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, "HC1"))
  m$se <- se
  m$model_type <- class(m)
  class(m) <- 'rse'

  m
}

#' Prints RSE model
#' @param m RSE model.
#' @export
print.rse <- function(m) {
  print(structure(m, class = m$model_type))
}

#' Extract number of observations
#' @param m RSE model.
#' @export
nobs.rse <- function(m) {
  m <- structure(m, class = m$model_type)
  UseMethod('nobs')
}

#' Summarizes RSE models
#' @param m RSE model.
#' @export
summary.rse <- function(m) {
  x <- summary(structure(m, class = m$model_type))
  x$coefficients <- m$se
  x
}

#' Prints the adjusted standard erorr model.
#' @param mod A regression model.
#' @param transformation Either a function to be transform a vector of
#'   standard errors or a vector of new standard errors to replace the model
#'   standard errors
#' @return A custom S3 representation of the regression model
#'   with adjusted standard errors.
#' @export
adj_se <- function(mod, transformation) {
  UseMethod("adj_se")
}

#' Prints the adjusted standard erorr model.
#' @param mod A regression model.
#' @param transformation Either a function to be transform a vector of
#'   standard errors or a vector of new standard errors to replace the model
#'   standard errors
#' @return A custom S3 representation of the regression model
#'   with adjusted standard errors.
#' @export
adj_se.default <- function(mod, transformation) {
  se <- summary(mod)$coefficients
  if (class(transformation) == "function"){
    se[,2] <- transformation(se[,2])
  }
  else if (is.vector(transformation) & length(transformation) == length(se[,2])) {
    se[,2] <- transformation
  }
  # t-stat = beta_hat / se
  se[,3] <- se[,1] / se[,2]
  se[,4] <- stats::pt(se[,3], df = mod$df.residual) #TODO CHECK THIS LINE
  mod$se <- se
  mod$model_type <- class(mod)
  class(mod) <- 'rse'

  mod
}

#' Prints adjusted SE model#'
#' @param m Adjusted SE model.
#' @export
print.adj_se <- function(m) {
  print(structure(m, class = m$model_type))
}

#' Extracts number of observations of adjusted SE model
#' @param m Adjusted SE model.
#' @export
nobs.adj_se <- function(m) {
  m <- structure(m, class = m$model_type)
  UseMethod('nobs')
}

#' Summarizes adjusted SE model
#' @param m Adjusted SE model.
#' @export
summary.adj_se <- function(m) {
  x <- summary(structure(m, class = m$model_type))
  x$coefficients <- m$se
  x
}
