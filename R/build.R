build <- function(..., dep_names = NA, indep_names = NA, note='', title = 'Model results',
                  max_precision = 6, stat='p', path = NA, rse = FALSE,
                  silent = FALSE, landscape = FALSE, report = 'p', stats='oraf', pre_stats=NA,
                  md = FALSE, header = TRUE) {
  mods <- list(...)
  fit_stats <- list(std = 2, t = 3, p = 4)
  # assigns the dependent variable in the correct order with intercept at end by default
  dn <- format_dep_names(mods, dep_names)

  # Iterates over a model fit and returns the coefficients and test statistics
  extract <- function(vn) {
    if (rse) {
      suppressMessages(require(foreign))
      suppressMessages(require(sandwich))
      suppressMessages(require(lmtest))
    }

    coefs <- c()
    errors <- c()
    pvals <- c()
    for (m in mods) {
      tryCatch({
        if(rse) {
          m_rse <- coeftest(m, vcov = vcovHC(mod, "HC1"))
          coefs <- c(coefs, m_rse[vn, 1])
          pvals <- c(pvals, m_rse[vn, 4])
          errors <- c(errors, m_rse[vn, fit_stats[[report]]])
        }
        else {
          coefs <- c(coefs, m$coefficients[[vn]])
          errors <- c(errors, summary(m)$coefficients[vn, fit_stats[[report]]])
          pvals <- c(pvals, summary(m)$coefficients[vn, 4])
        }
      }, error = function(e) NA)
    }

    list(coef = coefs, error = errors, pval = pvals)
  }

  # Robust standard errors are calculated if necessary
  ### TODO: implement
  line_data <- lapply(names(dn), extract)
  names(line_data) <- dn

  fit_char <- gen_stats(stats, mods, nobs, pre_stats)
  #print(line_data)
  sig <- list('***' = 0.01,
              '**'  = 0.05,
              '*'   = 0.1)
  if (md) to_tex_m(reg_data=line_data, max_precision=max_precision, fit_char=fit_char, sig=sig, path=stdout(), title=title)
  else {
    if (!silent) to_text_m(reg_data=line_data, max_precision=max_precision, fit_char=fit_char, sig=sig, title=title)
    if (!is.na(path)) to_tex_m(reg_data=line_data, max_precision=maxprecision, fit_char=fit_char, sig=sig, path=path, title=title)
  }
}
