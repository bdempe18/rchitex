sig_at <- function(v, sigs) {
  sa <- unlist(lapply(v, function(x) {
    tryCatch({
      names(sigs[x <= sigs])[[1]]
    }, error = function(e) ''
    )}))
  sa[is.na(sa)] <- ''
  sa
}

gen_stats <- function(stats='oraf', mods, pre_stats=NA) {
  s <- unlist(strsplit(tolower(stats), ''))
  generate <- list('o' = 'Observations',
                   'r' = 'R2',
                   'a' = 'Adjusted R2',
                   'f' = 'F Statistic')

  fit_char <- list('Observations' = unlist(lapply(mods, stats::nobs)),
                   'R2'           = unlist(lapply(mods, function(m) round(summary(m)$r.squared, 4))),
                   'Adjusted R2'  = unlist(lapply(mods, function(m) round(summary(m)$adj.r.squared,4))),
                   'F Statistic'  = unlist(lapply(mods, function(m) f_to_string(summary(m)$fstatistic)))
  )

  out <- fit_char[unlist(generate[s], use.names=F)]
  if(!is.na(pre_stats)) {
    stopifnot(class(pre_stats) == 'list')
    out <- c(pre_stats, out)
  }
  out
}

format_indep_names <- function(mods, indep_names=NA) {
  stopifnot(class(indep_names) == 'list' || is.na(indep_names))

  if (class(indep_names) == "list") {
    stopifnot(length(mods) <= length(indep_names))
    return(indep_names)
  }
  idn <- unique(unlist(lapply(mods, function(m) names(stats::coef(m)))))
  idn <- c(setdiff(idn, '(Intercept)'), '(Intercept)')
  idn <- as.list(idn)
  names(idn) <- idn
  idn
}

f_to_string <- function(f_stat) {
  format(f_stat[[1]], format='d', big.mark=',')
}

center_text <- function(text, width) {
  n_blank <- (width - nchar(text))%/%2
  paste0(strrep(' ', n_blank), text, strrep(' ', width-n_blank-nchar(text)))
}
