sig_at <- function(v, sigs) {
  sa <- unlist(lapply(v, function(x) {
    tryCatch({
      names(sigs[x <= sigs])[[1]]
    }, error = function(e) ''
    )}))
  sa[is.na(sa)] <- ''
  sa
}

format_indep_names <- function(mods, indep_names=NA) {
  idn <- unique(unlist(lapply(mods, function(m) names(stats::coef(m)))))
  idn <- c(setdiff(idn, '(Intercept)'), '(Intercept)')

  if (class(indep_names) == 'list')  return (indep_names)

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

roundr_fac <- function(max_precision, min_digs=0) {
  roundr <- function(num, nsmall=min_digs) {
    if (!is.numeric(num)) return(num)
    format(round(as.numeric(unlist(num), use.names=F),
                 max_precision), big.mark=',', nsmall=nsmall)
  }
  roundr
}
