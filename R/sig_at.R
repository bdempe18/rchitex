sig_at <- function(v, sigs) {
  sa <- unlist(lapply(v, function(x) {
    tryCatch({
      names(sig[x <= sigs])[[1]]
    }, error = function(e) ''
    )}))
  sa[is.na(sa)] <- ''
  sa
}
