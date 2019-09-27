gen_stats <- function(stats='oraf', mods, nobs, pre_stats=NA) {
  s <- unlist(strsplit(tolower(stats), ''))
  generate <- list('o' = 'Observations',
                   'r' = 'R2',
                   'a' = 'Adjusted R2',
                   'f' = 'F Statistic')

  fit_char <- list('Observations' = unlist(lapply(mods, nobs)),
                   'R2'           = unlist(lapply(mods, function(m) round(summary(m)$r.squared, 4))),
                   'Adjusted R2'  = unlist(lapply(mods, function(m) round(summary(m)$adj.r.squared,4))),
                   'F Statistic'  = unlist(lapply(mods, function(m) f_to_string(summary(mod)$fstatistic)))
  )

  out <- fit_char[unlist(generate[s], use.names=F)]
  if(!is.na(pre_stats)) {
    stopifnot(class(pre_stats) == 'list')
    out <- c(pre_stats, out)
  }
  out
}
