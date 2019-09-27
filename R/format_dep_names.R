format_dep_names <- function(mods, dep_names=NA) {
  stopifnot(class(dep_names) == 'list' || is.na(dep_names))

  if (class(dep_names) == "list") {
    stopifnot(length(mods) <= length(dep_names))
    return(dep_names)
  }
  dn <- unique(unlist(lapply(mods, function(m) names(coef(m)))))
  dn <- c(setdiff(dn, '(Intercept)'), '(Intercept)')
  dn <- as.list(dn)
  names(dn) <- dn
  dn
}
