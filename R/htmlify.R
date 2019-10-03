header <- function(text) {
  paste0('<th>', text, '</th>\n', collapse='')
}

row_el <- function(text, align=NA) {
  style <- ''
  if (!is.na(align)) {
    style <- paste('style="text-align', align, '"')
  }
  paste0('<td ',style, '>', text, '</td>\n', collapse='')
}

row_start <- function(text) {
  paste0('<tr>', text, '</tr> \n')
}

blank_row <- function() {
  row_start(c(row_el('', 'left'), row_el('')))
}

to_html <- function(data, title='Summary statistics') {
  citation <- '<-- Table generating using RCHITEX by Ben Dempe (2019) -->'
  hr <- row_start(paste('<td colspan="', ncol(data) + 1,
                        '" style="border-bottom: 1px solid black"></td>', sep=''))
  preamble <- paste('<table style = "text-align: center;">',
                    '<caption>', title, '</caption>', hr,
                    row_start('<td style="text-align:left"></td>'),
                    row_start(row_el(c('',colnames(data)))), hr)
  body <- unlist(lapply(rownames(data), function(r)
    row_start(row_el(c(r, data[r, ])))))
  body <- paste(body, sep='', collapse='')
  post <- '</table>'
  paste(preamble, body, post, sep='', collapse='')

}

to_html_m <- function(reg_data, max_precision, fit_char, sig = list(), path = NA, note='', title='',
                     header = FALSE) {

  # Preamble
  citation <- '<-- Table generating using RCHITEX by Ben Dempe (2019) -->'
  n_mods <- length(reg_data[[1]]$coef)
  col_names <- paste0('(', 1:n_mods, ')')
  col_width <- paste('<col width=175>\n', strrep('<col width=120em>\n', n_mods))
  hr <- row_start(paste('<td colspan="', n_mods + 1,
                        '" style="border-bottom: 1px solid black"></td>', sep=''))


  preamble <- paste('<table style = "text-align: center;">',
                 '<caption>', title, '</caption>', hr,
  row_start('<td style="text-align:left"></td>'),
  row_start(row_el(c('',col_names))), hr)

  # Body
  body <- unlist(lapply(names(reg_data), function(r) {
    ests <- ifelse(is.na(reg_data[[r]]$coef), '', format(round(
      reg_data[[r]]$coef, max_precision), nsmall=1, big.mark = ','))
    ests <- paste0(ests, '<sup>', sig_at(reg_data[[r]]$pval, sig), '</sup>')
    errs <- ifelse(is.na(reg_data[[r]]$error), '',
                   paste('(',format(round(reg_data[[r]]$error, 3),
                                    nsmall=3, big.mark = ', ', scientific = FALSE),') ', sep=''))
    rel_c <- row_el(c(r, ests), 'left')
    rel_e <- row_el(c(' ', errs), 'left')
    row_start(c(rel_c, rel_e, blank_row()))
  }))

  body <- paste(body, sep='')

  # Post
  fit <- unlist(lapply(names(fit_char), function(fc) {
    row_start(row_el(c(fc, fit_char[[fc]]), 'left'))
  }))

  p_post <- unlist(lapply(names(sig), function(s) {
    paste0('<sup>',s, '>/sup><', sig[[s]], ' ', collapse='')
  }))
  ## TODO Finish
  p_post <- row_start(c(row_el('Note:', 'left'), row_el(p_post, 'right'))) # Maybe...? Need to check
  #<tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>

  post <- '</table>'
  #writeLines(c(preamble, body, hr, fit, hr, post))
  #paste0(preamble,post, body, sep="\n", collapse='')
  c(preamble, body, hr, fit, hr, post)
}
