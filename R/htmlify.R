header <- function(text) {
  paste0('<th>', text, '</th>\n', collapse='')
}

row_el <- function(text, align=NA, border=NA, colspan=NA) {
  style <- cols <- ''
  if (!is.na(align)) style <- paste('style="text-align:', align, '"')
  if (!is.na(border)) style <- 'style="border-bottom: 1px solid black"'
  if (!is.na(colspan)) cols <- paste('colspan="', colspan, '"', sep='')
  paste0('<td ',cols, style, '>', text, '</td>', collapse='', sep='')
}

row_start <- function(text) {
  paste0('<tr>', text, '</tr>')
}

blank_row <- function() {
  row_start(c(row_el('', 'left'), row_el('')))
}

to_html <- function(data, title='Summary statistics', header=TRUE) {
  citation <- ifelse(header,
    '<-- Table generating using RCHITEX by Ben Dempe (2019) -->',
    '')
  hr <- row_start(paste('<td colspan="', ncol(data) + 1,
                        '" style="border-bottom: 1px solid black"></td>', sep=''))
  preamble <- paste('<table style = "text-align: center;" cellingpadding=100px>',
                    '<caption>', title, '</caption>', hr,
                    row_start('<td style="text-align:left"></td>'),
                    row_start(row_el(c('',colnames(data)))), hr)
  body <- unlist(lapply(rownames(data), function(r)
    row_start(row_el(c(r, data[r, ])))))
  body <- paste(body, sep='\n', collapse='')
  post <- '</table>'
  paste(preamble, body, post, sep='', collapse='')

}

to_html_m <- function(reg_data, max_precision, fit_char, reporter, sig = list(),
                      path = NA, note='', title='', idn = NULL, sig_levels,
                      col_names, grouped_label=NULL)  {

  # Preamble
  citation <- '<-- Table generating using RCHITEX by Ben Dempe (2019) -->'
  n_mods <- length(reg_data[[1]])
  col_width <- paste('<col width=175>\n', strrep('<col width=120em>\n', n_mods))
  hr <- row_start(paste('<td colspan="', n_mods + 1,
                        '" style="border-bottom: 1px solid black"></td>', sep=''))

  # grouped labels
  if (!is.null(grouped_label)) {
    h <- group_labels(grouped_label, n_mods, TRUE)
    h <- row_start(paste0(row_el(''), paste0(h, collapse=''), collapse=''))
  } else h <- ''

  preamble <- paste('<table style = "text-align: center;">',
                 '<caption>', title, '</caption>', '\n', hr, '\n', h,
  row_start('<td style="text-align:left"></td>'),
  row_start(row_el(c('',col_names))),'\n',hr,'\n')


  # Body
  body <- unlist(lapply(names(idn), function(r) {
    ests <- ifelse(is.na(reg_data[[r]]), '', paste0(reg_data[[r]],
                                                    '<sup>', sig[[r]], '</sup>'))
    errs <- ifelse(is.na(reg_data[[r]]), '', paste('(', reporter[[r]], ')',
                                                   sep=''))
    lab <- row_el(idn[[r]], 'left')
    rel_c <- row_el(ests)
    rel_c <- paste0(lab, rel_c, collapse='')
    rel_e <- row_el(c(' ', errs))
    paste0(row_start(c(rel_c, rel_e)), blank_row())
  }))

  body <- paste0(body, sep='\n', collapse='')

  # Post
  fit <- unlist(lapply(names(fit_char), function(fc) {
    lab <- row_el(fc, 'left')
    rw <- row_el(fit_char[[fc]])
    row_start(paste0(lab, rw, collapse=''))
  }))
  fit <- paste0(fit, sep='', collapse='\n')

  p_post <- paste0(lapply(names(sig_levels), function(s) {
    paste0('<sup>', s, '</sup>p&lt;', sig_levels[[s]], ' ', collapse='')
  }), collapse='')


  ## TODO Finish
  p_post <- paste0(row_start((row_el('', colspan=3))),
                   row_start(paste0(row_el(paste0('<em>Note: </em>', note, collapse=''), 'left'),
                                    row_el(p_post, 'right', colspan=n_mods), collapse='')), collapse='')
  post <- '</table>'
  c(preamble, body, hr, fit, hr, p_post, post)
}
