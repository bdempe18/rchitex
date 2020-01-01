tag_factory <- function(tag_type) {
  force(tag_type)
  function(text, padding = '0px 0px 0px 10px',
                 border = '1px', text_align = 'center', colspan=0) {
    padding <- paste('padding: ', padding, sep='')
    border <- paste('border: ', border, sep='')
    text_align <- paste('text-align: ', text_align, sep='')
    style <- paste0(c(padding, border, text_align), collapse = '', sep = '; ')
    style <- paste0('style="', style, '"', collapse = '', sep = '')
    if (colspan  > 0 ) style <- paste(style, 'colspan="', colspan, '"', sep = '')

    paste0('<', tag_type, ' ',  style, '>', text, '</', tag_type, '>\n',
           collapse = '', sep ='')
  }
}

td <- tag_factory('td')
th <- tag_factory('th')
tr <- function(text, border = NULL, border_top = NA) {
  bt <- ifelse(is.na(border_top), '', paste0('; border-top:', border_top))
  if (!is.null(border)) {
    paste0('<tr style="border-bottom: ', border, bt,
           '"> ', text, ' </tr>\n\n', collapse = '', sep = '')
  }
  else paste0('<tr> ', text, ' </tr>\n\n', collapse = '', sep = '')
}

## ----------------------------------------------------------------------------

to_html <- function(data, title='Summary statistics', header=TRUE) {
  citation <- ifelse(header,
    '<-- Table generating using RCHITEX by Ben Dempe (2019) -->\n',
    '')
  preamble <- paste('<table style = "line-height: 1.6; border-bottom: 0">',
                    '<caption>', title, '</caption>\n', sep = '')
  header <- tr(th(text = c('', colnames(data)), text_align = 'center'))
  body <- unlist(lapply(rownames(data), function(r) {
    tbl_row <- paste(td(r, text_align = 'left'),
                 td(data[r, ], text_align = 'right'), sep = ' ')
    tr(tbl_row)
  }))
  body <- paste0(body, sep = '', collapse = '')
  post <- '</table>'
  paste(preamble,  header, body, post, sep='\n', collapse='')
}

## ----------------------------------------------------------------------------

to_html_m <- function(reg_data, max_precision, fit_char, reporter, sig = list(),
                      path = NA, note='', title='', idn = NULL, sig_levels,
                      col_names, grouped_label=NULL)  {

  # Preamble
  citation <- '<-- Table generating using RCHITEX by Ben Dempe (2019) -->'
  n_mods <- length(reg_data[[1]])
  col_width <- paste('<col width=175>\n', strrep('<col width=120em>\n', n_mods))

  # grouped labels
  if (!is.null(grouped_label)) {
    h <- group_labels(grouped_label, n_mods, TRUE)
    h <- tr(paste0(td(''), paste0(h, collapse=''), collapse=''))
  } else h <- ''

  preamble <- paste('<table style = "text-align: center; border-bottom: 0; border-top: 1px solid #ccc">',
                 '<caption><em>', title, '</em></caption>', h)

  header <- tr(td(text = c('', col_names), text_align = 'center',
                  padding = '5px 15px 5px 15px'), border = '1px solid #ccc')

  # Body
  body <- unlist(lapply(names(idn), function(r) {
    ests <- ifelse(is.na(reg_data[[r]]), '', paste0(reg_data[[r]],
                                                    '<sup>', sig[[r]], '</sup>'))
    errs <- ifelse(is.na(reg_data[[r]]), '', paste('(', reporter[[r]], ')',
                                                   sep=''))
    lab <- td(text = idn[[r]], text_align = 'left', padding = '0px 25px 0px 0px')
    rel_c <- td(text = ests, padding = '0px 15px 0px 0px ')
    rel_c <- paste0(lab, rel_c, collapse='')
    rel_e <- td(text = c(' ', errs), padding = '0px 15px 5px 0px')
    border <- switch(r == tail(names(idn), 1), '1px solid #ccc', NULL)
    paste0(tr(rel_c), tr(rel_e, border=border), sep = '\n', collapse='')
  }))

  body <- paste0(body, sep='\n', collapse='')

  # Post
  fit <- unlist(lapply(names(fit_char), function(fc) {
    lab <- td(text = fc, text_align = 'left', padding = '0px 25px 0px 0px')
    rw <- td(text = fit_char[[fc]], padding = '0px 15px 0px 0px')
    tr(paste0(lab, rw, collapse=''), border = '0px solid #ccc')
  }))
  fit <- paste0(fit, sep='', collapse='\n')

  p_post <- paste0(lapply(names(sig_levels), function(s) {
    paste0('<sup>', s, ' </sup>p&lt;', sig_levels[[s]], ' ', collapse='')
  }), collapse='')


  ## TODO Finish
  p_post <- paste0(tr((td('', colspan=3)), border = '1px solid #ccc'),
                   tr(paste0(td(paste0('Note:', note, collapse=''),
                                text_align = 'left',
                                padding = '0px 25px 0px 0px'),
                                    td(p_post, text_align = 'right', colspan=n_mods),
                             collapse='')), collapse='')
  post <- '</table>'
  c(preamble, header, body, fit, p_post, post)
}
