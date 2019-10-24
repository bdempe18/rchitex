model2text <- function(coefs, reporter, fits, sigs, idvn, max_precision,
                       sig_levels,
                       title = 'Regression Results',
                       note = '') {
  n_mods <- length(coefs[[1]])
  dn <- paste0('(', seq(1,n_mods),')')
  longest_idvn <- max(nchar(idvn))

  # calculates the maximum space needed in each column
  lengths <- lapply(names(idvn), function(ivar) nchar(coefs[[ivar]]) +
                      nchar(sigs[[ivar]]))
  fit_lengths <- lapply(fits, function(fc) nchar(fc))
  fit_lengths <- do.call(rbind, fit_lengths)
  fit_lengths <- apply(fit_lengths, 2, max, na.rm=TRUE)
  lengths <- do.call(rbind, lengths)
  lengths <- unlist(Map(max, fit_lengths, apply(lengths, 2, max, na.rm=TRUE)))
  lengths <- c(longest_idvn, lengths)
  line_width <- sum(lengths) + 5 * (n_mods - 1) + 5 # corrected for white space between cols

  tbl <- paste0(center(toupper(title), line_width - nchar(title)), '\n')
  # preamble
  tbl <- paste0(tbl, strrep('=', line_width), '\n', collapse='')
  tbl <- paste0(tbl, strrep(' ', longest_idvn + 5), collapse='')
  tbl <- paste0(tbl, paste0(center(dn, lengths[2:length(lengths)] -
                                     nchar(dn)), collapse=''),
                '\n', collapse='')
  tbl <- paste0(tbl, strrep('~', line_width), '\n', collapse='')

  tbl_text <- lapply(names(idvn), function(iv) {
    ests <- ifelse(is.na(coefs[[iv]]),
                          '', paste0(coefs[[iv]], sigs[[iv]]))
    spaces <- lengths[2:length(lengths)] - nchar(ests)
    ests <- center(ests, spaces)
    nam <- paste0(idvn[[iv]], strrep(' ', lengths[1] - nchar(idvn[[iv]]) + 5))
    ests <- paste0(nam, ests, '\n', collapse='')

    ep <-ifelse(is.na(reporter[[iv]]),
                      '', paste0('(',reporter[[iv]], ')'))
    spaces <- lengths[2:length(lengths)] - nchar(ep)
    ep <- center(ep, spaces)
    blank <- strrep(' ', lengths[1] + 5)
    ep <- paste0(blank, ep, collapse='')
    paste0(ests, ep, '\n', sep='\n', collapse='')
  })

  tbl_text <- paste0(tbl_text, collapse='')
  tbl <- paste0(tbl, substr(tbl_text, 1, nchar(tbl_text)-1), collapse='')
  tbl <- paste0(tbl, strrep('~', line_width), '\n', collapse='')

  # model fits
  fit_txt <- lapply(names(fits), function(fc) {
    paste0(fc, strrep(' ', lengths[1] - nchar(fc) + 5),
           center(fits[[fc]], lengths[2:length(lengths)] -nchar(fits[[fc]])),
           '\n', collapse='')
  })
  tbl <- paste0(tbl, paste0(fit_txt, collapse=''), collapse='')
  tbl <- paste0(tbl, strrep('=', line_width), '\n', collapse='')
  tbl <- paste0(tbl, 'Sig: ', collapse='')

  # row of p-val cut offs at
  txt <- paste0(names(sig_levels),'p<',unlist(sig_levels, use.names=F),' ', collapse='')
  txt <- paste0(strrep(' ', line_width-nchar(txt) - nchar('Sig:')),
                txt, collapse='')
  tbl <- paste0(tbl, txt, sep='\n', collapse='')

  tbl
}

center <- function(word, total_spaces, sepr = '     ') {
  paste0(strrep(' ', total_spaces %/% 2),
         word,
         strrep(' ', total_spaces - (total_spaces %/% 2)), collapse = '', sep=sepr)
}


data2text <- function(stat_mat, title='Summary Statistics') {
  tbl_len <- apply(stat_mat, 2, function(x) max(nchar(x)))
  # compares max lens of data cols and col names
  tbl_len <- unlist(Map(function(a,b) max(a, nchar(b)),
                        tbl_len, colnames(stat_mat)))
  lbls <- c('Statistic', colnames(stat_mat))
  # total table length is sum of row names max and prev. col lgths
  tbl_len <- c(max(nchar(c('Statistic', rownames(stat_mat)))), tbl_len)
  line_width <- sum(tbl_len) + 3 * (length(lbls) - 1)
  tbl <- paste0(center(toupper(title), line_width - nchar(title)), '\n', collapse='')
  tbl <- paste0(tbl, paste0(strrep('=', line_width), '\n', collapse=''),
                collapse='')

  # labels formatted left align
  lbl_line <- Map(function(a,b) {
    paste0(a, strrep(' ', b - nchar(a) + 3))
  }, lbls, tbl_len)

  tbl <- paste0(tbl, paste0(lbl_line, collapse=''), '\n', collapse='')
  tbl <- paste0(tbl, strrep('-', line_width), '\n', collapse='')

  # data in table is center aligned
  body <- lapply(rownames(stat_mat), function(r) {
    row <- center(stat_mat[r, ],
                  tbl_len[1:ncol(stat_mat)+1] - nchar(stat_mat[r, ]),
                  sepr='   ')
    paste0(r, strrep(' ', tbl_len[[1]] - nchar(r) + 3),
           paste0(row, collapse=''),
           collapse='')
  })

  tbl <- paste0(tbl, paste0(body, collapse='', sep='\n'), collapse='')
  tbl
}
