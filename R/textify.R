model2text <- function(coefs, reporter, fits, sigs, idvn, max_precision,
                       sig_levels,
                       title = 'Regression Results',
                       note = '', dn,
                       grouped_label=NULL) {
  n_mods <- length(coefs[[1]])
  longest_idvn <- max(nchar(idvn))

  # calculates the maximum space needed in each column
  lengths <- lapply(names(idvn), function(ivar) nchar(coefs[[ivar]]) +
                      nchar(sigs[[ivar]]))
  fit_lengths <- lapply(fits, function(fc) nchar(fc))
  fit_lengths <- do.call(rbind, fit_lengths)
  fit_lengths <- apply(fit_lengths, 2, max, na.rm=TRUE)
  dep_name_lengths <- Map(nchar, dn)
  lengths <- do.call(rbind, lengths)
  lengths <- unlist(Map(max, fit_lengths, apply(lengths, 2, max, na.rm=TRUE)))
  lengths <- unlist(Map(max, lengths, dep_name_lengths))
  lengths <- c(longest_idvn, lengths)
  # row of p-val cut offs at
  p_row <- paste0(names(sig_levels),'p<',unlist(sig_levels, use.names=F),' ', collapse='')
  p_row <- substr(p_row, 1, nchar(p_row) - 1)

  # the minimum width of a table is the space it takes to print "sig..p < 0.1"....
  # if the minimum width is not met, the difference is made up in the label col

  line_width <- sum(lengths) + 5 * (n_mods - 1) + 5 # corrected for white space between cols
  if (line_width - nchar(p_row) - nchar("Sig: ") < 0) {
    big_line <- nchar('Sig: ') + nchar(p_row)
    lengths[1] <- big_line - line_width + lengths[1]
    line_width <- big_line

  }


  tbl <- paste0(center(toupper(title), line_width - nchar(title)), '\n')
  # preamble
  tbl <- paste0(tbl, strrep('=', line_width), '\n', collapse='')

  # grouped labels if applicable
  if (!is.null(grouped_label)) {

    gl_vector <- group_labels(grouped_label, n_mods, missing=1)
    temp <- names(gl_vector)
    gl_vector <- as.numeric(gl_vector)
    names(gl_vector) <- temp

    # centered labels
    gl <- lapply(seq_along(gl_vector), function(i) {
      start_col <- if (i>1) sum(gl_vector[1:i-1]) + 2 else 2
      if (is.na(names(gl_vector[i]))) {
        return(strrep(' ', lengths[start_col] + 5))
      }

      x <- start_col + gl_vector[i]-1
      col_width <- sum(lengths[start_col: x])
      lab <- names(gl_vector[i])
      return(center(lab, col_width + 5*(gl_vector[i]-1)- nchar(lab)))
    })
    tbl <- paste0(tbl, strrep(' ', lengths[1] + 5),
                  paste0(gl, collapse=''), '\n', collapse='')

    # dashed underline

    gl <- lapply(seq_along(gl_vector), function(i) {
      start_col <- if (i>1) sum(gl_vector[1:i-1]) + 2 else 2

      if (is.na(names(gl_vector[i]))) {
        return(strrep(' ', lengths[start_col] + 5))
      }

      # intermediate variable is here to fix a weird bug
      x <- start_col + gl_vector[i]-1
      col_width <- sum(lengths[start_col: x])

      row <- paste0(strrep('-', col_width + 5*(gl_vector[i]-1)),
                    strrep(' ', 5))
      return(row)
    })

    # if len labs exceed line width, gl is truncated
    gl <- strtrim(paste0(gl, collapse=''), line_width-lengths[1]-5)

    tbl <- paste0(tbl, strrep(' ', lengths[1] + 5),
                  gl, '\n', collapse='')
  }


  tbl <- paste0(tbl, strrep(' ', lengths[1] + 5), collapse='')
  tbl <- paste0(tbl, paste0(center(dn, lengths[2:length(lengths)] -
                                     nchar(dn)), collapse=''),
                '\n', collapse='')
  tbl <- paste0(tbl, strrep('~', line_width), '\n', collapse='')
    tbl_text <- lapply(names(idvn), function(iv) {
    ests <- ifelse(is.na(coefs[[iv]]),
                          '', paste0(coefs[[iv]], sigs[[iv]]))
    spaces <- lengths[2:length(lengths)] - nchar(ests) + 3
    ests <- center(ests, spaces)
    nam <- paste0(idvn[[iv]], strrep(' ', lengths[1] - nchar(idvn[[iv]]) + 5))
    ests <- paste0(nam, substr(ests, 1, nchar(ests)-3), '\n', collapse='')

    ep <-ifelse(is.na(reporter[[iv]]),
                      '', paste0('(',reporter[[iv]], ')'))
    spaces <- lengths[2:length(lengths)] - nchar(ep) + 3
    ep <- center(ep, spaces)
    blank <- strrep(' ', lengths[1] + 5)
    ep <- paste0(blank, ep, collapse='')
    paste0(ests, substr(ep, 1, nchar(ep)-3), '\n', sep='\n', collapse='')
  })

  tbl_text <- paste0(tbl_text, collapse='')
  tbl <- paste0(tbl, substr(tbl_text, 1, nchar(tbl_text)-1), collapse='')
  tbl <- paste0(tbl, strrep('~', line_width), '\n', collapse='')

  # model fits
  fit_txt <- lapply(names(fits), function(fc) {
    el <- center(fits[[fc]], lengths[2:length(lengths)] -nchar(fits[[fc]]) + 3)
    el <- substr(el, 1, nchar(el)-3)
    paste0(fc, strrep(' ', lengths[1] - nchar(fc) + 5), el,
           '\n', collapse='')
  })

  fit_txt <- paste0(fit_txt, collapse='')
  tbl <- paste0(tbl, fit_txt, collapse='')
  tbl <- paste0(tbl, strrep('=', line_width), '\n', collapse='')
  tbl <- paste0(tbl, 'Sig:', collapse='')

  p_row <- paste0(strrep(' ', line_width-nchar(p_row) - nchar('Sig:')),
                  p_row, collapse='')

  tbl <- paste0(tbl, p_row, sep='\n', collapse='')

  tbl
}

center <- function(word, total_spaces, sepr = '') {
  total_spaces <- vapply(total_spaces, function(a) max(a, 0), 0)
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
  lbl_line <- paste0(lbl_line, collapse='')
  lbl_line <- substr(lbl_line, 1, nchar(lbl_line)-3)

  tbl <- paste0(tbl, lbl_line, '\n', collapse='')
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
  body <- lapply(body, function(row) substr(row, 1, nchar(row)-3))

  tbl <- paste0(tbl, paste0(body, collapse='', sep='\n'), collapse='')
  tbl
}
