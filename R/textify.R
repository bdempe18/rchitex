# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Calculates the maximum number of spaces needed in each column
#' @param idvn_names vector of independent variable names
#' @param fits
#' @param dv_names vector or dependent variable names
#' @param coefs vector of coefficients
#' @param sigs
column_widths <- function(idvn_names, fits, dv_names, coefs, sigs) {
  # needed to format width of var name col
  longest_idvn <- max(nchar(idvn_names))

  col_widths <- lapply(idvn_names, function(ivar) nchar(coefs[[ivar]]) +
                      nchar(sigs[[ivar]]))

  fit_lengths <- lapply(fits, function(fc) nchar(fc))
  fit_lengths <- do.call(rbind, fit_lengths)
  fit_lengths <- apply(fit_lengths, 2, max, na.rm=TRUE)
  dep_name_lengths <- Map(nchar, dv_names)
  col_widths <- do.call(rbind, col_widths)
  col_widths <- unlist(Map(max, fit_lengths,
                           apply(col_widths, 2, max, na.rm=TRUE)))
  col_widths <- unlist(Map(max, col_widths, dep_name_lengths))
  c(longest_idvn, col_widths)
}

#' Produces the row of p values and cut offs. Located at bottom of table
p_value_row <- function(sig_levels) {
  p_row <- paste0(names(sig_levels),'p<', unlist(sig_levels, use.names=F),' ',
                  collapse='')
  substr(p_row, 1, nchar(p_row) - 1)
}

#' Centers a phrase within a n-length string. Eg '"Ben", 7' -> "  Ben  "
center <- function(word, total_spaces, sepr = '') {
  total_spaces <- vapply(total_spaces, function(a) max(a, 0), 0)
  paste0(strrep(' ', total_spaces %/% 2),
         word,
         strrep(' ', total_spaces - (total_spaces %/% 2)), collapse = '', sep=sepr)
}

#' created centered multi-column grouped labels with approporiate underlining
#' @param gl_vector
grouper <- function(gl_vector, col_widths) {
  print_func <- function(x, i, vect) {
    lab <- names(vect[i])
    center(lab, x + 5*(vect[i] - 1) - nchar(lab))
  }
  gl_print(gl_vector, print_func, col_widths)
}

#' aids dasher and grouper
#' DRY implementation
gl_print <- function(gl_vector, func, col_widths) {

  wrapper <- function(i) {
    start_col <- (if (i>1) sum(gl_vector[1:i-1]) else 0)
    label <- ''

    if(is.na(names(gl_vector[i]))) {
      label <- strrep(' ', col_widths[start_col] + 5)
    } else {
      x <- start_col + gl_vector[i]-1
      col_width <- sum(col_widths[start_col: x])
      label <- func(col_width, i, gl_vector)
    }
    label
  }
  lapply(seq_along(gl_vector), wrapper)
}

#' Creates dashed lines under each grouped label
dasher <- function(gl_vector, col_widths, line_width) {
  # prints dashes of appropriate length
  print_func <- function(x, i, vect) {
    # strrep at end controls how many spaces after dash el
    paste0(strrep('-', x + 5*(vect[i]-1)), strrep(' ', 3))
  }
  gl <- gl_print(gl_vector, print_func, col_widths)

  # if len labs exceed line width, gl is truncated
  strtrim(paste0(gl, collapse=''), line_width-col_widths[1]-5)
}

#' Writes 2 lines of reg table for a given dependent variable
#' note - function is vectorized
write_idv <- function(iv, idvn, coefs, sigs, col_widths, reporter) {
  mod_width <- col_widths[2: length(col_widths)]

  # concats coef and sig symbol for each model if applicable
  row_coef <- coefs[[iv]]
  row_coef[is.na(row_coef)] <- ''
  ests <- paste0(row_coef, sigs[[iv]])
  spaces <- mod_width - nchar(ests) + 3
  ests <- center(ests, spaces)
  ests <- substr(ests, 1, nchar(ests) - 3) # eliminates trailing whitespace

  # independent variable label
  lab_margin <- strrep(' ', col_widths[1] - nchar(idvn[[iv]]) + 5)
  lab <- paste0(idvn[[iv]], lab_margin)

  row <- paste0(lab, ests, '\n', collapse='')

  # statistical significance on the second line
  ep <- reporter[[iv]]
  ep[!is.na(ep)] <- paste0('(', ep[!is.na(ep)], ')')
  ep[is.na(ep)] <- ''
  ep <- paste0(ep)
  #if(!is.na(reporter[[iv]])) {
  #  ep <- paste0('(', reporter[[iv]], ')')
  #}

  spaces <- mod_width - nchar(ep) + 3
  ep <- center(ep, spaces)
  # blank label on second line
  blank_lab <- strrep(' ', col_widths[1] + 5)
  ep <- paste0(blank_lab, ep, collapse = '')
  ep <- substr(ep, 1, nchar(ep)-2)

  paste0(row, ep, '\n', sep='\n', collapse='')
}

#' write annotations
write_fits <- function(fc, fits, col_widths) {
  mod_width <- col_widths[2: length(col_widths)]
  el <- center(fits[[fc]], mod_width - nchar(fits[[fc]]) + 3)
  el <- substr(el, 1, nchar(el) - 3)
  lab_margin <- strrep(' ', col_widths[1] - nchar(fc) + 5)

  paste0(fc, lab_margin, el, '\n', collapse='')
}


# ============================================================================
# CONVERTS A CORE RCHITEX TYPE TO A TEXT TABLE
# supported regressional model -> text table
# ============================================================================

#' Convert a regression-like model to a text table format
#' @param coefs vector of coefficients
#' @param reporter
#' @param fits
#' @param signs
#' @param idvn vector of independent variable names
#' @param max_precision int > 0, number of digits to display (e.g. 0.XXXX = 4)
#' @param sig_levels list of significance levels and the corresponding symbols
#' @param note (UNSUPPORTED) string note at the bottom of the table
#' @param dn
#' @grouped_label
model2text <- function(coefs, reporter, fits, sigs, idvn, max_precision,
                       sig_levels, title = 'Regression Results',
                       note = '', dn, grouped_label=NULL) {

  # --- First step is to calculate necessary length of each column ---
  n_mods <- length(coefs[[1]]) # number of models
  col_widths <- column_widths(idvn = names(idvn), fits, dn, coefs, sigs)


  # Populates the informative P-VALUE ROW below annotations
  p_row <- p_value_row(sig_levels)

  # -- Calculates table width from col widths + some spacing restrictions ---
  # TODO this seems like hacky way to handle the problem
  line_width <- sum(col_widths) + (5 * (n_mods - 1)) + 5 - 2 * (n_mods - 1) + 1

  # the minimum width of a table is the space it takes to print "sig..p < 0.1".
  # if the minimum width is not met, the difference is made up in the label col
  if (line_width - nchar(p_row) - nchar("Sig: ") < 0) {
    big_line <- nchar('Sig: ') + nchar(p_row)
    col_widths[1] <- col_widths[1] + (big_line - line_width)
    line_width <- big_line
  }

  tbl <- paste0(center(toupper(title), line_width - nchar(title)), '\n')
  tbl <- paste0(tbl, strrep('=', line_width), '\n', collapse='')

  # --- grouped labels if applicable ---
  if (!is.null(grouped_label)) {
    gl_vector <- group_labels(grouped_label, n_mods, missing=1)
    gl <- grouper(gl_vector, col_widths)
    gl_dash <- dasher(gl_vector, col_widths, line_width)

    tbl <- paste0(tbl, strrep(' ', col_widths[1] + 5),
                  paste0(gl, collapse=''), '\n', collapse='')
    tbl <- paste0(tbl, strrep(' ', col_widths[1] + 5),
                  gl_dash, '\n', collapse='')

  }

  # blank line?
  tbl <- paste0(tbl, strrep(' ', col_widths[1] + 5), collapse='')
  # dependant variable line
  tbl <- paste0(tbl, paste0(center(dn, col_widths[2: length(col_widths)], sepr=''), collapse=''),
                            '\n', collapse='')
  tbl <- paste0(tbl, strrep('~', line_width), '\n', collapse='')

  # table body
  tbl_text <-lapply(names(idvn), write_idv, coefs=coefs, sigs=sigs,
                                            col_widths=col_widths,
                                            reporter=reporter, idvn=idvn)

  # concating header and body
  tbl_text <- paste0(tbl_text, collapse='')
  tbl_text <- substr(tbl_text, 1, nchar(tbl_text) - 1)
  tbl <- paste0(tbl, tbl_text, collapse='')
  tbl <- paste0(tbl, strrep('~', line_width), '\n', collapse='')

  # model fits
  fit_txt <- lapply(names(fits), write_fits, fits=fits, col_widths=col_widths)

  fit_txt <- paste0(fit_txt, collapse='')
  tbl <- paste0(tbl, fit_txt, collapse='')
  tbl <- paste0(tbl, strrep('=', line_width), '\n', collapse='')
  tbl <- paste0(tbl, 'Sig:', collapse='')

  # footer
  p_row <- paste0(strrep(' ', line_width-nchar(p_row) - nchar('Sig:')),
                  p_row, collapse='')
  tbl <- paste0(tbl, p_row, sep='\n', collapse='')

  tbl
}

# ============================================================================
# CONVERTS SUMMARY STATISTICS TO A TEXT TABLE
# ============================================================================

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
