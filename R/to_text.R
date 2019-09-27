to_text <- function(stat_mat, colmax = 7) {
  tbl_len<- apply(stat_mat, 2, function(x) max(nchar(x))) # TODO min max goes here
  tbl_len <- unlist(Map(function(a,b) max(a, nchar(b)), tbl_len, colnames(stat_mat)))
  lbls <- c('Statistic', colnames(stat_mat))
  tbl_len<- c(max(nchar(c('Statistic', rownames(stat_mat)))), tbl_len)
  line_width =  sum(tbl_len) + 2 * length(lbls) - 1
  writeLines(strrep('=', line_width))

  fix_width <- function(text, max_width) {
    paste(strrep(' ', max_width - nchar(text)), text, ' ', sep='')
  }

  # what the fuck is this
  center_align <- function(text, max_width, digits) {
    out <- Map(function(s, w, d) {
      n <- as.character(format(as.numeric(s), nsmall=max(d-2,0)))
      paste(strrep(' ', max((w-nchar(n))%/%2,0)),n,
            strrep(' ', max((w-nchar(n))%/%2,0)), sep=' ')
    }, text[2:length(text)], max_width[2:length(max_width)], digits)
    c(paste(text[1], strrep(' ', max_width[1] - nchar(text[1]))),out, sep='')
  }

  digits <- apply(stat_mat, 2, function(x) max(nchar(x)))

  first_row <- paste0(Map(fix_width, lbls, tbl_len), collapse=' ')
  writeLines(first_row, sep=' ')
  writeLines('\n', sep='')
  writeLines(strrep('-', line_width))
  centered <- paste0(lapply(rownames(stat_mat), function(r) {
    paste0(center_align(c(r, stat_mat[r,]), tbl_len, digits), collapse='')
  }), collapse='\n')
  writeLines(centered)
  writeLines(strrep('-', line_width))

}
