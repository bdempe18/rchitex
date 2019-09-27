center_text <- function(text, width) {
  n_blank <- (width - nchar(text))%/%2
  paste0(strrep(' ', n_blank), text, strrep(' ', width-n_blank-nchar(text)))
}
