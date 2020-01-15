# This is a branch
library(tidyverse)
library(stringr)
library(pdftools)

# matches words of at least length three that are formmatted as a label
lab_split <- '(((\\\n)|[ ]{2,})([1-9\\.]{1,3})?([a-zA-Z]{3,})((\\\r)|(\\\n)){1,})'
ref_delim <- '^(\\s*)?((WORKS CITED)|(REFERENCES))\\b'
# breaks at [#] or newlined #.
num_break <- '(^|(\\\r)|(\\\n))(\\[)?([1-9]{1,})(\\]|\\.){1,}([ ]|(\\\t))'
folder <- 'C:/Users/bedempe/Desktop/bib/'
master <- paste0(folder, 'meat_consumption_scalco.pdf')

#' @export
load_bib <- function(folder) {
  # loads in pdfs from folder
  files <- list.files(path = folder, full.names = T,
                      pattern=".pdf$")
  corp <- Corpus(URISource(files), readerControl = list(reader = readPDF))


  corp <- lapply(corp, function(x) x$content[grep(ref_split, x$content)])
  # exploration
  x <- corp[[3]]$content
  x <- x[grepl(ref_split, x)]
  y <- strsplit(x, '\n')
}

load_master_bib <- function(path) {
  # inputs a readable pdf and returns a vector of
  # sections delimited by a global regex command
  file <- pdf_text(path)
  file <- paste0(file, sep='', collapse='')
  # locates the label splits and inserts markers
  file <- gsub(pattern = lab_split, perl = TRUE, replacement = '=~~~=\\U\\1', file)
  file <- unlist(stringr::str_split(file, "=~~~="))
  file <- file[grepl(ref_delim, file)]
  file <- as.character(stringr::str_remove(file, ref_delim))
  file
}


separate_bib <- function(bib) {
  # splits a bib section into individual entries
  # right now it is only split on numbered entries
  bib_f <- unlist(stringr::str_split(bib, num_break))
  bib_f <- bib_f[bib_f != '']
  bib_f <- unlist(lapply(bib_f, str_remove_all, pattern=num_break))
  bib_f <- unlist(lapply(bib_f, str_remove_all,
                         pattern='([ ]{2,})|(\\\n)|(\\\t)|(\\\r)'))
  # removes residual new lines and return carriages
  bib_f
}

read_bib <- function(bib) {
  #given a vector of bib entries, a dataframe is created and returned
  # the columns of the data frame are the critical information: title, author, date, etc
  # assumes APA for now

  # Extracts dates as being a 4 digit number between parantheses
  dates <- stringr::str_extract(bib, '(\\()(\\d{4})(\\))')
  dates <- stringr::str_remove_all(dates,'[(\\())(\\()]')

  # separates author list from rest of bib
  bib_f <- str_split(bib_f, paste0('\\s\\(', dates, '\\).\\s'))
  # separates authors by reference 'and'
  authors <- lapply(bib_f, function(x) x[1])
  authors <- str_split(authors, '\\s(and)\\s')
  max_n <- max(unlist(lapply(x, length)))
  authors <- lapply(x, function(x) c(x, vector(mode='character',
                                               length=max_n - length(x))))
  authors_df <- data.frame(do.call(rbind, authors), stringsAsFactors = FALSE)
  colnames(authors_df) <- str_replace(colnames(authors_df), 'X', 'author')

  # sep attributed to spies006 on stackoverflow
  sep_ <- function(...) {
    dots <- list(...)
    n <- stringr::str_count(dots[[1]][[dots[[2]]]], "\\d+")
    tidyr::separate_(..., into = sprintf("%s_%s", dots[[2]],
                                  c('last', 'first')),
              sep=',', fill = 'right')
  }

  bib_df <- authors_df %>%
    Reduce(f = sep_, x = colnames(authors_df)) %>%
    dplyr::mutate_all(tidyr::replace_na, replace='')
  bib_df$date <- dates

  # removes authors and date from string
  bib_f <- lapply(bib_f, function(x) x[2])
  bib_f <- stringr::str_remove(bib_f, '^.*(\\)\\.\\s)')
  titles <- stringr::str_split(bib_f, '(\\.\\s)')
  titles <- unlist(lapply(titles, function(x)x[[1]]))
  bib_df$title <- titles
}

