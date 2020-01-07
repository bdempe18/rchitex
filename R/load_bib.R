library(stringr)
library(pdftools)
ref_split <- '^References\n|^Bibliography\n|^Works Cited\n|\nReferences\n'
folder <- '/home/ben/Documents/dump/sample_bib'
path <- paste0(folder, '/sample_paper.pdf')

#' @export`
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
  file <- pdf_text(path)
  file <- file[grepl(ref_split, file)]
  file <- stringr::str_remove(file, ref_split) # removes section reference
}


separate_bib <- function(bib) {
  # given a character bib (in apa) a character vector of stripped entries is returned
  bib_f <- unlist(str_split(bib, '\n\\w')) # each newline entry is separated by a newline and no spaces
  bib_f
}

read_bib <- function(bib) {
  #given a vector of bib entries, a dataframe is created and returned
  # the columns of the data frame are the critical information: title, author, date, etc
  # assumes APA for now
  bib_f <- str_replace_all(bib, '[\\s]+', ' ')
  bib_f <- str_replace_all(bib_f, '\n|\t', ' ')

  dates <- str_split(bib_f, '\\(|\\)')
  dates <- unlist(lapply(dates, function(x) x[[2]]))

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

