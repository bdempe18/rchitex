gen_preamble <- function(title, n_cols) {
  paste0('\\begin{table}[!htbp]\n',
         '\t\\centering\n\t\\caption{',
         title,'}\n\t\\label{}\n',
         '\\begin{tabular}{@{\\extracolsep{5pt}}l ',
         strrep('c',n_cols),'}',
         '\\\\[-1.8ex]\\hline\\hline \\\\[-1.8ex]\n')
}
