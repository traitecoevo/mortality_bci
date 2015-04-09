
pandoc_build <- function(file){
  args <- list('--template=data/include.tex', '--latex-engine=xelatex')
  pandoc_convert(file, output= paste0(tools::file_path_sans_ext(file), ".pdf"), citeproc = TRUE, options = args, verbose = TRUE, wd = ".")
}

get_amnat_csl <- function(dest, url="https://raw.githubusercontent.com/citation-style-language/styles/master/the-american-naturalist.csl"){
	cat(getURL(url), file=dest)
}
# define accessoryfunctions
inv_cloglog <- function(x) {1 - exp(-exp(x))}

bernoulli_log <- function(y, p) {
  dbinom(y, size=1, p, log = TRUE)
}

inv_cloglog <- function(x) {1 - exp(-exp(x))}

bernoulli_log <- function(y, p) {
  dbinom(y, size=1, p, log = TRUE)
}

to_df <- function(x) {
	d <- as.list(x)
  names(d) <- paste0("X", seq_along(d))
  attr(d, "row.names") <- "1"
  class(d) <- "data.frame"
  d
}
