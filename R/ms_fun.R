
pandoc_build <- function(file){
  args <- list('--template=data/include.tex', '--latex-engine=xelatex')
  pandoc_convert(file, output= paste0(tools::file_path_sans_ext(file), ".pdf"), citeproc = TRUE, options = args, verbose = TRUE, wd = ".")
}

get_amnat_csl <- function(dest, url="https://raw.githubusercontent.com/citation-style-language/styles/master/the-american-naturalist.csl"){
	cat(getURL(url), file=dest)
}
