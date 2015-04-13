
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

# An extension of capture.output function, allows you to run and
# log both output and results to file
capture_output2 <- function(..., name, divert_messages=!interactive()) {

  filename <- sprintf("%s.rds", name)
  logfile <- sprintf("%s.txt", name)

  dir.create(dirname(name), showWarnings=FALSE, recursive=TRUE)

  if (divert_messages) {
    message("Diverting messages to ", logfile)
    con <- file(logfile, open="wt")
    sink(con, type="message") # Dangerous!
    sink(con, type="output")
    on.exit(sink(NULL, type="message"))
    on.exit(sink(NULL, type="output"), add=TRUE)
  }
  message("--- Starting at ", Sys.time())

  # This next section adapated from capture.output
  # Evaluate all expressions in parent frame
  # NB only last one is returned
  args <- substitute(list(...))[-1L]
  for (i in seq_along(args)) {
      expr <- args[[i]]
      tmp <- eval(expr,   envir = parent.frame())
  }

  saveRDS(tmp, filename)
  message("--- Finishing at ", Sys.time())
  tmp
}

str_eval <- function(x) {eval(parse(text=x))}
