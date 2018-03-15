
pbs_filename <- function(i, s){
  sprintf("results/pbs/%s/job_%d.pbs", s, i)
}

write_pbs <- function(id, pbs.whisker, s) {
    template <- readLines(pbs.whisker)
    str <- whisker::whisker.render(template)
    file <- pbs_filename(id, s)
    dir.create(dirname(file), FALSE, TRUE)
    writeLines(str, file)
    file
}

qsub <- function(pbs_filenames, echo_only=TRUE, verbose=TRUE) {
  if (echo_only) {
    system2 <- function(command, args, ...) {
      message(paste(command, args, ...))
    }
  }
  pbs_ids <- vector("list", length=length(pbs_filenames))
  for (i in seq_along(pbs_filenames)) {
    if (verbose) {
      message("Launching ", pbs_filenames[[i]])
    }
    pbs_ids[[i]] <- system2("qsub", pbs_filenames[[i]], stdout=TRUE)
  }
  ## TODO: Throw an error if the job was refused.
  invisible(pbs_ids)
}

append_jobfile <- function(dat, jobfile="pbs_jobs.csv") {
  if (file.exists(jobfile)) {
    prev <- read.csv(jobfile, stringsAsFactors=FALSE)
    ## *Replace* existing ids.
    v <- c("id", "pbs_id")
    hash_prev <- apply(prev[v], 1, paste, collapse="\r")
    hash_dat <- apply(dat[v], 1, paste, collapse="\r")
    prev <- prev[!(hash_prev %in% hash_dat),]
  } else {
    prev <- NULL
  }
  write.csv(rbind(prev, dat), jobfile, row.names=FALSE)
}

process_pbs <- function(id, pbs) {
  data.frame(id=id,
             pbs_id=as.integer(sub("\\..+$", "", pbs)),
             stringsAsFactors=FALSE)
}

# Delete jobs from queue
qdel <-function(i){
   msg <- paste(paste0(i, ".katana.science.unsw.edu.au"), collapse= " ")
   system2("qdel", msg, stdout=TRUE)
}

# # null models
# jobfile <- "results/pbs/null_model/_jobs.csv"
# i <- 1:30
# files <- lapply(i, write_pbs, pbs.whisker = "pbs_null.whisker", s = "null_model")
# res <- sapply(files, qsub, echo_only=FALSE, verbose=TRUE)
# dat <- process_pbs(i, res)
# append_jobfile(dat, jobfile)

# # functional growth comparison
# jobfile <- "results/pbs1/function_growth_comparison/_jobs.csv"
# i <- 1:180
# files <- lapply(i, write_pbs, pbs.whisker = "pbs_fg.whisker", s = "function_growth_comparison")
# res <- sapply(files, qsub, echo_only=FALSE, verbose=TRUE)
# dat <- process_pbs(i, res)
# append_jobfile(dat, jobfile)

# # wood density comparisons
# jobfile <- "results/pbs/rho_combinations/_jobs.csv"
# i <- 108:240
# files <- lapply(i, write_pbs, pbs.whisker = "pbs_rho.whisker", s = "rho_combinations")
# res <- sapply(files, qsub, echo_only=FALSE, verbose=TRUE)
# dat <- process_pbs(i, res)
# append_jobfile(dat, jobfile)

# # species random effects
# jobfile <- "results/pbs/species_random_effects/_jobs.csv"
# i <- 1:30
# files <- lapply(i, write_pbs, pbs.whisker = "pbs_spre.whisker", s = "species_random_effects")
# res <- sapply(files, qsub, echo_only=FALSE, verbose=TRUE)
# dat <- process_pbs(i, res)
# append_jobfile(dat, jobfile)
