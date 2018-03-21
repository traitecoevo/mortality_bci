
launch_task_i <- function(i, task_name, pbs.whisker, iter, sleep=0) {
  files <- lapply(i, write_pbs, pbs.whisker = pbs.whisker, task_name = task_name, iter = iter)
  res <- sapply(files, qsub, echo_only=FALSE, verbose=TRUE, sleep=sleep)
  dat <- process_pbs(i, res)
  append_jobfile(dat, file.path("results/pbs", task_name, "_jobs.csv"))
}

pbs_filename <- function(i, s){
  sprintf("results/pbs/%s/job_%d.pbs", s, i)
}

write_pbs <- function(id, pbs.whisker, task_name, iter = 2000) {
    template <- readLines(pbs.whisker)
    str <- whisker::whisker.render(template)
    file <- pbs_filename(id, task_name)
    dir.create(dirname(file), FALSE, TRUE)
    writeLines(str, file)
    file
}

qsub <- function(pbs_filenames, echo_only=TRUE, verbose=TRUE, sleep=0) {
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
  if(sleep > 0){
    Sys.sleep(sleep)
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
qdel <-function(i, txt = ".katana.science.unsw.edu.au"){
   msg <- paste(paste0(i, txt), collapse= " ")
   system2("qdel", msg, stdout=TRUE)
}
