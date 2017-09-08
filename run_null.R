#!/usr/bin/env Rscript

i <- as.integer(commandArgs(TRUE)[1])

if(!is.integer(i))
  stop(paste("need to pass integer:", i))

for (p in c("dplyr","rstan"))
  library(p, character.only=TRUE, quietly=TRUE)

for (s in c("R/bci_data_processing.R", "R/model.R","R/stan_functions.R"))
  source(s)

df_to_list <- function(x) {
  attr(x, "out.attrs") <- NULL
  unname(lapply(split(x, seq_len(nrow(x))), as.list))
}

options(mc.cores = 1)

tasks <- tasks_2_run("null_model")
task <- df_to_list(tasks)[[i]]
dir.create(dirname(task$filename), FALSE, TRUE)
model_compiler(task)

