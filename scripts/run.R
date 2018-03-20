#!/usr/bin/env Rscript

# load packages and functions
for (p in c("dplyr","rstan"))
  suppressWarnings(suppressPackageStartupMessages(library(p, character.only=TRUE, quietly=TRUE)))
for (s in c("R/bci_data_processing.R", "R/model.R","R/stan_functions.R"))
  source(s)

options(mc.cores = 1)

# parse and check input variables 
task_name <- as.character(commandArgs(TRUE)[1])
i <- as.integer(commandArgs(TRUE)[2])
iter <- as.integer(commandArgs(TRUE)[3])

if(! task_name %in% c("function_growth_comparison", "null_model", "rho_combinations", "species_random_effects"))
  stop(sprintf("invalid task name: %s", commandArgs(TRUE)[1]))

if(is.na(iter) || iter < 1 || iter > 2000)
  stop(sprintf("Invalid value for iterations: %s", commandArgs(TRUE)[3]))

tasks <- tasks_2_run(task_name, iter = iter)

if(is.na(i) || i < 1 || i >  nrow(tasks))
  stop(sprintf("Invalid value for tasks in '%s': %s", task_name, commandArgs(TRUE)[2]))

# Run job

task <- df_to_list(tasks)[[i]]
dir.create(dirname(task$filename), FALSE, TRUE)
model_compiler(task)
