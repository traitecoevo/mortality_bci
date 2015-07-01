tasks_2_run <- function(iter, name, growth_measure, rho_combo, tasks_run=tasks_run) {
  n_kfolds <- 10
  n_chains <- 3
  
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measure,
                     rho_combo=rho_combo,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$modelid <- rep(1:nrow(unique(ret[,c('experiment','growth_measure','rho_combo','kfold')])),each = n_chains)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$fold_data <- sprintf("export/bci_data_%s.rds", ret$kfold)
  create_dirs(unique(dirname(ret$filename)))
  if(isTRUE(tasks_run)) {
    tasks <- tasks_growth()
    i <- match(do.call(paste, ret[, c('chain', 'growth_measure', 'rho_combo', 'kfold')]),
               do.call(paste, tasks[, c('chain', 'growth_measure', 'rho_combo', 'kfold')]))
    ff <- file.copy(tasks$filename[na.omit(i)], ret$filename[which(!is.na(i))])
    if(!all(ff)) {
      warning(sprintf('Some previously run model outputs failed to copy:\n%s',
                      paste(tasks$filename[na.omit(i)][!ff], collapse='\n')))
    }
    return(ret[is.na(i), ]  )
  }
  ret
}


# What growth rate best predicts mortality using full model
tasks_growth <- function(iter=2000, name = 'growth_comparison') {
  tasks_2_run(iter, 
    name=name, 
    growth_measure =  c("dbh_dt", 
                        "dbh_dt_rel", 
                        "basal_area_dt",
                        "basal_area_dt_rel"),
    rho_combo="abc",
    tasks_run=FALSE)
}

# Based on best growth rate model which combination of rho effects is most parsimonous
tasks_rho_combos <- function(iter=2000, growth_measure, name="rho_combinations", tasks_run) {
  rho_combo <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  rho_combo <- sapply(split(rho_combo, seq_len(nrow(rho_combo))), function(x) paste0(x, collapse=''))
  tasks_2_run(iter, 
       name=name, 
       growth_measure =  growth_measure,
       rho_combo=rho_combo,
       tasks_run=tasks_run)
}

# Launching growth comparison analysis
run_growth_comparison <- function(iter=2000) {
  tasks <- tasks_growth(iter = iter, name="growth_comparison")
  ret <- mclapply(df_to_list(tasks), model_compiler)
}

# Launching rho combination analysis

run_rho_combination <- function(iter=2000, growth_measure) {
  tasks <- tasks_rho_combos(iter = iter, growth_measure = growth_measure, tasks_run=TRUE)

  # Check is any of tasks already run in prev experiment, if so copy across and give correct id
  tasks_done <- tasks_growth(iter = iter)
  # TO DO : check parameter columns for same values in table above, then copy results to new directory
  # and remove from list to run
  ret <- mclapply(df_to_list(tasks), model_compiler)
}
