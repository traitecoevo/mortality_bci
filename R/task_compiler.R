# What growth rate best predicts mortality using full model
pars <- function(iter, name, growth_measures, perms) {
  n_kfolds <- 10
  n_chains <- 3
 
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measures,
                     rho_combo=perms,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$modelid <- rep(1:nrow(unique(ret[,c('experiment','growth_measure','rho_combo','kfold')])),each = n_chains)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$fold_data <- sprintf("export/bci_data_%s.rds", ret$kfold)
  ret
}


# What growth rate best predicts mortality using full model
pars_growth <- function(iter, name) {
  pars(iter, 
    name=name, 
    growth_measures =  c("dbh_dt", 
                        "dbh_dt_rel", 
                        "basal_area_dt",
                        "basal_area_dt_rel"),
    perms="abc")
}

# Based on best growth rate model which combination of rho effects is most parsimonous
pars_rho_combos <- function(iter, growth_measures, name="rho_combinations") {
  perms <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  perms <-  sapply(split(perms, seq_len(nrow(perms))), function(x) paste0(x, collapse=''))
  pars(iter, 
    name=name, 
    growth_measures =  growth_measures,
    perms=perms)
}

# Launching growth comparison analysis
run_growth_comparison <- function(iter=2000) {
  tasks <- pars_growth(iter = iter, name="growth_comparison")
  create_dirs(unique(dirname(tasks$filename)))
  ret <- mclapply(df_to_list(tasks), model_compiler)
}

# Launching rho combination analysis

run_rho_combination <- function(iter=2000, growth_measure) {
  tasks <- pars_rho_combos(iter = iter, growth_measure = growth_measure)
  create_dirs(unique(dirname(tasks$filename)))

  # Check is any of tasks already run in prev experiment, if so copy across and give correct id
  tasks_done <- pars_growth(iter = iter)
  # TO DO : check parameter columns for same values in table above, then copy results to new directory
  # and remove from list to run
  ret <- mclapply(df_to_list(tasks), model_compiler)
}
