# What growth rate best predicts mortality using full model
pars_growth <- function(iter=2000, name="growth_comparison") {
  n_kfolds <- 10
  n_chains <- 3
  growth_measures <-  c("dbh_dt", 
                        "dbh_dt_rel", 
                        "basal_area_dt",
                        "basal_area_dt_rel")
  
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measures,
                     rho_combo="abc",
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$fold_data <- sprintf("export/bci_data_%s.rds", ret$kfold)
  ret
}

# Based on best growth rate model which combination of rho effects is most parsimonous
pars_rho_combos <- function(iter=2000, name="rho_combinations", growth_measure) {
  n_kfolds <- 10
  n_chains <- 3
  perms <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  perms <-  sapply(split(perms, seq_len(nrow(perms))), function(x) paste0(x, collapse=''))
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measure,
                     rho_combo = perms,
                     kfold = seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$fold_data <- sprintf("export/bci_data_%s.rds", ret$kfold)
  ret
}

# Launching growth comparison analysis
run_growth_comparison <- function(iter=2000) {
  pars <- pars_growth(iter = iter)
  pars_list <- df_to_list(pars)
  for (d in unique(dirname(pars$filename))) {
    dir.create(d, FALSE, TRUE)
  }
  ret <- mclapply(pars_list, model_data)
}

# Launching rho combination analysis

run_rho_combination <- function(iter=2000) {
  pars <- pars_rho_combs(iter = iter)
  pars_list <- df_to_list(pars)
  for (d in unique(dirname(pars$filename))) {
    dir.create(d, FALSE, TRUE)
  }
  ret <- mclapply(pars_list, model_data)
}