# Run test to ensure models work
pars_test  <- function(iter=5, name="test") {
  n_kfolds <- 1
  n_chains <- 1
  model <- seq_len(3)
  effects <- c("model_constant","model_no_trait", "model_census_err", "model_no_spp_err", "model_full")
  growth_measures <-  c("dbh_dt")
  
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     rho_effects_on =NA,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  
  model <- 1
  effects <- "model_full_rho_combs"
  perms <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  perms <-  sapply(split(perms, seq_len(nrow(perms))), function(x) paste0(x, collapse=''))
  growth_measures <- c("dbh_dt") # To be determined by growth comparison analysis
  ret2 <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     model=model,
                     effect=effects,
                     rho_effects_on = perms,
                     growth_measure=growth_measures,
                     kfold = seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret_test <- rbind(ret,ret2)
  
  ret_test$jobid <- seq_len(nrow(ret_test))
  ret_test$filename <- sprintf("results/%s/%d.rds", name, ret_test$jobid)
  ret_test$fold_data <- sprintf("export/bci_data_%s.rds", ret_test$kfold)
  ret_test
}

# What growth rate best predicts mortality using full model
pars_growth <- function(iter=2000, name="growth_comparison") {
  n_kfolds <- 10
  n_chains <- 3
  model <- 3
  effects <- c("full_models")
  growth_measures <-  c("dbh_dt", 
                        "dbh_dt_rel", 
                        "basal_area_dt",
                        "basal_area_dt_rel")
  
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$fold_data <- sprintf("export/bci_data_%s.rds", ret$kfold)
  ret
}

# Based on best growth rate model which combination of rho effects is most parsimonous
pars_rho_combs <- function(iter=2000, name="rho_combinations") {
  n_kfolds <- 10
  n_chains <- 3
  model <- 1
  effects <- c("full_model_rho_effects")
  perms <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  perms <-  sapply(split(perms, seq_len(nrow(perms))), function(x) paste0(x, collapse=''))
  growth_measures <- c("dbh_dt") # To be determined by growth comparison analysis
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     model=model,
                     effect=effects,
                     rho_effects_on = perms,
                     growth_measure=growth_measures,
                     kfold = seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$fold_data <- sprintf("export/bci_data_%s.rds", ret$kfold)
  ret
}

# Based on best growth rate model which functional form is most parsimonous?
# Useful for comparison to other models eg. sortie and 'mean' models.
pars_functional_parsimony  <- function(iter=2000, name="functional_form_comparison") {
  n_kfolds <- 10
  n_chains <- 3
  model <- seq_len(3)
  effects <- c("model_full")
  growth_measures <-  c("basal_area_dt") # To be determined by growth comparison analysis
  
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$fold_data <- sprintf("export/bci_data_%s.rds", ret$kfold)
  ret
}

# Launching model tests
run_test <- function(iter=5) {
  pars <- pars_test(iter = iter)
  pars_list <- df_to_list(pars)
  create_dirs(unique(dirname(pars$filename)))
  
  ret <- mclapply(pars_list, model_data)
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

# Launching rho combination analysis
run_rho_functional_parsimony <- function(iter=2000) {
  pars <- pars_functional_parsimony(iter = iter)
  pars_list <- df_to_list(pars)
  for (d in unique(dirname(pars$filename))) {
    dir.create(d, FALSE, TRUE)
  }
  ret <- mclapply(pars_list, model_data)
}