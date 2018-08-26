#' Create a dataframe of jobs to complete
#' 
#' Create a dataframe of jobs to complete
#' @param comparison Character. Name of model comparison
#' @param iter Integer. Number of iterations to run
#' @param path Character. Parent directory to store output
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
tasks_2_run <- function(comparison,iter=2000,path='.') {
  
  `%>%` <- magrittr::`%>%`
  
  # SUB FUNCTION: COMPARISON CHECK
  check_task_is_allowed <- function(task_name) {
    
    allowed_tasks <- function() {
      c("null_model", "function_growth_comparison","rho_combinations", "gap_combinations", "size_combinations","species_random_effects",
        "multi_trait_all","multi_trait_parsimony","rho_gap_all", "rho_size_all", "gap_size_all", "final_model","final_base_growth_hazard_re")
    }
    
    if(! task_name %in% allowed_tasks())
      stop(sprintf("invalid task name: %s, must be one of %s", task_name, paste(allowed_tasks(), collapse = ", ")))
  }
  
  # SUB FUNCTION: Cross validation jobs
  kfold_tasks <- function(comparison,iter=2000,path='.') {
    n_kfolds = 10
    n_chains = 3
    
    effects_combo <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
    effects_combo <- sapply(
      base::split(effects_combo, seq_len(nrow(effects_combo))),
      function(x) paste0(x, collapse=''))
    effects_combo[effects_combo==''] <- "none"
    
    base::switch(comparison,
                 "null_model" = {
                   growth_measure <- "true_dbh_dt"
                   rho_combo <- "none"
                   gap_combo <- "none"
                   size_combo <- "none"
                   model  <- "null_model"
                 },
                 "function_growth_comparison" = {
                   growth_measure <- c("true_dbh_dt",'true_basal_area_dt')
                   rho_combo <- "none"
                   gap_combo <- "none"
                   size_combo <- "none"
                   model  <- c("base_hazard","growth_hazard","base_growth_hazard")
                 },
                 "species_random_effects" = {
                   growth_measure <- c("true_dbh_dt")
                   rho_combo <- "none"
                   gap_combo <- "none"
                   size_combo <- "none"
                   model  <- "base_growth_hazard_re"
                 },
                 "rho_combinations" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- effects_combo
                   gap_combo <- "none"
                   size_combo <- "none"
                   model <- "base_growth_hazard"
                 },
                 "gap_combinations" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- "none"
                   gap_combo <- effects_combo
                   size_combo <- "none"
                   model <- "base_growth_hazard"
                 },
                 "size_combinations" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- "none"
                   gap_combo <- "none"
                   size_combo <- effects_combo
                   model <- "base_growth_hazard"
                 },
                 "multi_trait_all" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- "abc"
                   gap_combo <- "abc"
                   size_combo <- "abc"
                   model <- "base_growth_hazard"
                 },
                 "multi_trait_parsimony" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- "c"
                   gap_combo <- "c"
                   size_combo <- "none"
                   model <- "base_growth_hazard"
                 },
                 "rho_gap_all" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- "abc"
                   gap_combo <- "abc"
                   size_combo <- "none"
                   model <- "base_growth_hazard"
                 },
                 "rho_size_all" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- "abc"
                   gap_combo <- "none"
                   size_combo <- "abc"
                   model <- "base_growth_hazard"
                 }, 
                 "gap_size_all" = {
                   growth_measure <- 'true_dbh_dt'
                   rho_combo <- "none"
                   gap_combo <- "abc"
                   size_combo <- "abc"
                   model <- "base_growth_hazard"
                 }, 
                 NA
    )
    ret <- expand.grid(comparison=comparison,
                       model = model,
                       iter=iter,
                       chain=seq_len(n_chains),
                       growth_measure=growth_measure,
                       rho_combo=rho_combo,
                       gap_combo=gap_combo,
                       size_combo=size_combo,
                       kfold=seq_len(n_kfolds),
                       stringsAsFactors=FALSE) %>%
      dplyr::arrange(model, growth_measure)
    
    variations <- unique(ret[,c('comparison','model','growth_measure','rho_combo','gap_combo','size_combo','kfold')])
    
    ret$modelid <- rep(1:nrow(variations),each = n_chains)
    ret <- ret %>%
      dplyr::mutate(jobid = seq_len(n()),
                    filename = sprintf("%s/results/chain_fits/%s/%d.rds", path, comparison, jobid),
                    fold_data = sprintf("%s/data/kfold_data/bci_data_%s.rds", path, kfold))
    return(ret)
  }
  
  # SUB FUNCTION: FINAL MODEL
  full_data_tasks <- function(model, iter=2000,path='.') {
    n_chains = 3
    base::switch(model,
                 "final_model" = {
                   rho_combo = "abc"
                   gap_combo = "abc"
                   size_combo = "abc"
                   comparison = 'final_model'
                 },
                 "final_base_growth_hazard_re" = {
                   rho_combo = "none"
                   gap_combo = "none"
                   size_combo = "none"
                   comparison = "final_base_growth_hazard_re"
                 },
                 stop("Model must be either 'final_model' or 'final_base_growth_hazard_re'"))
    
    ret <- expand.grid(comparison=comparison,
                       model=model,
                       iter=iter,
                       chain=seq_len(n_chains),
                       growth_measure= "true_dbh_dt",
                       rho_combo= rho_combo,
                       gap_combo=gap_combo,
                       size_combo=size_combo,
                       kfold=0,
                       stringsAsFactors=FALSE)
    
    variations <- unique(ret[,c('comparison','model','growth_measure','rho_combo','gap_combo','size_combo','kfold')])
    
    ret$modelid <- rep(1:nrow(variations),each = n_chains)
    ret <- ret %>%
      dplyr::mutate(jobid = seq_len(n()),
                    filename = sprintf("%s/results/chain_fits/%s/%d.rds", path, model, jobid),
                    fold_data = sprintf("%s/data/bci_data_full.rds", path))
    return(ret)
  }
  
  check_task_is_allowed(comparison)
  
  if(comparison %in% c("final_model","final_base_growth_hazard_re")) {
    full_data_tasks(comparison, iter, path)
  }
  else {
    kfold_tasks(comparison, iter, path)
  }
}