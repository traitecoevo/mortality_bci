# Merge chains related to a given model/kfold combination
combine_stan_chains <- function(files) {
  sflist2stanfit(lapply(files, readRDS))
}

# Compile all models related to a given analyses
compile_models <- function(analysis) {
  if(!analysis %in% c("null_model","null_model_random_effects",
                      "no_gamma_model", "no_gamma_model_random_effects",
                      "growth_comparison","rho_combinations")) {
    stop("analysis can only be one of the following: 
                      'null_model,'null_model_random_effects',
                      'no_gamma_model', 'no_gamma_model_random_effects',
                      'growth_comparison','rho_combinations'")
  }
  
  if(analysis=='growth_comparison') {
    growth_measure <- c('true_dbh_dt','true_basal_area_dt')
  }
  else {
    growth_measure <- 'true_dbh_dt'
  }
  tasks <- tasks_2_run(analysis, growth_measure = growth_measure, iter = 1000)
  sets <- split(tasks,  list(tasks$analysis,tasks$growth_measure,tasks$rho_combo,tasks$kfold), sep='_', drop=TRUE)
  
  fits <- lapply(sets, function(s) combine_stan_chains(s[['filename']]))
  pars <- lapply(sets,  function(s) s[1, c("analysis","growth_measure","rho_combo","kfold")])
  
  list(model_info=pars, fits=fits)
}

# Compile multiple analyses at once
compile_multiple_analyses <- function(analysis) {
  sapply(analysis, function(x) compile_models(x), simplify = FALSE)
}

# Examine model diagnostics for single analysis
kfold_diagnostics <- function(analysis) {
  fits <- analysis$fits
  info <- analysis$model_info
  out1 <- bind_rows(lapply(fits, function(x) {
    summary_model <- summary(x)$summary
    sampler_params <- get_sampler_params(x, inc_warmup=FALSE)
    data.frame(
      min_n_eff = min(summary_model[, 'n_eff']),
      max_rhat = max(summary_model[, 'Rhat']),
      n_bad_rhat = length(which(summary_model[, 'Rhat'] > 1.1)),
      n_divergent = sum(sapply(sampler_params, function(y) y[,'n_divergent__'])),
      max_treedepth = max(sapply(sampler_params, function(y) y[,'treedepth__'])))
  }))
  
  out2 <- suppressWarnings(bind_rows(lapply(info, function(x) {
    data.frame(
      analysis = x$analysis,
      growth_measure = x$growth_measure,
      rho_combo = x$rho_combo,
      kfold = as.integer(x$kfold))
  })))
  
  res <- cbind(out2,out1) %>%
    arrange(analysis, growth_measure, rho_combo, kfold)
  
  row.names(res) <- NULL
  return(res)
}

# Examine model diagnostics for multiple analysis
multi_analysis_kfold_diagnostics <- function(list_of_analyses) {
  out <- suppressWarnings(bind_rows(lapply(list_of_analyses, function(x) {
    kfold_model_diagnostics(x)})))
  row.names(out) <- NULL
  return(out)
}

# Extract log likelihood samples for single analysis
extract_loglik_samples <- function(analysis) {
  fits <- analysis$fits
  info <- plyr::ldply(analysis$model_info, .id='modelid')
  samples <- lapply(fits, function(x) 
    rstan::extract(x, pars = c('sum_log_lik_fit','sum_log_lik_heldout')))
  
  res <- plyr::ldply(lapply(samples, function(x) {
    gather(data.frame(x),'likelihood','estimate')}), .id='modelid')
  
  left_join(info, res, 'modelid') %>%
    select(-modelid)
}

# Extract log likelihood samples for multiple analyses.
extract_multi_analysis_loglik_samples <- function(list_of_analyses){
  samples <- lapply(list_of_analyses, extract_loglik_samples)
  plyr::ldply(samples, .id='model') %>%
    select(-model)
}
# Summarise log likelihood samples
summarise_loglik_samples <- function(samples) {
  samples %>%
    group_by(analysis,growth_measure,rho_combo, likelihood) %>%
    summarise(mean = mean(estimate),
              `2.5%` = quantile(estimate, 0.025),
              `10%` = quantile(estimate, 0.1),
              `50%` = quantile(estimate, 0.5),
              `90%` = quantile(estimate, 0.9),
              `97.5%` = quantile(estimate, 0.975))
}
