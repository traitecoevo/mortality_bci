# Extracts optimization estimates
extract_true_dbh_estimates <- function(optimization_results) {
  fit <- optimization_results$par
  data.frame(fit, nm=names(fit)) %>%
    separate(nm, c('variable', 'ind'), sep='\\[', fill='right') %>%
    mutate(ind=sub('\\]', '', ind)) %>%
    filter(variable %in% c('true_dbh1','true_dbh2','true_growth_rate')) %>%
    spread(variable, fit) %>%
    mutate(ind = as.integer(ind)) %>%
    arrange(ind)
}

# stan's chain merger
combine_stan_chains <- function(files) {
  rstan::sflist2stanfit(lapply(files, readRDS))
}

# sub function to compile chains for our workflow
compile_chains <- function(comparison) {
  if(!comparison %in% c("function_growth_comparison","species_random_effects","rho_combinations","final_model","final_base_growth_hazard_re")) {
    stop('comparison can only be one of the following: 
                      "function_growth_comparison","species_random_effects","rho_combinations","final_model", "final_base_growth_hazard_re"')
  }
  tasks <- tasks_2_run(comparison)
  sets <- split(tasks,  list(tasks$comparison,tasks$model,tasks$growth_measure,tasks$rho_combo,tasks$kfold), sep='_', drop=TRUE)
  pars <- lapply(sets,  function(s) s[1, c("comparison","model","growth_measure","rho_combo","kfold")])
  fits <- lapply(sets, function(s) combine_stan_chains(s[['filename']]))
  list(model_info=pars, fits=fits)
}

# Compile chains for all models
compile_models <- function(comparison) {
  if(length(comparison) == 1) {
    compile_chains(comparison)
  }
  else {
    sapply(comparison, function(x) compile_chains(x), simplify = FALSE)
  }
}

# Diagnostic function
diagnose <- function(model) {
  fits <- model$fits
  info <- model$model_info
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
      comparison = x$comparison,
      model = x$model,
      growth_measure = x$growth_measure,
      rho_combo = x$rho_combo,
      kfold = as.integer(x$kfold))
  })))
  
  res <- cbind(out2,out1) %>%
    arrange(comparison,model,growth_measure,rho_combo,kfold)
  
  row.names(res) <- NULL
  return(res)
}

# Examine model diagnostics for all analysis
model_diagnostics <- function(comparison) {
  model <- compile_models(comparison)
  if(is.null(model$fits)) { #Check to see if object is multi model 
    out <- suppressWarnings(bind_rows(lapply(model, function(x) {
      diagnose(x)})))
    row.names(out) <- NULL
  }
  else {
    out <- diagnose(model)
  }
  return(out)
}

# Extract logloss for single model
logloss_samples <- function(model) {
  fits <- model$fits
  info <- plyr::ldply(model$model_info, .id='modelid')
  samples <- lapply(fits, function(x) 
    rstan::extract(x, pars = grep('logloss', slot(x, 'model_pars'), value=TRUE)))
  
  res <- plyr::ldply(lapply(samples, function(x) {
    tidyr::gather(data.frame(x),'logloss','estimate')}), .id='modelid')
  
  left_join(info, res, 'modelid') %>%
    select(-modelid)
}

# Extract log loss samples for all models.
extract_logloss_samples <- function(model) {
  if(is.null(model$fits)) { #Check to see if object is multi model 
  samples <- lapply(model, logloss_samples)
  plyr::ldply(samples, .id='modelid') %>%
    select(-modelid)
  }
  else { 
    logloss_samples(model)
  }
}

# Summarise log loss samples
summarise_crossval_logloss <- function(comparison) {
  models <- compile_models(comparison)
  # We don't make an explict target of compiled models because of
  # a lack of support for long vectors in digest (remake issue #76)
  samples <- extract_logloss_samples(models)
  samples %>%
    group_by(comparison, model, growth_measure, rho_combo, kfold, logloss) %>%
    summarise(kfold_logloss = mean(estimate)) %>%
    ungroup() %>%
     group_by(comparison, model, growth_measure, rho_combo, logloss) %>%
    summarise(mean = mean(kfold_logloss),
              st_err = sd(kfold_logloss)/sqrt(n())) %>%
    mutate(ci = 1.96 * st_err,
           `2.5%` = mean - ci,
           `97.5%` = mean + ci) %>%
    ungroup()
}

#
get_times <- function(comparison) {
   fits <- comparison$fits
  info <- plyr::ldply(comparison$model_info, .id='modelid')
  times <- lapply(fits, function(x) 
    rstan::get_elapsed_time(x))
  
  res <- plyr::ldply(lapply(times, function(x) {
    tidyr::gather(data.frame(x),'warmup','sample')}), .id='modelid')
  
  left_join(info, res, 'modelid') %>%
    select(-modelid) %>%
    mutate(total_hours = ((warmup + sample)/3600))
}


summarise_times <- function(times) {
  res <- times %>%
    group_by(comparison, model, growth_measure, rho_combo) %>%
    summarise(mn = median(total_hours))
  return(res)
}
