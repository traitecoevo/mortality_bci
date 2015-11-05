# packages <- c("rstan")
# sources <- c("R/model.R",
#              "R/stan_functions.R",
#              "R/utils.R")
# 
# for (p in packages) {
#   library(p, character.only=TRUE, quietly=TRUE)
# }
# for (s in sources) {
#   source(s)
# }

# Merge chains related to a given model/kfold combination
combine_stan_chains <- function(files) {
   sflist2stanfit(lapply(files, readRDS))
}

# compile all models related to a given analyses
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
  tasks <- tasks_2_run(analysis, growth_measure = growth_measure)
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
kfold_model_diagnostics <- function(models) {
  fits <- models$fits
  info <- models$model_info
  out1 <- do.call(rbind, lapply(fits, function(x) {
    summary_model <- summary(x)$summary
    sampler_params <- get_sampler_params(x, inc_warmup=FALSE)
    data.frame(
      min_n_eff = min(summary_model[, 'n_eff']),
      max_rhat = max(summary_model[, 'Rhat']),
      n_bad_rhat = length(which(summary_model[, 'Rhat'] > 1.1)),
      n_divergent = sum(sapply(sampler_params, function(y) y[,'n_divergent__'])),
      max_treedepth = max(sapply(sampler_params, function(y) y[,'treedepth__'])))
  }))
  
  out2 <- do.call(rbind, lapply(info, function(x) {
    data.frame(
      analysis = x$analysis,
      growth_measure = x$growth_measure,
      rho_combo = x$rho_combo,
      kfold = as.integer(x$kfold)
    )
    }))
  res <- cbind(out2,out1) %>%
    arrange(analysis, growth_measure, rho_combo, kfold)
  
  row.names(res) <- NULL
  return(res)
}

# Examine model diagnostics for multiple analysis
multi_analysis_kfold_diagnostics <- function(list_of_analyses) {
  out <- do.call(rbind, lapply(list_of_analyses, function(x) {
    kfold_model_diagnostics(x)}))
  row.names(out) <- NULL
  return(out)
}

extract_samples <- function(models,subset_pars=NULL) {
  fits <- models$fits
  info <- models$model_info
  samples <- lapply(fits, function(x) {
    if(is.null(subset_pars)) {
      extract(x)
    } else {
      extract(x, pars = subset_pars)
    }
  })
  list(model_info = info, model_samples = samples)
}

multi_analysis_extract <- function(list_of_analyses, subset_pars=NULL) {
  lapply(list_of_analyses, extract_samples)
}

summarise_samples <- function(samples, quant = c(0.025,0.5,0.975)) {
  
  lapply(samples$model_samples, function(x) {
    cbind.data.frame(
      mn = sapply(x, mean),
      std = sapply(x, sd),
      t(sapply(x, quantile, quant)))
  })
}

multi_analysis_summarise_samples <- function(multi_analysis_samples, quant = c(0.025,0.5,0.975)) {
  lapply(multi_analysis_samples, summarise_samples)
}

# Merge kfolds based on unique analysis, growth and rho combination.
