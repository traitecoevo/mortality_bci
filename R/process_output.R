## Process output functions
# Note most of these functions require stan_functions.R to be sourced

# Extracts optimization estimates for dbh at each time
extract_true_dbh_estimates <- function(optimization_results) {
  fit <- optimization_results$par
  base::data.frame(fit, nm=names(fit)) %>%
    tidyr::separate(nm, c('variable', 'ind'), sep='\\[', fill='right') %>%
    dplyr::mutate(ind=sub('\\]', '', ind)) %>%
    dplyr::filter(variable %in% c('true_dbh1','true_dbh2','true_growth_rate')) %>%
    tidyr::spread(variable, fit) %>%
    dplyr::mutate(ind = as.integer(ind)) %>%
    dplyr::arrange(ind)
}

# Compile chains for multiple model comparisons
compile_models <- function(comparison) {
  if(length(comparison) == 1) {
    compile_chains(comparison)
  }
  else {
    sapply(comparison, function(x) compile_chains(x), simplify = FALSE)
  }
}

# Sub-function to compile_models for our workflow
compile_chains <- function(comparison) {
  
  check_task_is_allowed(comparison)
  
  tasks <- tasks_2_run(comparison)
  sets <- base::split(tasks,  list(tasks$comparison,tasks$model,tasks$growth_measure, tasks$rho_combo, tasks$gap_combo, tasks$size_combo, tasks$kfold), sep='_', drop=TRUE)
  pars <- lapply(sets,  function(s) s[1, c("comparison", "model", "growth_measure", "rho_combo", "gap_combo", "size_combo", "kfold")])
  fits <- lapply(sets, function(s) combine_stan_chains(s[['filename']]))
  list(model_info=pars, fits=fits)
}

# sub function for compile_chains combines chains for a given model
combine_stan_chains <- function(files) {
  rstan::sflist2stanfit(lapply(files, readRDS))
}

# Diagnostics summary function for multiple model comparisons
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

# Sub function for model diagnostics
diagnose <- function(model) {
  fits <- model$fits
  info <- model$model_info
  out1 <- dplyr::bind_rows(lapply(fits, function(x) {
    summary_model <- summary(x)$summary
    sampler_params <- get_sampler_params(x, inc_warmup=FALSE)
    data.frame(
      min_n_eff = min(summary_model[, 'n_eff']),
      max_rhat = max(summary_model[, 'Rhat']),
      n_bad_rhat = length(which(summary_model[, 'Rhat'] > 1.1)),
      n_divergent = sum(sapply(sampler_params, function(y) y[,'divergent__'])),
      max_treedepth = max(sapply(sampler_params, function(y) y[,'treedepth__'])))
  }))
  
  out2 <- suppressWarnings(bind_rows(lapply(info, function(x) {
    data.frame(
      comparison = x$comparison,
      model = x$model,
      growth_measure = x$growth_measure,
      rho_combo = x$rho_combo,
      gap_combo = x$gap_combo,
      size_combo = x$size_combo,
      kfold = as.integer(x$kfold))
  })))
  
  res <- cbind(out2,out1) %>%
    dplyr::arrange(comparison,model,growth_measure,rho_combo, gap_combo, size_combo, kfold)
  
  row.names(res) <- NULL
  return(res)
}

# Combines all cross val logloss outputs.
combine_logloss_summaries <- function() {
  logloss_null_model <- summarise_crossval_logloss("null_model")
  logloss_func_growth <- summarise_crossval_logloss("function_growth_comparison")
  logloss_rho_comparisons <- summarise_crossval_logloss("rho_combinations")
  logloss_gap_comparisons <- summarise_crossval_logloss("gap_combinations")
  logloss_size_comparisons <- summarise_crossval_logloss("size_combinations")
  logloss_multi_trait_all <- summarise_crossval_logloss("multi_trait_all")
  logloss_rho_gap_all <- summarise_crossval_logloss("rho_gap_all")
  logloss_rho_size_all <- summarise_crossval_logloss("rho_size_all")
  logloss_gap_size_all <- summarise_crossval_logloss("gap_size_all")
  logloss_multi_trait_parsimony <- summarise_crossval_logloss("multi_trait_parsimony")
  logloss_re_comparison <- summarise_crossval_logloss("species_random_effects")
  
  vars <- c("growth_hazard_a", "growth_hazard_b", "growth_hazard_ab", "growth_hazard_c", "growth_hazard_ac", "growth_hazard_bc", "growth_hazard_abc")
  
  logloss_func_growth %>%
    dplyr::bind_rows(list(
      logloss_null_model,
      logloss_rho_comparisons,
      logloss_gap_comparisons, 
      logloss_size_comparisons,
      logloss_multi_trait_all,
      logloss_rho_gap_all,
      logloss_rho_size_all,
      logloss_gap_size_all,
      logloss_multi_trait_parsimony,
      logloss_re_comparison)
    ) %>%
    dplyr::arrange(comparison, growth_measure) %>%
    # remove duplicate / redundant fits
    dplyr::filter(!(
      # unnecessary model base_hazard fit
      (model == "base_hazard" & growth_measure =='true_basal_area_dt') |
        # Already included in logloss_func_growth
        (comparison =='rho_combinations'  &   rho_combo=='none') |
        (comparison =='gap_combinations'  &   gap_combo=='none') |
        (comparison =='size_combinations' &  size_combo=='none')
    )
    ) %>%
    dplyr::mutate(
      # No growth for base model
      growth_measure = base::replace(growth_measure, model %in% c("null_model", "base_hazard"), "none"), 
      # rename for plotting purposes
      model = base::replace(model, model=="base_growth_hazard_re", "base_growth_hazard")
    ) %>%
    dplyr::mutate(
      # paste together strings as identifier for model
      model_type = paste(sep='_', comparison, model,
                         ifelse(comparison == "gap_combinations", gap_combo,
                                ifelse(comparison == "size_combinations", size_combo,
                                       ifelse(comparison == "rho_combinations",rho_combo, 
                                              ifelse(comparison %in% c("function_growth_comparison","species_random_effects", "null_model"), "none",
                                                     ifelse(comparison == "multi_trait_all", "rho_gap_size_abc",
                                                            ifelse(comparison == "rho_gap_all", "rho_gap_abc",
                                                                   ifelse(comparison == "rho_size_all", "rho_size_abc",
                                                                          ifelse(comparison == "gap_size_all", "gap_size_abc", "rho_gap_c"))))))))),
      # make factors with specified order
      model = factor(model, levels=c('null_model','base_hazard','growth_hazard','base_growth_hazard')),
      growth_measure = factor(growth_measure, levels=c('none','true_basal_area_dt','true_dbh_dt')),
      model_type = factor(model_type, levels=
                            c("null_model_null_model_none",
                              sprintf("function_growth_comparison_%s", c("base_hazard_none", "growth_hazard_none", "base_growth_hazard_none")),
                              sprintf("rho_combinations_base_%s", vars),
                              sprintf("gap_combinations_base_%s", vars),
                              sprintf("size_combinations_base_%s", vars),
                              "multi_trait_all_base_growth_hazard_rho_gap_size_abc",
                              "rho_gap_all_base_growth_hazard_rho_gap_abc",
                              "rho_size_all_base_growth_hazard_rho_size_abc",
                              "rho_gap_all_base_growth_hazard_gap_size_abc",
                              "gap_size_all_base_growth_hazard_gap_size_abc",
                              "species_random_effects_base_growth_hazard_none",
                              "multi_trait_parsimony_base_growth_hazard_rho_gap_c")),
      modelid = as.factor(as.numeric(model_type))
    ) %>%
    dplyr::arrange(modelid) %>%
    dplyr::select(modelid, model_type, comparison, model,
                  growth_measure, rho_combo, gap_combo, size_combo, logloss,
                  mean, st_err, ci, `2.5%`,`97.5%`)
}


# Summarise log loss samples
summarise_crossval_logloss <- function(comparison) {
  models <- compile_models(comparison)
  # We don't make an explicit target of compiled models because of
  # a lack of support for long vectors in digest (remake issue #76)
  samples <- extract_logloss_samples(models)
  samples %>%
    dplyr::group_by(comparison, model, growth_measure, rho_combo,gap_combo,size_combo,kfold, logloss) %>%
    dplyr::summarise(kfold_logloss = mean(estimate)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(comparison, model, growth_measure, rho_combo,gap_combo,size_combo, logloss) %>%
    dplyr::summarise(mean = mean(kfold_logloss),
                     st_err = sd(kfold_logloss)/sqrt(n())) %>%
    dplyr::mutate(ci = 1.96 * st_err,
                  `2.5%` = mean - ci,
                  `97.5%` = mean + ci) %>%
    dplyr::ungroup()
}

# Extract log loss samples for multiple model comparisons.
extract_logloss_samples <- function(model) {
  if(is.null(model$fits)) { #Check to see if object is multi model 
    samples <- lapply(model, logloss_samples)
    dplyr::bind_rows(samples, .id='modelid') %>%
      dplyr::select(-modelid)
    
  }
  else { 
    logloss_samples(model)
  }
}

# sub function for extract_logloss_samples
logloss_samples <- function(model) {
  fits <- model$fits
  info <- dplyr::bind_rows(model$model_info, .id='modelid')
  samples <- lapply(fits, function(x) 
    rstan::extract(x, pars = grep('logloss', slot(x, 'model_pars'), value=TRUE)))
  
  res <- lapply(samples, function(x) {
    tidyr::gather(data.frame(x),'logloss','estimate')}) %>% dplyr::bind_rows(., .id='modelid')
  
  dplyr::left_join(info, res, 'modelid') %>%
    dplyr::select(-modelid)
}

# get times of chain runs
get_times <- function(comparison) {
  fits <- comparison$fits
  info <- dplyr::bind_rows(comparison$model_info, .id='modelid')
  times <- lapply(fits, function(x)
    rstan::get_elapsed_time(x))
  
  res <- lapply(times, function(x) {
    tidyr::gather(data.frame(x),'warmup','sample')}) %>% dplyr::bind_rows(., .id='modelid')
  
  left_join(info, res, 'modelid') %>%
    dplyr::select(-modelid) %>%
    dplyr::mutate(total_hours = ((warmup + sample)/3600))
}

# summarise chain times by model group
summarise_times <- function(times) {
  res <- times %>%
    dplyr::group_by(comparison, model, growth_measure, rho_combo) %>%
    dplyr::summarise(mn = median(total_hours))
  return(res)
}

# Summarise hyper parameters
summarise_hyper_params <- function(model, data) {
  fit <- model$fits[[1]]
  dat <- prep_full_data_for_stan(data)
  samples <- rstan::extract(fit, pars=c("mu_log_alpha","mu_log_beta","mu_log_gamma",
                                        "sigma_log_alpha", "sigma_log_beta", "sigma_log_gamma"))
  
  lapply(samples, function(x) {
    cbind.data.frame(
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      t(quantile(x, c(0.025,0.975))))
  })
}

# Summarise posteriors of species level parameters
summarise_spp_params <- function(model, data, logscale=FALSE) {
  fit <- model$fits[[1]]
  dat <- prep_full_data_for_stan(data)
  samples <- rstan::extract(fit, pars=c("alpha","beta","gamma"))
  samples[["alpha_gamma"]] <- samples$alpha + samples$gamma
  
  if(logscale==TRUE) {
    res <-lapply(samples, function(x) {
      cbind.data.frame(
        species = dat$species,
        sp = dat$sp,
        wood_density = dat$raw_rho,
        mean = apply(log(x),2, mean),
        median = apply(log(x),2, median),
        sd = apply(log(x),2,sd),
        aperm(apply(log(x),2, quantile, c(0.025,0.975)), c(2,1)))
    })
    names(res) <- paste0('log_', names(res))
    return(res)
  }
  else {
    lapply(samples, function(x) {
      cbind.data.frame(
        species = dat$species,
        sp = dat$sp,
        wood_density = dat$raw_rho,
        mean = apply(x,2, mean),
        median = apply(x,2, median),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, c(0.025,0.975)), c(2,1)))
    })
  }
}

# Predict hazard rates/ annual mortality rates for each species
predict_spp_hazard <- function(model, data, growth_range = c(0.03,0.5)) {
  
  spp_parameters <- summarise_spp_params(model, data)
  growth_rates <- base::data.frame(dbh_growth = seq(min(growth_range),max(growth_range),length.out = 100), 
                                   dbh_growth_centered = seq(min(growth_range),max(growth_range),length.out = 100) - 0.172)
  
  res <- spp_parameters %>%
    dplyr::bind_rows(.,.id='paramater') %>%
    dplyr::select(sp,wood_density, paramater, mean) %>%
    tidyr::spread(paramater, mean) %>%
    base::merge(growth_rates) %>%
    dplyr::mutate(
      inst_hazard = alpha * exp(-beta * dbh_growth_centered) + gamma,
      annual_prob_mort = 1- exp(-(alpha * exp(-beta * dbh_growth_centered) + gamma)))
}


# Predict hazard rates for median species
predict_mu_hazards <- function(model,wood_density=c(0.3,0.8), growth_range = c(0.03,0.5), hazard_curve = TRUE) {
  fit <- model$fits[[1]]
  samples <- rstan::extract(fit, pars=c("mu_log_alpha","mu_log_beta","mu_log_gamma","c1"))
  samples <- base::as.data.frame(lapply(samples, as.vector)) 
  covariates <- base::data.frame(type = as.factor(rep(c('low','high'), each = 100)),
                                 dbh_growth = rep(seq(min(growth_range),max(growth_range),length.out = 100),2), 
                                 dbh_growth_centered = rep(seq(min(growth_range),max(growth_range),length.out = 100) - 0.172,2),
                                 wood_density = rep(wood_density, each = 100),
                                 wood_density_centered = rep(wood_density/0.6, each = 100))
  
  output <-samples %>% # only propogate uncertainty in wood density effect
    base::merge(covariates) %>%
    dplyr::mutate(mu_alpha = mean(exp(mu_log_alpha)),
                  mu_beta = mean(exp(mu_log_beta)),
                  mu_gamma = mean(exp(mu_log_gamma))) %>%
    dplyr::mutate(
      inst_hazard = mu_alpha * exp(-mu_beta * dbh_growth_centered) + mu_gamma * wood_density_centered^c1,
      annual_prob_mort = 1-exp(-(mu_alpha * exp(-mu_beta * dbh_growth_centered) + mu_gamma * wood_density_centered^c1))) %>%
    dplyr::group_by(type, dbh_growth, wood_density)
  
  if(hazard_curve == FALSE) { 
    output %>%
      dplyr::summarise(mean = mean(annual_prob_mort),
                       `2.5%` = quantile(annual_prob_mort,0.025),
                       `97.5%` = quantile(annual_prob_mort, 0.975)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(type, dbh_growth)
  }
  else {
    output %>%
      dplyr::summarise(mean = mean(inst_hazard),
                       `2.5%` = quantile(inst_hazard,0.025),
                       `97.5%` = quantile(inst_hazard, 0.975)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(type, dbh_growth)
  }
}

# Predict median baseline hazard
predict_mu_baseline_hazard_by_rho <- function(model,data) {
  wood_density <- range(data$rho)
  fit <- model$fits[[1]]
  samples <- rstan::extract(fit, pars=c("mu_log_gamma","c1"))
  samples <- base::as.data.frame(lapply(samples, as.vector)) 
  covariates <- base::data.frame(wood_density = seq(min(wood_density),max(wood_density),length.out = 100),
                                 wood_density_centered = seq(min(wood_density),max(wood_density),length.out = 100)/0.6)
  
  output <-samples %>%
    base::merge(covariates) %>%
    dplyr::mutate(inst_hazard = exp(mu_log_gamma) * wood_density_centered^c1) %>%
    dplyr::group_by(wood_density) %>%
    dplyr::summarise(mean = mean(inst_hazard),
                     `2.5%` = quantile(inst_hazard,0.025),
                     `97.5%` = quantile(inst_hazard, 0.975)) %>%
    dplyr::ungroup()
}

# Predict observations outside stan
predict_observations <- function(model, data) {
  spp_effects <- summarise_spp_params(model, data)
  wd_effects <- median(rstan::extract(model$fits[[1]], pars=c('c1'))$c1)
  census_error <- base::as.data.frame(rstan::extract(model$fits[[1]], pars=c('census_err'))$census_err) %>%
    dplyr::summarise_all(dplyr::funs(median)) %>%
    tidyr::gather('censusid','census_err') %>%
    dplyr::mutate(censusid =c(1,2,3))
  
  dplyr::bind_rows(spp, .id='parameter') %>%
    dplyr::select(parameter,sp, wood_density, median) %>%
    tidyr::spread(parameter, median) %>%
    base::merge(data, by.all=sp) %>%
    base::merge(census_error, by.all=censusid) %>%
    dplyr::mutate(wd_effects = wd_effects) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hazard_rate = (alpha * exp(-beta * (true_dbh_dt - 0.172)) + gamma*(rho/0.6)^wd_effects)*census_err,
                  prob_death = 1-exp(-census_interval * (alpha * exp(-beta * (true_dbh_dt - 0.172)) + gamma * (rho/0.6)^wd_effects)*census_err)) %>%
    dplyr::mutate(logloss = logloss(dead_next_census,prob_death))
}

# Calculate proportion of model variance explained by each parameter
get_param_variance_explained <- function(model, data) {
  
  # Sum of squares
  sum_squares <- function(x) {
    sum((x-mean(x))^2)
  }
  # Extract species random effects
  spp_effects <- summarise_spp_params(model, data)
  
  # Extract wood density effect
  c1 <- mean(rstan::extract(model$fits[[1]], pars=c('c1'))$c1)
  # Extract hyper parameters
  mu_effects <- rstan::extract(model$fits[[1]], pars=c('mu_log_alpha', 'mu_log_beta','mu_log_gamma'))
  mu_effects <- base::as.data.frame(lapply(mu_effects,as.vector)) %>% dplyr::summarise_all(dplyr::funs(mean))
  
  # Extract census effects
  census_error <- as.data.frame(rstan::extract(model$fits[[1]], pars=c('census_err'))$census_err) %>%
    dplyr::summarise_all(dplyr::funs(mean)) %>%
    tidyr::gather('censusid','census_err') %>%
    dplyr::mutate(censusid =c(1,2,3)) # To match with data
  
  
  # function to predict probability of death for different levels of effects and time interval dt
  mort <- function(growth_indepenent, growth_depenent, census, dt = 1) {
    1-exp(-dt * ((growth_indepenent + growth_depenent)*census))
  }
  
  effects <- dplyr::bind_rows(spp_effects, .id='parameter') %>%
    dplyr::select(parameter,sp, wood_density, mean) %>%
    tidyr::spread(parameter, mean) %>%
    base::merge(data, by.all=sp) %>%
    base::merge(census_error, by.all=censusid) %>%
    base::merge(mu_effects) %>%
    dplyr::mutate(
      c1 = c1,
      rho_c = rho/0.6, # centers mean to what models used
      true_dbh_dt_c = true_dbh_dt - 0.172 # centers growth to what models used
    ) %>%
    # Now calculate predicted 1-yr mortality removing different effects included
    dplyr::mutate(
      full_model = mort( alpha * exp(-beta * true_dbh_dt_c), gamma, census_err ),
      full_minus_census = mort( alpha * exp(-beta * true_dbh_dt_c), gamma, mean(census_err) ),
      full_minus_rho = mort( alpha * exp(-beta * true_dbh_dt_c), gamma/(rho_c^c1), census_err ),
      full_minus_spp = mort( median( alpha) * exp(-median(beta) * true_dbh_dt_c), median(gamma), census_err ),
      full_minus_growthdep = mort(median( alpha * exp(-beta * true_dbh_dt_c)), gamma, census_err),
      full_minus_growthindep =  mort( alpha * exp(-beta * true_dbh_dt_c), median(gamma), census_err )
    ) %>%
    dplyr::select(
      full_model, 
      census = full_minus_census, 
      wood_density = full_minus_rho, 
      species = full_minus_spp, 
      growth_dependent = full_minus_growthdep, 
      growth_independent = full_minus_growthindep
    ) %>%
    # sum of squares & proportion variance explained for each model
    dplyr::summarise_all(dplyr::funs(sum_squares)) %>%
    tidyr::gather(param, SS, - full_model) %>%
    dplyr::mutate(proportion = 1- (SS/full_model))
  
}

# Merge estimated model parameters with other covariates
merge_spp_params_covs <- function(spp_params,recruit_gap_conditions, raw_plot_data) {
  suppressWarnings(lapply(spp_params, function(x) {
    dplyr::left_join(x, get_spp_dbh_95(raw_plot_data), by ='sp') %>%
      dplyr::left_join(get_mean_spp_gap_index(recruit_gap_conditions), by = 'sp') %>%
      dplyr::select(species,sp,wood_density, dbh_95, gap_index, mean, median, sd, `2.5%`,`97.5%`)
  }))
}


# Create table of hyper parameter estimates
hyperparam_table <- function(model) {
  
  param1 <- base::as.data.frame(
    rstan::summary(model$fits[[1]], 
                   c('mu_log_alpha',
                     "mu_log_beta",
                     "mu_log_gamma"))$summary) %>%
    dplyr::mutate(Parameter = row.names(.)) %>%
    dplyr::select(Parameter, mean, `2.5%`, `97.5%`) %>%
    dplyr::mutate_at(vars(-Parameter), dplyr::funs(exp))
  
  param2 <- base::as.data.frame(rstan::summary(model$fits[[1]], 'c1')$summary) %>%
    dplyr::mutate(Parameter = row.names(.)) %>%
    dplyr::select(Parameter, mean, `2.5%`, `97.5%`)
  
  param3 <- base::as.data.frame(rstan::summary(model$fits[[1]], 'census_err')$summary) %>%
    dplyr::mutate(Parameter = row.names(.)) %>%
    dplyr::select(Parameter, mean, `2.5%`, `97.5%`)
  
  x <- dplyr::bind_rows(param1,param2, param3) %>%
    dplyr::select(Parameter, "Geometric mean" = mean, `2.5%`, `97.5%`) %>%
    dplyr::mutate_at(dplyr::vars(-Parameter), dplyr::funs(round(.,4))) %>%
    dplyr::mutate(Parameter = factor(Parameter, 
                                     levels = c("census_err[3]",
                                                "census_err[2]",
                                                "census_err[1]",
                                                "c1",
                                                "mu_log_gamma",
                                                "mu_log_beta",
                                                "mu_log_alpha"),
                                     labels = c("$\\delta_3$",
                                                "$\\delta_2$",
                                                "$\\delta_1$",
                                                "Wood density ($\\rho$)",
                                                "$\\gamma$",
                                                "$\\beta$",
                                                "$\\alpha$")))
  
  Hmisc::latex(x, file = "", booktabs = TRUE, rowname = NULL,  
               colnamesTexCmd="bfseries",
               col.just = c(rep("c", 4)), label = "table:1",
               where = "!h",
               caption ="Hyper parameters and census effects estimated from full model")
}
