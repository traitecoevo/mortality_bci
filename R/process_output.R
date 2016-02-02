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
  
  left_join(info, res, 'modelid')
}

# Extract log likelihood samples for multiple analyses.
extract_multi_analysis_loglik_samples <- function(list_of_analyses){
  samples <- lapply(list_of_analyses, extract_loglik_samples)
  plyr::ldply(samples, .id='model')
}
# Summarise log likelihood samples
summarise_loglik_samples <- function(samples) {
  samples %>%
    group_by(modelid, model, analysis,growth_measure,rho_combo,kfold, likelihood) %>%
    summarise(klpd = mean(log_sum_exp(estimate))) %>%
    ungroup() %>%
    group_by(model, analysis, growth_measure, rho_combo, likelihood) %>%
    summarise(elpd = mean(klpd),
              st_err = sd(klpd)/sqrt(n())) %>%
    mutate(ci = 1.96 * st_err,
           upper_ci = elpd + ci,
           lower_ci = elpd - ci) %>%
    ungroup()
}

coefficent_plot_theme <- function() {
  theme_classic() + theme(axis.title.x = element_text(face = "bold", size = 12),
                          axis.title.y = element_blank(),
                          axis.text.x = element_text(size = 10),
                          axis.text.y = element_text(size = 12),
                          plot.title = element_text(size = 18),
                          title = element_text(face = "bold"),
                          panel.margin = unit(4,"mm"))
}

test2 <-subset(test, model %in% c('rho_combinations','no_gamma_model', 'null_model') & likelihood =='sum_log_lik_heldout' & rho_combo=='' & growth_measure=='true_dbh_dt')

plot_functional_form_comparison <- function(log_likelihood_summary) {
  dat <- filter(log_likelihood_summary, model %in% c('null_model','no_gamma_model', 
                                                     'rho_combinations', 'null_model_random_effects', 
                                                     'no_gamma_model_random_effects', 'growth_comparison') & 
                  likelihood =='sum_log_lik_heldout' & 
                  rho_combo=='' & 
                  growth_measure=='true_dbh_dt') %>%
    mutate(model = factor(model, levels=c('null_model','no_gamma_model', 
                                          'rho_combinations', 'null_model_random_effects', 
                                          'no_gamma_model_random_effects', 'growth_comparison'))) %>%
    mutate(model = factor(model, labels = c( "Baseline hazard","Growth hazard","Baseline & growth hazard",
                                             "Baseline hazard (spp)","Growth hazard (spp)",
                                             "Baseline & growth hazard (spp)"))) %>%
    arrange(model)
  
  ggplot(dat, aes(x = elpd,y = model)) + 
    geom_segment(aes(x=lower_ci,y=model, xend=upper_ci, yend=model), size=0.5)+
    geom_point(aes(x=elpd, y=model), size =3) +
    xlab('Log likelihood') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    coefficent_plot_theme()
}

plot_growth_comparison <- function(log_likelihood_summary) {
  dat <- filter(log_likelihood_summary, model =='growth_comparison' & 
                  likelihood =='sum_log_lik_heldout' & 
                  rho_combo=='') %>%
    mutate(growth_measure = factor(growth_measure, levels=c("true_basal_area_dt","true_dbh_dt"))) %>%
    mutate(growth_measure = factor(growth_measure, labels = c("Area growth","Increment growth"))) %>%
    arrange(growth_measure)
  
  ggplot(dat, aes(x = elpd,y = growth_measure)) + 
    geom_segment(aes(x=lower_ci,y=growth_measure, xend=upper_ci, yend=growth_measure), size=0.5)+
    geom_point(aes(x=elpd, y=growth_measure), size =3) +
    xlab('Log likelihood') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    coefficent_plot_theme()
}


plot_rho_comparison <- function(log_likelihood_summary) {
  dat <- filter(log_likelihood_summary, model =='rho_combinations' & 
                  likelihood =='sum_log_lik_heldout') %>%
    mutate(rho_combo = factor(rho_combo, levels=c("abc","bc","ac","ab","c","b","a",""))) %>%
    mutate(rho_combo = factor(rho_combo, labels = c("all parameters","beta & gamma","alpha & gamma",'alpha & beta', "gamma", "beta","alpha","none"))) %>%
    arrange(rho_combo)
  
  ggplot(dat, aes(x = elpd,y = rho_combo)) + 
    geom_segment(aes(x=lower_ci,y=rho_combo, xend=upper_ci, yend=rho_combo), size=0.5)+
    geom_point(aes(x=elpd, y=rho_combo), size =3) +
    xlab('Log likelihood') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    coefficent_plot_theme()
}

