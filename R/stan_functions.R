
create_dirs <- function(pars_list) {
  tmp <- lapply(pars_list, function(x) dir.create(dirname(x$filename), FALSE, TRUE))
}

train_model <- function(pars) {
  data <- readRDS(pars$fold_data)

  ## Assemble the stan model:
  chunks <- get_chunks_for_model(pars)
  model <- make_stan_model(chunks, growth_measure=pars$growth_measure)

  ## Actually run the model
  res <- run_single_stan_chain(model, data,
                               chain_id=pars$chain,
                               iter=pars$iter)

  ## The model output is large so instead of returning it we'll just
  ## dump into a file.
  saveRDS(res, pars$filename)
  pars$filename
}

combine_stan_chains <- function(..., d=list(...), tmp=NULL) {
	sflist2stanfit(d)
 }

run_single_stan_chain <- function(model, data, chain_id, iter=1000,
  sample_file=NA, diagnostic_file=NA) {

  data_for_stan <- prep_data_for_stan(data, model$growth_measure)
	stan(model_code = model$model_code,
       data = data_for_stan,
       pars = model$pars,
       iter = iter,
       chains=1, chain_id=chain_id,
       refresh=100,
       sample_file=sample_file,
	     control = list(stepsize=0.01, adapt_delta=0.9),
       diagnostic_file=diagnostic_file)
}

prep_data_for_stan <- function(data, growth_measure = "dbh_dt") {

  list(
    n_obs = nrow(data$train),
    n_spp = length(unique(data$train$sp)),
    spp = as.numeric(factor(data$train$sp)),
    n_census = length(unique(data$train$censusid)),
    census = as.numeric(factor(data$train$censusid)),
    y = as.integer(data$train$dead_next_census),
    census_length = data$train$census_interval,
    growth_dt =  data$train[[growth_measure]],
    log_rho_c  = (log(unique(data$train$rho)) - log(600)),
    n_obs_test = nrow(data$test),
    n_spp_test = length(unique(data$test$sp)),
    spp_test = as.numeric(factor(data$test$sp)),
    census_test = as.numeric(factor(data$test$censusid)),
    y_test = as.integer(data$test$dead_next_census),
    census_length_test = data$test$census_interval,
    growth_dt_test =  data$test[[growth_measure]],
    log_rho_c_test  = (log(unique(data$test$rho)) - log(600))
    )
}


make_stan_model <- function(chunks, growth_measure= "dbh_dt", rho_effect_on = c('a','b','c')) {
	list(
		pars = chunks$pars,
    growth_measure = growth_measure,
		model_code = sprintf('
data {
  int<lower=1> n_obs;
  int<lower=1> n_spp;
  int<lower=1> spp[n_obs];
  int<lower=1> n_census;
  int<lower=1> census[n_obs];
  int<lower=0, upper=1> y[n_obs];
  vector[n_obs] census_length;
  vector[n_obs] growth_dt;
  vector[n_obs] log_rho_c;
  
  // Held out data
  int<lower=1> n_obs_test;
  int<lower=1 n_spp_test;
  int<lower=1> spp_test[n_obs_test];
  int<lower=1> census_test[n_obs_test];
  int<lower=0, upper=1> y_test[n_obs_test];
  vector[n_obs_test] census_length_test;
  vector[n_obs_test] growth_dt_test;
	vector[n_spp_test] log_rho_c_test;
}

parameters { // assumes uniform priors on all parameters
  %s
}

model {
  %s
}

generated quantities {
  %s
}
', chunks$parameters, chunks$model, chunks$generated_quantities)
  )
}

# define accessory functions - equivalenets to those used in stan
inv_cloglog <- function(x) {1 - exp(-exp(x))}

bernoulli_log <- function(y, p) {
  dbinom(y, size=1, p, log = TRUE)
}
