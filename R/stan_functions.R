create_dirs <- function(pars_list) {
  tmp <- lapply(pars_list, function(x) dir.create(dirname(x$filename), FALSE, TRUE))
}

model_compiler <- function(pars) {
  data <- readRDS(pars$fold_data)
  
  ## Assemble the stan model:
  chunks <- get_model_chunks(pars)
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
       control =list(adapt_delta=0.9, max_treedepth=15),
       refresh=100,
       sample_file=sample_file,
       diagnostic_file=diagnostic_file)
}

prep_data_for_stan <- function(data, growth_measure) {
  
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
    n_obs_tilde = nrow(data$heldout),
    n_spp_tilde = length(unique(data$heldout$sp)),
    spp_tilde = as.numeric(factor(data$heldout$sp)),
    census_tilde = as.numeric(factor(data$heldout$censusid)),
    y_tilde = as.integer(data$heldout$dead_next_census),
    census_length_tilde = data$heldout$census_interval,
    growth_dt_tilde =  data$heldout[[growth_measure]],
    log_rho_c_tilde  = (log(unique(data$heldout$rho)) - log(600))
  )
}


make_stan_model <- function(chunks, growth_measure) {
  list(
    pars = chunks$pars,
    growth_measure = growth_measure,
    model_code = sprintf("
      data {
        int<lower=1> n_obs;
        int<lower=1> n_spp;
        int<lower=1> spp[n_obs];
        int<lower=1> n_census;
        int<lower=1> census[n_obs];
        int<lower=0, upper=1> y[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        vector[n_spp] log_rho_c;
        
        // Held out data
        int<lower=1> n_obs_tilde;
        int<lower=1> n_spp_tilde;
        int<lower=1> spp_tilde[n_obs_tilde];
        int<lower=1> census_tilde[n_obs_tilde];
        int<lower=0, upper=1> y_tilde[n_obs_tilde];
        vector[n_obs_tilde] census_length_tilde;
        vector[n_obs_tilde] growth_dt_tilde;
        vector[n_spp_tilde] log_rho_c_tilde;
      }
      
      parameters { 
        %s
      }
      
      transformed parameters {
        %s
      }
      model {
        %s
      }
      
      generated quantities {
        %s
      }", chunks$parameters, chunks$transformed_parameters, chunks$model, chunks$generated_quantities)
  )
}
