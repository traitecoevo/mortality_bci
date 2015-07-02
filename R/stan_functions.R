create_dirs <- function(task_list) {
  tmp <- lapply(task_list, function(x) dir.create(dirname(x$filename), FALSE, TRUE))
}

model_compiler <- function(tasks) {
  data <- readRDS(tasks$fold_data)
  
  ## Assemble the stan model:
  chunks <- get_model_chunks(tasks)
  model <- make_stan_model(chunks)
  
  ## Actually run the model
  res <- run_single_stan_chain(model, data,
                               chain_id=tasks$chain,
                               iter=tasks$iter)
  
  ## The model output is large so instead of returning it we'll just
  ## dump into a file.
  saveRDS(res, tasks$filename)
  tasks$filename
}

combine_stan_chains <- function(..., d=list(...), tmp=NULL) {
  sflist2stanfit(d)
}

run_single_stan_chain <- function(model, data, chain_id, iter=1000,
                                  sample_file=NA, diagnostic_file=NA) {
  
  data_for_stan <- prep_data_for_stan(data)
  stan(model_code = model$model_code,
       data = data_for_stan,
       pars = model$pars,
       iter = iter,
       chains=1, chain_id=chain_id,
       control =list(adapt_delta=0.9, max_treedepth=15),
       refresh=1,
       sample_file=sample_file,
       diagnostic_file=diagnostic_file)
}

prep_data_for_stan <- function(data) {
  
  list(
    n_obs = nrow(data$train),
    n_spp = length(unique(data$train$sp)),
    spp = as.numeric(factor(data$train$sp)),
    n_census = length(unique(data$train$censusid)),
    census = as.numeric(factor(data$train$censusid)),
    y = as.integer(data$train$dead_next_census),
    census_length = data$train$census_interval,
    obs_dbh1 = data$train$dbh_prev,
    obs_dbh2 = data$train$dbh,
    log_rho_c  = (log(unique(data$train$rho)) - log(0.6)),
    n_obs_heldout = nrow(data$heldout),
    n_spp_heldout = length(unique(data$heldout$sp)),
    spp_heldout = as.numeric(factor(data$heldout$sp)),
    census_heldout = as.numeric(factor(data$heldout$censusid)),
    y_heldout = as.integer(data$heldout$dead_next_census),
    census_length_heldout = data$heldout$census_interval,
    obs_dbh1_heldout = data$heldout$dbh_prev,
    obs_dbh2_heldout = data$heldout$dbh,
    log_rho_c_heldout  = (log(unique(data$heldout$rho)) - log(0.6))
  )
}


make_stan_model <- function(chunks) {
  list(
    pars = chunks$pars,
    model_code = sprintf("
      data {
        int<lower=1> n_obs;
        int<lower=1> n_spp;
        int<lower=1> spp[n_obs];
        int<lower=1> n_census;
        int<lower=1> census[n_obs];
        int<lower=0, upper=1> y[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] obs_dbh1;
        vector[n_obs] obs_dbh2;
        vector[n_spp] log_rho_c;
        
        // Held out data
        int<lower=1> n_obs_heldout;
        int<lower=1> n_spp_heldout;
        int<lower=1> spp_heldout[n_obs_heldout];
        int<lower=1> census_heldout[n_obs_heldout];
        int<lower=0, upper=1> y_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
        vector[n_obs_heldout] obs_dbh1_heldout;
        vector[n_obs_heldout] obs_dbh2_heldout;
        vector[n_spp_heldout] log_rho_c_heldout;
      }
      
      parameters { 
        %s
      }
  
      model {
        %s
      }
      
      generated quantities {
        %s
      }", chunks$parameters, chunks$model, chunks$generated_quantities)
  )
}
