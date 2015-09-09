create_dirs <- function(task_list) {
  tmp <- lapply(task_list, function(x) dir.create(dirname(x$filename), FALSE, TRUE))
}

model_compiler <- function(task) {
  data <- readRDS(task$fold_data)

  ## Make sure the output directory exists
  path <- dirname(task$filename)
  if(!file.exists(path)){
    dir.create(path, TRUE, TRUE)
  }

  ## Assemble the stan model:
  chunks <- get_model_chunks(task)
  model <- make_stan_model(chunks, growth_measure = task$growth_measure)

  ## Bit if a Rube Goldberg machine here as I'm avoiding assuming that
  ## this will always have been done, but possibly it would be neater
  ## to require it.
  filename <- precompile(task, path)
  message("Loading precompiled model")
  model$fit <- readRDS(filename)

  ## Actually run the model
  res <- run_single_stan_chain(model, data,
                               chain_id=task$chain,
                               iter=task$iter)

  ## The model output is large so instead of returning it we'll just
  ## dump into a file.

  saveRDS(res, task$filename)
}

combine_stan_chains <- function(..., d=list(...), tmp=NULL) {
  sflist2stanfit(d)
}

run_single_stan_chain <- function(model, data, chain_id, iter=1000,
                                  sample_file=NA, diagnostic_file=NA) {

  data_for_stan <- prep_data_for_stan(data, model$growth_measure)
  stan(model_code = model$model_code,
       fit = model$fit,
       data = data_for_stan,
       pars = model$pars,
       iter = iter,
       chains=1,
       chain_id=chain_id,
       control =list(stepsize=0.01, max_treedepth=15),
       refresh=1,
       sample_file=sample_file,
       diagnostic_file=diagnostic_file)
}

prep_data_for_stan <- function(data, growth_measure) {
  list(
    n_obs = nrow(data$train),
    n_spp = length(unique(data$train$sp)),
    spp = as.numeric(factor(data$train$sp)),
    y = as.integer(data$train$dead_next_census),
    census_length = data$train$census_interval,
    growth_dt = data$train[[growth_measure]],
    rho_c  = unique(data$train$rho)/0.6,
    n_obs_heldout = nrow(data$heldout),
    n_spp_heldout = length(unique(data$heldout$sp)),
    spp_heldout = as.numeric(factor(data$heldout$sp)),
    y_heldout = as.integer(data$heldout$dead_next_census),
    census_length_heldout = data$heldout$census_interval,
    growth_dt_heldout = data$heldout[[growth_measure]],
    rho_c_heldout  = unique(data$heldout$rho/0.6)
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
        int<lower=0, upper=1> y[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        vector[n_spp] rho_c;

        // Held out data
        int<lower=1> n_obs_heldout;
        int<lower=1> n_spp_heldout;
        int<lower=1> spp_heldout[n_obs_heldout];
        int<lower=0, upper=1> y_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
        vector[n_obs_heldout] growth_dt_heldout;
        vector[n_spp_heldout] rho_c_heldout;
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

precompile <- function(task, path="models") {
  chunks <- get_model_chunks(task)
  model <- make_stan_model(chunks, growth_measure = task$growth_measure)
  sig <- digest::digest(model)
  fmt <- "%s/%s.%s"
  dir.create(path, FALSE, TRUE)
  filename_stan <- sprintf(fmt, path, sig, "stan")
  filename_rds  <- sprintf(fmt, path, sig, "rds")
  if (!file.exists(filename_rds)) {
    message("Compiling model: ", sig)
    writeLines(model$model_code, filename_stan)
    res <- stan(filename_stan, iter=0L)
    message("Ignore the previous error, everything is OK")
    saveRDS(res, filename_rds)
    message("Finished model: ", sig)
  }
  filename_rds
}

precompile_tasks <- function(tasks) {
  invisible(vapply(df_to_list(tasks), precompile, character(1)))
}
