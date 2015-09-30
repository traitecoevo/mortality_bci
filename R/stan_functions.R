# Generic task builder function for clusterous
tasks_2_run <- function(analysis,iter, growth_measure, rho_combo="", path=".") {
  if(!analysis %in% c("null_model","null_model_random_effects",
                      "no_gamma_model", "no_gamma_model_random_effects",
                      "growth_comparison","rho_combinations")) {
    stop("analysis can only be one of the following: 
                      'null_model,'null_model_random_effects',
                      'no_gamma_model', 'no_gamma_model_random_effects',
                      'growth_comparison','rho_combinations'")
  }
  
  if(analysis =="rho_combinations") {
  rho_combo <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  rho_combo <- sapply(split(rho_combo, seq_len(nrow(rho_combo))), function(x) paste0(x, collapse=''))
  }
  
  n_kfolds <- 10
  n_chains <- 3
  
  ret <- expand.grid(analysis=analysis,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measure,
                     rho_combo=rho_combo,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$modelid <- rep(1:nrow(unique(ret[,c('analysis','growth_measure','rho_combo','kfold')])),each = n_chains)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("%s/results/%s/%d.rds", path, analysis, ret$jobid)
  ret$fold_data <- sprintf("%s/export/bci_data_%s.rds", path, ret$kfold)
  return(ret)
}

# Compiles models for clusterous
model_compiler <- function(task) {
  data <- readRDS(task$fold_data)
  dir.create(dirname(task$filename), FALSE, TRUE)
  analysis <- task$analysis
  ## Assemble the stan model:
  if(analysis == "null_model") {
    chunks <- get_model_chunks_null(task)
  }
  if(analysis == "null_model_random_effects") {
    chunks <- get_model_chunks_null_re(task)
  }
  if(analysis == "no_gamma_model") {
    chunks <- get_model_chunks_no_gamma(task)
  }
  if(analysis == "no_gamma_model_random_effects") {
    chunks <- get_model_chunks_no_gamma_re(task)
  }
  if(analysis == "growth_comparison") {
    chunks <- get_model_chunks_growth_comparison(task)
  }
  if(analysis == "rho_combinations") {
    chunks <- get_model_chunks_rho_combinations(task)
  }
  model <- make_stan_model(chunks)
  
  filename <- precompile(task)
  message("Loading precompiled model from ", filename)
  model$fit <- readRDS(filename)
  
  ## Actually run the model
  res <- run_single_stan_chain(model, data,
                               chain_id=task$chain,
                               iter=task$iter)
  ## dump into a file.
  saveRDS(res, task$filename)
  task$filename
}

# Runs single chain
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
# Prepares data for models clusterous jobs
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

# Prepares data for 'best' model fitted to full dataset
prep_full_data_for_stan <- function(data, growth_measure) {
  list(
    n_obs = nrow(data$train),
    n_spp = length(unique(data$train$sp)),
    spp = as.numeric(factor(data$train$sp)),
    y = as.integer(data$train$dead_next_census),
    census_length = data$train$census_interval,
    growth_dt = data$train[[growth_measure]],
    rho_c  = unique(data$train$rho)/0.6
  )
}
# Builds the model code
make_stan_model <- function(chunks) {
  list(
    growth_measure = chunks$growth_measure,
    pars = chunks$pars,
    model_code = sprintf("
      data {
        %s
      }

      parameters {
        %s
      }

      model {
        %s
      }

      generated quantities {
        %s
      }",chunks$data, chunks$parameters, chunks$model, chunks$generated_quantities)
  )
}

# Precompiles model for clustereous
precompile <- function(task) {
  path <- precompile_model_path()
  analysis <- task$analysis
  if(analysis == "null_model") {
    chunks <- get_model_chunks_null(task)
  }
  if(analysis == "null_model_random_effects") {
    chunks <- get_model_chunks_null_re(task)
  }
  if(analysis == "no_gamma_model") {
    chunks <- get_model_chunks_no_gamma(task)
  }
  if(analysis == "no_gamma_model_random_effects") {
    chunks <- get_model_chunks_no_gamma_re(task)
  }
  if(analysis == "growth_comparison") {
    chunks <- get_model_chunks_growth_comparison(task)
  }
  if(analysis == "rho_combinations") {
    chunks <- get_model_chunks_rho_combinations(task)
  }
  model <- make_stan_model(chunks)
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

#THIS ISN'T ELEGANT BUT IT WORKS
precompile_all <- function() {
  #Growth comparison models
  growth_tasks <- tasks_2_run(analysis = 'growth_comparison',iter = 10, 
                              growth_measure = c('true_dbh_dt','true_basal_area_dt'))
  vapply(df_to_list(growth_tasks), precompile, character(1))
  
  #Rho combination models
  rho_tasks <- tasks_2_run(analysis = 'rho_combinations',iter = 10, 
                           growth_measure = 'true_dbh_dt')
  vapply(df_to_list(rho_tasks), precompile, character(1))
  
  #Null model
  null_tasks <- tasks_2_run(analysis = 'null_model',iter = 10, 
                            growth_measure = 'true_dbh_dt')
  vapply(df_to_list(null_tasks), precompile, character(1))
  
  #Null model with random effects
  null_re_tasks <- tasks_2_run(analysis = 'null_model_random_effects',iter = 10, 
                               growth_measure = 'true_dbh_dt')
  vapply(df_to_list(null_re_tasks), precompile, character(1))

  #No gamma model
  no_gamma_tasks <- tasks_2_run(analysis = 'no_gamma_model',iter = 10, 
                                growth_measure = 'true_dbh_dt')
  vapply(df_to_list(no_gamma_tasks), precompile, character(1))
  
  #No gamma model random effect
  no_gamma_re_tasks <- tasks_2_run(analysis = 'no_gamma_model_random_effects',iter = 10, 
                                   growth_measure = 'true_dbh_dt')
  vapply(df_to_list(no_gamma_re_tasks), precompile, character(1))
}

## Wrapper around platform information that will try to determine if
## we're in a container or not.  This means that multiple compiled
## copies of the model can peacefully coexist.
platform <- function() {
  name <- tolower(Sys.info()[["sysname"]])
  if (name == "linux") {
    tmp <- strsplit(readLines("/proc/self/cgroup"), ":", fixed=TRUE)
    if (any(grepl("docker", vapply(tmp, "[[", character(1), 3L)))) {
      name <- "docker"
    }
  }
  name
}

precompile_model_path <- function(name=platform()) {
  file.path("models", name)
}

precompile_docker <- function(docker_image) {
  if (FALSE) {
    ## Little trick to depend on the appropriate functions (this will
    ## be picked up by remake's dependency detection, but never run).
    precompile_all()
  }
  unlink(precompile_model_path("docker"), recursive=TRUE)
  
  cmd <- '"remake::dump_environment(verbose=FALSE, allow_missing_packages=TRUE); precompile_all()"'
  dockertest::launch(name=docker_image$name,
                     filename="docker/dockertest.yml",
                     args=c("r", "-e", cmd))
}

