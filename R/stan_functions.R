# Generic task builder function for clusterous
tasks_2_run <- function(iter, name, growth_measure, rho_combo, tasks_run=tasks_run, path=".") {
  n_kfolds <- 10
  n_chains <- 3
  
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measure,
                     rho_combo=rho_combo,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE)
  ret$modelid <- rep(1:nrow(unique(ret[,c('experiment','growth_measure','rho_combo','kfold')])),each = n_chains)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("%s/results/%s/%d.rds", path, name, ret$jobid)
  ret$fold_data <- sprintf("%s/export/bci_data_%s.rds", path, ret$kfold)
  if(isTRUE(tasks_run)) {
    tasks <- tasks_growth()
    i <- match(do.call(paste, ret[, c('chain', 'growth_measure', 'rho_combo', 'kfold')]),
               do.call(paste, tasks[, c('chain', 'growth_measure', 'rho_combo', 'kfold')]))
    ff <- file.copy(tasks$filename[na.omit(i)], ret$filename[which(!is.na(i))])
    if(!all(ff)) {
      warning(sprintf('Some previously run growth comparison models outputs failed to copy:\n%s',
                      paste(tasks$filename[na.omit(i)][!ff], collapse='\n')))
    }
    return(ret[is.na(i), ]  )
  }
  return(ret)
}

# Function that builds list of growth comparison jobs for clusterous
tasks_growth <- function(iter=1000, name = 'growth_comparison',...) {
  tasks_2_run(iter,
              name=name,
              growth_measure =  c("true_dbh_dt","true_basal_area_dt"),
              rho_combo="",
              tasks_run=FALSE, ...)
}

# Function that builds list of rho combination jobs for clusterous
tasks_rho_combos <- function(iter=1000, growth_measure, name="rho_combinations", tasks_run) {
  rho_combo <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  rho_combo <- sapply(split(rho_combo, seq_len(nrow(rho_combo))), function(x) paste0(x, collapse=''))
  tasks_2_run(iter,
              name=name,
              growth_measure =  growth_measure,
              rho_combo=rho_combo,
              tasks_run=tasks_run)
}

# Compiles models for clusterous
model_compiler <- function(task) {
  data <- readRDS(task$fold_data)
  dir.create(dirname(task$filename), FALSE, TRUE)
  
  ## Assemble the stan model:
  chunks <- get_model_chunks(task)
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
  chunks <- get_model_chunks(task)
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

precompile_all <- function() {
  tasks <- tasks_growth(iter=10)
  vapply(df_to_list(tasks), precompile, character(1))
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

run_full_data_model <- function(iter, growth_measure, rho_combo, data) {
  chunks <- get_model_chunks_full_fit(rho_combo)
  model <- make_stan_model(chunks)
  stan_data <- prep_full_data_for_stan(data, growth_measure)
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- stan(model_code = model$model_code, 
              data= stan_data,
              pars = model$pars, 
              chains = 3,
              iter = iter, 
              control=list(adapt_delta=0.9,stepsize=0.05),
              refresh=1,
              thin=5)
  return(fit)
}
