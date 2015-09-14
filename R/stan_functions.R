model_compiler <- function(task) {
  data <- readRDS(task$fold_data)

  ## Make sure the output directory exists; this is important on
  ## clusterous-created clusters because the output directory will not
  ## exist on startup.
  dir.create(dirname(task$filename), FALSE, TRUE)

  ## Assemble the stan model:
  chunks <- get_model_chunks(task)
  model <- make_stan_model(chunks, growth_measure = task$growth_measure)

  ## Bit if a Rube Goldberg machine here as I'm avoiding assuming that
  ## this will always have been done, but possibly it would be neater
  ## to require it.
  filename <- precompile(task)
  message("Loading precompiled model from ", filename)
  model$fit <- readRDS(filename)

  ## Actually run the model
  res <- run_single_stan_chain(model, data,
                               chain_id=task$chain,
                               iter=task$iter)

  ## The model output is large so instead of returning it we'll just
  ## dump into a file.
  saveRDS(res, task$filename)
  task$filename
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

precompile <- function(task) {
  path <- precompile_model_path()
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
