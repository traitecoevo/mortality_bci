# Kfold tasks
kfold_tasks <- function(comparison,iter=4000,path='.') {
  n_kfolds = 10
  n_chains = 3
  switch(comparison,
         "null_model" = {
           growth_measure <- "true_dbh_dt"
           rho_combo <- "none"
           model  <- "base_hazard"
         }, 
         "function_growth_comparison" = {
           growth_measure <- c("true_dbh_dt",'true_basal_area_dt')
           rho_combo <- "none"
           model  <- c("base_hazard","growth_hazard","base_growth_hazard")
         }, 
         "species_random_effects" = {
           growth_measure <- c("true_dbh_dt")
           rho_combo <- "none"
           model  <- "base_growth_hazard_re"
         },
         "rho_combinations" = {
           growth_measure <- 'true_dbh_dt'
           rho_combo <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
           rho_combo <- sapply(split(rho_combo, seq_len(nrow(rho_combo))), function(x) paste0(x, collapse=''))
           rho_combo[rho_combo==''] <- "none"
           model <- "base_growth_hazard"
         },
         stop('comparison can only be one of the following: 
                      "function_growth_comparison","species_random_effects","rho_combinations"')
         )
  ret <- expand.grid(comparison=comparison,
                     model = model,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measure,
                     rho_combo=rho_combo,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE) %>%
    arrange(model, growth_measure)
  
  ret$modelid <- rep(1:nrow(unique(ret[,c('comparison','model','growth_measure','rho_combo','kfold')])),each = n_chains)
  ret <- ret %>%
    mutate(jobid = seq_len(n()),
           filename = sprintf("%s/results/chain_fits/%s/%d.rds", path, comparison, jobid),
           fold_data = sprintf("%s/precompile/kfold_data/bci_data_%s.rds", path, kfold))
  return(ret)
}

# Full data tasks
full_data_tasks <- function(model, iter=4000,path='.') {
  n_chains = 3
  switch(model,
         "final_model" = {
           rho_combo = "c"
           comparison = 'final_model'
         },
         "final_base_growth_hazard_re" = {
           rho_combo = "none"
           comparison = "final_base_growth_hazard_re"
         }, 
         stop("Model must be either 'final_model' or 'final_base_growth_hazard_re'"))
         
         ret <- expand.grid(comparison=comparison,
                            model=model,
                            iter=iter,
                            chain=seq_len(n_chains),
                            growth_measure= "true_dbh_dt",
                            rho_combo= rho_combo,
                            kfold=0,
                            stringsAsFactors=FALSE)
         
         ret$modelid <- rep(1:nrow(unique(ret[,c('comparison','model','growth_measure','rho_combo','kfold')])),each = n_chains)
         ret <- ret %>%
           mutate(jobid = seq_len(n()),
                  filename = sprintf("%s/results/chain_fits/%s/%d.rds", path, model, jobid),
                  fold_data = sprintf("%s/precompile/bci_data_full.rds", path))
         return(ret)
}

# Wrapper function to allow for both full data and kfold tasks
tasks_2_run <- function(comparison,iter=4000,path='.') {
  if(comparison %in% c("final_model","final_base_growth_hazard_re")) {
    full_data_tasks(comparison, iter, path)
  } 
  else {
    kfold_tasks(comparison, iter, path)
  }
}

# Compiles models for clusterous
model_compiler <- function(task) {
  data <- readRDS(task$fold_data)
  dir.create(dirname(task$filename), FALSE, TRUE)
  model <- task$model
  ## Assemble the stan model:
  switch(model, 
         "null_model"= {
           chunks <- get_model_chunks_null(task)
           chunks$crossval <- TRUE
         },
         "base_hazard"= {
           chunks <- get_model_chunks_base_haz(task)
           chunks$crossval <- TRUE
         },
         "base_hazard_re"= {
           chunks <- get_model_chunks_base_haz_re(task)
           chunks$crossval <- TRUE
         },
         "growth_hazard"= {
           chunks <- get_model_chunks_growth_haz(task)
           chunks$crossval <- TRUE
         },
         "growth_hazard_re"= {
           chunks <- get_model_chunks_growth_haz_re(task)
           chunks$crossval <- TRUE
         },
         "base_growth_hazard"= {
           chunks <- get_model_chunks_base_growth_haz(task)
           chunks$crossval <- TRUE
         },
         "base_growth_hazard_re"= {
           chunks <- get_model_chunks_base_growth_haz_re(task)
           chunks$crossval <- TRUE
         },
         'final_model'= {
           chunks <- get_final_model_chunks(task)
           chunks$crossval <- FALSE
         },
         "final_base_growth_hazard_re"= {
           chunks <- get_model_chunks_base_growth_haz_re(task, TRUE)
           chunks$crossval <- FALSE
         })
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
run_single_stan_chain <- function(model, data, chain_id, iter=4000,
                                  sample_file=NA, diagnostic_file=NA) {
  data_for_stan <- prep_data_for_stan(data, model$growth_measure, model$crossval)
  stan(model_code = model$model_code,
       fit = model$fit,
       data = data_for_stan,
       pars = model$pars,
       iter = iter,
       chains=1,
       seed = 12345,
       chain_id=chain_id,
       control =list(stepsize=0.1, adapt_delta=0.99, max_treedepth=15))
}

# Prepares kfold data for use with stan
prep_kfold_data_for_stan <- function(data, growth_measure) {
  switch(growth_measure,
         'true_dbh_dt' = {
           growth_dt <- data$train$true_dbh_dt - 0.172
           growth_dt_heldout = data$heldout[[growth_measure]] - 0.172
         },
         'true_basal_area_dt' = {
           growth_dt <- data$train$true_basal_area_dt - 0.338
           growth_dt_heldout = data$heldout$true_basal_area_dt - 0.338
         })
  list(
    n_obs = nrow(data$train),
    n_census = max(data$train$censusid),
    n_spp = max(data$train$sp_id),
    census = data$train$censusid,
    spp = data$train$sp_id,
    census_length = data$train$census_interval,
    growth_dt = growth_dt,
    rho_c  = unique(data$train$rho)/0.6,
    y = as.integer(data$train$dead_next_census),
    n_obs_heldout = nrow(data$heldout),
    n_census_heldout = max(data$heldout$censusid),
    n_spp_heldout = max(data$heldout$sp_id),
    census_heldout = data$heldout$censusid,
    spp_heldout = data$heldout$sp_id,
    census_length_heldout = data$heldout$census_interval,
    growth_dt_heldout = growth_dt_heldout,
    rho_c_heldout = unique(data$heldout$rho/0.6),
    y_heldout = as.integer(data$heldout$dead_next_census)
  )
}

# Prepares full dataset for use with stan
prep_full_data_for_stan <- function(data) {
  list(
    n_obs = nrow(data),
    n_census = max(data$censusid),
    n_spp = max(data$sp_id),
    census = data$censusid,
    spp = data$sp_id,
    census_length = data$census_interval,
    growth_dt = data$true_dbh_dt - 0.172,
    rho_c  = unique(data$rho)/0.6,
    y = as.integer(data$dead_next_census),
    sp = unique(data$sp),
    raw_rho = unique(data$rho))
}

# Wrapper function to call either kfold or full data for stan
prep_data_for_stan <- function(data, growth_measure, crossval) {
  if(crossval==TRUE) {
    prep_kfold_data_for_stan(data, growth_measure)
  }
  else {
    prep_full_data_for_stan(data)
  }
}

# Builds the model code
make_stan_model <- function(chunks) {
  list(
    crossval = chunks$crossval,
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
  model <- task$model
  ## Assemble the stan model:
  switch(model, 
         "null_model"= {
           chunks <- get_model_chunks_null(task)
           chunks$crossval <- TRUE
         },
         "base_hazard"= {
           chunks <- get_model_chunks_base_haz(task)
           chunks$crossval <- TRUE
         },
         "base_hazard_re"= {
           chunks <- get_model_chunks_base_haz_re(task)
           chunks$crossval <- TRUE
         },
         "growth_hazard"= {
           chunks <- get_model_chunks_growth_haz(task)
           chunks$crossval <- TRUE
         },
         "growth_hazard_re"= {
           chunks <- get_model_chunks_growth_haz_re(task)
           chunks$crossval <- TRUE
         },
         "base_growth_hazard"= {
           chunks <- get_model_chunks_base_growth_haz(task)
           chunks$crossval <- TRUE
         },
         "base_growth_hazard_re"= {
           chunks <- get_model_chunks_base_growth_haz_re(task)
           chunks$crossval <- TRUE
         },
         'final_model'= {
           chunks <- get_final_model_chunks(task)
           chunks$crossval <- FALSE
         },
         "final_base_growth_hazard_re"= {
           chunks <- get_model_chunks_base_growth_haz_re(task, TRUE)
           chunks$crossval <- FALSE
         })
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
precompile_crossval_models <- function() {
  # Null model
  stage1 <- tasks_2_run(comparison = 'null_model',iter = 10)
  vapply(df_to_list(stage1), precompile, character(1))
  
  # Functional form/growth comparison
  stage2 <- tasks_2_run(comparison = 'function_growth_comparison',iter = 10)
  vapply(df_to_list(stage2), precompile, character(1))
  
  #Constant vs species random effects
  stage3 <- tasks_2_run(comparison = 'species_random_effects',iter = 10)
  vapply(df_to_list(stage3), precompile, character(1))
  
  #Rho combinations
  stage4 <- tasks_2_run(comparison = 'rho_combinations',iter = 10)
  vapply(df_to_list(stage4), precompile, character(1))
}

precompile_fulldata_models <- function() {
#Full data RE only
  stage4 <- tasks_2_run(comparison = 'final_base_growth_hazard_re',iter = 10)
  vapply(df_to_list(stage4), precompile, character(1))
  
  #Final model
  stage5 <- tasks_2_run(comparison = 'final_model',iter = 10)
  vapply(df_to_list(stage5), precompile, character(1))
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
  file.path("precompile/precompiled_models/", name)
}

precompile_docker <- function(docker_image, crossval = TRUE) {
  if (FALSE) {
    ## Little trick to depend on the appropriate functions (this will
    ## be picked up by remake's dependency detection, but never run).
    precompile_crossval_models()
    precompile_fulldata_models()
  }
  if (crossval == TRUE) {
    cmd <- '"remake::dump_environment(verbose=FALSE, allow_missing_packages=TRUE); precompile_crossval_models()"'
    }
  else {
    cmd <- '"remake::dump_environment(verbose=FALSE, allow_missing_packages=TRUE); precompile_fulldata_models()"'
    }
  x<- 3
  unlink(precompile_model_path(), recursive=TRUE)
  dockertest::launch(name=docker_image,
                     filename="docker/dockertest.yml",
                     args=c("r", "-e", cmd))
}

