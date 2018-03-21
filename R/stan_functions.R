# FUNCTIONS FOR COMPILING AND DISTRIBUTING STAN MODELS

#### TASK FUNCTIONS ####
# Converts dataframe to list

df_to_list <- function(x) {
  attr(x, "out.attrs") <- NULL # expand.grid leaves this behind.
  unname(lapply(split(x, seq_len(nrow(x))), as.list))
}

# Wrapper function to allow for both full data and kfold tasks
tasks_2_run <- function(comparison,iter=2000,path='.') {
  if(comparison %in% c("final_model","final_base_growth_hazard_re")) {
    full_data_tasks(comparison, iter, path)
  }
  else {
    kfold_tasks(comparison, iter, path)
  }
}

# CROSS VALIDATION
kfold_tasks <- function(comparison,iter=2000,path='.') {
  n_kfolds = 10
  n_chains = 3

  effects_combo <- expand.grid(a=c('','a'), b=c('','b'), c=c('','c'), stringsAsFactors = FALSE)
  effects_combo <- sapply(
            split(effects_combo, seq_len(nrow(effects_combo))),
            function(x) paste0(x, collapse=''))
  effects_combo[effects_combo==''] <- "none"

  check_task_is_allowed(comparison)

  switch(comparison,
         "null_model" = {
           growth_measure <- "true_dbh_dt"
           rho_combo <- "none"
           gap_combo <- "none"
           size_combo <- "none"
           model  <- "null_model"
         },
         "function_growth_comparison" = {
           growth_measure <- c("true_dbh_dt",'true_basal_area_dt')
           rho_combo <- "none"
           gap_combo <- "none"
           size_combo <- "none"
           model  <- c("base_hazard","growth_hazard","base_growth_hazard")
         },
         "species_random_effects" = {
           growth_measure <- c("true_dbh_dt")
           rho_combo <- "none"
           gap_combo <- "none"
           size_combo <- "none"
           model  <- "base_growth_hazard_re"
         },
         "rho_combinations" = {
           growth_measure <- 'true_dbh_dt'
           rho_combo <- effects_combo
           gap_combo <- "none"
           size_combo <- "none"
           model <- "base_growth_hazard"
         },
          "gap_combinations" = {
           growth_measure <- 'true_dbh_dt'
           rho_combo <- "none"
           gap_combo <- effects_combo
           size_combo <- "none"
           model <- "base_growth_hazard"
         },
          "size_combinations" = {
           growth_measure <- 'true_dbh_dt'
           rho_combo <- "none"
           gap_combo <- "none"
           size_combo <- effects_combo
           model <- "base_growth_hazard"
         }, NA
  )
  ret <- expand.grid(comparison=comparison,
                     model = model,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure=growth_measure,
                     rho_combo=rho_combo,
                     gap_combo=gap_combo,
                     size_combo=size_combo,
                     kfold=seq_len(n_kfolds),
                     stringsAsFactors=FALSE) %>%
    arrange(model, growth_measure)

  variations <- unique(ret[,c('comparison','model','growth_measure','rho_combo','gap_combo','size_combo','kfold')])

  ret$modelid <- rep(1:nrow(variations),each = n_chains)
  ret <- ret %>%
    mutate(jobid = seq_len(n()),
           filename = sprintf("%s/results/chain_fits/%s/%d.rds", path, comparison, jobid),
           fold_data = sprintf("%s/data/kfold_data/bci_data_%s.rds", path, kfold))
  return(ret)
}

# FINAL MODEL
full_data_tasks <- function(model, iter=2000,path='.') {
  n_chains = 3
  switch(model,
         "final_model" = {
           rho_combo = "c"
           gap_combo = "none"
           size_combo = "none"
           comparison = 'final_model'
         },
         "final_base_growth_hazard_re" = {
           rho_combo = "none"
           gap_combo = "none"
           size_combo = "none"
           comparison = "final_base_growth_hazard_re"
         },
         stop("Model must be either 'final_model' or 'final_base_growth_hazard_re'"))

  ret <- expand.grid(comparison=comparison,
                     model=model,
                     iter=iter,
                     chain=seq_len(n_chains),
                     growth_measure= "true_dbh_dt",
                     rho_combo= rho_combo,
                     gap_combo=gap_combo,
                     size_combo=size_combo,
                     kfold=0,
                     stringsAsFactors=FALSE)

  variations <- unique(ret[,c('comparison','model','growth_measure','rho_combo','gap_combo','size_combo','kfold')])

  ret$modelid <- rep(1:nrow(variations),each = n_chains)
  ret <- ret %>%
    mutate(jobid = seq_len(n()),
           filename = sprintf("%s/results/chain_fits/%s/%d.rds", path, model, jobid),
           fold_data = sprintf("%s/data/bci_data_full.rds", path))
  return(ret)
}

##### MODEL FUNCTIONS #####
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

  filename <- task$filename %>%
          gsub(".rds", ".stan", .) %>%
          gsub("chain_fits", "stan_models", .)

  dir.create(dirname(filename), FALSE, TRUE)
  writeLines(model$model_code, filename)

  # Actually run the model
  res <- run_single_stan_chain(model, data,
                               chain_id=task$chain,
                               iter=task$iter)
  ## dump into a file.
  saveRDS(res, task$filename)
  task$filename
}

#### DEPENDENCIES ####

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

# Runs single chain (for all models)
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

# Wrapper function to call either kfold or full data for stan
prep_data_for_stan <- function(data, growth_measure, crossval) {
  if(crossval==TRUE) {
    prep_kfold_data_for_stan(data, growth_measure)
  }
  else {
    prep_full_data_for_stan(data)
  }
}

### DATA PREP FUNCTIONS
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

  # find index for first record of each species in each dataset
  # use this below to access traits records for each species
  i <- match(unique(data$train$sp_id), data$train$sp_id)
  h <- match(unique(data$heldout$sp_id), data$heldout$sp_id)

  list(
    n_obs = nrow(data$train),
    n_census = max(data$train$censusid),
    n_spp = max(data$train$sp_id),
    census = data$train$censusid,
    spp = data$train$sp_id,
    census_length = data$train$census_interval,
    growth_dt = growth_dt,
    rho_c  = data$train$rho[i]/0.6,
    gap_index_c  = data$train$gap_index[i]/0.7,
    dbh_95_c  = data$train$dbh_95[i]/15,
    y = as.integer(data$train$dead_next_census),
    n_obs_heldout = nrow(data$heldout),
    n_census_heldout = max(data$heldout$censusid),
    n_spp_heldout = max(data$heldout$sp_id),
    census_heldout = data$heldout$censusid,
    spp_heldout = data$heldout$sp_id,
    census_length_heldout = data$heldout$census_interval,
    growth_dt_heldout = growth_dt_heldout,
    rho_c_heldout = data$heldout$rho[h]/0.6,
    gap_index_c_heldout  = data$heldout$gap_index[h]/0.7,
    dbh_95_c_heldout  = data$heldout$dbh_95[h]/15,
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
    gap_index_c  = unique(data$gap_index)/0.7,
    dbh_95_c  = unique(data$dbh_95)/15,
    y = as.integer(data$dead_next_census),
    species = unique(data$species),
    sp = unique(data$sp),
    raw_rho = unique(data$rho))
}
