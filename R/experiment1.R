exp1_pars_test <- function(iter=10) {
  chains <- 1
  model <- seq_len(3)
  effects <- c("constant","species","trait", "trait_species")
  growth_measures <- c("dbh_dt")
  ndata <- 1
  
  ret <- expand.grid(experiment="test",
                     iter=iter,
                     chain=seq_len(chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     data=seq_len(ndata),
                     stringsAsFactors=FALSE)
  ret$filename <- sprintf("results/test/%d.rds", seq_len(nrow(ret)))
  
  ret
}

exp1_pars <- function(iter=2000) {
  chains <- 3
  model <- 3
  effects <- c("species")
  growth_measures <- c("dbh_dt", "dbh_dt_rel", "basal_area_dt",
                       "basal_area_dt_rel")
  ndata <- 10

  ret <- expand.grid(experiment="exp1",
                     iter=iter,
                     chain=seq_len(chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     data=seq_len(ndata),
                     stringsAsFactors=FALSE)
  ret$filename <- sprintf("results/exp1/%d.rds", seq_len(nrow(ret)))

  ret
}

exp1_run_model <- function(pars) {
  filename <- sprintf("export/bci_data_%s.rds", pars$data)
  data <- readRDS(filename)$train

  ## Assemble the stan model:
  chunks <- get_chunks(pars$model, pars$effect)
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
