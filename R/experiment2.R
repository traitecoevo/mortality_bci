exp2_pars <- function(iter=2000) {
  chains <- 3
  model <- 3
  effects <- c("trait_species")
  growth_measures <- c("dbh_dt", "dbh_dt_rel", "basal_area_dt",
                       "basal_area_dt_rel")
  ret <- expand.grid(experiment="exp2",
                     iter=iter,
                     chain=seq_len(chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     stringsAsFactors=FALSE)
  ret$filename <- sprintf("results/exp2/%d.rds", seq_len(nrow(ret)))

  ret
}

exp2_run_model <- function(pars) {
  filename <- "export/bci_data_full.rds"
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
