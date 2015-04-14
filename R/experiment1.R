exp1_pars <- function() {
  effects <- c("constant", "trait", "species", "trait_species")
  growth_measures <- c("dbh_dt", "dbh_dt_rel", "basal_area_dt",
                       "basal_area_dt_rel")
  ret <- expand.grid(iter=10,
                     chain=1:3,
                     model=3,
                     effect=effects,
                     growth_measure=growth_measures,
                     data=1:10,
                     stringsAsFactors = FALSE)
  ret$filename <- sprintf("results/exp1/%d.rds", seq_len(nrow(ret)))

  ret
}

exp1_run_model <- function(pars) {
  ## Data have been prepared already.  Loading this in could be done
  ## more efficiently but let's not worry about that for now.
  data <- readRDS("export/bci_data.rds")
  dsub <- data[[pars$data]]

  ## Assemble the stan model:
  chunks <- get_chunks(pars$model, pars$effect)
  model <- make_stan_model(chunks, growth_measure=pars$growth_measure)

  ## Actually run the model
  res <- run_single_stan_chain(model, dsub,
                               chain_id=pars$chain,
                               iter=pars$iter)

  ## The model output is large so instead of returning it we'll just
  ## dump into a file.
  saveRDS(res, pars$filename)
  pars$filename
}
