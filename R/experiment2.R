exp2_pars <- function(iter=2000, name="exp2") {
  chains <- 3
  model <- 3
  effects <- c("trait_species")
  growth_measures <- c("dbh_dt", "dbh_dt_rel", "basal_area_dt",
                       "basal_area_dt_rel")
  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)

  ret$train_data <- "export/bci_data_full.rds"

  ret
}
