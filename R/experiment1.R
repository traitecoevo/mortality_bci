exp1_pars_test <- function(iter=10, name="test") {
  chains <- 1
  model <- seq_len(3)
  effects <- c("constant","species","trait", "trait_species")
  growth_measures <- c("dbh_dt")
  ndata <- 1

  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     data=seq_len(ndata),
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$train_data <- sprintf("export/bci_data_%s.rds", ret$data)
  ret
}

exp1_pars <- function(iter=2000, name="exp1") {
  chains <- 3
  model <- 3
  effects <- c("species")
  growth_measures <- c("dbh_dt", "dbh_dt_rel", "basal_area_dt",
                       "basal_area_dt_rel")
  ndata <- 10

  ret <- expand.grid(experiment=name,
                     iter=iter,
                     chain=seq_len(chains),
                     model=model,
                     effect=effects,
                     growth_measure=growth_measures,
                     data=seq_len(ndata),
                     stringsAsFactors=FALSE)
  ret$jobid <- seq_len(nrow(ret))
  ret$filename <- sprintf("results/%s/%d.rds", name, ret$jobid)
  ret$train_data <- sprintf("export/bci_data_%s.rds", ret$data)
  ret
}
