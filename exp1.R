
options(mc.cores = 32L, mc.preschedule=FALSE)
library(parallel)

remake::create_bindings()

base_dir <- "results"

# function to run model given set of pars
run_model <- function(pars) {
	chunks <- str_eval(sprintf("get_chunks_model%d_%s()", pars$model, pars$effect ))

	capture_output2(
		run_single_stan_chain(
			make_stan_model(chunks, growth_measure=pars$growth_measure),
			get(pars$data)$train,
			chain_id=pars$chain, iter=pars$iter),
		name = pars$name, divert_messages=TRUE)
}

# First experiment - run model 3 species on training testing data, for different growth measures
pars_exp1_df <- expand.grid(iter = 10, stringsAsFactors = FALSE,
							experiment = "exp1",
							chain=1:3,
							model=3,
							effect=c("species"),
							growth_measure = c("dbh_dt", "dbh_dt_rel" , "basal_area_dt" ,"basal_area_dt_rel"),
							data=  sprintf("BCI_training_test_%d",1:10))
pars_exp1_df$name = sprintf("%s/exp1/%d", base_dir, seq_len(nrow(pars_exp1_df)))

## turn that into a list
all_pars <- split(pars_exp1_df, rownames(pars_exp1_df))

out <- mclapply(all_pars, run_model)
