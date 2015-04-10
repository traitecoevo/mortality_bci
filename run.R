
# Illustartes one technique for running series of mdoels via mclapply

options(mc.cores = 32L, mc.preschedule=FALSE)
library(parallel)

remake::create_bindings()

str_eval <- function(x) {eval(parse(text=x))}

# First experiment - run model 3 species on training testing data, for different growth measures
pars_exp1_df <- expand.grid(iter = 2000, stringsAsFactors = FALSE,
							experiment = "exp1",
							chain=1:3,
							model=3,
							effect=c("species"),
							growth_measure = c("dbh_dt", "dbh_dt_rel" , "basal_area_dt" ,"basal_area_dt_rel"),
							data=  sprintf("BCI_training_test_%d",1:10))
pars_exp1_df$name = sprintf("results/exp1/%d", seq_len(nrow(pars_exp1_df)))

# Second experiment - run model 3 species on training testing data, for different growth measures

pars_exp2_df <- expand.grid(iter = 2000, stringsAsFactors = FALSE,
							experiment = "exp2",
							chain=1:3,
							model=1:3,
							effect=c("constant", "trait", "species", "trait_species"),
							growth_measure = c("dbh_dt", "basal_area_dt"),
							data = "BCI_model_dataset_unique")
pars_exp2_df$name = sprintf("results/exp2/%d", seq_len(nrow(pars_exp2_df)))


## bind lists together

all_df <- rbind(pars_exp1_df, pars_exp2_df)

## turn that into a list
all_pars <- split(all_df, rownames(all_df))

## function to run model given set of pars
run_model <- function(pars) {
	chunks <- str_eval(sprintf("get_chunks_model%d_%s()", pars$model, pars$effect ))

	capture_output2(
		run_single_stan_chain(
			make_stan_model(chunks, growth_measure=pars$growth_measure),
			get(pars$data)$train,
			chain_id=pars$chain, iter=pars$iter),
		name = pars$name, divert_messages=TRUE)
}

# pars <- pars_exp1[[43]]
# run_model(pars)
out <- mclapply(all_pars, run_model)
