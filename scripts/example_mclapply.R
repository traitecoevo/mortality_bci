
# Illustartes one technique for running series of mdoels via mclapply

options(mc.cores = 32L)
library(parallel)

remake::create_bindings()

str_eval <- function(x) {eval(parse(text=x))}

# dataframe of parameters to run
all_pars_df <- expand.grid(iter = 10, stringsAsFactors = FALSE,
							chain=1:3,
							model=1:3,
							effect=c("constant", "trait", "species", "trait_species"),
							data="stan_data")

## turn that into a list
all_pars <- split(all_pars_df, rownames(all_pars_df))

## function to run model given set of pars
run_model <- function(pars) {
	chunks <- str_eval(sprintf("get_chunks_model%d_%s()", pars$model, pars$effect ))
	run_single_stan_chain(make_stan_model(chunks), get(pars$data), chain_id=pars$chain, iter=pars$iter)
}

out <- mclapply(all_pars, run_model)
