# Example script calculating quantities based on fitted model

# Goal: we want to calculate in R the predicted probabilities and log likelihood calculated in stan

remake::create_bindings()
iter <- 100

# Model: 1 with constant effects
chunks <- get_chunks_model1_constant()
model <- make_stan_model(chunks)

# run model in stan
x <- run_single_stan_chain(model, stan_data, chain_id=1, iter=iter)

log_likelihood <- function(stan_data, x, chunks) {

	pars <- as.data.frame(extract(x, chunks$pars))

	# Prob of mortality predicted by model
	# returns a list with elements correpsonding to samples in pars
	p <- 	lapply(seq_len(nrow(pars)), function(i) matrix(chunks$r_model(stan_data, pars[i,,drop=FALSE])))

	# sum of log likeProb of mortality predicted by model
	# returns a list with elements correpsonding to samples in pars

	# calculate sum log likelihood for each parameter set
	# returns a vector with rows correpsonding to rows in pars
	log_lik <- sapply(p, function(x) bernoulli_log(stan_data$y, x))
	log_lik
}


# check likelihood same as in stan
ll 			<- log_likelihood(stan_data, x, chunks)
all(log_lik-extract(x, "lp__")$lp__ < 1e-5)

# likeihood for test data

BCI_model_dataset_census5 <- subset_BCI_data_by_census(BCI_model_dataset_full, census=5)
test_data <- stan_data_BCI(BCI_model_dataset_census5)
ll_test <- log_likelihood(test_data, x, chunks)
