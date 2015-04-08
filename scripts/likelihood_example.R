# Example script calculating quantities based on fitted model

# Goal: we want to calculate in R the predicted probabilities and log likelihood calculated in stan

remake::create_bindings()
iter <- 100

# Model: 1 with constant effects
model <- list()
model <- make_stan_model(get_chunks_model1_constant())

model$pars <- c(model$pars,"p")

# define accessoryfunctions
inv_cloglog <- function(x) {1 - exp(-exp(x))}

bernoulli_log <- function(y, p) {
  dbinom(y, size=1, p, log = TRUE)
}

inv_cloglog <- function(x) {1 - exp(-exp(x))}

bernoulli_log <- function(y, p) {
  dbinom(y, size=1, p, log = TRUE)
}

# run model in stan
x <- run_single_stan_chain(model, stan_data, chain_id=1, iter=iter)

# Now extract parameters
fitted <- extract(x)
c0 <- fitted$c0
p_stan <- fitted$p

# Now define R euqivalent for model
prob_mort <- function(c0, census_length) {
	c_log <- c0
	inv_cloglog(log(census_length * (exp(c_log))))
}

# check our calculation of p matches that in stan
all(prob_mort(c0[1], stan_data$census_length) == p_stan[1,])

# make a matrix to match that of stan
p_l <- lapply(c0, prob_mort, census_length=stan_data$census_length)
p <- matrix(unlist(p_l), nrow= nrow(p_stan), ncol=ncol(p_stan), byrow = TRUE)

all(sapply(seq_len(nrow(p)), function(i) all(p[i,] == p_stan[i,])))

# Now check calculations of log likelihood
log_lik_stan <- fitted$lp__

log_lik <- sapply(seq_len(nrow(p)), function(i) sum(bernoulli_log(stan_data$y, p[i,])))
all(log_lik-log_lik_stan < 1e-5)
