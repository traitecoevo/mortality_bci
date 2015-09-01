run_dbh_error_model <- function(dat) {
stan_data <- list(
  n_obs = nrow(dat),
  discrep = dat$discrep)

dbh_error_model <- '
data {
int<lower=1> n_obs;
vector[n_obs] discrep;
}
parameters {
real<lower=0> sigma;
}
model {
sigma ~ cauchy(0, 2.5);
discrep ~ normal(0,sigma);
}'

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fit <-stan(model_code = dbh_error_model, 
           data= stan_data,
           pars = c("sigma"), 
           chains = 3, 
           iter = 1000, 
           control=list(stepsize=0.01, adapt_delta=0.99),
           refresh=1)
fit
}