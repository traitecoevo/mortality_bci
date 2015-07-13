run_dbh_error_model <- function(iter=1000) {
dat <- readRDS('data/dbh_error_data.rds')
#supplied directly from Ruger. 
# Need to do a bit of cleaning & MYSQL work if we wish to use the open source.

dat$dbh1 <- dat$dbh1/10 # convert mm to cm
dat$dbh2 <- dat$dbh2/10 # convert mm to cm 
dat$discrep <- dat$dbh1 - dat$dbh2 

create_dirs('results/dbh_error')

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

fit <- sflist2stanfit(mclapply(1:3, mc.cores = 3, 
                   function(i) stan(model_code = dbh_error_model, 
                                    data= stan_data,
                                    pars = c("sigma"), 
                                    chains = 1, chain_id=i, 
                                    iter = iter, 
                                    control=list(stepsize=0.01, adapt_delta=0.99),
                                    refresh=1)))

fit@.MISC <- emptyenv()
saveRDS(fit, 'results/dbh_error/dbh_error_model.rds')
}