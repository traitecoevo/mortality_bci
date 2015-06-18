#Before running code first run make() if data folds have not yet been compiled.

data <-readRDS('export/bci_data_1.rds')
data$train <- data$train[1:100,]
prep_data_for_stan <- function(data, growth_measure) {
  
  list(
    n_obs = nrow(data$train),
    n_spp = length(unique(data$train$sp)),
    spp = as.numeric(factor(data$train$sp)),
    census_length = data$train$census_interval,
    obs_gr =  data$train[[growth_measure]],
    dbh = data$train$dbh)
}


stan_data <- prep_data_for_stan(data, 'dbh_dt')
model <- '
  data {
    int<lower=1> n_obs;
    int<lower=1> n_spp;
    int<lower=1> spp[n_obs];
    vector[n_obs] census_length;
    vector[n_obs] obs_gr;
    vector[n_obs] dbh;
  }

parameters { 
  real<lower=0> true_gr_sigma[n_spp];
  real log_pred_mu;
  real<lower=0> log_pred_gr_sigma;
  real raw_true_gr[n_obs];
  real raw_log_pred[n_obs];
}
transformed parameters {
real<lower=0> true_gr[n_obs];
real log_pred[n_obs];
for(i in 1:n_obs) {
log_pred[i] <- raw_log_pred[i] * log_pred_gr_sigma + log_pred_mu;
true_gr[i] <- exp(raw_true_gr[i] * true_gr_sigma[spp[i]] + log_pred[i]);
}
}
model {
  log_pred_gr_sigma ~ cauchy(0, 2.5);
  for (s in 1:n_spp) {
    true_gr_sigma[s] ~ cauchy(0, 2.5);
  } 
  for (i in 1:n_obs) { # growth rate observation error outlined in Ruger et al 2011.
    raw_log_pred[i] ~ normal(0,1);
    raw_true_gr[i] ~ normal(0,1);
    increment_log_prob(log_sum_exp(log(0.9724) + normal_log(obs_gr[i], true_gr[i], (0.000927 + 0.0000038 * dbh[i])/census_length[i]),
                                   log(0.0276) + normal_log(obs_gr[i],true_gr[i], (0.0256/census_length[i]))));
  }
}'

gr_mod3 <- stan(model_code = model, data = stan_data, pars = c('true_gr','true_gr_sigma','log_pred_mu', 'raw_true_gr'), chains = 3, iter = 2000, control=list(adapt_delta=0.9))
gr_mod2 <- sflist2stanfit(sflist <-mclapply(1:3, mc.cores=3, function(i) stan(model_code = model, 
                                           data = stan_data, 
                                           pars = c('true_gr'), chains = 1, chain_id=i,
                                           iter = 2000, control=list(adapt_delta=0.9))))
         
x <- summary(gr_mod3, 'true_gr')$summary
plot(stan_data$obs_gr ~ as.vector(x[,'mean']))
