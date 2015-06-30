data <-readRDS('export/bci_data_1.rds')
data$train <- data$train[1:100,]
prep_data_for_stan <- function(data, growth_measure, size_measure) {
  
  list(
    n_obs = nrow(data$train),
    n_spp = length(unique(data$train$sp)),
    spp = as.numeric(factor(data$train$sp)),
    census_length = data$train$census_interval,
    #obs_gr =  data$train[[growth_measure]],
    obs_dbh1 = data$train$dbh_prev,
    obs_dbh2 = data$train$dbh,
    obs_dbh_g4 = data$train$dbh_dt)
}

#for(i in 1:n_obs)
#true_dbh2[i] <- true_dbh1[i] + diff[i];
#dbh_gr[i] <- (true_dbh2[i] - true_dbh1[i])/census_length[i];
#dbh_gr_rel[i] <- (log(true_dbh2[i]) - log(true_dbh1[i]))/census_length[i];
#basal_gr[i] <- (true_basal2[i] - true_basal1[i])/census_length[i];
#basal_gr_rel[i] <- (log(true_basal2[i]) - log(true_basal1))/census_length[i];
#}

stan_data <- prep_data_for_stan(data, 'dbh_dt', 'dbh')
model <- '
data {
int<lower=1> n_obs;
int<lower=1> n_spp;
int<lower=1> spp[n_obs];
vector[n_obs] census_length;
vector[n_obs] obs_dbh1;
vector[n_obs] obs_dbh2;
#vector[n_obs] size_measure;
}

parameters {
real raw_true_dbh1[n_obs];
real mu_true_dbh1;
real<lower=0> sigma_true_dbh1;

real raw_dbh_increment[n_obs];
real mu_dbh_increment;
real<lower=0> sigma_dbh_increment;
}
transformed parameters {
real<lower=0> true_dbh1[n_obs];
real<lower=0> true_dbh2[n_obs];
real<lower=0> dbh_increment[n_obs];
real<lower=0> dbh_gr[n_obs];
for(i in 1:n_obs) {
true_dbh1[i] <- exp(raw_true_dbh1[i] * sigma_true_dbh1 + mu_true_dbh1);
dbh_increment[i] <- exp(raw_dbh_increment[i] * sigma_dbh_increment + mu_dbh_increment);
true_dbh2[i] <- true_dbh1[i] + dbh_increment[i];
dbh_gr[i] <- (true_dbh2[i] - true_dbh1[i])/census_length[i];
}
}

model {
raw_true_dbh1 ~ normal(0,1);
mu_true_dbh1 ~ normal(0,5);
sigma_true_dbh1 ~ cauchy(0,2.5);

raw_dbh_increment ~ normal(0,1);
mu_dbh_increment ~ normal(0,5);
sigma_dbh_increment ~ cauchy(0,2.5);
  for (i in 1:n_obs) {
    increment_log_prob(log_sum_exp(log1m(0.046908) + normal_log(obs_dbh1[i], true_dbh1[i], 0.085871 + 0.005716 * true_dbh1[i]),
                                   log(0.046908) + normal_log(obs_dbh1[i], true_dbh1[i], 3.496852))); 

    increment_log_prob(log_sum_exp(log1m(0.046908) + normal_log(obs_dbh2[i], true_dbh2[i], 0.085871 + 0.005716 * true_dbh2[i]),
                                   log(0.046908) + normal_log(obs_dbh2[i], true_dbh2[i], 3.496852))); 
  }
}'

test2 <- stan(model_code = model, data= stan_data,
                     pars = c('true_dbh1','dbh_increment', 'dbh_gr','true_dbh2','mu_dbh_increment','sigma_dbh_increment','mu_true_dbh1','sigma_true_dbh1'), 
             chains = 3, iter = 2000, control=list(adapt_delta=0.9, max_treedepth=15))
