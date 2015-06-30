##SIMULATION

n_obs <-2000
dbh <- rlnorm(n_obs, 1.103296, 0.7891315) # in cm
sda <- 0.5
sdb <- 0.004
SD2 <- 2.5
theta <- 0.03
sim <- c(rnorm(n_obs*(1-theta), 0, sda + sdb*dbh), rnorm(n_obs*theta, 0, SD2))
###FULL MODEL - As outlined in Ruger et al. 2011 and Chave et al 2004.
# Note: Identifiability issues arise when using uninformative priors
# Priors are based on expectation (in cm) - theta should be low and size effect will be small

model_full <- '
data {
int<lower=1> n_obs;
vector[n_obs] sim;
vector[n_obs] dbh;
}
parameters {
real<lower=0, upper=0.2> theta;
positive_ordered[3] sigma; # sort sigmas from smallest to largest - helps with switching issues
}
model {
sigma[1] ~ uniform(0, 1);
sigma[2] ~ uniform(0, 10);
sigma[3] ~ uniform(0,100);
for (i in 1:n_obs) {
   increment_log_prob(log_sum_exp(log1m(theta) + normal_log(sim[i], 0, sigma[2] + sigma[1] * dbh[i]), #most of data
                                  log(theta) + normal_log(sim[i], 0, sigma[3])));  #few data
 }
}'


### Model without size dependence
# Less sensitive to priors

#SIMULATED DATA
n_obs <-1000
dbh <- rlnorm(n_obs, 1.103296, 0.7891315) # in cm
SD1 <- 0.5
SD2 <- 2.5
theta <- 0.03

sim <- c(rnorm(n_obs*(1-theta), 0, SD1), rnorm(n_obs*theta, 0, SD2))
model_nosize <- '
data {
int<lower=1> n_obs;
vector[n_obs] sim;
vector[n_obs] dbh;
}
parameters { 
positive_ordered[2] sigma;
real<lower=0, upper=0.1> theta;
}
model {
sigma[1] ~ cauchy(0,1);
sigma[2] ~ cauchy(0,5);
  for (i in 1:n_obs) {
    increment_log_prob(log_sum_exp(log1m(theta) + normal_log(sim[i], 0,sigma[1]),
                                   log(theta) + normal_log(sim[i], 0, sigma[2])));  
  }
}'

#RUN SIMULATED MODELS

full_mod_sim <- stan(model_code = model_full, data= list(n_obs=n_obs, dbh=dbh, sim=sim),
                      pars = c('sigma','theta'), chains = 3, iter = 2000, control=list(adapt_delta=0.9))
print(full_mod_sim, digits=6)

nosize_mod_sim <- stan(model_code = model_nosize, data = list(n_obs=n_obs, dbh=dbh, sim=sim), 
            pars = c('sigma','theta'), chains = 3, iter = 2000, control=list(adapt_delta=0.9))
print(nosize_mod_sim, digits=6)
# RUN DATA MODELS

load('data/goodremeasure.rdata') # supplied directly from Ruger. 
# Need to do a bit of cleaning & MYSQL work if we wish to use the open source.

remeasure$dbh1 <- remeasure$dbh1/10 # convert mm to cm
remeasure$dbh2 <- remeasure$dbh2/10 # convert mm to cm
#remeasure$best <- remeasure$best/10 # convert mm to cm
remeasure$discrep <- remeasure$dbh1 - remeasure$dbh2 #assuming no growth in this period.



full_mod_real <- stan(model_code = model_full, data= list(n_obs=nrow(remeasure), dbh=remeasure$dbh1, sim=remeasure$discrep),
                      pars = c('sigma','theta'), chains = 3, iter = 2000, control=list(adapt_delta=0.9))

print(full_mod_real, digits=6)
nosize_mod_real <- stan(model_code = model_nosize, data= list(n_obs=nrow(remeasure), dbh=remeasure$best, sim=remeasure$discrep),
                      pars = c('sigma','theta'), chains = 3, iter = 2000, control=list(adapt_delta=0.9))
print(nosize_mod_real, digits=6)
