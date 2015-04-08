# R script for Waic example.  Also needed in this directory:  data file hibbs.dat and Stan file lm_waic.stan

# Little function to calculate posterior variances from simulation
colVars <- function (a){
  diff <- a - matrix (colMeans(a), nrow(a), ncol(a), byrow=TRUE)
  vars <- colMeans (diff^2)*nrow(a)/(nrow(a)-1)
  return (vars)
}

# The calculation of Waic!  Returns lppd, p_waic_1, p_waic_2, and waic, which we define
# as 2*(lppd - p_waic_2), as recommmended in BDA
waic <- function (stanfit){
  log_lik <- extract (stanfit, "log_lik")$log_lik
  lppd <- sum (log (colMeans(exp(log_lik))))
  p_waic_1 <- 2*sum (log(colMeans(exp(log_lik))) - colMeans(log_lik))
  p_waic_2 <- sum (colVars(log_lik))
  waic_2 <- -2*lppd + 2*p_waic_2
  return (list (waic=waic_2, p_waic=p_waic_2, lppd=lppd, p_waic_1=p_waic_1))
}

# Read in and prepare the data 
hibbs <- read.table ("hibbs.dat", header=TRUE)
year <- hibbs[,1]
growth <- hibbs[,2]
vote <- hibbs[,3]
inc <- hibbs[,4]
other <- hibbs[,5]
y <- vote
N <- length (y)
X <- cbind (rep(1,N), growth)
K <- ncol (X)

# Fit the model in Stan!
library ("rstan")
lm_waic <- stan (file="lm_waic.stan", data=c("N","K","X","y"), iter = 2000, chains = 4)
print (lm_waic)

# Calculate and print Waic
print (waic (lm_waic))
