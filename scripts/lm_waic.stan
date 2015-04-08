# This is the simple linear regression model from p.165 of Bayesian Data Analysis (3rd edition)
# with Waic as calculated on p.177.

# This code illustrates the calculation of Waic in Stan:  we define the log likelihood as a vector
# in the transformed parameters block, then sum it up in the model block.  The output for the N # # # components of the vector "loglik" are combined in the waic() function in R.

data {
  int N;
  int K;
  vector[N] y;
  matrix[N,K] X;
}
parameters {
  vector[K] b;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] <- normal_log (y[n], X[n]*b, sigma);
  }
}
model {
  increment_log_prob (-log(sigma));    # log prior for p(sigma) proportional to 1/sigma
  increment_log_prob (sum (log_lik));   # log likelihood
}
