data {
  int<lower=0> nsamples;
  int<lower=0> p;
  int y[nsamples];
  matrix[nsamples, p] X; // X does not include the intercept term in first column
  vector[p+1] prior_means;
  vector[p+1] prior_variances;
  int C;
}
parameters {
  vector[p+1] beta; // beta_{0} is intercept term
}
transformed parameters  {
  real log_lambda[nsamples];
  for (i in 1:nsamples) {
    log_lambda[i] = beta[1] + X[i]*beta[2:];
  }
}
model {
  y ~ poisson_log(log_lambda);
  beta ~ normal(prior_means, sqrt(C*prior_variances));
}
