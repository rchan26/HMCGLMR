data {
  int<lower=0> nsamples;
  int<lower=0> p;
  int y[nsamples];
  matrix[nsamples, p] X; // X does not include the intercept term in first column
  vector[p+1] prior_means;
  vector[p+1] prior_variances;
  int C;
  real<lower=0> phi;
}
parameters {
  vector[p+1] beta; // beta_{0} is intercept term
}
model {
  y ~ neg_binomial_2_log_glm(X, beta[1], beta[2:], phi);
  beta ~ normal(prior_means, sqrt(C*prior_variances));
}
