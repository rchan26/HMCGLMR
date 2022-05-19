data {
  int<lower=0> nsamples;
  int<lower=0> p;
  int y[nsamples];
  matrix[nsamples, p] X; // X does not include the intercept term in first column
  vector[p] prior_means;
  vector[p] prior_variances;
  int C;
  real<lower=0> phi;
}
parameters {
  vector[p] beta; // no intercept term
}
model {
  y ~ neg_binomial_2_log_glm(X, 0, beta, phi);
  beta ~ normal(prior_means, sqrt(C*prior_variances));
}
