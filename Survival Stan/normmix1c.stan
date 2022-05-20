// copied from section 5.3 of stan users guide
// Modified for left and right censoring
data {
  int<lower=1> K;          // number of mixture components
  int<lower=1> N;          // number of data points
  real y[N];               // observations
  int<lower=0> N_right;    // number right censored
  real y_right[N_right];   // right censored observations
  int<lower=0> N_left;     // number left censored
  real y_left[N_left];     // left censored observations
}
parameters {
  simplex[K] theta;          // mixing proportions
  ordered[K] mu;             // locations of mixture components
  vector<lower=0>[K] sigma;  // scales of mixture components
}
model {
  vector[K] log_theta = log(theta);  // cache log calculation
  sigma ~ lognormal(0, 2);
  mu ~ normal(0, 10);
  for (n in 1:N) {  // uncensored
    vector[K] lps = log_theta;
    for (k in 1:K)
      lps[k] += normal_lpdf(y[n] | mu[k], sigma[k]);
    target += log_sum_exp(lps);
  }
  for (n in 1:N_right) {  // right censored
    vector[K] lps = log_theta;
    for (k in 1:K)
      lps[k] += normal_lccdf(y_right[n] | mu[k], sigma[k]);
    target += log_sum_exp(lps);
  }
  for (n in 1:N_left) {  // left censored
    vector[K] lps = log_theta;
    for (k in 1:K)
      lps[k] += normal_lcdf(y_left[n] | mu[k], sigma[k]);
    target += log_sum_exp(lps);
  }
}

