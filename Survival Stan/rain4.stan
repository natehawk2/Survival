data {
  int<lower=0> N;
  int<lower=0> N_cens;
  real<lower=0> y[N];
  real<lower=0> y_cens[N_cens];
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}
model {
  alpha ~ exponential(0.001); // priors
  beta  ~ exponential(0.001);

  y ~ gamma(alpha, beta);
  
  for(i in 1:N_cens) {
    target += gamma_lccdf(y_cens[i] | alpha, beta);
  }
}
generated quantities {
  real mu;
 
  mu=alpha/beta;
}
