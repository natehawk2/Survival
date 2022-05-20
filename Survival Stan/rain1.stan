data {
  int<lower=0> N;
  real<lower=0> y[N];
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}
model {
  alpha ~ exponential(0.001); // priors
  beta  ~ exponential(0.001);
  
  y ~ gamma(alpha, beta);
}
generated quantities {
  real mu;
 
  mu=alpha/beta;
}
