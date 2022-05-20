data {
  int<lower=0> N;
  real<lower=0> y[N];
  real<lower=0> trnc;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}
model {
  alpha ~ exponential(0.001); // priors
  beta  ~ exponential(0.001);
  
  for(i in 1:N) {
    y[i] ~ gamma(alpha, beta) T[trnc,];
  }
}
generated quantities {
  real mu;
 
  mu=alpha/beta;
}
