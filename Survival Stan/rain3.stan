data {
  int<lower=0> N;
  int<lower=0> N_cens;
  real<lower=0> y[N];
  vector<lower=0>[N_cens] y_cens;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  
  vector<lower=0>[N_cens] y2;
}
transformed parameters {
  vector[N_cens] y3 = y_cens + y2;
}
model {
  alpha ~ exponential(0.001); // priors
  beta  ~ exponential(0.001);

  y ~ gamma(alpha, beta);
  y3 ~ gamma(alpha, beta);
  
}
generated quantities {
  real mu;
 
  mu=alpha/beta;
}
