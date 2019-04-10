data {
  int<lower=0> I;               // # items (reviews)
  int<lower=0> N;               // # annotations
  int<lower=0> D;               // # of predictors for each item
  matrix[I,D] x;                // [n,d] predictors d for item n
  int<lower=0,upper=1> y[N];    // observed annotations
  int<lower=1> ii[N];           // indicator to which item the n-th annotation belongs
}

parameters {
  vector[D] w;                          // logistic regression coeffs
  real w0;                              // intercept
}

model {
  vector[I] logit_z_hat;
  w ~ normal(0,2);
  w0 ~ normal(0,5);
  
  logit_z_hat = w0 + x * w;
  for(n in 1:N)
    y[n] ~ bernoulli_logit(logit_z_hat[ii[n]]);
}


